#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define STB_DS_IMPLEMENTATION
#include "stb_ds.h"

/** useful shit **/

typedef int8_t int8;
typedef int16_t int16;
typedef int32_t int32;
typedef int64_t int64;
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;
typedef float float32;

#undef assert

#define assert(condition) \
    do { \
    if (!(condition)) { \
        fprintf(stderr, "Assertion failed, line %d: %s\n", __LINE__, #condition); \
        fflush(stderr); \
        abort(); \
    } \
    } while(0)

#define assert_message(condition, message) \
    do { \
    if (!(condition)) { \
        fprintf(stderr, "Assertion failed, line %d: %s\n", __LINE__, #condition); \
        fprintf(stderr, "\t%s\n", message); \
        fflush(stderr); \
        abort(); \
    } \
    } while(0)

#define assert_format(condition, fmt, ...) \
    do { \
    if (!(condition)) { \
        fprintf(stderr, "Assertion failed, line %d: %s\n", __LINE__, #condition); \
        fprintf(stderr, "\t" fmt "\n", __VA_ARGS__); \
        fflush(stderr); \
        abort(); \
    } \
    } while(0)

void dump(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
}

/** .MIS data types **/

#pragma pack(push, 1)

static const char DEADBEEF[4] = {0xDE,0xAD,0xBE,0xEF};

typedef struct LGVector {
    float32 x, y, z;
} LGVector;

typedef struct LGDBVersion {
    uint32 major;
    uint32 minor;
} LGDBVersion;

typedef struct LGDBFileHeader {
    uint32 table_offset;
    LGDBVersion version;
    uint8 pad[256];
    char deadbeef[4];
} LGDBFileHeader;

typedef struct LGDBTOCHeader {
    uint32 entry_count;
} LGDBTOCHeader;

typedef struct LGDBTOCEntry {
    char name[12];
    uint32 offset;
    uint32 data_size;
} LGDBTOCEntry;

typedef struct LGDBChunkHeader {
    char name[12];
    LGDBVersion version;
    uint32 pad;
} LGDBChunkHeader;

typedef struct LGWRHeader {
    uint32 data_size;   // Added in WR version 18
    uint32 cell_count;
} LGWRHeader;

typedef struct LGWREXTHeader {
    uint32 unknown0;
    uint32 unknown1; 
    uint32 unknown2; 
    uint32 lightmap_format; // 0: 16 bit; 1: 32 bit; 2: 32 bit 2x
    int32 lightmap_scale;   // 0: 1x; 2: 2x; 4: 4x; -2: 0.5x; -4: 0.25x
                            // non power of two values may be stored in
                            // here; just ignore all but the highest bit,
                            // and use the sign bit to determine if it
                            // is a multiply or a divide.
    uint32 data_size;   // TODO: not sure exactly what size this is!! supposed to be the size to allocate... but is wrong??
    uint32 cell_count;
} LGWREXTHeader;

typedef struct LGWRCellHeader {
    uint8 num_vertices;
    uint8 num_polys;
    uint8 num_render_polys;
    uint8 num_portal_polys;
    uint8 num_planes;
    uint8 medium;
    uint8 flags;
    int32 portal_vertex_list;   // ??? index in vertices of first portal vertex?
    uint16 num_vlist;           // (obsolete, should be 0) number of vertex-lighting entries
    uint8 num_anim_lights;
    uint8 motion_index;
    LGVector sphere_center;
    float32 sphere_radius;
} LGWRCellHeader;

typedef struct LGWRPoly {
    uint8 flags;
    uint8 num_vertices;
    uint8 planeid;
    uint8 clut_id;
    uint16 destination;
    uint8 motion_index;
    uint8 padding;
} LGWRPoly;

typedef struct LGWRRenderPoly {
    LGVector tex_u;
    LGVector tex_v;
    uint16 u_base;
    uint16 v_base;
    uint8 texture_id;
    uint8 texture_anchor;
    uint16 cached_surface;
    float32 texture_mag;
    LGVector center;
} LGWRRenderPoly;

typedef struct LGWREXTRenderPoly {
    LGVector tex_u;
    LGVector tex_v;
    float32 u_base;         // Changed in WREXT
    float32 v_base;         // Changed in WREXT
    uint16 texture_id;      // Changed in WREXT (texture_anchor removed)
    uint16 cached_surface;  // TODO: jk has this as texture_anchor!
    float32 texture_mag;
    LGVector center;
} LGWREXTRenderPoly;

typedef struct LGWRPlane {
    LGVector normal;
    float32 distance;
} LGWRPlane;

typedef struct LGWRLightMapInfo {
    int16 u_base;
    int16 v_base;
    int16 padded_width;
    uint8 height;
    uint8 width;
    uint32 data_ptr;            // Always zero on disk
    uint32 dynamic_light_ptr;   // Always zero on disk
    uint32 anim_light_bitmask;
} LGWRLightMapInfo;

typedef struct LGWRPortalPlane {
   LGVector normal;
   float32 plane_constant;
} LGWRPortalPlane;

typedef struct LGWRBSPNode {
    union {
        uint32 parent_index; // NOTE: must mask off high byte, `flags`.
        struct {
            uint8 pad1;
            uint8 pad2;
            uint8 pad3;
            uint8 flags;     // NOTE: overlaps high byte of `parent_index`.
        };
    };
    int32 plane_cell_id; // if -1, plane_id is an index into extra planes;
                         // otherwise, it is an index into this cell's planes.
    int32 plane_id;
    union {
        struct { // leaf nodes:
            uint32 cell_id;
            uint32 pad4;
        };
        struct { // non-leaf nodes:
            uint32 inside_index;
            uint32 outside_index;
        };
    };
} LGWRBSPNode;

#pragma pack(pop)

/** Caution: misdeeds ahead! **/

#define FILENAME_SIZE 1024
typedef struct {
    char s[FILENAME_SIZE];
} FileName;

static FileName *filename_copy_str(FileName *dest, const char *src) {
    strncpy(dest->s, src, FILENAME_SIZE);
    assert(dest->s[FILENAME_SIZE-1]==0);
    return dest;
}

static FileName *filename_append_str(FileName *dest, const char *src) {
    assert(strlen(dest->s)<FILENAME_SIZE-strlen(src));
    strcat(dest->s, src);
    return dest;
}

typedef struct DBTagBlockName {
    char s[12];
} DBTagBlockName;

typedef struct DBTagBlock {
    DBTagBlockName key;
    LGDBVersion version;
    uint32 size;
    char *data;
} DBTagBlock;

typedef struct DBFile {
    FileName filename;
    LGDBVersion version;
    DBTagBlock *tagblock_hash;
} DBFile;

static int tag_name_eq(DBTagBlockName name0, DBTagBlockName name1) {
    return (memcmp(&name0, &name1, sizeof(DBTagBlockName))==0);
}

static DBTagBlockName tag_name_from_str(const char *src) {
    DBTagBlockName dest;
    strncpy(dest.s, src, (sizeof dest.s)/(sizeof dest.s[0]));
    return dest;
}

static DBTagBlock *dbfile_get_tag(DBFile *dbfile, const char *name_str) {
    DBTagBlockName name = tag_name_from_str(name_str);
    return hmgetp_null(dbfile->tagblock_hash, name);
}

DBFile *dbfile_load(const char *filename) {
    DBFile *dbfile = calloc(1, sizeof(DBFile));
    filename_copy_str(&(dbfile->filename), filename);

    FILE *file = fopen(filename, "rb");
    #define READ_SIZE(buf, size) \
        do { \
        size_t n = fread(buf, size, 1, file); \
        assert(n==1); \
        } while(0)
    #define READ(var) READ_SIZE(&var, sizeof(var))

    LGDBFileHeader header;
    READ(header);
    assert_message(
        memcmp(header.deadbeef, DEADBEEF, sizeof(DEADBEEF))==0,
        "Wrong file type (missing DEADBEEF)");
    assert_format(
        header.version.major==0 && header.version.minor==1,
        "Unsupported file version %d.%d",
        header.version.major, header.version.minor);
    dbfile->version = header.version;

    fseek(file, header.table_offset, SEEK_SET);
    LGDBTOCHeader toc_header;
    READ(toc_header);

    for (int i=0, iend=(int)toc_header.entry_count; i<iend; ++i) {
        DBTagBlock tagblock;

        LGDBTOCEntry toc_entry;
        READ(toc_entry);
        tagblock.key = tag_name_from_str(toc_entry.name);
        tagblock.size = toc_entry.data_size;
        tagblock.data = NULL;

        LGDBChunkHeader chunk_header;
        uint32 position = (uint32)ftell(file);
        fseek(file, toc_entry.offset, SEEK_SET);
        READ(chunk_header);
        assert(tag_name_eq(tagblock.key, tag_name_from_str(chunk_header.name)));
        tagblock.version = chunk_header.version;
        if (tagblock.size>0) {
            tagblock.data = malloc(tagblock.size);
            READ_SIZE(tagblock.data, tagblock.size);
        }
        fseek(file, position, SEEK_SET);

        hmputs(dbfile->tagblock_hash, tagblock);
    }

    #undef READ
    #undef READ_SIZE
    fclose(file);

    return dbfile;
}

void dbfile_save(DBFile *dbfile, const char *filename) {
    FileName temp_filename = {0};
    filename_copy_str(&temp_filename, filename);
    filename_append_str(&temp_filename, "~tmp");

    FILE *file = fopen(temp_filename.s, "wb");
    #define WRITE_SIZE(buf, size) \
        do { \
        size_t n = fwrite(buf, size, 1, file); \
        assert(n==1); \
        } while(0)
    #define WRITE(var) WRITE_SIZE(&var, sizeof(var))

    LGDBFileHeader header = {0};
    // NOTE: table_offset will be written later.
    header.version = dbfile->version;
    memcpy(header.deadbeef, DEADBEEF, sizeof(DEADBEEF));
    WRITE(header);

    LGDBTOCEntry *toc_array = NULL;
    for (int i=0, iend=(int)hmlen(dbfile->tagblock_hash); i<iend; ++i) {
        DBTagBlock *tagblock = &dbfile->tagblock_hash[i];

        LGDBTOCEntry entry = {0};
        memcpy(entry.name, tagblock->key.s, sizeof(entry.name));
        entry.offset = (uint32)ftell(file);
        entry.data_size = tagblock->size;
        arrput(toc_array, entry);

        LGDBChunkHeader chunk_header = {0};
        memcpy(chunk_header.name, tagblock->key.s, sizeof(chunk_header.name));
        chunk_header.version = tagblock->version;
        WRITE(chunk_header);
        if (tagblock->size>0) {
            WRITE_SIZE(tagblock->data, tagblock->size);
        }
    }

    uint32 toc_offset = (uint32)ftell(file);
    LGDBTOCHeader toc_header = {0};
    toc_header.entry_count = (uint32)arrlen(toc_array);
    WRITE(toc_header);
    for (int i=0, iend=(int)arrlen(toc_array); i<iend; ++i) {
        WRITE(toc_array[i]);
    }
    arrfree(toc_array);

    fseek(file, 0, SEEK_SET);
    WRITE(toc_offset);

    #undef WRITE
    #undef WRITE_SIZE
    fclose(file);

    _unlink(filename);
    rename(temp_filename.s, filename);
}

DBFile *dbfile_free(DBFile *dbfile) {
    for (int i=0, iend=(int)hmlen(dbfile->tagblock_hash); i<iend; ++i) {
        DBTagBlock *tagblock = &dbfile->tagblock_hash[i];
        free(tagblock->data);
    }
    hmfree(dbfile->tagblock_hash);
    free(dbfile);
    return NULL;
}

/** WorldRep stuff */

#define MAX_CELLS 32678UL           // Imposed by Dromed
#define MAX_VERTICES (256UL*1024UL) // Imposed by Dromed
#define MAX_FACES (256UL*1024UL)    // Rough guess
#define MAX_FACE_INDICES 32UL       // Imposed by Dromed
#define MAX_INDICES (MAX_FACE_INDICES*MAX_FACES)

// TODO: using unparsed wr cells is probably okay to get
//       started with, but e.g. light ids will need to
//       be updated later.
//       unless... do LGWRPoly.plane_id and/or clut_id
//       need renumbering anyway???
typedef struct WorldRepCell_Unparsed {
    uint32 size;
    void *data;     // DO NOT FREE! unowned pointer into DBTagBlock.data
} WorldRepCell_Unparsed;

typedef struct WorldRepCell {
    LGWRCellHeader *header;
    LGVector *vertices;
    LGWRPoly *polys;
    // TODO: if version < 30, use LGWRRenderPoly
    LGWREXTRenderPoly *render_polys;
    uint32 index_count;
    uint8 *index_list;
    LGWRPlane *plane_list;
    uint16 *anim_lights;
    LGWRLightMapInfo *light_list;
    char *lightmaps;
    uint32 lightmaps_size;
    uint32 num_light_indices;
    uint16 *light_indices;
} WorldRepCell;

typedef struct WorldRepLightmapFormat {
    int lightmap_bpp;
    int lightmap_2x_modulation;
    float lightmap_scale;
} WorldRepLightmapFormat;

typedef struct WorldRep {
    WorldRepLightmapFormat lightmap_format;
    uint32 cell_count;
    WorldRepCell_Unparsed cells[MAX_CELLS];
} WorldRep;

float32 _wrext_lightmap_scale_factor(int32 lightmap_scale) {
    int32 value = lightmap_scale;
    int32 sign = (value>=0 ? 1 : -1);
    if (value==0) value = 1;
    int32 exponent = (int32)log2f((float)abs(value));
    return powf(2.0f, (float)(sign*exponent));
}

WorldRepLightmapFormat _wr_get_lightmap_format(LGDBVersion wr_version, LGWREXTHeader *header) {
    // `header` should be NULL for WR/WRRGB.
    // WR lightmap data is 8bpp
    // WRRGB is 16bpp (xB5G5R5)
    // WREXT can be 8, 16, or 32bpp.
    WorldRepLightmapFormat format;
    format.lightmap_bpp = 0;
    format.lightmap_2x_modulation = 0;
    format.lightmap_scale = 1.0;
    switch (wr_version.minor) {
    case 23: format.lightmap_bpp = 6; break;
    case 24: format.lightmap_bpp = 16; break;
    case 30:
        assert(header!=NULL);
        format.lightmap_scale = _wrext_lightmap_scale_factor(header->lightmap_scale);
        switch (header->lightmap_format) {
        case 0: format.lightmap_bpp = 16; break;
        case 1: format.lightmap_bpp = 32; break;
        case 2:
            format.lightmap_bpp = 32;
            format.lightmap_2x_modulation = 1;
            break;
        default: assert_message(0, "Unrecognized lightmap_format");
        }
        break;
    default: assert_message(0, "Unsupported WR/WRRGB/WREXT version");
    }
    return format;
}

uint32 bit_count(uint32 v) {
    int c = 0;
    while (v) {
        if (v&1) ++c;
        v >>= 1;
    }
    return c;
}

#if 0

char *wrext_read_cell(char *p, WorldRepCell *pcell, int lightmap_bpp) {
    pcell->header = (LGWRCellHeader *)p; p += sizeof(LGWRCellHeader);
    pcell->vertices = (LGVector *)p; p += pcell->header->num_vertices*sizeof(LGVector);
    pcell->polys = (LGWRPoly *)p; p += pcell->header->num_polys*sizeof(LGWRPoly);
    // TODO: if version < 30, use LGWRRenderPoly
    pcell->render_polys = (LGWREXTRenderPoly *)p; p += pcell->header->num_render_polys*sizeof(LGWREXTRenderPoly);
    pcell->index_count = *(uint32 *)p; p += sizeof(uint32);
    pcell->index_list = (uint8 *)p; p += pcell->index_count*sizeof(uint8);
    pcell->plane_list = (LGWRPlane *)p; p += pcell->header->num_planes*sizeof(LGWRPlane);
    pcell->anim_lights = (uint16 *)p; p += pcell->header->num_anim_lights*sizeof(uint16);
    pcell->light_list = (LGWRLightMapInfo *)p; p += pcell->header->num_render_polys*sizeof(LGWRLightMapInfo);
    uint32 total_lightmap_size = 0;
    for (int light=0; light<pcell->header->num_render_polys; ++light) {
        LGWRLightMapInfo *info = &pcell->light_list[light];
        uint32 lightmap_size = info->padded_width*info->height*(lightmap_bpp/8);
        uint32 light_count = 1+bit_count(info->anim_light_bitmask); // 1 base lightmap, plus 1 per animlight
        total_lightmap_size += light_count*lightmap_size;
    }
    pcell->lightmaps = p; p += total_lightmap_size;
    pcell->lightmaps_size = total_lightmap_size;
    pcell->num_light_indices = *(uint32 *)p; p += sizeof(uint32);
    pcell->light_indices = (uint16 *)p; p += pcell->num_light_indices*sizeof(uint16);
    return p;
}

#endif

void *wr_load_cell_unparsed(WorldRepCell_Unparsed *pcell, void *pdata, int is_wrext, int lightmap_bpp) {
    // Read a cell from *pdata, setting pcell->size and pcell->data.
    // Return the next read offset, e.g. (p+pcell->size).
    char *p = (char *)pdata;
    pcell->data = p;
    pcell->size = 0;

    LGWRCellHeader header;
    memcpy(&header, p, sizeof(header)); p += sizeof(header);

    // Unfortunately _some_ parsing is necessary simply to read a cell:

    p += header.num_vertices*sizeof(LGVector);
    p += header.num_polys*sizeof(LGWRPoly);
    if (is_wrext) {
        p += header.num_render_polys*sizeof(LGWREXTRenderPoly);
    } else {
        p += header.num_render_polys*sizeof(LGWRRenderPoly);
    }
    uint32 index_count;
    memcpy(&index_count, p, sizeof(index_count)); p += sizeof(index_count);
    p += index_count*sizeof(uint8);
    p += header.num_planes*sizeof(LGWRPlane);
    p += header.num_anim_lights*sizeof(uint16);

    uint32 lightmapinfo_count = header.num_render_polys;
    LGWRLightMapInfo *lightmapinfo_array = NULL;
    arrsetlen(lightmapinfo_array, lightmapinfo_count);
    memcpy(lightmapinfo_array, p, lightmapinfo_count*sizeof(LGWRLightMapInfo));
    p += lightmapinfo_count*sizeof(LGWRLightMapInfo);
    for (uint32 i=0; i<lightmapinfo_count; ++i) {
        LGWRLightMapInfo *info = &lightmapinfo_array[i];
        uint32 lightmap_size = info->padded_width*info->height*(lightmap_bpp/8);
        uint32 light_count = 1+bit_count(info->anim_light_bitmask); // 1 base lightmap, plus 1 per animlight
        p += light_count*lightmap_size;
    }
    arrfree(lightmapinfo_array);

    uint32 num_light_indices;
    memcpy(&num_light_indices, p, sizeof(num_light_indices)); p += sizeof(num_light_indices);
    p += num_light_indices*sizeof(uint16);

    pcell->size = (uint32)(p-(char *)pcell->data);
    return p;
}

WorldRep *wr_load_from_tagblock(DBTagBlock *wr) {
    #define READ_SIZE(buf, size) \
        do { \
        memcpy(buf, pread, size); pread += size; \
        } while(0)
    #define READ(var) READ_SIZE(&var, sizeof(var))
    
    int debug_dump_wr = 1;
    int is_wr = tag_name_eq(wr->key, tag_name_from_str("WR"));
    int is_wrrgb = tag_name_eq(wr->key, tag_name_from_str("WRRGB"));
    int is_wrext = tag_name_eq(wr->key, tag_name_from_str("WREXT"));
    uint32 required_minor_version = 0;
    if (is_wr) required_minor_version = 23;
    else if (is_wrrgb) required_minor_version = 24;
    else required_minor_version = 30;
    assert_format(wr->version.major==0 && wr->version.minor==required_minor_version,
        "%s %d.%d not supported.", wr->key.s, wr->version.major, wr->version.minor);

    WorldRep *worldrep = calloc(1, sizeof(WorldRep));
    char *pread = wr->data;

    if(debug_dump_wr) {
        char filename[FILENAME_SIZE] = "";
        strcat(filename, "out.");
        strcat(filename, wr->key.s);
        FILE *f = fopen(filename, "wb");
        fwrite(wr->data, wr->size, 1, f);
        fclose(f);
    }

    dump("%s chunk:\n", wr->key.s);
    dump("  version: %d.%d\n", wr->version.major, wr->version.minor);

    if (is_wrext) {
        LGWREXTHeader header;
        READ(header);
        worldrep->cell_count = header.cell_count;
        worldrep->lightmap_format = _wr_get_lightmap_format(wr->version, &header);

        dump("  unknown0: 0x%08x\n", header.unknown0);
        dump("  unknown1: 0x%08x\n", header.unknown1);
        dump("  unknown2: 0x%08x\n", header.unknown2);
        dump("  lightmap_format: %ld\n", header.lightmap_format);
        dump("  lightmap_scale: 0x%08x\n", header.lightmap_scale);
        dump("  data_size: %lu\n", header.data_size);
        dump("  cell_count: %lu\n", header.cell_count);
    } else {
        LGWRHeader header;
        READ(header);
        worldrep->cell_count = header.cell_count;
        worldrep->lightmap_format = _wr_get_lightmap_format(wr->version, NULL);

        dump("  data_size: %lu\n", header.data_size);
        dump("  cell_count: %lu\n", header.cell_count);
    }

    int lightmap_bpp = worldrep->lightmap_format.lightmap_bpp;
    for (uint32 cell_index=0; cell_index<worldrep->cell_count; ++cell_index) {
        WorldRepCell_Unparsed *cell = &worldrep->cells[cell_index];
        pread = wr_load_cell_unparsed(cell, pread, is_wrext, lightmap_bpp);
    }

    { // TEMP: to try to figure out why header.data_size is the size it is...
        uint32 offset = (uint32)(pread-(char *)wr->data);
        dump("offset after cells: 0x%08x. (compare header.data_size)\n",
            offset);
    }

    uint32 bsp_extraplane_count;
    READ(bsp_extraplane_count);
    pread += bsp_extraplane_count*sizeof(LGWRPortalPlane);
    dump("  bsp_extraplane_count: %lu\n", bsp_extraplane_count);
    uint32 bsp_node_count;
    READ(bsp_node_count);
    pread += bsp_node_count*sizeof(LGWRBSPNode);
    dump("  bsp_node_count: %lu\n", bsp_node_count);

    if(debug_dump_wr) {
        uint32 offset = (uint32)(pread-(char *)wr->data);
        uint32 size = wr->size-offset;
        char filename[FILENAME_SIZE] = "";
        strcat(filename, "out.");
        strcat(filename, wr->key.s);
        strcat(filename, "_suffix");
        FILE *f = fopen(filename, "wb");
        fwrite(pread, size, 1, f);
        fclose(f);
    }

    // NOTE: this assertion fails, because the WR has more info
    //       after the bsp cells!
    assert(pread==(wr->data+wr->size));

    return worldrep;

    #undef READ
    #undef READ_SIZE
}

int main(int argc, char *argv[]) {
    //mis_read_wrext(mis);

    DBFile *dbfile = dbfile_load("e:/dev/thief/T2FM/test_misdeed/part1v24.mis");
    DBTagBlock *wr_tagblock;
    wr_tagblock = dbfile_get_tag(dbfile, "WREXT");
    if (! wr_tagblock) wr_tagblock = dbfile_get_tag(dbfile, "WRRGB");
    if (! wr_tagblock) wr_tagblock = dbfile_get_tag(dbfile, "WR");
    assert_message(wr_tagblock, "No WREXT/WRRGB/WR tagblock.");

    dump("%s read ok, 0x%08x bytes of data.\n", wr_tagblock->key.s, wr_tagblock->size);
    WorldRep *worldrep = wr_load_from_tagblock(wr_tagblock);
    free(worldrep);
    //dbfile_save(dbfile, "e:/dev/thief/T2FM/test_misdeed/out.mis");
    dbfile = dbfile_free(dbfile);
    dump("Ok.\n");
}
