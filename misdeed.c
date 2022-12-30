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
typedef double float64;

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

#define MEM_ZERO(buf, size) memset(buf, 0, size)

#define MEM_READ_SIZE(buf, size, p) \
    do { \
    memcpy(buf, p, size); p = (void *)((char *)p + size); \
    } while(0)
#define MEM_READ(var, p) MEM_READ_SIZE(&var, sizeof(var), p)
#define MEM_READ_ARRAY(array, count, p) \
    do { \
    arrsetlen(array, count); \
    memcpy(array, p, count*sizeof(array[0])); p = (void *)((char *)p + count*sizeof(array[0])); \
    } while(0)

#define FILE_READ_SIZE(buf, size, f) \
    do { \
    size_t n = fread(buf, size, 1, f); \
    assert(n==1); \
    } while(0)
#define FILE_READ(var, f) FILE_READ_SIZE(&var, sizeof(var), f)

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
    // TODO: will need to understand why data_size != sizeof(cell_data)
    //       before we can merge cells, because data_size is used to
    //       allocate space for cells, as i understand.
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
    // TODO: will need to understand why data_size != sizeof(cell_data)
    //       before we can merge cells, because data_size is used to
    //       allocate space for cells, as i understand.
    uint32 data_size;
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

typedef struct LGWRWhiteLight {
    LGVector location;
    LGVector direction;
    float bright;
    float inner;
    float outer;
    float radius;
} LGWRWhiteLight;

typedef struct LGWRRGBLight {
    LGVector location;
    LGVector direction;
    LGVector bright; // TODO: ? bright/hue/sat ?
    float inner;
    float outer;
    float radius;
} LGWRRGBLight;

typedef struct LGWRAnimlightToCell {
    uint16 cell_index;
    uint8 pos_in_cell_palette;
    uint8 pad0;
} LGWRAnimlightToCell;

typedef struct LGWRCSGPlane {
   float64 a,b,c;   // Normal to the plane
   float64 d;       // Plane equation: ax + by + cz + d = 0
} LGWRCSGPlane;

// TODO: is this different in EXT? check PinkDot's info
typedef struct LGWRCSGSurfaceRef {
   int32 cell;
   uint8 surface;
   uint8 brush_face;
   int16 vertex;
} LGWRCSGSurfaceRef;

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

    LGDBFileHeader header;
    FILE_READ(header, file);
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
    FILE_READ(toc_header, file);

    for (int i=0, iend=(int)toc_header.entry_count; i<iend; ++i) {
        DBTagBlock tagblock;

        LGDBTOCEntry toc_entry;
        FILE_READ(toc_entry, file);
        tagblock.key = tag_name_from_str(toc_entry.name);
        tagblock.size = toc_entry.data_size;
        tagblock.data = NULL;

        LGDBChunkHeader chunk_header;
        uint32 position = (uint32)ftell(file);
        fseek(file, toc_entry.offset, SEEK_SET);
        FILE_READ(chunk_header, file);
        assert(tag_name_eq(tagblock.key, tag_name_from_str(chunk_header.name)));
        tagblock.version = chunk_header.version;
        if (tagblock.size>0) {
            tagblock.data = malloc(tagblock.size);
            FILE_READ_SIZE(tagblock.data, tagblock.size, file);
        }
        fseek(file, position, SEEK_SET);

        hmputs(dbfile->tagblock_hash, tagblock);
    }

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

typedef struct WorldRepCell {
    int is_ext;
    LGWRCellHeader header;
    LGVector *vertex_array;
    LGWRPoly *poly_array;
    LGWRRenderPoly *renderpoly_array;          // only if !is_ext
    LGWREXTRenderPoly *renderpoly_ext_array;   // only if is_ext
    uint8 *index_array;
    LGWRPlane *plane_array;
    uint16 *animlight_array;
    LGWRLightMapInfo *lightmapinfo_array;
    uint16 *light_index_array;
    uint32 lightmaps_size;
    void *lightmaps;
} WorldRepCell;

typedef struct WorldRepLightmapFormat {
    int lightmap_bpp;
    int lightmap_2x_modulation;
    float lightmap_scale;
} WorldRepLightmapFormat;

typedef struct WorldRep {
    WorldRepLightmapFormat lightmap_format;
    uint32 cell_count;
    WorldRepCell cells[MAX_CELLS];
    LGWRPortalPlane *bsp_extraplane_array;
    LGWRBSPNode *bsp_node_array;
    // TODO: is this array always zeros? if not, what is it for?
    uint8 *cell_unknown0_array;                 // only if is_wrext
    LGWRWhiteLight *static_whitelight_array;    // only if is_wr
    LGWRRGBLight *static_rgblight_array;        // only if is_wrrgb/is_wrext
    LGWRWhiteLight *dynamic_whitelight_array;   // only if is_wr
    LGWRRGBLight *dynamic_rgblight_array;       // only if is_wrrgb/is_wrext

    LGWRAnimlightToCell *animlight_to_cell;
    // NOTE: csg_brush_index = (brface>>8)
    //       face_index = (brface&0xff)
    // TODO: does br=faces mean brush_polys? rename it?
    int32 *csg_brfaces_array;                       // one brface per renderpoly, per cell
    int32 *csg_brush_plane_count_array;             // number of planes, per brush
    LGWRCSGPlane *csg_brush_planes_array;           // all planes
    int32 *csg_brush_surfaceref_count_array;        // number of surfacerefs, per brush
    LGWRCSGSurfaceRef *csg_brush_surfacerefs_array; // all surfacerefs
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
    case 23: format.lightmap_bpp = 8; break;
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

void wr_wipe_cell(WorldRepCell *cell) {
    if (cell->vertex_array) arrfree(cell->vertex_array);
    if (cell->poly_array) arrfree(cell->poly_array);
    if (cell->renderpoly_ext_array) arrfree(cell->renderpoly_ext_array);
    if (cell->renderpoly_array) arrfree(cell->renderpoly_array);
    if (cell->index_array) arrfree(cell->index_array);
    if (cell->plane_array) arrfree(cell->plane_array);
    if (cell->animlight_array) arrfree(cell->animlight_array);
    if (cell->lightmapinfo_array) arrfree(cell->lightmapinfo_array);
    if (cell->light_index_array) arrfree(cell->light_index_array);
    if (cell->lightmaps) free(cell->lightmaps);
    MEM_ZERO(cell, sizeof(*cell));
}

void *wr_load_cell(WorldRepCell *cell, int is_ext, int lightmap_bpp, void *pdata) {
    char *pread = (char *)pdata;
    MEM_ZERO(cell, sizeof(*cell));
    cell->is_ext = is_ext;
    LGWRCellHeader header;
    MEM_READ(header, pread);
    cell->header = header;
    MEM_READ_ARRAY(cell->vertex_array, header.num_vertices, pread);
    MEM_READ_ARRAY(cell->poly_array, header.num_polys, pread);
    if (is_ext) {
        MEM_READ_ARRAY(cell->renderpoly_ext_array, header.num_render_polys, pread);
    } else {
        MEM_READ_ARRAY(cell->renderpoly_array, header.num_render_polys, pread);
    }
    uint32 index_count;
    MEM_READ(index_count, pread);
    MEM_READ_ARRAY(cell->index_array, index_count, pread);
    MEM_READ_ARRAY(cell->plane_array, header.num_planes, pread);
    MEM_READ_ARRAY(cell->animlight_array, header.num_anim_lights, pread);
    MEM_READ_ARRAY(cell->lightmapinfo_array, header.num_render_polys, pread);
    cell->lightmaps_size = 0;
    for (uint32 i=0, iend=(uint32)arrlen(cell->lightmapinfo_array); i<iend; ++i) {
        LGWRLightMapInfo *info = &cell->lightmapinfo_array[i];
        uint32 lightmap_size = info->padded_width*info->height*(lightmap_bpp/8);
        uint32 light_count = 1+bit_count(info->anim_light_bitmask); // 1 base lightmap, plus 1 per animlight
        cell->lightmaps_size += light_count*lightmap_size;
    }
    cell->lightmaps = malloc(cell->lightmaps_size);
    MEM_READ_SIZE(cell->lightmaps, cell->lightmaps_size, pread);
    uint32 num_light_indices;
    MEM_READ(num_light_indices, pread);
    MEM_READ_ARRAY(cell->light_index_array, num_light_indices, pread);
    return pread;
}

WorldRep *wr_load_from_tagblock(DBTagBlock *wr) {
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
        MEM_READ(header, pread);
        worldrep->cell_count = header.cell_count;
        worldrep->lightmap_format = _wr_get_lightmap_format(wr->version, &header);

        dump("  unknown0: 0x%08x\n", header.unknown0);
        dump("  unknown1: 0x%08x\n", header.unknown1);
        dump("  unknown2: 0x%08x\n", header.unknown2);
        dump("  lightmap_format: %ld\n", header.lightmap_format);
        dump("  lightmap_scale: 0x%08x\n", header.lightmap_scale);
        dump("  data_size: 0x%lx\n", header.data_size);
        dump("  cell_count: %lu\n", header.cell_count);
    } else {
        LGWRHeader header;
        MEM_READ(header, pread);
        worldrep->cell_count = header.cell_count;
        worldrep->lightmap_format = _wr_get_lightmap_format(wr->version, NULL);

        dump("  data_size: 0x%lx\n", header.data_size);
        dump("  cell_count: %lu\n", header.cell_count);
    }

    int lightmap_bpp = worldrep->lightmap_format.lightmap_bpp;
    for (uint32 cell_index=0; cell_index<worldrep->cell_count; ++cell_index) {
        pread = wr_load_cell(&worldrep->cells[cell_index], is_wrext, lightmap_bpp, pread);
    }

    { // TEMP: to try to figure out why header.data_size is the size it is...
        uint32 offset = (uint32)(pread-(char *)wr->data);
        dump("offset after cells: 0x%08x. (compare header.data_size)\n",
            offset);
    }

    uint32 bsp_extraplane_count;
    MEM_READ(bsp_extraplane_count, pread);
    MEM_READ_ARRAY(worldrep->bsp_extraplane_array, bsp_extraplane_count, pread);
    dump("  bsp_extraplane_count: %lu\n", bsp_extraplane_count);

    uint32 bsp_node_count;
    MEM_READ(bsp_node_count, pread);
    MEM_READ_ARRAY(worldrep->bsp_node_array, bsp_node_count, pread);
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

    if (is_wrext) {
        MEM_READ_ARRAY(worldrep->cell_unknown0_array, worldrep->cell_count, pread);
        for (uint32 i=0, iend=(uint32)arrlen(worldrep->cell_unknown0_array); i<iend; ++i) {
            if (worldrep->cell_unknown0_array[i]!=0) {
                dump("*** cell_unknown0_array is not zeros ***\n");
                break;
            }
        }
    }

    uint32 num_static_lights;
    MEM_READ(num_static_lights, pread);
    uint32 num_dynamic_lights;
    MEM_READ(num_dynamic_lights, pread);

    // WR,WRRGB always store 768 static light records, even when there are
    // fewer static lights. Skip the remainder.
    uint32 num_extra_static_light_records = is_wrext ? 0 : (768-num_static_lights);
    if (is_wr) {
        MEM_READ_ARRAY(worldrep->static_whitelight_array, num_static_lights, pread);
        pread += num_extra_static_light_records*sizeof(LGWRWhiteLight);
    } else {
        MEM_READ_ARRAY(worldrep->static_rgblight_array, num_static_lights, pread);
        pread += num_extra_static_light_records*sizeof(LGWRRGBLight);
    }

    // WR,WRRGB,WREXT always store 32 dynamic light records, even when there are
    // fewer dynamic lights. Skip the remainder.
    uint32 num_extra_dynamic_light_records = (32-num_dynamic_lights);
    if (is_wr) {
        MEM_READ_ARRAY(worldrep->dynamic_whitelight_array, num_dynamic_lights, pread);
        pread += num_extra_dynamic_light_records*sizeof(LGWRWhiteLight);
    } else {
        MEM_READ_ARRAY(worldrep->dynamic_rgblight_array, num_dynamic_lights, pread);
        pread += num_extra_dynamic_light_records*sizeof(LGWRRGBLight);
    }

    uint32 num_animlight_to_cell;
    MEM_READ(num_animlight_to_cell, pread);
    MEM_READ_ARRAY(worldrep->animlight_to_cell, num_animlight_to_cell, pread);

    uint32 csg_cell_count;
    MEM_READ(csg_cell_count, pread);
    assert(csg_cell_count==worldrep->cell_count);
    dump("csg_cell_count: %lu\n", csg_cell_count);
    uint32 csg_brfaces_count = 0;
    for (uint32 i=0, iend=csg_cell_count; i<iend; ++i) {
        uint32 renderpoly_count;
        if (is_wrext) {
            renderpoly_count = (uint32)arrlen(worldrep->cells[i].renderpoly_ext_array);
        } else {
            renderpoly_count = (uint32)arrlen(worldrep->cells[i].renderpoly_array);
        }
        csg_brfaces_count += renderpoly_count;
    }
    dump("csg_brfaces_count: %lu\n", csg_brfaces_count);
    assert(csg_brfaces_count!=0);
    MEM_READ_ARRAY(worldrep->csg_brfaces_array, csg_brfaces_count, pread);
    uint32 csg_brush_count;
    MEM_READ(csg_brush_count, pread);
    dump("csg_brush_count: %lu\n", csg_brush_count);
    MEM_READ_ARRAY(worldrep->csg_brush_plane_count_array, csg_brush_count, pread);
    uint32 csg_brush_plane_total_count = 0;
    for (uint32 i=0, iend=(uint32)arrlen(worldrep->csg_brush_plane_count_array); i<iend; ++i) {
        csg_brush_plane_total_count += worldrep->csg_brush_plane_count_array[i];
    }
    dump("csg_brush_plane_total_count: %lu\n", csg_brush_plane_total_count);
    MEM_READ_ARRAY(worldrep->csg_brush_planes_array, csg_brush_plane_total_count, pread);
    MEM_READ_ARRAY(worldrep->csg_brush_surfaceref_count_array, csg_brush_count, pread);
    uint32 csg_brush_surfaceref_total_count = 0;
    for (uint32 i=0, iend=(uint32)arrlen(worldrep->csg_brush_surfaceref_count_array); i<iend; ++i) {
        csg_brush_surfaceref_total_count += worldrep->csg_brush_surfaceref_count_array[i];
    }
    MEM_READ_ARRAY(worldrep->csg_brush_surfacerefs_array, csg_brush_surfaceref_total_count, pread);

    assert(pread==(wr->data+wr->size));

    return worldrep;
}

int main(int argc, char *argv[]) {
    DBFile *dbfile = dbfile_load("e:/dev/thief/T2FM/test_misdeed/part1v30.mis");
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
