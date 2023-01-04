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

/** debug shit **/

static int DEBUG_DUMP_WR = 0;


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

#define abort_message(message) \
    do { \
    fprintf(stderr, "\t%s\n", message); \
    fflush(stderr); \
    abort(); \
    } while(0)

#define abort_format(fmt, ...) \
    do { \
    fprintf(stderr, "\t" fmt "\n", __VA_ARGS__); \
    fflush(stderr); \
    abort(); \
    } while(0)

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
        abort_message(message); \
    } \
    } while(0)

#define assert_format(condition, fmt, ...) \
    do { \
    if (!(condition)) { \
        fprintf(stderr, "Assertion failed, line %d: %s\n", __LINE__, #condition); \
        abort_format("\t" fmt "\n", __VA_ARGS__); \
    } \
    } while(0)

void dump(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
}

#define arrsize(arr) ((arr) ? ((uint32)(arrlenu(arr)*sizeof((arr)[0]))) : 0UL)

#define MEM_ZERO(buf, size) memset(buf, 0, size)

#define MEM_READ_SIZE(buf, size, p) \
    do { \
    memcpy(buf, p, size); p = (void *)((char *)p + size); \
    } while(0)
#define MEM_READ(var, p) MEM_READ_SIZE(&var, sizeof(var), p)
#define MEM_READ_ARRAY(array, count, p) \
    do { \
    arrsetlen(array, count); \
    size_t size = count*sizeof(array[0]); \
    memcpy(array, p, size); p = (void *)((char *)p + size); \
    } while(0)

#define MEM_WRITE_SIZE(buf, size, p) \
    do { \
    memcpy(p, buf, size); p = (void *)((char *)p + size); \
    } while(0)
#define MEM_WRITE(var, p) MEM_WRITE_SIZE(&var, sizeof(var), p)
#define MEM_WRITE_ARRAY(array, p) \
    do { \
    size_t size = arrlenu(array)*sizeof(array[0]); \
    memcpy(p, array, size); p = (void *)((char *)p + size); \
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
    uint32 cell_alloc_size;   // Added in WR version 18 (0x12)
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
    uint32 cell_alloc_size;
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

static void dbtagblock_wipe(DBTagBlock *tagblock) {
    tagblock->size = 0;
    free(tagblock->data);
    tagblock->data = 0;
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
    toc_header.entry_count = (uint32)arrlenu(toc_array);
    WRITE(toc_header);
    for (uint32 i=0, iend=(uint32)arrlenu(toc_array); i<iend; ++i) {
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
        dbtagblock_wipe(tagblock);
    }
    hmfree(dbfile->tagblock_hash);
    free(dbfile);
    return NULL;
}

/** WorldRep stuff */

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
    int lightmap_bpp;           // 8, 16, or 32
    int lightmap_2x_modulation; // 0 or 1
    float lightmap_scale;
} WorldRepLightmapFormat;

typedef enum WorldRepFormat {
    WorldRepFormatWR = 0,
    WorldRepFormatWRRGB = 1,
    WorldRepFormatWREXT = 2,
} WorldRepFormat;

static const LGDBVersion SupportedWRVersion[] = {
    /* WorldRepFormatWR */    { 0, 23 },
    /* WorldRepFormatWRRGB */ { 0, 24 },
    /* WorldRepFormatWREXT */ { 0, 30 },
};

typedef struct WorldRep {
    WorldRepFormat format;
    WorldRepLightmapFormat lightmap_format;
    WorldRepCell *cell_array;
    LGWRPortalPlane *bsp_extraplane_array;
    LGWRBSPNode *bsp_node_array;
    // TODO: is this array always zeros? if not, what is it for?
    uint8 *cell_unknown0_array;                 // only if WREXT
    LGWRWhiteLight *static_whitelight_array;    // only if WR
    LGWRRGBLight *static_rgblight_array;        // only if WRRGB/WREXT
    LGWRWhiteLight *dynamic_whitelight_array;   // only if WR
    LGWRRGBLight *dynamic_rgblight_array;       // only if WRRGB/WREXT

    LGWRAnimlightToCell *animlight_to_cell_array;
    // NOTE: csg_brush_index = (brface>>8)
    //       face_index = (brface&0xff)
    // TODO: does br=faces mean brush_polys? rename it?
    int32 *csg_brfaces_array;                       // one brface per renderpoly, per cell
    int32 *csg_brush_plane_count_array;             // number of planes, per brush
    LGWRCSGPlane *csg_brush_planes_array;           // all planes
    int32 *csg_brush_surfaceref_count_array;        // number of surfacerefs, per brush
    LGWRCSGSurfaceRef *csg_brush_surfacerefs_array; // all surfacerefs
} WorldRep;

float32 _wr_decode_lightmap_scale(int32 lightmap_scale) {
    int32 value = lightmap_scale;
    int32 sign = (value>=0 ? 1 : -1);
    if (value==0) value = 1;
    int32 exponent = (int32)log2f((float)abs(value));
    return powf(2.0f, (float)(sign*exponent));
}

int32 _wr_encode_lightmap_scale(float32 scale_factor) {
    return 2*(int32)log2f(scale_factor);
}

WorldRepLightmapFormat _wr_decode_lightmap_format(WorldRepFormat wr_format, LGWREXTHeader *header) {
    // `header` should be NULL for WR/WRRGB.
    // WR lightmap data is 8bpp
    // WRRGB is 16bpp (xB5G5R5)
    // WREXT can be 8, 16, or 32bpp.
    WorldRepLightmapFormat format;
    format.lightmap_bpp = 0;
    format.lightmap_2x_modulation = 0;
    format.lightmap_scale = 1.0;
    switch (wr_format) {
    case WorldRepFormatWR: format.lightmap_bpp = 8; break;
    case WorldRepFormatWRRGB: format.lightmap_bpp = 16; break;
    case WorldRepFormatWREXT:
        assert(header!=NULL);
        format.lightmap_scale = _wr_decode_lightmap_scale(header->lightmap_scale);
        switch (header->lightmap_format) {
        case 0: format.lightmap_bpp = 16; break;
        case 1: format.lightmap_bpp = 32; break;
        case 2:
            format.lightmap_bpp = 32;
            format.lightmap_2x_modulation = 1;
            break;
        default: abort_message("Unrecognized lightmap_format");
        }
        break;
    default: abort_message("Unsupported WR/WRRGB/WREXT version");
    }
    return format;
}

uint32 _wr_encode_lightmap_format(WorldRepFormat wr_format, WorldRepLightmapFormat format) {
    assert (wr_format==WorldRepFormatWREXT);
    switch (format.lightmap_bpp) {
    case 16: return 0;
    case 32:
        if (format.lightmap_2x_modulation) return 2;
        else return 1;
    default:
        abort_message("Invalid WorldRepLightmapFormat.");
        /* die */
    }
}

uint32 _wr_calc_cell_alloc_size(WorldRep *wr) {
    uint32 size = 0;
    for (uint32 c=0, cend=(uint32)arrlenu(wr->cell_array); c<cend; ++c) {
        WorldRepCell *cell = &wr->cell_array[c];
        size += 84; // sizeof(PortalCell)
        size += arrsize(cell->vertex_array);
        size += arrsize(cell->poly_array);
        if (wr->format==WorldRepFormatWREXT) {
            size += arrsize(cell->renderpoly_ext_array);
        } else {
            size += arrsize(cell->renderpoly_array);
        }
        size += arrsize(cell->index_array);
        size += arrsize(cell->plane_array);
        size += arrsize(cell->animlight_array);
        size += arrsize(cell->lightmapinfo_array);
        size += cell->lightmaps_size;
        size += arrsize(cell->light_index_array);
    }
    return size;
}

uint32 bit_count(uint32 v) {
    int c = 0;
    while (v) {
        if (v&1) ++c;
        v >>= 1;
    }
    return c;
}

void wr_free_cell(WorldRepCell *cell) {
    arrfree(cell->vertex_array);
    arrfree(cell->poly_array);
    arrfree(cell->renderpoly_ext_array);
    arrfree(cell->renderpoly_array);
    arrfree(cell->index_array);
    arrfree(cell->plane_array);
    arrfree(cell->animlight_array);
    arrfree(cell->lightmapinfo_array);
    arrfree(cell->light_index_array);
    free(cell->lightmaps); cell->lightmaps = NULL;
}

void *wr_read_cell(WorldRepCell *cell, int is_ext, int lightmap_bpp, void *pdata) {
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
    for (uint32 i=0, iend=(uint32)arrlenu(cell->lightmapinfo_array); i<iend; ++i) {
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

void *wr_write_cell(WorldRepCell *cell, int is_ext, int lightmap_bpp, void *pdata) {
    char *pwrite = (char *)pdata;
    assert(cell->is_ext==is_ext);
    LGWRCellHeader header = cell->header;
    MEM_WRITE(header, pwrite);

    MEM_WRITE_ARRAY(cell->vertex_array, pwrite);
    MEM_WRITE_ARRAY(cell->poly_array, pwrite);
    if (is_ext) {
        MEM_WRITE_ARRAY(cell->renderpoly_ext_array, pwrite);
    } else {
        MEM_WRITE_ARRAY(cell->renderpoly_array, pwrite);
    }
    uint32 index_count = (uint32)arrlenu(cell->index_array);
    MEM_WRITE(index_count, pwrite);
    MEM_WRITE_ARRAY(cell->index_array, pwrite);
    MEM_WRITE_ARRAY(cell->plane_array, pwrite);
    MEM_WRITE_ARRAY(cell->animlight_array, pwrite);
    MEM_WRITE_ARRAY(cell->lightmapinfo_array, pwrite);
    MEM_WRITE_SIZE(cell->lightmaps, cell->lightmaps_size, pwrite);
    uint32 num_light_indices = (uint32)arrlenu(cell->light_index_array);
    MEM_WRITE(num_light_indices, pwrite);
    MEM_WRITE_ARRAY(cell->light_index_array, pwrite);
    return pwrite;
}

void wr_free(WorldRep **pwr) {
    WorldRep *wr = *pwr;
    for (uint32 i=0, iend=(uint32)arrlenu(wr->cell_array); i<iend; ++i) {
        wr_free_cell(&wr->cell_array[i]);
    }
    arrfree(wr->cell_array);
    arrfree(wr->bsp_extraplane_array);
    arrfree(wr->bsp_node_array);
    arrfree(wr->cell_unknown0_array);
    arrfree(wr->static_whitelight_array);
    arrfree(wr->static_rgblight_array);
    arrfree(wr->dynamic_whitelight_array);
    arrfree(wr->dynamic_rgblight_array);
    arrfree(wr->animlight_to_cell_array);
    arrfree(wr->csg_brfaces_array);
    arrfree(wr->csg_brush_plane_count_array);
    arrfree(wr->csg_brush_planes_array);
    arrfree(wr->csg_brush_surfaceref_count_array);
    arrfree(wr->csg_brush_surfacerefs_array);
    free(wr);
    *pwr = NULL;
}

WorldRep *wr_load_from_tagblock(DBTagBlock *tagblock) {
    WorldRep *wr = calloc(1, sizeof(WorldRep));
    char *pread = tagblock->data;

    if (tag_name_eq(tagblock->key, tag_name_from_str("WR"))) {
        wr->format = WorldRepFormatWR;
    } else if (tag_name_eq(tagblock->key, tag_name_from_str("WRRGB"))) {
        wr->format = WorldRepFormatWRRGB;
    } else if (tag_name_eq(tagblock->key, tag_name_from_str("WREXT"))) {
        wr->format = WorldRepFormatWREXT;
    } else {
        abort_format("%s not supported for WorldRep.", tagblock->key.s);
        /* die */
    }
    LGDBVersion supported_version = SupportedWRVersion[wr->format];
    assert_format(tagblock->version.major==supported_version.major
               && tagblock->version.minor==supported_version.minor,
        "%s %d.%d not supported.", tagblock->key.s, tagblock->version.major, tagblock->version.minor);
    int is_wr = (wr->format==WorldRepFormatWR);
    int is_wrext = (wr->format==WorldRepFormatWREXT);

    if(DEBUG_DUMP_WR) {
        char filename[FILENAME_SIZE] = "";
        strcat(filename, "in.");
        strcat(filename, tagblock->key.s);
        FILE *f = fopen(filename, "wb");
        fwrite(tagblock->data, tagblock->size, 1, f);
        fclose(f);
    }

    dump("%s chunk:\n", tagblock->key.s);
    dump("  version: %d.%d\n", tagblock->version.major, tagblock->version.minor);

    uint32 header_cell_alloc_size;
    uint32 cell_count;
    if (is_wrext) {
        LGWREXTHeader header;
        MEM_READ(header, pread);
        cell_count = header.cell_count;
        wr->lightmap_format = _wr_decode_lightmap_format(wr->format, &header);
        header_cell_alloc_size = header.cell_alloc_size;
        dump("  unknown0: 0x%08x\n", header.unknown0);
        dump("  unknown1: 0x%08x\n", header.unknown1);
        dump("  unknown2: 0x%08x\n", header.unknown2);
        dump("  lightmap_format: %ld\n", header.lightmap_format);
        dump("  lightmap_scale: 0x%08x\n", header.lightmap_scale);
        dump("  cell_alloc_size: %lu\n", header.cell_alloc_size);
        dump("  cell_count: %lu\n", header.cell_count);

        // NOTE: We don't know what these values mean yet, so just make
        //       sure they are the same as we typically see -- because
        //       we don't store them, but will write that same value
        //       back out.
        assert(header.unknown0==0x14);
        assert(header.unknown1==0x05);
        // NOTE: header.unknown2 is flags including "lightmapped water";
        //       for now we ignore it and will write back out 0, assuming
        //       that whatever the flags are will be update when
        //       re-lighting.
    } else {
        LGWRHeader header;
        MEM_READ(header, pread);
        cell_count = header.cell_count;
        wr->lightmap_format = _wr_decode_lightmap_format(wr->format, NULL);
        header_cell_alloc_size = header.cell_alloc_size;

        dump("  cell_alloc_size: %lu\n", header.cell_alloc_size);
        dump("  cell_count: %lu\n", header.cell_count);
    }

    int lightmap_bpp = wr->lightmap_format.lightmap_bpp;
    arrsetlen(wr->cell_array, cell_count);
    for (uint32 i=0, iend=(uint32)arrlenu(wr->cell_array); i<iend; ++i) {
        pread = wr_read_cell(&wr->cell_array[i], is_wrext, lightmap_bpp, pread);
    }

    uint32 bsp_extraplane_count;
    MEM_READ(bsp_extraplane_count, pread);
    MEM_READ_ARRAY(wr->bsp_extraplane_array, bsp_extraplane_count, pread);
    dump("  bsp_extraplane_count: %lu\n", bsp_extraplane_count);

    uint32 bsp_node_count;
    MEM_READ(bsp_node_count, pread);
    MEM_READ_ARRAY(wr->bsp_node_array, bsp_node_count, pread);
    dump("  bsp_node_count: %lu\n", bsp_node_count);

    if(DEBUG_DUMP_WR) {
        uint32 offset = (uint32)(pread-(char *)tagblock->data);
        uint32 size = tagblock->size-offset;
        char filename[FILENAME_SIZE] = "";
        strcat(filename, "in.");
        strcat(filename, tagblock->key.s);
        strcat(filename, "_suffix");
        FILE *f = fopen(filename, "wb");
        fwrite(pread, size, 1, f);
        fclose(f);
    }

    if (is_wrext) {
        MEM_READ_ARRAY(wr->cell_unknown0_array, arrlenu(wr->cell_array), pread);
        for (uint32 i=0, iend=(uint32)arrlenu(wr->cell_unknown0_array); i<iend; ++i) {
            if (wr->cell_unknown0_array[i]!=0) {
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
        MEM_READ_ARRAY(wr->static_whitelight_array, num_static_lights, pread);
        pread += num_extra_static_light_records*sizeof(LGWRWhiteLight);
    } else {
        MEM_READ_ARRAY(wr->static_rgblight_array, num_static_lights, pread);
        pread += num_extra_static_light_records*sizeof(LGWRRGBLight);
    }

    // WR,WRRGB,WREXT always store 32 dynamic light records, even when there are
    // fewer dynamic lights. Skip the remainder.
    uint32 num_extra_dynamic_light_records = (32-num_dynamic_lights);
    if (is_wr) {
        MEM_READ_ARRAY(wr->dynamic_whitelight_array, num_dynamic_lights, pread);
        pread += num_extra_dynamic_light_records*sizeof(LGWRWhiteLight);
    } else {
        MEM_READ_ARRAY(wr->dynamic_rgblight_array, num_dynamic_lights, pread);
        pread += num_extra_dynamic_light_records*sizeof(LGWRRGBLight);
    }

    uint32 num_animlight_to_cell;
    MEM_READ(num_animlight_to_cell, pread);
    MEM_READ_ARRAY(wr->animlight_to_cell_array, num_animlight_to_cell, pread);

    uint32 csg_cell_count;
    MEM_READ(csg_cell_count, pread);
    assert(csg_cell_count==arrlenu(wr->cell_array));
    dump("csg_cell_count: %lu\n", csg_cell_count);
    uint32 csg_brfaces_count = 0;
    for (uint32 i=0, iend=csg_cell_count; i<iend; ++i) {
        uint32 renderpoly_count;
        if (is_wrext) {
            renderpoly_count = (uint32)arrlenu(wr->cell_array[i].renderpoly_ext_array);
        } else {
            renderpoly_count = (uint32)arrlenu(wr->cell_array[i].renderpoly_array);
        }
        csg_brfaces_count += renderpoly_count;
    }
    dump("csg_brfaces_count: %lu\n", csg_brfaces_count);
    assert(csg_brfaces_count!=0);
    MEM_READ_ARRAY(wr->csg_brfaces_array, csg_brfaces_count, pread);
    uint32 csg_brush_count;
    MEM_READ(csg_brush_count, pread);
    dump("csg_brush_count: %lu\n", csg_brush_count);
    MEM_READ_ARRAY(wr->csg_brush_plane_count_array, csg_brush_count, pread);
    uint32 csg_brush_plane_total_count = 0;
    for (uint32 i=0, iend=(uint32)arrlenu(wr->csg_brush_plane_count_array); i<iend; ++i) {
        csg_brush_plane_total_count += wr->csg_brush_plane_count_array[i];
    }
    dump("csg_brush_plane_total_count: %lu\n", csg_brush_plane_total_count);
    MEM_READ_ARRAY(wr->csg_brush_planes_array, csg_brush_plane_total_count, pread);
    MEM_READ_ARRAY(wr->csg_brush_surfaceref_count_array, csg_brush_count, pread);
    uint32 csg_brush_surfaceref_total_count = 0;
    for (uint32 i=0, iend=(uint32)arrlenu(wr->csg_brush_surfaceref_count_array); i<iend; ++i) {
        csg_brush_surfaceref_total_count += wr->csg_brush_surfaceref_count_array[i];
    }
    MEM_READ_ARRAY(wr->csg_brush_surfacerefs_array, csg_brush_surfaceref_total_count, pread);

    // Ensure we have read all the available data:
    assert(pread==(tagblock->data+tagblock->size));
    // Ensure we have a correct cell_alloc_size calculation:
    uint32 calc_cell_alloc_size = _wr_calc_cell_alloc_size(wr);
    assert_format(calc_cell_alloc_size==header_cell_alloc_size,
        "Size mismatch! header_size:%lu calculated cell_alloc_size:%lu",
        header_cell_alloc_size, calc_cell_alloc_size);

    return wr;
}

void wr_save_to_tagblock(DBTagBlock *tagblock, WorldRep *wr) {
    assert(tagblock->data==NULL);
    // We write to a 256MB temporary buffer, and at the end we malloc
    // tagblock->data and copy into that. Its not the most efficient
    // way to go about things, but it's simpler, and that's more
    // valuable here.
    void *buffer = malloc(256UL*1048576UL);
    char *pwrite = buffer;

    switch (wr->format) {
    case WorldRepFormatWR: tagblock->key = tag_name_from_str("WR"); break;
    case WorldRepFormatWRRGB: tagblock->key = tag_name_from_str("WRRGB"); break;
    case WorldRepFormatWREXT: tagblock->key = tag_name_from_str("WREXT"); break;
    default:
        abort_message("Invalid WorldRepFormat.");
        /* die */
    }
    tagblock->version = SupportedWRVersion[wr->format];
    dump("%s chunk:\n", tagblock->key.s);
    dump("  version: %d.%d\n", tagblock->version.major, tagblock->version.minor);

    int is_wr = (wr->format==WorldRepFormatWR);
    int is_wrext = (wr->format==WorldRepFormatWREXT);

    if (is_wrext) {
        LGWREXTHeader header;
        // NOTE: we don't know what these values mean, so we just write
        //       out the typical values. Note that we assert on them
        //       on input to maintain consistency.
        header.unknown0 = 0x14;
        header.unknown1 = 0x05;
        header.unknown2 = 0;
        header.lightmap_format = _wr_encode_lightmap_format(wr->format, wr->lightmap_format);
        header.lightmap_scale = _wr_encode_lightmap_scale(wr->lightmap_format.lightmap_scale);
        header.cell_alloc_size = _wr_calc_cell_alloc_size(wr);
        header.cell_count = (uint32)arrlenu(wr->cell_array);
        MEM_WRITE(header, pwrite);

        dump("  unknown0: 0x%08x\n", header.unknown0);
        dump("  unknown1: 0x%08x\n", header.unknown1);
        dump("  unknown2: 0x%08x\n", header.unknown2);
        dump("  lightmap_format: %ld\n", header.lightmap_format);
        dump("  lightmap_scale: 0x%08x\n", header.lightmap_scale);
        dump("  cell_alloc_size: %lu\n", header.cell_alloc_size);
        dump("  cell_count: %lu\n", header.cell_count);
    } else {
        LGWRHeader header;
        header.cell_alloc_size = _wr_calc_cell_alloc_size(wr);
        header.cell_count = (uint32)arrlenu(wr->cell_array);
        MEM_WRITE(header, pwrite);

        dump("  cell_alloc_size: %lu\n", header.cell_alloc_size);
        dump("  cell_count: %lu\n", header.cell_count);
    }

    int lightmap_bpp = wr->lightmap_format.lightmap_bpp;
    for (uint32 i=0, iend=(uint32)arrlenu(wr->cell_array); i<iend; ++i) {
        pwrite = wr_write_cell(&wr->cell_array[i], is_wrext, lightmap_bpp, pwrite);
    }

#if 0

    uint32 bsp_extraplane_count;
    MEM_READ(bsp_extraplane_count, pread);
    MEM_READ_ARRAY(wr->bsp_extraplane_array, bsp_extraplane_count, pread);
    dump("  bsp_extraplane_count: %lu\n", bsp_extraplane_count);

    uint32 bsp_node_count;
    MEM_READ(bsp_node_count, pread);
    MEM_READ_ARRAY(wr->bsp_node_array, bsp_node_count, pread);
    dump("  bsp_node_count: %lu\n", bsp_node_count);

    if(DEBUG_DUMP_WR) {
        uint32 offset = (uint32)(pread-(char *)tagblock->data);
        uint32 size = tagblock->size-offset;
        char filename[FILENAME_SIZE] = "";
        strcat(filename, "out.");
        strcat(filename, tagblock->key.s);
        strcat(filename, "_suffix");
        FILE *f = fopen(filename, "wb");
        fwrite(pread, size, 1, f);
        fclose(f);
    }

    if (is_wrext) {
        MEM_READ_ARRAY(wr->cell_unknown0_array, wr->cell_count, pread);
        for (uint32 i=0, iend=(uint32)arrlenu(wr->cell_unknown0_array); i<iend; ++i) {
            if (wr->cell_unknown0_array[i]!=0) {
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
        MEM_READ_ARRAY(wr->static_whitelight_array, num_static_lights, pread);
        pread += num_extra_static_light_records*sizeof(LGWRWhiteLight);
    } else {
        MEM_READ_ARRAY(wr->static_rgblight_array, num_static_lights, pread);
        pread += num_extra_static_light_records*sizeof(LGWRRGBLight);
    }

    // WR,WRRGB,WREXT always store 32 dynamic light records, even when there are
    // fewer dynamic lights. Skip the remainder.
    uint32 num_extra_dynamic_light_records = (32-num_dynamic_lights);
    if (is_wr) {
        MEM_READ_ARRAY(wr->dynamic_whitelight_array, num_dynamic_lights, pread);
        pread += num_extra_dynamic_light_records*sizeof(LGWRWhiteLight);
    } else {
        MEM_READ_ARRAY(wr->dynamic_rgblight_array, num_dynamic_lights, pread);
        pread += num_extra_dynamic_light_records*sizeof(LGWRRGBLight);
    }

    uint32 num_animlight_to_cell;
    MEM_READ(num_animlight_to_cell, pread);
    MEM_READ_ARRAY(wr->animlight_to_cell_array, num_animlight_to_cell, pread);

    uint32 csg_cell_count;
    MEM_READ(csg_cell_count, pread);
    assert(csg_cell_count==wr->cell_count);
    dump("csg_cell_count: %lu\n", csg_cell_count);
    uint32 csg_brfaces_count = 0;
    for (uint32 i=0, iend=csg_cell_count; i<iend; ++i) {
        uint32 renderpoly_count;
        if (is_wrext) {
            renderpoly_count = (uint32)arrlenu(wr->cell_array[i].renderpoly_ext_array);
        } else {
            renderpoly_count = (uint32)arrlenu(wr->cell_array[i].renderpoly_array);
        }
        csg_brfaces_count += renderpoly_count;
    }
    dump("csg_brfaces_count: %lu\n", csg_brfaces_count);
    assert(csg_brfaces_count!=0);
    MEM_READ_ARRAY(wr->csg_brfaces_array, csg_brfaces_count, pread);
    uint32 csg_brush_count;
    MEM_READ(csg_brush_count, pread);
    dump("csg_brush_count: %lu\n", csg_brush_count);
    MEM_READ_ARRAY(wr->csg_brush_plane_count_array, csg_brush_count, pread);
    uint32 csg_brush_plane_total_count = 0;
    for (uint32 i=0, iend=(uint32)arrlenu(wr->csg_brush_plane_count_array); i<iend; ++i) {
        csg_brush_plane_total_count += wr->csg_brush_plane_count_array[i];
    }
    dump("csg_brush_plane_total_count: %lu\n", csg_brush_plane_total_count);
    MEM_READ_ARRAY(wr->csg_brush_planes_array, csg_brush_plane_total_count, pread);
    MEM_READ_ARRAY(wr->csg_brush_surfaceref_count_array, csg_brush_count, pread);
    uint32 csg_brush_surfaceref_total_count = 0;
    for (uint32 i=0, iend=(uint32)arrlenu(wr->csg_brush_surfaceref_count_array); i<iend; ++i) {
        csg_brush_surfaceref_total_count += wr->csg_brush_surfaceref_count_array[i];
    }
    MEM_READ_ARRAY(wr->csg_brush_surfacerefs_array, csg_brush_surfaceref_total_count, pread);

    assert(pread==(tagblock->data+tagblock->size));

    if(DEBUG_DUMP_WR) {
        char filename[FILENAME_SIZE] = "";
        strcat(filename, "out.");
        strcat(filename, tagblock->key.s);
        FILE *f = fopen(filename, "wb");
        fwrite(tagblock->data, tagblock->size, 1, f);
        fclose(f);
    }

    return wr;
#endif
}

int main(int argc, char *argv[]) {
    // if (argc<2) {
    //     abort_message("give me a filename!");
    // }
    // char *filename = argv[1];
    const char *filename = "e:/dev/thief/T2FM/test_misdeed/part1v24.mis";
    // const char *filename = "e:/dev/thief/T2FM/test_misdeed/miss1.mis";
    dump("File: \"%s\"\n", filename);
    DBFile *dbfile = dbfile_load(filename);
    DBTagBlock *wr_tagblock;
    wr_tagblock = dbfile_get_tag(dbfile, "WREXT");
    if (! wr_tagblock) wr_tagblock = dbfile_get_tag(dbfile, "WRRGB");
    if (! wr_tagblock) wr_tagblock = dbfile_get_tag(dbfile, "WR");
    assert_message(wr_tagblock, "No WREXT/WRRGB/WR tagblock.");

    dump("%s read ok, 0x%08x bytes of data.\n\n", wr_tagblock->key.s, wr_tagblock->size);
    WorldRep *wr = wr_load_from_tagblock(wr_tagblock);

    dump("\n");

    DBTagBlock wr_tagblock2 = {0};
    wr_save_to_tagblock(&wr_tagblock2, wr);
    assert(wr_tagblock->size==wr_tagblock2.size);
    assert(memcmp(wr_tagblock->data, wr_tagblock2.data, wr_tagblock2.size)==0);

    wr_free(&wr);
    //dbfile_save(dbfile, "e:/dev/thief/T2FM/test_misdeed/out.mis");
    dbfile = dbfile_free(dbfile);
    dump("Ok.\n");
}
