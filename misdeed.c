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
typedef int8_t bool8;
typedef int32_t bool32;

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

#define arrleni32(arr) ((arr) ? (int32)(arrlenu(arr)) : 0L)
#define arrlenu32(arr) ((arr) ? (uint32)(arrlenu(arr)) : 0UL)
#define arrsize(arr) ((arr) ? ((uint32)(arrlenu(arr)*sizeof((arr)[0]))) : 0UL)

#define arrcopy(a,b) do{ \
    if (b) { \
        int len = (int)arrlen(b); \
        arrsetlen((a),len); \
        memcpy((a),(b),len*sizeof((a)[0])); \
    } else if (a) { \
        arrfree(a); \
        (a) = NULL; \
    } }while(0)

#define arrappend(a,b) do{ \
    assert(sizeof((a)[0])==sizeof((b)[0])); \
    size_t sa = arrlenu(a); \
    size_t sb = arrlenu(b); \
    arrsetlen((a),sa+sb); \
    memcpy((a)+sa,(b),sb*sizeof((a)[0])); \
    }while(0)

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
#define TAG_WR      "WR"
#define TAG_WRRGB   "WRRGB"
#define TAG_WREXT   "WREXT"
#define TAG_FAMILY  "FAMILY"
#define TAG_TXLIST  "TXLIST"
#define TAG_PROP_ANIMLIGHT "P$AnimLight"

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

#define LGWREXTHeaderSize 20
#define LGWREXTHeaderWRVersionMax 5

#define LGWREXTFlagLegacy               (1UL<<0)
#define LGWREXTFlagLightmappedWater     (1UL<<1)
#define LGWREXTFlagCellRenderOptions    (1UL<<2)

typedef struct LGWREXTHeader {
    uint32 size;            // Always 20 to date.
    uint32 wr_version;      // Currently 5. have seen 3 and 4 from older newdark versions.
    uint32 flags;           // 1: legacy?? 2: lightmapped water. 4: cell_renderoptions present.
    uint32 lightmap_format; // 0: 16 bit; 1: 32 bit; 2: 32 bit 2x
    int32 lightmap_scale;   // 0: 1x; 2: 2x; 4: 4x; -2: 0.5x; -4: 0.25x
                            // non power of two values may be stored in
                            // here; just ignore all but the highest bit,
                            // and use the sign bit to determine if it
                            // is a multiply or a divide.
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

#define LGWRCellWeatherFog(zone) (((uint8)(zone))&0xf)
#define LGWRCellWeatherAmbient(zone) ((((uint8)(zone))>>4)&0xf)
#define LGWRCellWeatherZone(fog, ambient) (((((uint8)ambient)&0xf)<<4)|(((uint8)fog)&0xf))

// NOTE: the lower 6 bits are for the environment map; it is unclear
//       as of yet whether the upper 2 bits are used.
#define LGWRCellEnvironmentMap(renderoptions) ((uint8)(renderoptions)&0x3f)
#define LGWRCellRenderOptions(envmap) ((uint8)(envmap)&0x3f)

typedef struct LGWRPoly {
    uint8 flags;
    uint8 num_vertices;
    uint8 planeid;       // index into cell's planes
    uint8 clut_id;       // TODO: maybe fixup this too?
    int16 destination;
    uint8 motion_index;  // TODO: maybe fixup this too?
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

#define BSP_INVALID 0x00FFFFFFUL
#define BSP_GET_PARENT(node) ((node)->parent_index&0x00FFFFFFUL)
#define BSP_SET_PARENT(node,new_index) \
    do { \
        LGWRBSPNode *n = (node); \
        n->parent_index = (n->parent_index&0xFF000000UL)|(new_index&0x00FFFFFFUL); \
    } while(0)
#define BSP_GET_FLAGS(node) (((node)->parent_index&0xFF000000UL)>>24)
#define BSP_SET_FLAGS(node,new_flags) \
    do { \
        LGWRBSPNode *n = (node); \
        n->parent_index = ((new_flags<<24)&0xFF000000UL)|(n->parent_index&0x00FFFFFFUL); \
    } while(0)
#define BSP_IS_LEAF(node) ((BSP_GET_FLAGS(node)&kIsLeaf)!=0)

typedef enum {
   kIsLeaf     = 0x01,
   kIsMarked   = 0x02,
   kIsReversed = 0x04,
} LGWRBSPNodeFlags;

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
    int32 plane_id;      // will be -1 for leaf nodes.
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
    LGVector bright; // TODO: ? bright/hue/sat ? r/g/b?
    float inner;
    float outer;
    float radius;
} LGWRRGBLight;

typedef struct LGWRAnimLightToCell {
    uint16 cell_index;
    uint8 pos_in_cell_palette;
    uint8 pad0;
} LGWRAnimLightToCell;

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

// NOTE: Two additional leading slots are used for sky and (default) water;
//       these are not included in LGFAMILY_COUNT_MAX_*.
#define LGFAMILY_COUNT_MAX_OLDDARK 16
#define LGFAMILY_COUNT_MAX_NEWDARK 32
#define LGFAMILY_NAME_SIZE 24
#define LGFAMILY_NAME_NULL "NULL"

typedef struct LGFAMILYHeader {
    uint32 record_size;
    uint32 record_count;    // NOTE: includes sky and water slots.
} LGFAMILYHeader;

typedef struct LGFAMILYRecord {
    char name[LGFAMILY_NAME_SIZE];
} LGFAMILYRecord;

#define LGTXLIST_TOKEN_NAME_SIZE 16
#define LGTXLIST_ITEM_NAME_SIZE 15
#define LGTXLIST_NAME_NULL "null"

typedef struct LGTXLISTHeader {
    uint32 size;
    uint32 item_count;
    uint32 token_count;
} LGTXLISTHeader;

typedef struct LGTXLISTToken {
    char name[LGTXLIST_TOKEN_NAME_SIZE];
} LGTXLISTToken;

typedef struct LGTXLISTItem {
    char tokens[4];
    char name[LGTXLIST_ITEM_NAME_SIZE];
    char pad;
} LGTXLISTItem;

typedef struct LGPropHeader {
    int32   id;
    uint32  size;
} LGPropHeader;

typedef struct LGBaseLight {
    float32 brightness;
    LGVector offset;
} LGBaseLight;

typedef struct LGAnimLightAnimation {
    bool32 refresh;
    // connection to world rep
    int16 lighttocell_start;   // index of first entry in animlight_to_cell_array
    int16 lighttocell_count;   // count of entires in animlight_to_cell_array
    int16 light_array_index;    // index of this light in light_white/rgb_array
    // control of fluctuation
    int16 mode;
    int32 time_rising_ms;
    int32 time_falling_ms;
    float32 min_brightness;
    float32 max_brightness;
    // current state
    float32 brightness;
    bool32 is_rising;
    int32 countdown_ms;
    bool32 inactive;
} LGAnimLightAnimation;

typedef struct LGAnimLight {
    LGBaseLight base;
    LGAnimLightAnimation animation;
    float radius;
    int32 notify_script_objid;
    bool32 quad;
    float32 inner_radius;
    bool32 is_dynamic;      // New in NewDark
} LGAnimLight;

typedef struct LGAnimLightProp {
    LGPropHeader header;
    LGAnimLight prop;
} LGAnimLightProp;

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
    LGDBVersion version;
    DBTagBlock *tagblock_hash;
} DBFile;

static DBTagBlockName tag_name_from_str(const char *src) {
    static DBTagBlockName dest;
    strncpy(dest.s, src, (sizeof dest.s)/(sizeof dest.s[0]));
    return dest;
}

static int tag_name_eq(DBTagBlockName name0, DBTagBlockName name1) {
    return (memcmp(&name0, &name1, sizeof(DBTagBlockName))==0);
}

static int tag_name_eq_str(DBTagBlockName name0, const char *name1) {
    return tag_name_eq(name0, tag_name_from_str(name1));
}

static DBTagBlock *dbfile_get_tag_nullable(DBFile *dbfile, const char *name_str) {
    DBTagBlockName name = tag_name_from_str(name_str);
    return hmgetp_null(dbfile->tagblock_hash, name);
}

static DBTagBlock *dbfile_get_tag(DBFile *dbfile, const char * name_str) {
    DBTagBlock *tagblock = dbfile_get_tag_nullable(dbfile, name_str);
    assert_format(tagblock, "No %s tagblock.", name_str);
    return tagblock;
}

static uint32 dbfile_tag_count(DBFile *dbfile) {
    return (uint32)hmlenu(dbfile->tagblock_hash);
}

static DBTagBlock *dbfile_tag_at_index(DBFile *dbfile, uint32 index) {
    return &dbfile->tagblock_hash[index];
}

static void dbtagblock_copy(DBTagBlock *dest, DBTagBlock *src) {
    memcpy(dest, src, sizeof(DBTagBlock));
    if (dest->size > 0) {
        dest->data = malloc(dest->size);
        memcpy(dest->data, src->data, dest->size);
    } else {
        dest->data = 0;
    }
}

static void dbtagblock_wipe(DBTagBlock *tagblock) {
    tagblock->size = 0;
    free(tagblock->data);
    tagblock->data = 0;
}

#define DBTAGBLOCK_READ_ARRAY(array, tagblock) \
    do { \
    DBTagBlock *tb = (tagblock); \
    uint32 item_size = sizeof(array[0]); \
    assert_format(tb->size%item_size==0, \
        "%s tagblock size is not a multiple of %u.", tb->key.s, item_size); \
    uint32 count = tb->size/item_size; \
    arrsetlen(array, count); \
    memcpy(array, tb->data, tb->size); \
    } while(0)

#define DBTAGBLOCK_WRITE_ARRAY(tagblock, array) \
    do { \
    DBTagBlock *tb = (tagblock); \
    dbtagblock_write_array(tb, array, sizeof(array[0]), (uint32)arrlenu(array)); \
    } while(0)

static void dbtagblock_write_array(
    DBTagBlock *tagblock,
    void *array,
    uint32 item_size,
    uint32 item_count)
{
    assert(tagblock->data==NULL);
    uint32 size = item_count*item_size;
    tagblock->size = size;
    tagblock->data = malloc(size);
    memcpy(tagblock->data, array, size);
}


DBFile *dbfile_load(const char *filename) {
    DBFile *dbfile = calloc(1, sizeof(DBFile));
    FILE *file = fopen(filename, "rb");
    assert(file);

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
    assert(file);
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
    toc_header.entry_count = arrlenu32(toc_array);
    WRITE(toc_header);
    for (uint32 i=0, iend=arrlenu32(toc_array); i<iend; ++i) {
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

DBTagBlock *dbfile_get_wr_tagblock(DBFile *dbfile) {
    DBTagBlock *tagblock;
    tagblock = dbfile_get_tag_nullable(dbfile, TAG_WREXT);
    if (! tagblock) tagblock = dbfile_get_tag_nullable(dbfile, TAG_WRRGB);
    if (! tagblock) tagblock = dbfile_get_tag_nullable(dbfile, TAG_WR);
    assert_format(tagblock, "No %s/%s/%s tagblock.", TAG_WR, TAG_WRRGB, TAG_WREXT);
    return tagblock;
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
    int16 *animlight_array;
    LGWRLightMapInfo *lightmapinfo_array;
    int16 *light_index_array;
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
    uint32 flags;                               // = LGWREXTHeader.flags
    WorldRepCell *cell_array;
    LGWRPlane *bsp_extraplane_array;
    LGWRBSPNode *bsp_node_array;
    uint8 *cell_weatherzones_array;             // only if WREXT
    uint8 *cell_renderoptions_array;            // only if WREXT.flags & LGWREXTFlagCellRenderOptions
    uint32 num_static_lights;                   // NOTE: num_static_lights+num_dynamic_lights
    uint32 num_dynamic_lights;                  //       will == arrlen(light_*_array).
    // NOTE: although WR uses LGWRWhiteLight, for simplicity we convert those
    //       to/from LGWRRGBLight when reading/writing our WorldRep.
    LGWRRGBLight *light_array;

    LGWRAnimLightToCell *animlight_to_cell_array;
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

WorldRepLightmapFormat _wr_decode_lightmap_format(WorldRepFormat wr_format, LGWREXTHeader *ext_header) {
    // `ext_header` should be NULL for WR/WRRGB.
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
        assert(ext_header!=NULL);
        format.lightmap_scale = _wr_decode_lightmap_scale(ext_header->lightmap_scale);
        switch (ext_header->lightmap_format) {
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
    for (uint32 c=0, cend=arrlenu32(wr->cell_array); c<cend; ++c) {
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
    for (uint32 i=0, iend=arrlenu32(cell->lightmapinfo_array); i<iend; ++i) {
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
    uint32 index_count = arrlenu32(cell->index_array);
    MEM_WRITE(index_count, pwrite);
    MEM_WRITE_ARRAY(cell->index_array, pwrite);
    MEM_WRITE_ARRAY(cell->plane_array, pwrite);
    MEM_WRITE_ARRAY(cell->animlight_array, pwrite);
    MEM_WRITE_ARRAY(cell->lightmapinfo_array, pwrite);
    MEM_WRITE_SIZE(cell->lightmaps, cell->lightmaps_size, pwrite);
    uint32 num_light_indices = arrlenu32(cell->light_index_array);
    MEM_WRITE(num_light_indices, pwrite);
    MEM_WRITE_ARRAY(cell->light_index_array, pwrite);
    return pwrite;
}

void wr_copy_cell(WorldRepCell *out_cell, WorldRepCell *cell) {
    MEM_ZERO(out_cell, sizeof(*out_cell));
    out_cell->is_ext = cell->is_ext;
    out_cell->header = cell->header;
    arrcopy(out_cell->vertex_array, cell->vertex_array);
    arrcopy(out_cell->poly_array, cell->poly_array);
    arrcopy(out_cell->renderpoly_ext_array, cell->renderpoly_ext_array);
    arrcopy(out_cell->renderpoly_array, cell->renderpoly_array);
    arrcopy(out_cell->index_array, cell->index_array);
    arrcopy(out_cell->plane_array, cell->plane_array);
    arrcopy(out_cell->animlight_array, cell->animlight_array);
    arrcopy(out_cell->lightmapinfo_array, cell->lightmapinfo_array);
    arrcopy(out_cell->light_index_array, cell->light_index_array);
    out_cell->lightmaps_size = cell->lightmaps_size;
    out_cell->lightmaps = malloc(out_cell->lightmaps_size);
    memcpy(out_cell->lightmaps, cell->lightmaps, out_cell->lightmaps_size);
}

void wr_free(WorldRep **pwr) {
    WorldRep *wr = *pwr;
    for (uint32 i=0, iend=arrlenu32(wr->cell_array); i<iend; ++i) {
        wr_free_cell(&wr->cell_array[i]);
    }
    arrfree(wr->cell_array);
    arrfree(wr->bsp_extraplane_array);
    arrfree(wr->bsp_node_array);
    arrfree(wr->cell_weatherzones_array);
    arrfree(wr->cell_renderoptions_array);
    arrfree(wr->light_array);
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

    if (tag_name_eq(tagblock->key, tag_name_from_str(TAG_WR))) {
        wr->format = WorldRepFormatWR;
    } else if (tag_name_eq(tagblock->key, tag_name_from_str(TAG_WRRGB))) {
        wr->format = WorldRepFormatWRRGB;
    } else if (tag_name_eq(tagblock->key, tag_name_from_str(TAG_WREXT))) {
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

    dump("%s chunk:\n", tagblock->key.s);
    dump("  version: %d.%d\n", tagblock->version.major, tagblock->version.minor);

    if (is_wrext) {
        LGWREXTHeader ext_header;
        MEM_READ(ext_header, pread);
        assert(ext_header.size==LGWREXTHeaderSize);
        assert(ext_header.wr_version<=LGWREXTHeaderWRVersionMax);
        assert(! (ext_header.flags&LGWREXTFlagLegacy)); // Don't understand what the flag means yet, so don't allow it.
        wr->lightmap_format = _wr_decode_lightmap_format(wr->format, &ext_header);
        wr->flags = ext_header.flags;

        dump("  size: %lu\n", ext_header.size);
        dump("  wr_version: %lu\n", ext_header.wr_version);
        dump("  flags: 0x%08x\n", ext_header.flags);
        dump("  lightmap_format: %ld\n", ext_header.lightmap_format);
        dump("  lightmap_scale: 0x%08x\n", ext_header.lightmap_scale);
    }

    LGWRHeader header;
    MEM_READ(header, pread);
    if (! is_wrext) {
        wr->lightmap_format = _wr_decode_lightmap_format(wr->format, NULL);
    }
    dump("  cell_alloc_size: %lu\n", header.cell_alloc_size);
    dump("  cell_count: %lu\n", header.cell_count);

    int lightmap_bpp = wr->lightmap_format.lightmap_bpp;
    arrsetlen(wr->cell_array, header.cell_count);
    for (uint32 i=0, iend=arrlenu32(wr->cell_array); i<iend; ++i) {
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

    if (is_wrext) {
        MEM_READ_ARRAY(wr->cell_weatherzones_array, arrlenu(wr->cell_array), pread);
        if (wr->flags & LGWREXTFlagCellRenderOptions) {
            MEM_READ_ARRAY(wr->cell_renderoptions_array, arrlenu(wr->cell_array), pread);
        }
    }

    MEM_READ(wr->num_static_lights, pread);
    MEM_READ(wr->num_dynamic_lights, pread);
    uint32 num_total_lights = wr->num_static_lights+wr->num_dynamic_lights;
    // WR,WRRGB always store 768 light records, even when there are
    // fewer total lights. Skip the remainder.
    // NOTE: Dromed's test for this is if ext_header.wr_version<3; I think
    //       that means it assigns version 1 to T1/TG WR, and version 2 to
    //       T2 WRRGB -- but I am not certain.
    uint32 num_extra_light_records = is_wrext? 0 : (768-num_total_lights);
    if (is_wr) {
        LGWRWhiteLight *light_white_array = NULL;
        MEM_READ_ARRAY(light_white_array, num_total_lights, pread);
        pread += num_extra_light_records*sizeof(LGWRWhiteLight);
        // Copy white lights into rgb lights:
        arrsetlen(wr->light_array, num_total_lights);
        for (uint32 i=0; i<num_total_lights; ++i) {
            LGWRWhiteLight *wl = &light_white_array[i];
            LGWRRGBLight *rgbl = &wr->light_array[i];
            rgbl->location = wl->location;
            rgbl->direction = wl->direction;
            rgbl->bright.x = wl->bright;
            rgbl->bright.y = wl->bright;
            rgbl->bright.z = wl->bright;
            rgbl->inner = wl->inner;
            rgbl->outer = wl->outer;
            rgbl->radius = wl->radius;
        }
        arrfree(light_white_array);
    } else {
        MEM_READ_ARRAY(wr->light_array, num_total_lights, pread);
        pread += num_extra_light_records*sizeof(LGWRRGBLight);
    }

    // WR,WRRGB,WREXT always store 32 additional light records for some reason!
    // In the code they're "light_this", used as a scratchpad for adding up
    // lighting contributions for an object. Kind of a bug that they are written
    // to the worldrep! Skip reading them.
    uint32 num_objlight_records = 32;
    if (is_wr) {
        pread += num_objlight_records*sizeof(LGWRWhiteLight);
    } else {
        pread += num_objlight_records*sizeof(LGWRRGBLight);
    }

    uint32 num_animlight_to_cell;
    MEM_READ(num_animlight_to_cell, pread);
    MEM_READ_ARRAY(wr->animlight_to_cell_array, num_animlight_to_cell, pread);

    uint32 csg_cell_count;
    MEM_READ(csg_cell_count, pread);
    if (csg_cell_count>0) {
        assert(csg_cell_count==arrlenu(wr->cell_array));
        dump("csg_cell_count: %lu\n", csg_cell_count);
        uint32 csg_brfaces_count = 0;
        for (uint32 i=0, iend=csg_cell_count; i<iend; ++i) {
            uint32 renderpoly_count;
            if (is_wrext) {
                renderpoly_count = arrlenu32(wr->cell_array[i].renderpoly_ext_array);
            } else {
                renderpoly_count = arrlenu32(wr->cell_array[i].renderpoly_array);
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
        for (uint32 i=0, iend=arrlenu32(wr->csg_brush_plane_count_array); i<iend; ++i) {
            csg_brush_plane_total_count += wr->csg_brush_plane_count_array[i];
        }
        dump("csg_brush_plane_total_count: %lu\n", csg_brush_plane_total_count);
        MEM_READ_ARRAY(wr->csg_brush_planes_array, csg_brush_plane_total_count, pread);
        MEM_READ_ARRAY(wr->csg_brush_surfaceref_count_array, csg_brush_count, pread);
        uint32 csg_brush_surfaceref_total_count = 0;
        for (uint32 i=0, iend=arrlenu32(wr->csg_brush_surfaceref_count_array); i<iend; ++i) {
            csg_brush_surfaceref_total_count += wr->csg_brush_surfaceref_count_array[i];
        }
        MEM_READ_ARRAY(wr->csg_brush_surfacerefs_array, csg_brush_surfaceref_total_count, pread);
    } else {
        uint32 csg_brush_count;
        MEM_READ(csg_brush_count, pread);
        assert(csg_brush_count==0);
    }

    // Ensure we have read all the available data:
    assert(pread==(tagblock->data+tagblock->size));
    // Ensure we have a correct cell_alloc_size calculation:
    uint32 calc_cell_alloc_size = _wr_calc_cell_alloc_size(wr);
    assert_format(calc_cell_alloc_size==header.cell_alloc_size,
        "Size mismatch! header.cell_alloc_size:%lu calculated cell_alloc_size:%lu",
        header.cell_alloc_size, calc_cell_alloc_size);

    return wr;
}

void wr_save_to_tagblock(DBTagBlock *tagblock, WorldRep *wr, int include_csg) {
    assert(tagblock->data==NULL);
    // We write to a 256MB temporary buffer, and at the end we malloc
    // tagblock->data and copy into that. Its not the most efficient
    // way to go about things, but it's simpler, and that's more
    // valuable here.
    uint32 buffer_size = 256UL*1048576UL;
    void *buffer = malloc(buffer_size);

    char *pwrite = buffer;

    switch (wr->format) {
    case WorldRepFormatWR: tagblock->key = tag_name_from_str(TAG_WR); break;
    case WorldRepFormatWRRGB: tagblock->key = tag_name_from_str(TAG_WRRGB); break;
    case WorldRepFormatWREXT: tagblock->key = tag_name_from_str(TAG_WREXT); break;
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
        LGWREXTHeader ext_header;
        ext_header.size = LGWREXTHeaderSize;
        ext_header.wr_version = LGWREXTHeaderWRVersionMax;
        ext_header.flags = wr->flags;
        ext_header.lightmap_format = _wr_encode_lightmap_format(wr->format, wr->lightmap_format);
        ext_header.lightmap_scale = _wr_encode_lightmap_scale(wr->lightmap_format.lightmap_scale);
        MEM_WRITE(ext_header, pwrite);

        dump("  size: %lu\n", ext_header.size);
        dump("  wr_version: %lu\n", ext_header.wr_version);
        dump("  flags: 0x%08x\n", ext_header.flags);
        dump("  lightmap_format: %ld\n", ext_header.lightmap_format);
        dump("  lightmap_scale: 0x%08x\n", ext_header.lightmap_scale);
    }

    LGWRHeader header;
    header.cell_alloc_size = _wr_calc_cell_alloc_size(wr);
    header.cell_count = arrlenu32(wr->cell_array);
    MEM_WRITE(header, pwrite);

    dump("  cell_alloc_size: %lu\n", header.cell_alloc_size);
    dump("  cell_count: %lu\n", header.cell_count);

    int lightmap_bpp = wr->lightmap_format.lightmap_bpp;
    for (uint32 i=0, iend=arrlenu32(wr->cell_array); i<iend; ++i) {
        pwrite = wr_write_cell(&wr->cell_array[i], is_wrext, lightmap_bpp, pwrite);
    }

    uint32 bsp_extraplane_count = arrlenu32(wr->bsp_extraplane_array);
    MEM_WRITE(bsp_extraplane_count, pwrite);
    MEM_WRITE_ARRAY(wr->bsp_extraplane_array, pwrite);
    dump("  bsp_extraplane_count: %lu\n", bsp_extraplane_count);

    uint32 bsp_node_count = arrlenu32(wr->bsp_node_array);
    MEM_WRITE(bsp_node_count, pwrite);
    MEM_WRITE_ARRAY(wr->bsp_node_array, pwrite);
    dump("  bsp_node_count: %lu\n", bsp_node_count);

    if (is_wrext) {
        MEM_WRITE_ARRAY(wr->cell_weatherzones_array, pwrite);
        if (wr->flags & LGWREXTFlagCellRenderOptions) {
            MEM_WRITE_ARRAY(wr->cell_renderoptions_array, pwrite);
        }
    }

    uint32 num_total_lights = arrlenu32(wr->light_array);
    assert(wr->num_static_lights+wr->num_dynamic_lights==num_total_lights);
    MEM_WRITE(wr->num_static_lights, pwrite);
    MEM_WRITE(wr->num_dynamic_lights, pwrite);

    // WR,WRRGB always store 768 light records, even when there are
    // fewer total lights. Zero the remainder.
    uint32 num_extra_light_records = is_wrext ? 0 : (768-num_total_lights);
    if (is_wr) {
        LGWRWhiteLight *light_white_array = NULL;
        arrsetlen(light_white_array, num_total_lights);
        for (uint32 i=0; i<num_total_lights; ++i) {
            LGWRWhiteLight *wl = &light_white_array[i];
            LGWRRGBLight *rgbl = &wr->light_array[i];
            wl->location = rgbl->location;
            wl->direction = rgbl->direction;
            wl->bright = rgbl->bright.x;
            wl->inner = rgbl->inner;
            wl->outer = rgbl->outer;
            wl->radius = rgbl->radius;
        }
        MEM_WRITE_ARRAY(light_white_array, pwrite);
        arrfree(light_white_array);
        LGWRWhiteLight dummy = {0};
        for (uint32 i=0, iend=num_extra_light_records; i<iend; ++i) {
            MEM_WRITE(dummy, pwrite);
        }
    } else {
        MEM_WRITE_ARRAY(wr->light_array, pwrite);
        LGWRRGBLight dummy = {0};
        for (uint32 i=0, iend=num_extra_light_records; i<iend; ++i) {
            MEM_WRITE(dummy, pwrite);
        }
    }

    // WR,WRRGB,WREXT always store 32 additioanl light records for some reason!
    // In the code they're "light_this", used as a scratchpad for adding up
    // lighting contributions for an object. Kind of a bug that they are written
    // to the worldrep! Zero them.
    uint32 num_objlight_records = 32;
    if (is_wr) {
        LGWRWhiteLight dummy = {0};
        for (uint32 i=0, iend=num_objlight_records; i<iend; ++i) {
            MEM_WRITE(dummy, pwrite);
        }
    } else {
        LGWRRGBLight dummy = {0};
        for (uint32 i=0, iend=num_objlight_records; i<iend; ++i) {
            MEM_WRITE(dummy, pwrite);
        }
    }

    uint32 num_animlight_to_cell = arrlenu32(wr->animlight_to_cell_array);
    MEM_WRITE(num_animlight_to_cell, pwrite);
    MEM_WRITE_ARRAY(wr->animlight_to_cell_array, pwrite);

    if (include_csg) {
        uint32 csg_cell_count = arrlenu32(wr->cell_array);
        MEM_WRITE(csg_cell_count, pwrite);
        dump("csg_cell_count: %lu\n", csg_cell_count);
        uint32 csg_brfaces_count = arrlenu32(wr->csg_brfaces_array);
        dump("csg_brfaces_count: %lu\n", csg_brfaces_count);
        MEM_WRITE_ARRAY(wr->csg_brfaces_array, pwrite);
        uint32 csg_brush_count = arrlenu32(wr->csg_brush_plane_count_array);
        MEM_WRITE(csg_brush_count, pwrite);
        dump("csg_brush_count: %lu\n", csg_brush_count);
        MEM_WRITE_ARRAY(wr->csg_brush_plane_count_array, pwrite);
        uint32 csg_brush_plane_total_count = arrlenu32(wr->csg_brush_planes_array);
        dump("csg_brush_plane_total_count: %lu\n", csg_brush_plane_total_count);
        MEM_WRITE_ARRAY(wr->csg_brush_planes_array, pwrite);
        MEM_WRITE_ARRAY(wr->csg_brush_surfaceref_count_array, pwrite);
        MEM_WRITE_ARRAY(wr->csg_brush_surfacerefs_array, pwrite);
    } else {
        uint32 csg_cell_count = 0;
        MEM_WRITE(csg_cell_count, pwrite);
        dump("csg_cell_count: %lu\n", csg_cell_count);
        uint32 csg_brush_count = 0;
        MEM_WRITE(csg_brush_count, pwrite);
    }

    uint32 size = (uint32)(pwrite-(char *)buffer);
    assert(size<=buffer_size);
    tagblock->size = size;
    tagblock->data = malloc(size);
    memcpy(tagblock->data, buffer, size);
    free(buffer);
}

DBFile *dbfile_merge_worldreps(
    DBFile *dbfile1,
    DBFile *dbfile2,
    LGWRPlane split_plane)
{
    // Load both worldreps and merge them.
    WorldRep *wr1 = wr_load_from_tagblock(dbfile_get_wr_tagblock(dbfile1));
    WorldRep *wr2 = wr_load_from_tagblock(dbfile_get_wr_tagblock(dbfile2));
    assert(wr1->format==wr2->format);
    assert(wr1->lightmap_format.lightmap_bpp==wr2->lightmap_format.lightmap_bpp);
    assert(wr1->lightmap_format.lightmap_2x_modulation==wr2->lightmap_format.lightmap_2x_modulation);
    assert(wr1->lightmap_format.lightmap_scale==wr2->lightmap_format.lightmap_scale);
    assert(wr1->flags==wr2->flags);
    WorldRep *wrm = calloc(1, sizeof(WorldRep));
    wrm->format = wr1->format;
    wrm->lightmap_format = wr1->lightmap_format;
    wrm->flags = wr1->flags;
    int is_wr = (wr1->format==WorldRepFormatWR);
    int is_wrext = (wr1->format==WorldRepFormatWREXT);

    // wrm cells := [wr1 cells] [wr2 cells]
    int16 wr1_cell_count = arrlenu32(wr1->cell_array);
    int16 wr2_cell_count = arrlenu32(wr2->cell_array);
    assert_message(wr1_cell_count<=32000-wr2_cell_count, "too many cells!");
    int16 wrm_cell_count = wr1_cell_count+wr2_cell_count;
    int16 wr1_cell_start = 0,
          wr1_cell_end = wr1_cell_start+wr1_cell_count;
    int16 wr2_cell_start = wr1_cell_end,
          wr2_cell_end = wr2_cell_start+wr2_cell_count;
    // Cells must be copied individually (their memory is owned by the worldrep).
    arrsetlen(wrm->cell_array, wrm_cell_count);
    for (int16 i=0, j=wr1_cell_start; i<wr1_cell_count; ++i, ++j)
        wr_copy_cell(&wrm->cell_array[j], &wr1->cell_array[i]);
    for (int16 i=0, j=wr2_cell_start; i<wr2_cell_count; ++i, ++j)
        wr_copy_cell(&wrm->cell_array[j], &wr2->cell_array[i]);

    // wrm bsp extraplanes := [split plane] [wr1 planes] [wr2 planes]
    uint32 split_plane_count = 1;
    uint32 wr1_bsp_extraplane_count = arrlenu32(wr1->bsp_extraplane_array);
    uint32 wr2_bsp_extraplane_count = arrlenu32(wr2->bsp_extraplane_array);
    uint32 wrm_bsp_extraplane_count = split_plane_count+wr1_bsp_extraplane_count+wr2_bsp_extraplane_count;
    uint32 wr1_bsp_extraplane_start = split_plane_count,
           wr1_bsp_extraplane_end = wr1_bsp_extraplane_start+wr1_bsp_extraplane_count;
    uint32 wr2_bsp_extraplane_start = wr1_bsp_extraplane_end,
           wr2_bsp_extraplane_end = wr2_bsp_extraplane_start+wr2_bsp_extraplane_count;
    arrsetlen(wrm->bsp_extraplane_array, wrm_bsp_extraplane_count);
    wrm->bsp_extraplane_array[0] = split_plane;
    for (uint32 i=0, j=wr1_bsp_extraplane_start; i<wr1_bsp_extraplane_count; ++i, ++j)
        wrm->bsp_extraplane_array[j] = wr1->bsp_extraplane_array[i];
    for (uint32 i=0, j=wr2_bsp_extraplane_start; i<wr2_bsp_extraplane_count; ++i, ++j)
        wrm->bsp_extraplane_array[j] = wr2->bsp_extraplane_array[i];

    // wrm bsp nodes := [split node] [wr1 nodes] [wr2 nodes]
    uint32 split_node_count = 1;
    uint32 wr1_bsp_node_count = arrlenu32(wr1->bsp_node_array);
    uint32 wr2_bsp_node_count = arrlenu32(wr2->bsp_node_array);
    uint32 wrm_bsp_node_count = split_node_count+wr1_bsp_node_count+wr2_bsp_node_count;
    uint32 wr1_bsp_node_start = split_node_count,
           wr1_bsp_node_end = wr1_bsp_node_start+wr1_bsp_node_count;
    uint32 wr2_bsp_node_start = wr1_bsp_node_end,
           wr2_bsp_node_end = wr2_bsp_node_start+wr2_bsp_node_count;
    arrsetlen(wrm->bsp_node_array, wrm_bsp_node_count);
    LGWRBSPNode split_node;
    split_node.parent_index = BSP_INVALID;
    split_node.plane_cell_id = -1;
    split_node.plane_id = 0;
    split_node.inside_index = wr1_bsp_node_start;
    split_node.outside_index = wr2_bsp_node_start;
    wrm->bsp_node_array[0] = split_node;
    for (uint32 i=0, j=wr1_bsp_node_start; i<wr1_bsp_node_count; ++i, ++j)
        wrm->bsp_node_array[j] = wr1->bsp_node_array[i];
    for (uint32 i=0, j=wr2_bsp_node_start; i<wr2_bsp_node_count; ++i, ++j)
        wrm->bsp_node_array[j] = wr2->bsp_node_array[i];

    // Fixup cells:
    for (int16 i=0; i<wrm_cell_count; ++i) {
        int16 cell_fixup;
        if (i>=wr1_cell_start && i<wr1_cell_end) {
            cell_fixup = wr1_cell_start;
        } else if (i>=wr2_cell_start && i<wr2_cell_end) {
            cell_fixup = wr2_cell_start;
        } else {
            cell_fixup = 0;
        }
        WorldRepCell *cell = &wrm->cell_array[i];
        // Fixup portal destinations.
        uint32 portal_start = cell->header.num_polys-cell->header.num_portal_polys;
        uint32 portal_end = cell->header.num_polys;
        for (uint32 p=portal_start; p<portal_end; ++p) {
            cell->poly_array[p].destination += cell_fixup;
        }
    }

    // Fixup bsp nodes:
    for (uint32 i=0; i<wrm_bsp_node_count; ++i) {
        int16 cell_fixup;
        uint32 node_fixup;
        uint32 extraplane_fixup;
        if (i>=wr1_bsp_node_start && i<wr1_bsp_node_end) {
            cell_fixup = wr1_cell_start;
            node_fixup = wr1_bsp_node_start;
            extraplane_fixup = wr1_bsp_extraplane_start;
        } else if (i>=wr2_bsp_node_start && i<wr2_bsp_node_end) {
            cell_fixup = wr2_cell_start;
            node_fixup = wr2_bsp_node_start;
            extraplane_fixup = wr2_bsp_extraplane_start;
        } else {
            cell_fixup = 0;
            node_fixup = 0;
            extraplane_fixup = 0;
        }
        // Fixup node parents.
        LGWRBSPNode *node = &wrm->bsp_node_array[i];
        uint32 parent_index = BSP_GET_PARENT(node);
        if (parent_index==BSP_INVALID) {
            parent_index = 0;
        } else {
            parent_index += node_fixup;
        }
        BSP_SET_PARENT(node, parent_index);
        // Fixup node cells and planes.
        if (BSP_IS_LEAF(node)) {
            node->cell_id += cell_fixup;
        } else {
            if (node->plane_cell_id==-1) {
                node->plane_id += extraplane_fixup;
            } else {
                node->plane_cell_id += cell_fixup;
            }
            if (node->inside_index!=BSP_INVALID)
                node->inside_index += node_fixup;
            if (node->outside_index!=BSP_INVALID)
                node->outside_index += node_fixup;
        }
        // Fix Marked flag.
        // NOTE: When rendering, setup_bsp() *skips* clearing the Marked flag for
        //       a node's subtree if it itself does not have the Marked flag.
        //       Obviously the Marked flag should be transient and not saved -- and
        //       yet it *is* saved. And so when we happen to get one of our bsp
        //       trees to be merged having the Marked flag, but *our new root node
        //       is not Marked*, this causes the clearing to not happen. And so
        //       then sort_via_bsp() does the wrong thing, and we end up with a
        //       crash! We address this either by unmarking all the nodes.
        uint8 flags = BSP_GET_FLAGS(node);
        flags &= ~kIsMarked;
        BSP_SET_FLAGS(node, flags);
    }

    // wrm cell-weatherzones := [wr1 cell-weatherzones] [wr2 cell-weatherzones]
    if (is_wrext) {
        int16 wr1_cell_weatherzones_count = arrlenu32(wr1->cell_weatherzones_array);
        int16 wr2_cell_weatherzones_count = arrlenu32(wr2->cell_weatherzones_array);
        assert(wr1_cell_weatherzones_count==wr1_cell_count);
        assert(wr2_cell_weatherzones_count==wr2_cell_count);
        int16 wrm_cell_weatherzones_count = wr1_cell_weatherzones_count+wr2_cell_weatherzones_count;
        int16 wr1_cell_weatherzones_start = 0;
        int16 wr2_cell_weatherzones_start = wr1_cell_weatherzones_start+wr1_cell_weatherzones_count;
        arrsetlen(wrm->cell_weatherzones_array, wrm_cell_weatherzones_count);
        for (int16 i=0, j=wr1_cell_weatherzones_start; i<wr1_cell_weatherzones_count; ++i, ++j)
            wrm->cell_weatherzones_array[j] = wr1->cell_weatherzones_array[i];
        for (int16 i=0, j=wr2_cell_weatherzones_start; i<wr2_cell_weatherzones_count; ++i, ++j)
            wrm->cell_weatherzones_array[j] = wr2->cell_weatherzones_array[i];
    }

    // wrm cell-renderoptions := [wr1 cell-renderoptions] [wr2 cell-renderoptions]
    if (is_wrext
    && wrm->flags&LGWREXTFlagCellRenderOptions) {
        int16 wr1_cell_renderoptions_count = arrlenu32(wr1->cell_renderoptions_array);
        int16 wr2_cell_renderoptions_count = arrlenu32(wr2->cell_renderoptions_array);
        assert(wr1_cell_renderoptions_count==wr1_cell_count);
        assert(wr2_cell_renderoptions_count==wr2_cell_count);
        int16 wrm_cell_renderoptions_count = wr1_cell_renderoptions_count+wr2_cell_renderoptions_count;
        int16 wr1_cell_renderoptions_start = 0;
        int16 wr2_cell_renderoptions_start = wr1_cell_renderoptions_start+wr1_cell_renderoptions_count;
        arrsetlen(wrm->cell_renderoptions_array, wrm_cell_renderoptions_count);
        for (int16 i=0, j=wr1_cell_renderoptions_start; i<wr1_cell_renderoptions_count; ++i, ++j)
            wrm->cell_renderoptions_array[j] = wr1->cell_renderoptions_array[i];
        for (int16 i=0, j=wr2_cell_renderoptions_start; i<wr2_cell_renderoptions_count; ++i, ++j)
            wrm->cell_renderoptions_array[j] = wr2->cell_renderoptions_array[i];
    }

    // wrm lights := [sun]
    //               [wr1 static lights, except sun]
    //               [wr2 static lights, except sun]
    //               [wr1 dynamic lights]
    //               [wr2 dynamic lights]
    // NOTE: The first entry in the light table is always present, and reserved
    //       for sunlight.
    // NOTE: "static" lights are Renderer>Light and Renderer>AnimLight.
    //       "dynamic" lights are Renderer>Dynamic Light.
    // TODO: how does NewDark's "dynamic light" checkbox on AnimLights complicate this?
    // TODO: are dynamic lights put in the table even if outside the world?
    //       would make sense, right? so concatenatic the dynamic lights might be wrong!
    int16 wr1_static_light_count = wr1->num_static_lights-1;
    int16 wr2_static_light_count = wr2->num_static_lights-1;
    int16 wr1_dynamic_light_count = wr1->num_dynamic_lights;
    int16 wr2_dynamic_light_count = wr2->num_dynamic_lights;
    wrm->num_static_lights = 1+wr1_static_light_count+wr2_static_light_count;
    wrm->num_dynamic_lights = wr1_dynamic_light_count+wr2_dynamic_light_count;
    assert(wrm->num_static_lights<=768); // TODO: did NewDark raise this limit?
    assert(wrm->num_dynamic_lights<=32);
    int16 wrm_total_light_count = wrm->num_static_lights+wrm->num_dynamic_lights;
    int16 wr1_static_light_start = 1,
          wr1_static_light_end = wr1_static_light_start+wr1_static_light_count;
    int16 wr2_static_light_start = wr1_static_light_end,
          wr2_static_light_end = wr2_static_light_start+wr2_static_light_count;
    int16 wr1_dynamic_light_start = wr2_static_light_end,
          wr1_dynamic_light_end = wr1_dynamic_light_start+wr1_dynamic_light_count;
    int16 wr2_dynamic_light_start = wr1_dynamic_light_end,
          wr2_dynamic_light_end = wr2_dynamic_light_start+wr2_dynamic_light_count;
    assert(wrm_total_light_count==( arrleni32(wr1->light_array)
                                  + arrleni32(wr2->light_array)
                                  - 1 ));
    arrsetlen(wrm->light_array, wrm_total_light_count);
    wrm->light_array[0] = wr1->light_array[0];
    for (int16 i=0, j=wr1_static_light_start; i<wr1_static_light_count; ++i, ++j)
        wrm->light_array[j] = wr1->light_array[1+i];
    for (int16 i=0, j=wr2_static_light_start; i<wr2_static_light_count; ++i, ++j)
        wrm->light_array[j] = wr2->light_array[1+i];
    for (int16 i=0, j=wr1_dynamic_light_start; i<wr1_dynamic_light_count; ++i, ++j)
        wrm->light_array[j] = wr1->light_array[1+wr1_static_light_count+i];
    for (int16 i=0, j=wr2_dynamic_light_start; i<wr2_dynamic_light_count; ++i, ++j)
        wrm->light_array[j] = wr2->light_array[1+wr2_static_light_count+i];

    // TEMP: template for the copying of stuff?
    // uint32 wr1_xxx_count = arrlenu32(wr1->xxx_array);
    // uint32 wr2_xxx_count = arrlenu32(wr2->xxx_array);
    // uint32 wr_xxx_count = wr1_xxx_count+wr2_xxx_count;
    // uint32 wr1_xxx_start = 0;
    // uint32 wr2_xxx_start = wr1_xxx_start+wr1_xxx_count;
    // arrsetlen(wr->xxx_array, wr_xxx_count);
    // for (uint32 i=0, j=wr1_xxx_start; i<wr1_xxx_count; ++i, ++j)
    //     wr->xxx_array[j] = wr1->xxx_array[i];
    // for (uint32 i=0, j=wr2_xxx_start; i<wr2_xxx_count; ++i, ++j)
    //     wr->xxx_array[j] = wr2->xxx_array[i];

    aaaaaaargh what the fuck!?!?


    // foo

    about cell->animlight_array ??? these are also indices into the light_data table.
    i do NOT want to repeat those if (index<><><><>) a FIFTH TIME!!!

    // Fixup light_index_array in each wr1 and wr2 cell.
    // TODO: how does NewDark's "dynamic light" checkbox on AnimLights complicate this?
    for (uint32 i=wr1_cell_start, iend=wr1_cell_start+wr1_cell_count; i<iend; ++i) {
        WorldRepCell *cell = &wr->cell_array[i];
        for (uint32 j=0, jend=arrlenu32(cell->light_index_array); j<jend; ++j) {
            uint16 index = cell->light_index_array[j];
            if (index==0) {
                // sunlight index is unchanged.
            } else if (index<(1+wr1_static_light_count)) {
                // wr1 static light indexes are unchanged.
            } else {
                // wr1 dynamic lights now come after wr2 static lights
                index += wr2_static_light_count;
            }
            cell->light_index_array[j] = index;
        }
    }
    for (uint32 i=wr2_cell_start, iend=wr2_cell_start+wr2_cell_count; i<iend; ++i) {
        WorldRepCell *cell = &wr->cell_array[i];
        for (uint32 j=0, jend=arrlenu32(cell->light_index_array); j<jend; ++j) {
            uint16 index = cell->light_index_array[j];
            if (index==0) {
                // sunlight index is unchanged.
            } else if (index<(1+wr2_static_light_count)) {
                // wr2 static light indexes now come after wr1 static lights
                index += wr1_static_light_count;
            } else {
                // wr2 dynamic lights now come after wr1 static and dynamic lights 
                index += (wr1_static_light_count+wr1_dynamic_light_count);
            }
            cell->light_index_array[j] = index;
        }
    }

    // Merge animlight_to_cell_arrays:
    uint32 wr1_animlighttocell_count = arrlenu32(wr1->animlight_to_cell_array);
    uint32 wr2_animlighttocell_count = arrlenu32(wr2->animlight_to_cell_array);
    uint32 wr1_animlighttocell_start = 0;
    uint32 wr2_animlighttocell_start = wr1_animlighttocell_count;
    uint32 wr_animlighttocell_count = wr1_animlighttocell_count+wr2_animlighttocell_count;
    arrsetlen(wr->animlight_to_cell_array, wr_animlighttocell_count);
    {
        int16 cell_fixup = 0;
        for (uint32 i=0, j=wr1_animlighttocell_start; i<wr1_animlighttocell_count; ++i, ++j) {
            wr->animlight_to_cell_array[j] = wr1->animlight_to_cell_array[i];
            wr->animlight_to_cell_array[j].cell_index += cell_fixup;
        }
        cell_fixup = (uint16)wr1_cell_count;
        for (uint32 i=0, j=wr2_animlighttocell_start; i<wr2_animlighttocell_count; ++i, ++j) {
            wr->animlight_to_cell_array[j] = wr2->animlight_to_cell_array[i];
            wr->animlight_to_cell_array[j].cell_index += cell_fixup;
        }
    }
   
    // Merge and fixup AnimLight propertys.
    LGAnimLightProp *animlight1_array = NULL;
    LGAnimLightProp *animlight2_array = NULL;
    LGAnimLightProp *animlight_out_array = NULL;
    int16 lighttocell_fixup = wr1_animlighttocell_count;
    {
        DBTagBlock *tagblock = dbfile_get_tag(dbfile1, TAG_PROP_ANIMLIGHT);
        assert(tagblock->version.major==2
            && tagblock->version.minor==80);
        DBTAGBLOCK_READ_ARRAY(animlight1_array, tagblock);
        tagblock = dbfile_get_tag(dbfile2, TAG_PROP_ANIMLIGHT);
        assert(tagblock->version.major==2
            && tagblock->version.minor==80);
        DBTAGBLOCK_READ_ARRAY(animlight2_array, tagblock);
        assert(arrlenu32(animlight1_array)==arrlenu32(animlight2_array));
        arrsetlen(animlight_out_array, arrlen(animlight1_array));

        for (uint32 i=0, iend=arrlenu32(animlight_out_array); i<iend; ++i) {
            LGAnimLightAnimation *anim1 = &animlight1_array[i].prop.animation;
            LGAnimLightAnimation *anim2 = &animlight2_array[i].prop.animation;
            if (anim1->light_array_index==-1) {
                if (anim2->light_array_index==-1) {
                    // This light isnt doing anything! just copy it from 1.
                    animlight_out_array[i] = animlight1_array[i];
                } else {
                    // Copy this light from 2, and fix it up.
                    animlight_out_array[i] = animlight2_array[i];
                    LGAnimLightAnimation *anim = &animlight_out_array[i].prop.animation;
                    anim->lighttocell_start += lighttocell_fixup;
                    int16 index = anim->light_array_index;
                    if (index<=0) {
                        // invalid or sunlight index is unchanged.
                    } else if (index<(1+wr2_static_light_count)) {
                        // wr2 static light indexes now come after wr1 static lights
                        index += wr1_static_light_count;
                    } else {
                        // wr2 dynamic lights now come after wr1 static and dynamic lights 
                        index += (wr1_static_light_count+wr1_dynamic_light_count);
                    }
                    anim->light_array_index = index;
                }
            } else {
                if (anim2->light_array_index==-1) {
                    // Copy this light from 1, and fix it up.
                    animlight_out_array[i] = animlight2_array[i];
                    LGAnimLightAnimation *anim = &animlight_out_array[i].prop.animation;
                    // wr1 lights don't need lighttocell fixups.
                    int16 index = anim->light_array_index;
                    if (index<=0) {
                        // invalid or sunlight index is unchanged.
                    } else if (index<(1+wr1_static_light_count)) {
                        // wr1 static light indexes are unchanged.
                    } else {
                        // wr1 dynamic lights now come after wr2 static lights
                        index += wr2_static_light_count;
                    }
                    anim->light_array_index = index;
                } else {
                    abort_message("animlight in both worldreps is complicated and not supported yet.");
                }
            }
        }
    }

    /*
    OKAY: i think i _do_ need to copy the csg_* stuff. probably. seems like
          maybe dromed (old dromed at least) needs it for rendering in
          the editor viewport?
    */
    // int32 *csg_brfaces_array;                       // one brface per renderpoly, per cell
    // int32 *csg_brush_plane_count_array;             // number of planes, per brush
    // LGWRCSGPlane *csg_brush_planes_array;           // all brush planes
    // int32 *csg_brush_surfaceref_count_array;        // number of surfacerefs, per brush
    // LGWRCSGSurfaceRef *csg_brush_surfacerefs_array; // all surfacerefs

    // Write the output file.

    DBFile *dbfile_out = calloc(1, sizeof(DBFile));
    dbfile_out->version = (LGDBVersion){ 0, 1 };

    // Copy all other tagblocks from the first file into the output.
    for (int i=0, iend=dbfile_tag_count(dbfile1); i<iend; ++i) {
        DBTagBlock *src_tagblock = dbfile_tag_at_index(dbfile1, i);

        // Skip the tagblocks that we manually merged.
        if (tag_name_eq_str(src_tagblock->key, TAG_WR)
        || tag_name_eq_str(src_tagblock->key, TAG_WRRGB)
        || tag_name_eq_str(src_tagblock->key, TAG_WREXT)
        || tag_name_eq_str(src_tagblock->key, TAG_PROP_ANIMLIGHT))
            continue;

        DBTagBlock dest_tagblock = {0};
        dbtagblock_copy(&dest_tagblock, src_tagblock);
        hmputs(dbfile_out->tagblock_hash, dest_tagblock);
    }

    // Write the merged worldrep to the output.
    {
        DBTagBlock tagblock = {0};
        wr_save_to_tagblock(&tagblock, wr, 0);
        hmputs(dbfile_out->tagblock_hash, tagblock);
    }

    // Write the merged AnimLight prop to the output.
    {
        DBTagBlock tagblock = {0};
        tagblock.key = tag_name_from_str(TAG_PROP_ANIMLIGHT);
        tagblock.version.major = 2;
        tagblock.version.minor = 80;

        DBTAGBLOCK_WRITE_ARRAY(&tagblock, animlight_out_array);
        hmputs(dbfile_out->tagblock_hash, tagblock);
    }

    return dbfile_out;
}

/** Family stuff */

typedef struct FamilyList {
    int is_newdark;
    LGFAMILYRecord sky_family;
    LGFAMILYRecord water_family;
    LGFAMILYRecord *family_array;
} FamilyList;

typedef struct FamilyRemap {
    LGFAMILYRecord key;
    uint32 index;
} FamilyRemap;

FamilyList *family_load_from_tagblock(DBTagBlock *tagblock) {
    assert(tagblock->version.major==1 && tagblock->version.minor==0);
    FamilyList *fams = calloc(1, sizeof(FamilyList));
    char *pread = tagblock->data;

    LGFAMILYHeader header;
    MEM_READ(header, pread);
    assert(header.record_size==LGFAMILY_NAME_SIZE);
    assert(header.record_count==(LGFAMILY_COUNT_MAX_OLDDARK+2)
        || header.record_count==(LGFAMILY_COUNT_MAX_NEWDARK+2));
    fams->is_newdark = (header.record_count==(LGFAMILY_COUNT_MAX_NEWDARK+2));
    MEM_READ(fams->sky_family, pread);
    MEM_READ(fams->water_family, pread);
    MEM_READ_ARRAY(fams->family_array, (header.record_count-2), pread);

    return fams;
}

void family_save_to_tagblock(DBTagBlock *tagblock, FamilyList *fams) {
    assert(tagblock->data==NULL);

    uint32 count = arrlenu32(fams->family_array);
    assert(count==LGFAMILY_COUNT_MAX_OLDDARK
        || count==LGFAMILY_COUNT_MAX_NEWDARK);
    tagblock->size = sizeof(LGFAMILYHeader) + count*sizeof(LGFAMILYRecord);
    tagblock->data = malloc(tagblock->size);
    char *pwrite = tagblock->data;

    LGFAMILYHeader header;
    header.record_size = sizeof(LGFAMILYRecord);
    header.record_count = count+2;
    MEM_WRITE(header, pwrite);
    MEM_WRITE(fams->sky_family, pwrite);
    MEM_WRITE(fams->water_family, pwrite);
    MEM_WRITE_ARRAY(fams->family_array, pwrite);
}

void family_merge(FamilyList *fams1, FamilyList *fams2, FamilyList **out_fams, FamilyRemap **out_fams2_remap) {
    assert(fams1->is_newdark==fams2->is_newdark);
    uint32 fams1_count = arrlenu32(fams1->family_array);
    uint32 fams2_count = arrlenu32(fams2->family_array);
    uint32 out_count_max = fams1->is_newdark? LGFAMILY_COUNT_MAX_NEWDARK : LGFAMILY_COUNT_MAX_OLDDARK;

    FamilyRemap *lookup = NULL;
    FamilyRemap *remap = NULL;
    FamilyList *merged = calloc(1, sizeof(FamilyList));
    merged->is_newdark = fams1->is_newdark;
    merged->sky_family = fams1->sky_family;
    merged->water_family = fams1->water_family;

    arrsetlen(merged->family_array, out_count_max);

    // Copy in fams1
    uint32 fams2_start = 0;
    for (uint32 i=0, iend=fams1_count; i<iend; ++i) {
        LGFAMILYRecord *record = &fams1->family_array[i];
        merged->family_array[i] = *record;

        FamilyRemap entry;
        if (strcmp(record->name, LGFAMILY_NAME_NULL)==0)
            continue;
        entry.key = *record;
        entry.index = i;
        hmputs(lookup, entry);
        ++fams2_start;
    }

    // Copy in fams2
    for (uint32 i=0, iend=fams2_count, j=fams2_start; i<iend; ++i)
    {
        LGFAMILYRecord *record = &fams2->family_array[i];
        if (strcmp(record->name, LGFAMILY_NAME_NULL)==0)
            continue;
        FamilyRemap *matching_entry = hmgetp_null(lookup, *record);
        FamilyRemap entry;
        entry.key = *record;
        if (matching_entry) {
            entry.index = matching_entry->index;
        } else {
            assert(j<out_count_max);
            merged->family_array[j] = *record;
            entry.index = j;
            ++j;
        }
        hmputs(remap, entry);
    }
    hmfree(lookup);

    // Remap NULLs to the last slot. If something was referencing
    // a NULL family (can that even happen?) and the last slot after
    // merging is actually in use, then too bad.
    FamilyRemap entry = {0};
    strcpy(entry.key.name, LGFAMILY_NAME_NULL);
    entry.index = out_count_max-1;
    hmputs(remap, entry);

    // Fill the rest with nulls
    LGFAMILYRecord null_record = {0};
    strcpy(null_record.name, LGFAMILY_NAME_NULL);
    for (uint32 i=arrlenu32(merged->family_array), iend=out_count_max; i<iend; ++i) {
        merged->family_array[i] = null_record;
    }

    *out_fams = merged;
    *out_fams2_remap = remap;
}

/** Texture stuff */

// Up to 4 tokens each with a trailing /, then the name and null terminator.
#define TEXTURE_ENTRY_NAME_SIZE \
    (4*(LGTXLIST_TOKEN_NAME_SIZE+1)+LGTXLIST_ITEM_NAME_SIZE+1)

typedef struct TextureRecord {
    char name[TEXTURE_ENTRY_NAME_SIZE];
} TextureRecord;

typedef struct TextureList {
    TextureRecord *tex_array;
} TextureList;

typedef struct TextureRemap {
    TextureRecord key;
    uint32 index;
} TextureRemap;

TextureList *txlist_load_from_tagblock(DBTagBlock *tagblock) {
    assert(tagblock->version.major==1 && tagblock->version.minor==0);
    TextureList *texs = calloc(1, sizeof(TextureList));
    char *pread = tagblock->data;

    LGTXLISTHeader header;
    MEM_READ(header, pread);
    assert(header.size==tagblock->size);

    LGTXLISTToken *tokens = NULL;
    MEM_READ_ARRAY(tokens, header.token_count, pread);
    LGTXLISTItem *items = NULL;
    MEM_READ_ARRAY(items, header.item_count, pread);

    for (uint32 i=0; i<header.item_count; ++i) {
        LGTXLISTItem *item = &items[i];
        TextureRecord record = {0};
        for (uint32 j=0; j<4; ++j) {
            char t = item->tokens[j];
            if (t==0) break;
            strcat(record.name, tokens[t-1].name);
            strcat(record.name, "/");
        }
        strcat(record.name, item->name);
        arrput(texs->tex_array, record);
    }

    arrfree(tokens);
    arrfree(items);

    return texs;
}

void txlist_merge(TextureList *texs1, TextureList *texs2, TextureList **out_texs, TextureRemap **out_texs2_remap) {
    TextureRemap *lookup = NULL;
    // TODO: remap here needs to be remapping the ids here, not the names!
    //       also set hmdefault() of it to 0?? BUT we need to handle SKY
    //       and stuff sensibly!
    TextureRemap *remap = NULL;
    TextureList *merged = calloc(1, sizeof(TextureList));
    TextureRecord null_record = {0};
    strcpy(null_record.name, LGTXLIST_NAME_NULL);

    uint32 texs1_count = arrlenu32(texs1->tex_array);
    uint32 texs2_count = arrlenu32(texs2->tex_array);

    // Copy in texs1
    uint32 texs2_start = 1;
    for (uint32 i=0, iend=texs1_count; i<iend; ++i) {
        TextureRecord *record = &texs1->tex_array[i];
        arrput(merged->tex_array, *record);

        TextureRemap entry;
        if (strcmp(record->name, LGTXLIST_NAME_NULL)==0) {
            printf("## skipping %d: %s\n", i, record->name);
            continue;
        }
        entry.key = *record;
        entry.index = i;
        hmputs(lookup, entry);
        ++texs2_start;
    }

    // Copy in non-null texs2
    for (uint32 i=0, iend=texs2_count, j=texs2_start; i<iend; ++i)
    {
        TextureRecord *record = &texs2->tex_array[i];
        if (strcmp(record->name, LGTXLIST_NAME_NULL)==0) {
            printf("## skipping %d: %s\n", i, record->name);
            continue;
        }
        TextureRemap *matching_entry = hmgetp_null(lookup, *record);
        TextureRemap entry;
        entry.key = *record;
        if (matching_entry) {
            entry.index = matching_entry->index;
        } else {
            // Entries 247-255 (inclusive) are reserved.
            while (j>=247 && j<=255) {
                arrput(merged->tex_array, null_record);
                ++j;
            }

            arrput(merged->tex_array, *record);
            entry.index = j;
            ++j;
        }
        hmputs(remap, entry);
    }
    hmfree(lookup);

    *out_texs = merged;
    *out_texs2_remap = remap;
}

/** Commands and stuff */

struct command;
struct command {
    const char *s;
    int (*func)(int, char **, struct command *);
    const char *args;
    const char *help;
};

int do_help(int argc, char **argv, struct command *cmd);

int do_merge(int argc, char **argv, struct command *cmd) {
    if (argc!=4
    || strcmp(argv[2], "-o")!=0) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }

    char *in_filename[2];
    for (int i=0; i<2; ++i)
        in_filename[i] = argv[i];
    char *out_filename = argv[3];

    dump("Files:");
    for (int i=0; i<2; ++i)
        dump(" \"%s\"", in_filename[i]);
    dump("\n");

    DBFile *dbfile[2];
    for (int i=0; i<2; ++i)
        dbfile[i] = dbfile_load(in_filename[i]);

    // TODO: you know what, *i* dont need txlist merging or family merging!
    //       i can work from *one* source mission for terrain, and while doing
    //       terrain portalize vertical slices or single areas. thats fine.
    //       then i export that into three separate .mis, optimize them, merge
    //       them, and do object/lighting workflows with the result.
#if 0
    TextureList *texs[2];
    for (int i=0; i<2; ++i) {
        DBTagBlock *tagblock;
        tagblock = dbfile_get_tag(dbfile[i], TAG_TXLIST);
        texs[i] = txlist_load_from_tagblock(tagblock);
    }

    TextureList *texs_merged = NULL;
    TextureRemap *texs_remap = NULL;
    txlist_merge(texs[0], texs[1], &texs_merged, &texs_remap);

    printf("TXLIST 1:\n");
    for (uint32 i=0, iend=arrlenu32(texs[0]->tex_array); i<iend; ++i) {
        TextureRecord *record = &texs[0]->tex_array[i];
        printf("%u: %s\n", i, record->name);
    }
    printf("\n");

    printf("TXLIST 2:\n");
    for (uint32 i=0, iend=arrlenu32(texs[1]->tex_array); i<iend; ++i) {
        TextureRecord *record = &texs[1]->tex_array[i];
        ptrdiff_t n = hmgeti(texs_remap, *record);
        printf("%02u: %s => ", i, record->name);
        if (n>=0)
            printf("%02u\n", texs_remap[n].index);
        else
            printf("(not in remap)\n");
    }
    printf("\n");

    printf("MERGED:\n");
    for (uint32 i=0, iend=arrlenu32(texs_merged->tex_array); i<iend; ++i) {
        TextureRecord *record = &texs_merged->tex_array[i];
        printf("%u: %s\n", i, record->name);
    }
    printf("\n");
    abort_message("---- TEMP: stopping here ----");

    FamilyList *fams[2];
    for (int i=0; i<2; ++i) {
        DBTagBlock *tagblock;
        tagblock = dbfile_get_tag(dbfile[i], TAG_FAMILY);
        fams[i] = family_load_from_tagblock(tagblock);
    }

    FamilyList *fams_merged = NULL;
    FamilyRemap *fams_remap = NULL;
    family_merge(fams[0], fams[1], &fams_merged, &fams_remap);

    printf("FAMILY 1:\n");
    printf("sky: %s\n", fams[0]->sky_family.name);
    printf("water: %s\n", fams[0]->water_family.name);
    for (uint32 i=0, iend=(uint32)arrlen(fams[0]->family_array); i<iend; ++i) {
        printf("%02u: %s => %02u\n", i, fams[0]->family_array[i].name, i);
    }
    printf("\n");

    printf("FAMILY 2:\n");
    printf("sky: %s\n", fams[1]->sky_family.name);
    printf("water: %s\n", fams[1]->water_family.name);
    for (uint32 i=0, iend=(uint32)arrlen(fams[1]->family_array); i<iend; ++i) {
        ptrdiff_t n = hmgeti(fams_remap, fams[1]->family_array[i]);
        printf("%02u: %s => ", i, fams[1]->family_array[i].name);
        if (n>=0)
            printf("%02u\n", fams_remap[n].index);
        else
            printf("(not in remap)\n");
    }
    printf("\n");

    printf("MERGED:\n");
    printf("sky: %s\n", fams_merged->sky_family.name);
    printf("water: %s\n", fams_merged->water_family.name);
    for (uint32 i=0, iend=(uint32)arrlen(fams_merged->family_array); i<iend; ++i) {
        printf("%02u: %s\n", i, fams_merged->family_array[i].name);
    }
    printf("\n");
    abort_message("---- TEMP: stopping here ----");
#endif

    // TODO: make the split plane a parameter.
    LGWRPlane split_plane;
    split_plane.normal = (LGVector){ 0.0, 0.0, 1.0 };
    split_plane.distance = 0.0;

    DBFile *dbfile_out = dbfile_merge_worldreps(dbfile[0], dbfile[1], split_plane);
    dbfile_save(dbfile_out, out_filename);
    dump("Wrote: \"%s\"\n", out_filename);

    // and clean up maybe?

    for (int i=0; i<2; ++i) {
        dbfile[i] = dbfile_free(dbfile[i]);
    }
    dbfile_out = dbfile_free(dbfile_out);
    dump("Ok.\n");
    return 0;
}

int do_fam_list(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *filename = argv[0];
    DBFile *dbfile = dbfile_load(filename);

    DBTagBlock *tagblock = dbfile_get_tag(dbfile, TAG_FAMILY);

    FamilyList *fams = family_load_from_tagblock(tagblock);
    printf("sky: %s\n", fams->sky_family.name);
    printf("water: %s\n", fams->water_family.name);
    for (uint32 i=0, iend=(uint32)arrlen(fams->family_array); i<iend; ++i) {
        printf("%02u: %s\n", i, fams->family_array[i].name);
    }

    dbfile = dbfile_free(dbfile);
    return 0;
}

int do_tag_list(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *filename = argv[0];
    DBFile *dbfile = dbfile_load(filename);

    for (uint32 i=0, iend=dbfile_tag_count(dbfile); i<iend; ++i) {
        DBTagBlock *tag = dbfile_tag_at_index(dbfile, i);
        printf("%s\t%u.%u\n", tag->key.s, tag->version.major, tag->version.minor);
    }
    dbfile = dbfile_free(dbfile);
    return 0;
}

int do_tag_dump(int argc, char **argv, struct command *cmd) {
    if (argc!=2) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *filename = argv[0];
    DBFile *dbfile = dbfile_load(filename);

    char *tag = argv[1];
    DBTagBlock *tagblock = dbfile_get_tag(dbfile, tag);
    dump("%s version %u.%u: 0x%08x bytes",
        tagblock->key.s,
        tagblock->version.major,
        tagblock->version.minor,
        tagblock->size);

    FileName out_filename = {0};
    strcpy(out_filename.s, tag);
    strcat(out_filename.s, ".tagblock");
    FILE *outf = fopen(out_filename.s, "wb");
    assert(outf);
    fwrite(tagblock->data, tagblock->size, 1, outf);
    fclose(outf);

    dump(" written to \"%s\"\n", out_filename);
    dbfile = dbfile_free(dbfile);
    return 0;
}

int do_tex_list(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *filename = argv[0];
    DBFile *dbfile = dbfile_load(filename);
    DBTagBlock *tagblock = dbfile_get_tag(dbfile, TAG_TXLIST);
    TextureList *txlist = txlist_load_from_tagblock(tagblock);

    for (uint32 i=0, iend=arrlenu32(txlist->tex_array); i<iend; ++i) {
        TextureRecord *record = &txlist->tex_array[i];
        dump("%u: %s\n", i, record->name);
    }

    dbfile = dbfile_free(dbfile);
    return 0;
}

void dump_bsp_node_recursive(LGWRBSPNode *nodes, uint32 index) {
    dump("BSP node %u:\n", index);
    LGWRBSPNode *node = &nodes[index];
    dump("  parent: %u, flags: 0x%02x\n", BSP_GET_PARENT(node), BSP_GET_FLAGS(node));
    dump("  plane_cell_id: %d, plane_id: %d\n", node->plane_cell_id, node->plane_id);
    if (BSP_IS_LEAF(node)) {
        dump("  LEAF. cell_id: %u\n", node->cell_id);
    } else {
        dump("  NODE. inside: %u, outside: %u\n", node->inside_index, node->outside_index);
        if (node->inside_index!=BSP_INVALID)
            dump_bsp_node_recursive(nodes, node->inside_index);
        if (node->outside_index!=BSP_INVALID)
            dump_bsp_node_recursive(nodes, node->outside_index);
    }
}

void dump_bsp_graphviz(WorldRep *wr, FILE *f) {
    fprintf(f, "digraph BSP {\n");
    fprintf(f, "  node [shape=record];\n");
    LGWRBSPNode *node_array = wr->bsp_node_array;
    for (uint32 i=0, iend=arrlenu32(node_array); i<iend; ++i) {
        LGWRBSPNode *node = &node_array[i];

        // Top row: node or leaf and id.

        if (BSP_IS_LEAF(node)) {
            fprintf(f, "  node_%u [label=\"{{LEAF %u|cell %u}",
                i, i, node->cell_id);
        } else {
            fprintf(f, "  node_%u [label=\"{{node %u}", i, i);
        }

        // Middle row: plane and flags

        if (BSP_IS_LEAF(node)) {
            assert(node->plane_cell_id==-1);
            assert(node->plane_id==-1);
            fprintf(f, "|{----}");
        } else {
            LGWRPlane *plane;
            if (node->plane_cell_id==-1) {
                plane = &wr->bsp_extraplane_array[node->plane_id];
            } else {
                plane = &wr->cell_array[node->plane_cell_id].plane_array[node->plane_id];
            }
            char *extra = (node->plane_cell_id==-1)? "X" : "";
            char *rev = (BSP_GET_FLAGS(node)&kIsReversed)? "R" : "";
            char *marked = (BSP_GET_FLAGS(node)&kIsMarked)? "M" : "";
            fprintf(f, "|{%0.2f,%0.2f,%0.2f d %0.2f %s%s%s}",
                plane->normal.x,
                plane->normal.y,
                plane->normal.z,
                plane->distance,
                extra, rev, marked);
        }

        // Bottom row (non-leaf): inside/outside

        if (BSP_IS_LEAF(node)) {
            fprintf(f, "}\"];\n");
        } else {
            fprintf(f, "|{");
            if (node->inside_index==BSP_INVALID)
                fprintf(f, "--");
            else
                fprintf(f, "<in>in %u", node->inside_index);
            fprintf(f, "|");
            if (node->outside_index==BSP_INVALID)
                fprintf(f, "--");
            else
                fprintf(f, "<out>out %u", node->outside_index);
            fprintf(f, "}}\"];\n");

            if (node->inside_index!=BSP_INVALID)
                fprintf(f, "  node_%u:in -> node_%u;\n", i, node->inside_index);
            if (node->outside_index!=BSP_INVALID)
                fprintf(f, "  node_%u:out -> node_%u;\n", i, node->outside_index);
        }
    }
    fprintf(f, "}\n");
}

LGVector vadd(LGVector a, LGVector b) {
    LGVector o;
    o.x = a.x+b.x;
    o.y = a.y+b.y;
    o.z = a.z+b.z;
    return o;
}

LGVector vneg(LGVector a) {
    LGVector o;
    o.x = -a.x;
    o.y = -a.y;
    o.z = -a.z;
    return o;
}

LGVector vmulf(LGVector a, float b) {
    LGVector o;
    o.x = a.x*b;
    o.y = a.y*b;
    o.z = a.z*b;
    return o;
}

float vdot(LGVector a, LGVector b) {
    return a.x*b.x + a.y*b.y + a.z*b.z;
}

LGVector vcross(LGVector a, LGVector b) {
    LGVector o;
    o.x = a.y*b.z - a.z*b.y;
    o.y = a.z*b.x - a.x*b.z;
    o.z = a.x*b.y - a.y*b.x;
    return o;
}

void dump_worldrep_obj(WorldRep *wr, FILE *f) {
    fprintf(f, "mtllib wr.mtl\n");
    for (uint32 c=0, cend=arrlenu32(wr->cell_array); c<cend; ++c) {
        fprintf(f, "o cell.%05u\n", c);
        WorldRepCell *cell = &wr->cell_array[c];
        for (uint32 v=0, vend=arrlenu32(cell->vertex_array); v<vend; ++v) {
            LGVector vert = cell->vertex_array[v];
            fprintf(f, "v %f %f %f\n", vert.x, vert.y, vert.z);
        }

        uint32 istart=0;
        int32 vcount = (int32)arrlen(cell->vertex_array);
        for (uint32 p=0, pend=arrlenu32(cell->poly_array); p<pend; ++p) {
            fprintf(f, "# poly %u\n", p);
            int is_render = (p<cell->header.num_render_polys);
            int is_portal = (p>=((uint32)cell->header.num_polys-(uint32)cell->header.num_portal_polys));
            char *mtl = is_portal? (is_render? "water" : "portal") : "rpoly";
            fprintf(f, "usemtl %s\n", mtl);
            fprintf(f, "f ");
            LGWRPoly *poly = &cell->poly_array[p];
            for (uint32 i=istart, iend=istart+poly->num_vertices; i<iend; ++i) {
                int32 v = (int32)cell->index_array[i];
                fprintf(f, " %d", (v-vcount));
            }
            fprintf(f, "\n");
            istart += poly->num_vertices;
        }
        fprintf(f, "\n");
    }

    LGWRBSPNode *node_array = wr->bsp_node_array;
    LGVector world_up = { 0.0, 0.0, 1.0 };
    LGVector world_forward = { 0.0, 1.0, 0.0 };
    for (uint32 n=0, nend=arrlenu32(node_array); n<nend; ++n) {
        LGWRBSPNode *node = &node_array[n];
        if (BSP_IS_LEAF(node))
            continue;
        fprintf(f, "o node.%05u\n", n);
        fprintf(f, "usemtl plane\n");
        LGWRPlane plane;
        if (node->plane_cell_id==-1) {
            plane = wr->bsp_extraplane_array[node->plane_id];
        } else {
            plane = wr->cell_array[node->plane_cell_id].plane_array[node->plane_id];
        }
        int rev = ((BSP_GET_FLAGS(node)&kIsReversed)!=0);
        if (rev) {
            plane.normal.x = -plane.normal.x;
            plane.normal.y = -plane.normal.y;
            plane.normal.z = -plane.normal.z;
            plane.distance = -plane.distance;
        }
        LGVector o = vmulf(plane.normal, -plane.distance);
        LGVector right, up;
        if (fabsf(vdot(plane.normal, world_up))>0.999f) {
            right = vcross(world_forward, plane.normal);
            up = vcross(right, plane.normal);
        } else {
            right = vcross(plane.normal, world_up);
            up = vcross(right, plane.normal);
        }
        LGVector v0 = vadd(o, vmulf(vadd(right, up), 3.0));
        LGVector v1 = vadd(o, vmulf(vadd(vneg(right), up), 3.0));
        LGVector v2 = vadd(o, vmulf(vadd(vneg(right), vneg(up)), 3.0));
        LGVector v3 = vadd(o, vmulf(vadd(right, vneg(up)), 3.0));
        fprintf(f, "v %f %f %f\n", v0.x, v0.y, v0.z);
        fprintf(f, "v %f %f %f\n", v1.x, v1.y, v1.z);
        fprintf(f, "v %f %f %f\n", v2.x, v2.y, v2.z);
        fprintf(f, "v %f %f %f\n", v3.x, v3.y, v3.z);
        fprintf(f, "f -4 -3 -2 -1\n");
    }
    fprintf(f, "}\n");
}

void bsp_sanity_check(WorldRep *wr) {
    dump("BSP sanity check.\n");
    LGWRBSPNode *node_array = wr->bsp_node_array;
    for (uint32 i=0, iend=arrlenu32(node_array); i<iend; ++i) {
        LGWRBSPNode *node = &node_array[i];

        if (BSP_IS_LEAF(node)) {
            assert(node->plane_cell_id==-1);
            assert(node->plane_id==-1);

            // Test this cell's vertexes against all parent planes.
            // If a vertex is clipped away, we repeat the loop with
            // verbose output.
            int clipped_away = 0;
            int repeat = 0;
            for (;;) {
                float EPSILON = 0.001f; // generous
                WorldRepCell *cell = &wr->cell_array[node->cell_id];
                for (uint32 v=0, vend=arrlenu32(cell->vertex_array); v<vend; ++v) {
                    if (repeat)
                        dump("\t!! Vert %u\n", v);
                    LGVector pos = cell->vertex_array[v];
                    LGWRPlane plane = {0};
                    uint32 n = BSP_GET_PARENT(node);
                    uint32 prevn = i;
                    while (n!=BSP_INVALID) {
                        LGWRBSPNode *parent = &node_array[n];
                        if (parent->plane_cell_id==-1) {
                            plane = wr->bsp_extraplane_array[parent->plane_id];
                        } else {
                            plane = wr->cell_array[parent->plane_cell_id].plane_array[parent->plane_id];
                        }
                        // Looks like LGWRPlane is constructed as:
                        //
                        //    normal.X + distance == 0
                        //
                        // So to test if a vertex is inside it, we must compare:
                        //
                        //    normal.vert <=> -distance
                        //            ==: on the plane
                        //            > : inside the plane (+ve halfspace)
                        //            < : outside the plane (-ve halfspace)
                        float dot = pos.x*plane.normal.x
                                  + pos.y*plane.normal.y
                                  + pos.z*plane.normal.z;
                        int test_inside = (parent->inside_index==prevn);
                        if (BSP_GET_FLAGS(parent)&kIsReversed)
                            test_inside = !test_inside;
                        int pass;
                        if (test_inside)
                            pass = (dot+plane.distance >= -EPSILON);
                        else
                            pass = (dot+plane.distance <= EPSILON);
                        if (repeat) {
                            dump("\tXX dot %f; -d %f; dot+d %f; pass: %d\n",
                                dot, -plane.distance, dot+plane.distance, pass);
                        }
                        char *rev = (BSP_GET_FLAGS(parent)&kIsReversed)? "R" : "";
                        if (!pass) {
                            dump("cell %u vertex %u"
                                    " (%0.3f,%0.3f,%0.3f) clipped away"
                                    " by node %u plane (%0.3f,%0.3f,%0.3f) d %0.3f %s;"
                                    " dot %0.3f!\n",
                                node->cell_id, v,
                                pos.x, pos.y, pos.z,
                                n, plane.normal.x, plane.normal.y, plane.normal.z, plane.distance, rev,
                                dot);
                            clipped_away = 1;
                            break;
                        }
                        prevn = n;
                        n = BSP_GET_PARENT(parent);
                    }
                }

                if (repeat) {
                    break;
                } else if (clipped_away) {
                    repeat = 1;
                    continue;
                } else {
                    break;
                }
            }
        }
    }
}

int do_dump_bsp(int argc, char **argv, struct command *cmd) {
    if (argc!=3
    || strcmp(argv[1], "-o")!=0) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *in_filename = argv[0];
    char *out_filename = argv[2];
    DBFile *dbfile = dbfile_load(in_filename);
    DBTagBlock *wr_tagblock = dbfile_get_wr_tagblock(dbfile);
    WorldRep *wr = wr_load_from_tagblock(wr_tagblock);

    FILE *f = fopen(out_filename, "wb");
    assert(f);
    dump_bsp_graphviz(wr, f);
    fclose(f);

    wr_free(&wr);
    dbfile = dbfile_free(dbfile);
    return 0;
}

int do_dump_obj(int argc, char **argv, struct command *cmd) {
    if (argc!=3
    || strcmp(argv[1], "-o")!=0) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *in_filename = argv[0];
    char *out_filename = argv[2];
    DBFile *dbfile = dbfile_load(in_filename);
    DBTagBlock *wr_tagblock = dbfile_get_wr_tagblock(dbfile);
    WorldRep *wr = wr_load_from_tagblock(wr_tagblock);

    FILE *f = fopen(out_filename, "wb");
    assert(f);
    dump_worldrep_obj(wr, f);
    fclose(f);

    wr_free(&wr);
    dbfile = dbfile_free(dbfile);
    return 0;
}

int do_bsp_sanity_check(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }

    char *filename = argv[0];
    DBFile *dbfile = dbfile_load(filename);
    DBTagBlock *wr_tagblock = dbfile_get_wr_tagblock(dbfile);
    WorldRep *wr = wr_load_from_tagblock(wr_tagblock);

    bsp_sanity_check(wr);

    wr_free(&wr);
    dbfile = dbfile_free(dbfile);
    return 0;
}

int do_test_worldrep(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *filename = argv[0];
    dump("File: \"%s\"\n", filename);
    DBFile *dbfile = dbfile_load(filename);
    DBTagBlock *wr_tagblock = dbfile_get_wr_tagblock(dbfile);

    dump("%s read ok, 0x%08x bytes of data.\n\n", wr_tagblock->key.s, wr_tagblock->size);
    WorldRep *wr = wr_load_from_tagblock(wr_tagblock);
    dump("\n");

    DBTagBlock wr_tagblock2 = {0};
    wr_save_to_tagblock(&wr_tagblock2, wr, 1);
    assert(wr_tagblock->size==wr_tagblock2.size);
    // BUG: this memcmp() will fail because we don't preserve the light_this
    //      records (because it would be worthless to do so!). But then how do
    //      I do this test??
    assert(memcmp(wr_tagblock->data, wr_tagblock2.data, wr_tagblock2.size)==0);

    wr_free(&wr);
    dbfile = dbfile_free(dbfile);
    dump("Ok.\n");
    return 0;
}

int do_test_write_minimal(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *filename = argv[0];
    dump("File: \"%s\"\n", filename);
    DBFile *dbfile = dbfile_load(filename);

    // NOTE: This is to test the minimum tagblocks we need for
    //       Dromed to be okay with the .mis. Turns out only
    //       FILE_TYPE is needed! But GAM_FILE is useful to keep.
    DBTagBlock file_type;
    DBTagBlock gam_file;
    dbtagblock_copy(&file_type, dbfile_get_tag(dbfile, "FILE_TYPE"));
    dbtagblock_copy(&gam_file, dbfile_get_tag(dbfile, "GAM_FILE"));
    DBFile *minimal_dbfile = calloc(1, sizeof(DBFile));
    minimal_dbfile->version = (LGDBVersion){ 0, 1 };
    hmputs(minimal_dbfile->tagblock_hash, file_type);
    hmputs(minimal_dbfile->tagblock_hash, gam_file);
    dbfile_save(minimal_dbfile, "e:/dev/thief/T2FM/test_misdeed/minimal.mis");

    dbfile = dbfile_free(dbfile);
    dump("Ok.\n");
    return 0;
}

int do_dump_wrlight(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *in_filename = argv[0];
    DBFile *dbfile = dbfile_load(in_filename);
    DBTagBlock *tagblock = dbfile_get_wr_tagblock(dbfile);
    WorldRep *wr = wr_load_from_tagblock(tagblock);
    int is_wr = (wr->format==WorldRepFormatWR);

    uint32 count = wr->num_static_lights+wr->num_dynamic_lights;
    for (uint32 i=0, iend=count; i<iend; ++i) {
        if (i==0)
            dump("Static lights:\n");
        if (i==wr->num_static_lights)
            dump("Dynamic lights:\n");
        dump("%u:\n", i);
        LGWRRGBLight *light = &wr->light_array[i];
        dump("\tlocation: %f %f %f\n", light->location.x, light->location.y, light->location.z);
        dump("\tdirection: %f %f %f\n", light->direction.x, light->direction.y, light->direction.z);
        if (is_wr) {
            dump("\tbright: %f\n", light->bright.x);
        } else {
            dump("\tbright: %f %f %f\n", light->bright.x, light->bright.y, light->bright.z);
        }
        dump("\tinner: %f\n", light->inner);
        dump("\touter: %f\n", light->outer);
        dump("\tradius: %f\n", light->radius);
        }
    }
    dump("\n");

    dump("AnimLight to Cell:\n");
    for (uint32 i=0, iend=arrlenu32(wr->animlight_to_cell_array); i<iend; ++i) {
        LGWRAnimLightToCell *item = &wr->animlight_to_cell_array[i];
        dump("%u:\tcell %u, palette index %u\n",
            i,
            (uint32)item->cell_index,
            (uint32)item->pos_in_cell_palette);
    }

    dbfile = dbfile_free(dbfile);
    return 0;
}

int do_dump_animlight(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *in_filename = argv[0];
    DBFile *dbfile = dbfile_load(in_filename);
    DBTagBlock *tagblock = dbfile_get_tag(dbfile, TAG_PROP_ANIMLIGHT);

    uint32 count = tagblock->size/sizeof(LGAnimLightProp);
    char *pread = (char *)tagblock->data;
    LGAnimLightProp *props = NULL;
    MEM_READ_ARRAY(props, count, pread);
    dump("%u animlights:\n", count);
    for (uint32 i=0; i<count; ++i) {
        LGAnimLightProp *prop = &props[i];
        dump("%4u:\n", i);
        dump("    id: %d\n", prop->header.id);
        dump("    size: %u\n", prop->header.size);
        dump("    sizeof(LGAnimLightProp): %u\n", (uint32)sizeof(LGAnimLight));
        assert(prop->header.size==sizeof(LGAnimLight));
        LGAnimLight *animlight = &(props[i].prop);
        LGBaseLight *base = &(animlight->base);
        LGAnimLightAnimation *animation = &(animlight->animation);
        dump("    brightness: %f\n", base->brightness);
        dump("    offset: %f %f %f\n", base->offset.x, base->offset.y, base->offset.z);
        dump("    refresh: %d\n", animation->refresh);
        dump("    lighttocell_start: %u\n", (uint32)animation->lighttocell_start);
        dump("    lighttocell_count: %u\n", (uint32)animation->lighttocell_count);
        dump("    light_array_index: %d\n", (int32)animation->light_array_index);
        dump("    mode: %d\n", (int32)animation->mode);
        dump("    time_rising_ms: %d\n", (int32)animation->time_rising_ms);
        dump("    time_falling_ms: %d\n", (int32)animation->time_falling_ms);
        dump("    min_brightness: %f\n", (int32)animation->min_brightness);
        dump("    max_brightness: %f\n", (int32)animation->max_brightness);
        // remaining fields are runtime state that i dont care about.
    }
    arrfree(props);

    // Ensure we have read all the available data:
    assert(pread==(tagblock->data+tagblock->size));

    dbfile = dbfile_free(dbfile);
    return 0;
}


struct command all_commands[] = {
    { "help", do_help,                              "[command]",            "List available commands; show help for a command." },
    { "tag_list", do_tag_list,                      "file.mis",             "List all tagblocks." },
    { "tag_dump", do_tag_dump,                      "file.mis tag",         "Dump tagblock to file." },
    { "fam_list", do_fam_list,                      "file.mis",             "List loaded families." },
    { "tex_list", do_tex_list,                      "file.mis",             "List all textures." },
    { "test_worldrep", do_test_worldrep,            "file.mis",             "Test reading and writing (to memory) the worldrep." },
    { "test_write_minimal", do_test_write_minimal,  "input.mis",            "Test writing a minimal dbfile." },
    { "merge", do_merge,                            "top.mis bottom.mis -o out.mis",  "Merge two worldreps." },
    { "dump_bsp", do_dump_bsp,                      "file.mis -o out.dot",  "dump the BSP tree to graphviz .DOT." },
    { "dump_obj", do_dump_obj,                      "file.mis -o out.obj",  "dump the WR and BSP to wavefront .OBJ." },
    { "dump_animlight", do_dump_animlight,          "file.mis",             "dump animlight table to stdout." },
    { "dump_wrlight", do_dump_wrlight,              "file.mis",             "dump WR light data table." },
    { "bsp_sanity_check", do_bsp_sanity_check,      "file.mis",             "do a BSP sanity check." },
    { NULL, NULL },
};

int do_help(int argc, char **argv, struct command *cmd) {
    if (argc>1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    if (argc==0) {
        fprintf(stderr, "Commands:\n");
        for (int i=0;; ++i) {
            struct command c = all_commands[i];
            if (! c.s || ! c.func) break;
            fprintf(stderr, "\t%s %s:\t\t%s\n", c.s, c.args, c.help);
        }
        return 0;
    } else {
        for (int i=0;; ++i) {
            struct command c = all_commands[i];
            if (! c.s || ! c.func) break;
            if (strcmp(c.s, argv[0])==0) {
                fprintf(stderr, "%s\n", c.help);
                return 0;
            }
        }
        abort_format("Unknown command: %s", argv[0]);
    }
}

int main(int argc, char **argv) {
    if (argc<2) {
        abort_format("Usage: %s command ...\nUse 'help' to see all commands.\n", argv[0]);
    }
    for (int i=0;; ++i) {
        struct command c = all_commands[i];
        if (! c.s || ! c.func) break;
        if (strcmp(c.s, argv[1])==0) {
            return (c.func)(argc-2, &argv[2], &c);
        }
    }
    abort_format("Unknown command: %s", argv[1]);
}
