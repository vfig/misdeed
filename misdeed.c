#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define this to build a tool with only the "fixup_cell_lights" command.
//#define TOOL_FIXUP_CELL_LIGHTS

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
    fprintf(stderr, "FATAL: %s\n", message); \
    fflush(stderr); \
    abort(); \
    } while(0)

#define abort_format(fmt, ...) \
    do { \
    fprintf(stderr, "FATAL: " fmt "\n", __VA_ARGS__); \
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

#define UNUSED(x) (void)(x)

#define MEM_ZERO_SIZE(buf, size) memset(buf, 0, size)
#define MEM_ZERO(var) MEM_ZERO_SIZE(&var, sizeof(var))

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
#define TAG_AIPATH  "AIPATH"
#define TAG_WR      "WR"
#define TAG_WRRGB   "WRRGB"
#define TAG_WREXT   "WREXT"
#define TAG_BRLIST  "BRLIST"
#define TAG_FAMILY  "FAMILY"
#define TAG_TXLIST  "TXLIST"
#define TAG_WEATHER "WEATHER"
#define TAG_PROP_POSITION "P$Position"
#define TAG_PROP_LIGHT "P$Light"
#define TAG_PROP_ANIMLIGHT "P$AnimLight"

typedef struct LGVector {
    float32 x, y, z;
} LGVector;

typedef struct LGAngleVector {
    uint16 x, y, z;
} LGAngleVector;

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
    uint16 cached_surface;  // Ignore this value; the engine zeroes it on WR load.
    float32 texture_mag;
    LGVector center;
} LGWRRenderPoly;

typedef struct LGWREXTRenderPoly {
    LGVector tex_u;
    LGVector tex_v;
    float32 u_base;         // Changed in WREXT
    float32 v_base;         // Changed in WREXT
    uint16 texture_id;      // Changed in WREXT (texture_anchor removed)
    uint16 cached_surface;  // TODO: jk has this as texture_anchor! which is it?
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

typedef struct LGWRCSGSurfaceRef {
    int32 cell;
    uint8 surface;      // Index of cell's render poly.
    uint8 brush_face;   // Index of brush face (brush id is known from your
                        //     position in csg_brush_surfaceref_count_array).
    int16 vertex;
} LGWRCSGSurfaceRef;

// AIPATH

typedef uint32 LGAIPATHCellID;
typedef uint32 LGAIPATHVertexID;
typedef uint32 LGAIPATHCell2CellLinkID;
typedef uint32 LGAIPATHCell2VertexLinkID;

typedef uint16 LGAIPATHCellIDPacked;
typedef uint16 LGAIPATHVertexIDPacked;
typedef uint16 LGAIPATHCell2CellLinkIDPacked;
typedef uint16 LGAIPATHCell2VertexLinkIDPacked;

typedef uint16 LGAIPATHZone;
typedef uint8 LGAIPATHOkBits;

typedef struct LGAIPATHCellv3_3 {
    LGAIPATHCell2VertexLinkIDPacked firstVertex; // in pathCell2VertexLink array
    LGAIPATHCell2CellLinkIDPacked firstCell;   // in pathCell2CellLink array
    LGAIPATHCellIDPacked plane;
    LGAIPATHCellIDPacked next;            // in the A* search open list
    LGAIPATHCellIDPacked bestNeighbor;    // bestNeighbor yet found during the search, index in g_AIPathDB.m_Cells
    LGAIPATHCell2CellLinkIDPacked linkFromNeighbor;// link used to connect neighbor to me
    uint8 vertexCount; // in pathCell2VertexLink array
    uint8 pathFlags;
    uint8 cellCount; // in pathCell2CellLink array
    uint8 wrapFlags; // set if any ID's have wrapped. Only used in processing, not in game!
    LGVector center;
         // extra info needed for each cell, to help decide what motions to
         // play & whatever; all handled in terms of bits
    uint32 flags;
#if 0    
         // constants matching bit field sizes
         #define kAIPathCellNumLightLevels 16
         #define kAIPathCellNumStairSizes 4

         // Each group of fields represents one byte.  4 bytes total.
         uint m_LightLevel : 4;
         uint m_IsRamp : 1;    // Is it slanted?
         uint m_IsStair : 1;   // Are nearby surfaces a little lower?
/*28*/   uint m_StairSize : 2;

         uint m_Volume : 3;
         uint m_CeilingHeight : 3;
         uint m_Doorway : 1;
/*29*/   uint m_Corner : 1;

         uint m_CliffEdge : 1;
         uint m_Water : 1;
/*30*/   uint pad30_2 : 6;

/*31*/   uint pad31 : 8;
#endif
} LGAIPATHCellv3_3;

typedef struct LGAIPATHCellv3_4 {
    LGAIPATHCell2VertexLinkID firstVertex; // in pathCell2VertexLink array
    LGAIPATHCell2CellLinkID firstCell;   // in pathCell2CellLink array
    LGAIPATHCellID plane;
    LGAIPATHCellID next;            // in the A* search open list
    LGAIPATHCellID bestNeighbor;    // bestNeighbor yet found during the search, index in g_AIPathDB.m_Cells
    LGAIPATHCell2CellLinkID linkFromNeighbor;// link used to connect neighbor to me
    uint8 vertexCount; // in pathCell2VertexLink array
    uint8 pathFlags;
    uint8 cellCount; // in pathCell2CellLink array
    uint8 wrapFlags; // set if any ID's have wrapped. Only used in processing, not in game!
    LGVector center;
         // extra info needed for each cell, to help decide what motions to
         // play & whatever; all handled in terms of bits
    uint32 flags;
#if 0    
         // constants matching bit field sizes
         #define kAIPathCellNumLightLevels 16
         #define kAIPathCellNumStairSizes 4

         // Each group of fields represents one byte.  4 bytes total.
         uint m_LightLevel : 4;
         uint m_IsRamp : 1;    // Is it slanted?
         uint m_IsStair : 1;   // Are nearby surfaces a little lower?
/*28*/   uint m_StairSize : 2;

         uint m_Volume : 3;
         uint m_CeilingHeight : 3;
         uint m_Doorway : 1;
/*29*/   uint m_Corner : 1;

         uint m_CliffEdge : 1;
         uint m_Water : 1;
/*30*/   uint pad30_2 : 6;

/*31*/   uint pad31 : 8;
#endif
} LGAIPATHCellv3_4;

typedef struct LGAIPATHCellPlane {
    LGVector normal;
    float32 constant;
} LGAIPATHCellPlane;

typedef struct LGAIPATHVertex {
    LGVector pt;
    int32 ptInfo;
} LGAIPATHVertex;

typedef struct LGAIPATHCellLink {
    LGAIPATHCellIDPacked dest;
    LGAIPATHVertexIDPacked vertex_1;
    LGAIPATHVertexIDPacked vertex_2;
    LGAIPATHOkBits okBits;
    uint8 cost;
} LGAIPATHCellLink;

typedef struct LGAIPATHCell2VertexLink {
    LGAIPATHVertexID id;
} LGAIPATHCell2VertexLink;

// BRLIST

#define LGBRLIST_FACE_COUNT_MAX_OLDDARK 12
#define LGBRLIST_FACE_COUNT_MAX_NEWDARK 28

typedef enum LGBrushType {
    BRTYPE_TERRAIN = 0,
    BRTYPE_LIGHT = 1,
    BRTYPE_AREA = 2,
    BRTYPE_OBJECT = 3,
    BRTYPE_FLOW = 4,
    BRTYPE_ROOM = 5,

    BRTYPE_INVALID = -1,
} LGBrushType;

typedef enum LGBrushShape {
    BRSHAPE_CUBE = 0,
    BRSHAPE_CYLINDER = 1,
    BRSHAPE_PYRAMID = 2,
    BRSHAPE_APEX_PYRAMID = 3,
    BRSHAPE_DODECAHEDRON = 4,
    BRSHAPE_WEDGE = 5,

    BRSHAPE_INVALID = -1,
} LGBrushShape;

typedef enum LGBrushAlign {
    BRALIGN_BY_VERTEX = 0,
    BRALIGN_BY_FACE = 1,
} LGBrushAlign;

typedef enum LGBrushMedium {
    BRMEDIUM_SOLID = 0,
    BRMEDIUM_AIR = 1,
    BRMEDIUM_WATER = 2,
    BRMEDIUM_FLOOD = 3,
    BRMEDIUM_EVAPORATE = 4,
    BRMEDIUM_SOLID2WATER = 5,
    BRMEDIUM_SOLID2AIR = 6,
    BRMEDIUM_AIR2SOLID = 7,
    BRMEDIUM_WATER2SOLID = 8,
    BRMEDIUM_BLOCKABLE = 9,

    BRMEDIUM_INVALID = -1,
} LGBrushMedium;

#pragma warning(push)
#pragma warning(disable: 4702) // unreachable code

static inline LGBrushType _lgbrush_get_type(int32 media) {
    if (media>=0)
        return BRTYPE_TERRAIN;
    else if (media>=-BRTYPE_ROOM && media<=-BRTYPE_LIGHT)
        return -media;
    abort_format("Unknown brush type %d", media);
    return BRTYPE_INVALID;
}

static inline LGBrushShape _lgbrush_get_shape(int32 primal) {
    int type = (primal>>9);
    int sides = (primal&0xff);
    switch (type) {
    case 0:
        switch (sides) {
        case 0: return BRSHAPE_CUBE; // cf. brush id 907 in T1 miss12.mis
        case 1: return BRSHAPE_CUBE;
        case 6: return BRSHAPE_DODECAHEDRON;
        case 7: return BRSHAPE_WEDGE;
        // 8: Line (?)
        // 9: Light (not terrain!)
        }
        break;
    case 1: return BRSHAPE_CYLINDER;
    case 2: return BRSHAPE_PYRAMID;
    case 3: return BRSHAPE_APEX_PYRAMID;
    }
    abort_format("Unknown terrain primal %d (0x%08x)", primal, primal);
    return BRSHAPE_INVALID;
}

static inline LGBrushMedium _lgbrush_get_medium(int32 media) {
    if (media>=BRMEDIUM_SOLID && media<=BRMEDIUM_BLOCKABLE)
        return media;
    else
        return BRMEDIUM_INVALID;
}

#pragma warning(pop)

static inline LGBrushAlign _lgbrush_get_align(int32 primal) {
    return ((primal>>8)&1);
}

static inline int _lgbrush_get_sides(int32 primal) {
    int type = (primal>>9);
    int sides = (primal&0xff);
    switch (type) {
    case 0:
        // cubes, dodecahedrons don't have variable side counts.
        return 0;
    case 1:
    case 2:
    case 3:
        return sides+3;
    }
    return 0;
}

static inline int _lgbrush_get_face_count(int32 media, uint8 num_faces) {
    if (media>=0)
        return num_faces;
    else
        return 0;
}

#define LGBRUSH_GET_TYPE(b) _lgbrush_get_type((b).media)
#define LGBRUSH_GET_TERR_SHAPE(b) _lgbrush_get_shape((b).terr_primal)
#define LGBRUSH_GET_TERR_SIDES(b) _lgbrush_get_sides((b).terr_primal)
#define LGBRUSH_GET_ALIGN(b) _lgbrush_get_align((b).terr_primal)
#define LGBRUSH_GET_FACE_COUNT(b) _lgbrush_get_face_count((b).media, (b).terr_num_faces)
#define LGBRUSH_GET_MEDIUM(b) _lgbrush_get_medium((b).media)

typedef struct LGBRLISTFace {
    int16 tx_id;
    uint16 rot;
    int16 scale;
    uint16 x;
    uint16 y;
} LGBRLISTFace;

typedef struct LGBRLISTGrid {
    float32 line_spacing;
    LGVector phase_shift;   // Unused by Dromed
    LGAngleVector orientation;   // Unused by Dromed
    bool8 grid_enabled;
} LGBRLISTGrid;

typedef struct LGBRLISTBrush {
    int16 br_id;
    int16 timestamp;
    union {
        int32 terr_primal;
        int32 light_number; // TODO: ??? is this another index into wr we need to fixup? D:
        int32 area_unknown0; // TODO: is this anything?
        int32 obj_id;
        int32 flow_unknown0; // TODO: is this anything?
        int32 room_archetype;
    };
    union {
        int16 terr_tx_id;
        int16 light_unknown1; // TODO: is this anything?
        int16 area_active;
        int16 obj_unknown1; // TODO: is this anything?
        int16 flow_gid; // TODO: what is a gid? (this is from pytaffers)
        int16 room_number; // TODO: what does this mean?
    };
    int8 media;
    int8 flags;
    LGVector pos;
    LGVector sz;
    LGAngleVector ang;
    int16 cur_face;                 // -- 44 bytes
    LGBRLISTGrid grid;
    union {
        uint8 terr_num_faces;
        uint8 light_type;
        uint8 area_unknown2; // TODO: is this anything?
        uint8 obj_unknown2; // TODO: is this anything?
        uint8 flow_unknown2; // TODO: is this anything?
        uint8 room_unknown2; // TODO: is this anything?
    };
    int8 edge;
    int8 point;
    int8 use_flg;
    int8 group_id;
    int32 pad0;
    // NOTE: On disk only num_faces of these are stored. But we keep the full
    //       array here so that we can make a flat array of LGBRLISTBrush.
    LGBRLISTFace faces[LGBRLIST_FACE_COUNT_MAX_NEWDARK];
} LGBRLISTBrush;

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

typedef struct LGLocation {
    LGVector vec;
    int16 cell; // always -1 on disk
    int16 hint; // always -1 on disk
} LGLocation;

typedef struct LGPosition {
   LGLocation loc;
   LGAngleVector fac;
} LGPosition;

typedef struct LGPositionProp {
    LGPropHeader header;
    LGPosition prop;
} LGPositionProp;

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

typedef struct LGAnimLight76 {
    LGBaseLight base;
    LGAnimLightAnimation animation;
    float32 radius;
    int32 notify_script_objid;
    bool32 quad;
    float32 inner_radius;
} LGAnimLight76;

typedef struct LGAnimLight80 {
    LGAnimLight76 x;
    bool32 is_dynamic;      // New in NewDark
} LGAnimLight80;

typedef struct LGLight {
    LGBaseLight base;
    float32 radius;
    bool32 quad;
    float32 inner_radius;
} LGLight;

typedef struct LGAnimLightProp76 {
    LGPropHeader header;
    LGAnimLight76 prop;
} LGAnimLightProp76;

typedef struct LGAnimLightProp80 {
    LGPropHeader header;
    LGAnimLight80 prop;
} LGAnimLightProp80;

typedef struct LGLightProp {
    LGPropHeader header;
    LGLight prop;
} LGLightProp;

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

// Property tagblocks encode the sPropertyDesc version in the high 16 bits, and
// the IDataOps version in the low 16 bits; I believe the latter is always
// the size of the record.
#define DBTAGBLOCK_PROP_VERSION_MAJOR(tagblock) ((tagblock)->version.major)
#define DBTAGBLOCK_PROP_VERSION_MINOR(tagblock) ((tagblock)->version.minor>>16)
#define DBTAGBLOCK_PROP_ITEM_SIZE(tagblock) ((tagblock)->version.minor&0xFFFFUL)

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
    if (! file) abort_format("Cannot read file: %s\n", filename);
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
    if (! file) abort_format("Cannot write file: %s\n", filename);
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

/** Vector math conveniences */

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

float vdist_sq(LGVector a, LGVector b) {
    LGVector d = vadd(a, vneg(b));
    return vdot(d, d);
}

/** Forward declarations */

LGAnimLightProp80 *animlight_prop_load_from_tagblock(DBTagBlock *tagblock);
LGLightProp *light_prop_load_from_tagblock(DBTagBlock *tagblock);

/** Weather stuff */

typedef uint8 LGCellWeather;

#define LGCELLWEATHER_DEBRIS 1
#define LGCELLWEATHER_PRECIPITATION = 2
#define LGCELLWEATHER_WIND = 4

#define LGWRCellWeatherDebris(v) ((v)&LGCELLWEATHER_DEBRIS)
#define LGWRCellWeatherPrecipitation(v) ((v)&LGCELLWEATHER_PRECIPITATION)
#define LGWRCellWeatherWind(v) ((v)&LGCELLWEATHER_WIND)


LGCellWeather *weather_load_from_tagblock(DBTagBlock *tagblock) {
    assert(tag_name_eq_str(tagblock->key, TAG_WEATHER));
    assert(tagblock->version.major==1 && tagblock->version.minor==3);

    LGCellWeather *array = NULL;
    DBTAGBLOCK_READ_ARRAY(array, tagblock);
    return array;
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
    int32 *csg_brfaces_array;                       // one brface per renderpoly, per cell
    int32 *csg_brush_plane_count_array;             // number of planes, per brush
    LGWRCSGPlane *csg_brush_planes_array;           // all planes
    int32 *csg_brush_surfaceref_count_array;        // number of surfacerefs, per brush
    LGWRCSGSurfaceRef *csg_brush_surfacerefs_array; // all surfacerefs
} WorldRep;

#define CSG_BRFACE_GET_BRUSH_INDEX(brface) ((brface)>>8)
#define CSG_BRFACE_GET_FACE_INDEX(brface) ((brface)&0xff)
#define CSG_BRFACE(br,face) (((br)<<8)|((face)&0xff))

LGBRLISTBrush *brlist_load_from_tagblock(DBTagBlock *tagblock);
LGBRLISTBrush *brlist_sorted_by_id(LGBRLISTBrush *brlist);
int16 brlist_get_id_max(LGBRLISTBrush *brushes);

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
    MEM_ZERO_SIZE(cell, sizeof(*cell));
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
    // TODO: WAAAAAAAAAAAAAAAAAAAAAAAAAAAAHGH
    //       we have some weird shenanigans going on.. it seems like
    //       light_index_array[0] -- yes, even on disk -- is the number
    //       of indices??? assert that that is true.
    //       corollary: we should never fixup light_index_array[0]!
    assert(num_light_indices>0);
    assert((uint32)(cell->light_index_array[0])==num_light_indices-1);
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
    MEM_ZERO_SIZE(out_cell, sizeof(*out_cell));
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

    if (is_wrext) {
        LGWREXTHeader ext_header;
        MEM_READ(ext_header, pread);
        assert(ext_header.size==LGWREXTHeaderSize);
        assert(ext_header.wr_version<=LGWREXTHeaderWRVersionMax);
        assert(! (ext_header.flags&LGWREXTFlagLegacy)); // Don't understand what the flag means yet, so don't allow it.
        wr->lightmap_format = _wr_decode_lightmap_format(wr->format, &ext_header);
        wr->flags = ext_header.flags;
    }

    LGWRHeader header;
    MEM_READ(header, pread);
    if (! is_wrext) {
        wr->lightmap_format = _wr_decode_lightmap_format(wr->format, NULL);
    }

    int lightmap_bpp = wr->lightmap_format.lightmap_bpp;
    arrsetlen(wr->cell_array, header.cell_count);
    for (uint32 i=0, iend=arrlenu32(wr->cell_array); i<iend; ++i) {
        pread = wr_read_cell(&wr->cell_array[i], is_wrext, lightmap_bpp, pread);
    }

    uint32 bsp_extraplane_count;
    MEM_READ(bsp_extraplane_count, pread);
    MEM_READ_ARRAY(wr->bsp_extraplane_array, bsp_extraplane_count, pread);

    uint32 bsp_node_count;
    MEM_READ(bsp_node_count, pread);
    MEM_READ_ARRAY(wr->bsp_node_array, bsp_node_count, pread);

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
        assert(csg_brfaces_count!=0);
        MEM_READ_ARRAY(wr->csg_brfaces_array, csg_brfaces_count, pread);
        uint32 csg_brush_count;
        MEM_READ(csg_brush_count, pread);
        MEM_READ_ARRAY(wr->csg_brush_plane_count_array, csg_brush_count, pread);
        uint32 csg_brush_plane_total_count = 0;
        for (uint32 i=0, iend=arrlenu32(wr->csg_brush_plane_count_array); i<iend; ++i) {
            csg_brush_plane_total_count += wr->csg_brush_plane_count_array[i];
        }
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

void wr_save_to_tagblock(DBTagBlock *tagblock, WorldRep *wr) {
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

    // WR,WRRGB,WREXT always store 32 additional light records for some reason!
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
    // Sanity check: verify BRLIST is identical in both files.
    // This is necessary because the WR csg_brush_plane* arrays depend on the
    // brush list.
    {
        DBTagBlock *tagblock1 = dbfile_get_tag(dbfile1, TAG_BRLIST);
        DBTagBlock *tagblock2 = dbfile_get_tag(dbfile2, TAG_BRLIST);
        assert(tagblock1->size==tagblock2->size);
        int result = memcmp(tagblock1->data, tagblock1->data, tagblock1->size);
        assert_message(result==0, "BRLIST is different.");
    }
    LGBRLISTBrush *brlist1 = brlist_load_from_tagblock(
        dbfile_get_tag(dbfile1, TAG_BRLIST));
    LGBRLISTBrush *brushes1 = brlist_sorted_by_id(brlist1);
    LGBRLISTBrush *brlist2 = brlist_load_from_tagblock(
        dbfile_get_tag(dbfile2, TAG_BRLIST));
    LGBRLISTBrush *brushes2 = brlist_sorted_by_id(brlist2);
    //
    // NOTE: When portalizing, Dromed inserts a new "blockable" brush for every
    //       visibility-blocking door, so that it can guarantee portals that
    //       can be closed when the door is closed. These brushes are appended
    //       to the brush list, then deleted after portalization completes.
    //       However, the "next available brush id" is not reset.
    //
    //       (You can see this effect if you open a .mis with doors, select the
    //       last brush, i.e. the highest id -- brush ids are shown below the
    //       "Time" field -- then create a new brush. Its id will be highest+1
    //       as expected. However if you portalize before creating the new
    //       brush, the new brush's id will be highest+X+1, where X is the
    //       number of door-blocking brushes that were made. This difference
    //       even compounds with subsequent portalizations.)
    //
    //       This doesn't affect any ordinary editor functions, nor the BRLIST
    //       at all; but it does affect csg_brush_plane_count_array and
    //       csg_brush_surfaceref_count_array, each of which are sized to be
    //       indexable by brush id. The result is that these arrays can
    //       contain many "phantom" entries after the highest brush id that is
    //       actually in use; and there is no sensible way to merge these
    //       phantom entries. So here we truncate these arrays (and the two
    //       related arrays) based on the actual highest brush id in use (which
    //       we require to be the same for both dbfiles), so that these csg
    //       arrays can be merged.
    //
    //       Note that non-terrain brushes, or unused brush ids due to deleted
    //       brushes might also contribute phantom entries in the lower parts
    //       of the array, but these will not matter.
    //
    int16 br_id_max = brlist_get_id_max(brushes1);
    assert(brlist_get_id_max(brushes2)==br_id_max);

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
    int is_wrext = (wr1->format==WorldRepFormatWREXT);

    // wrm cells := [wr1 cells] [wr2 cells]
    int16 wr1_cell_count = (int16)arrlen(wr1->cell_array);
    int16 wr2_cell_count = (int16)arrlen(wr2->cell_array);
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
    UNUSED(wr2_bsp_extraplane_end);
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
    BSP_SET_PARENT(&split_node, BSP_INVALID);
    BSP_SET_FLAGS(&split_node, 0);
    split_node.plane_cell_id = -1;
    split_node.plane_id = 0;
    split_node.inside_index = wr1_bsp_node_start;
    split_node.outside_index = wr2_bsp_node_start;
    wrm->bsp_node_array[0] = split_node;
    for (uint32 i=0, j=wr1_bsp_node_start; i<wr1_bsp_node_count; ++i, ++j)
        wrm->bsp_node_array[j] = wr1->bsp_node_array[i];
    for (uint32 i=0, j=wr2_bsp_node_start; i<wr2_bsp_node_count; ++i, ++j)
        wrm->bsp_node_array[j] = wr2->bsp_node_array[i];

    // wrm cell-weatherzones := [wr1 cell-weatherzones] [wr2 cell-weatherzones]
    if (is_wrext) {
        int16 wr1_cell_weatherzones_count = (int16)arrlen(wr1->cell_weatherzones_array);
        int16 wr2_cell_weatherzones_count = (int16)arrlen(wr2->cell_weatherzones_array);
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
        int16 wr1_cell_renderoptions_count = (int16)arrlen(wr1->cell_renderoptions_array);
        int16 wr2_cell_renderoptions_count = (int16)arrlen(wr2->cell_renderoptions_array);
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
    //       for sunlight; we treat it as a special case.
    // NOTE: "static" lights are Renderer>Light and Renderer>AnimLight.
    //       "dynamic" lights are Renderer>Dynamic Light.
    // TODO: how does NewDark's "dynamic light" checkbox on AnimLights complicate this?
    // TODO: are dynamic lights put in the table even if outside the world?
    //       would make sense, right? so concatenatic the dynamic lights might be wrong!
    int16 wr1_static_light_count = (int16)wr1->num_static_lights-1;
    int16 wr2_static_light_count = (int16)wr2->num_static_lights-1;
    int16 wr1_dynamic_light_count = (int16)wr1->num_dynamic_lights;
    int16 wr2_dynamic_light_count = (int16)wr2->num_dynamic_lights;
    wrm->num_static_lights = 1+wr1_static_light_count+wr2_static_light_count;
    wrm->num_dynamic_lights = wr1_dynamic_light_count+wr2_dynamic_light_count;
    assert(wrm->num_static_lights<=768); // TODO: did NewDark raise this limit?
    assert(wrm->num_dynamic_lights<=32);
    int16 wrm_total_light_count = (int16)(wrm->num_static_lights+wrm->num_dynamic_lights);
    int16 wr1_static_light_start = 1,
          wr1_static_light_end = wr1_static_light_start+wr1_static_light_count;
    int16 wr2_static_light_start = wr1_static_light_end,
          wr2_static_light_end = wr2_static_light_start+wr2_static_light_count;
    int16 wr1_dynamic_light_start = wr2_static_light_end,
          wr1_dynamic_light_end = wr1_dynamic_light_start+wr1_dynamic_light_count;
    int16 wr2_dynamic_light_start = wr1_dynamic_light_end,
          wr2_dynamic_light_end = wr2_dynamic_light_start+wr2_dynamic_light_count;
    UNUSED(wr2_dynamic_light_end);
    assert(wrm_total_light_count==( (int16)arrlen(wr1->light_array)
                                  + (int16)arrlen(wr2->light_array)
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
    // a: invalid or sunlight index is unchanged.
    // b: wr1 static light indexes are unchanged.
    // c: wr1 dynamic lights come after wr2 static lights.
    #define FIXUP_WR1_LIGHT_INDEX(i) ( \
        ((i)<=0)? (i) : \
        ((i)<(1+wr1_static_light_count))? (i) : \
            ((i)+wr2_static_light_count) )
    // a: invalid or sunlight index is unchanged.
    // b: wr2 static light indexes now come after wr1 static lights.
    // c: wr2 dynamic lights now come after wr1 static and dynamic lights 
    #define FIXUP_WR2_LIGHT_INDEX(i) ( \
        ((i)<=0)? (i) : \
        ((i)<(1+wr2_static_light_count))? ((i)+wr1_static_light_count) : \
            ((i)+wr1_static_light_count+wr1_dynamic_light_count) )

    // wrm animlighttocell := [wr1 animlighttocell] [wr2 animlighttocell]
    uint16 wr1_animlighttocell_count = (uint16)arrlenu(wr1->animlight_to_cell_array);
    uint16 wr2_animlighttocell_count = (uint16)arrlenu(wr2->animlight_to_cell_array);
    uint16 wr1_animlighttocell_start = 0,
           wr1_animlighttocell_end = wr1_animlighttocell_start+wr1_animlighttocell_count;
    uint16 wr2_animlighttocell_start = wr1_animlighttocell_end,
           wr2_animlighttocell_end = wr2_animlighttocell_start+wr2_animlighttocell_count;
    uint16 wrm_animlighttocell_count = wr1_animlighttocell_count+wr2_animlighttocell_count;
    arrsetlen(wrm->animlight_to_cell_array, wrm_animlighttocell_count);
    for (uint16 i=0, j=wr1_animlighttocell_start; i<wr1_animlighttocell_count; ++i, ++j)
        wrm->animlight_to_cell_array[j] = wr1->animlight_to_cell_array[i];
    for (uint16 i=0, j=wr2_animlighttocell_start; i<wr2_animlighttocell_count; ++i, ++j)
        wrm->animlight_to_cell_array[j] = wr2->animlight_to_cell_array[i];

    // wrm csg_brfaces := [wr1 csg_brfaces] [wr2 csg_brfaces]
    uint32 wr1_csg_brfaces_count = arrlenu32(wr1->csg_brfaces_array);
    uint32 wr2_csg_brfaces_count = arrlenu32(wr2->csg_brfaces_array);
    uint32 wrm_csg_brfaces_count = wr1_csg_brfaces_count+wr2_csg_brfaces_count;
    uint32 wr1_csg_brfaces_start = 0,
           wr1_csg_brfaces_end = wr1_csg_brfaces_start+wr1_csg_brfaces_count;
    uint32 wr2_csg_brfaces_start = wr1_csg_brfaces_end,
           wr2_csg_brfaces_end = wr2_csg_brfaces_start+wr2_csg_brfaces_count;
    UNUSED(wr2_csg_brfaces_end);
    arrsetlen(wrm->csg_brfaces_array, wrm_csg_brfaces_count);
    for (uint32 i=0, j=wr1_csg_brfaces_start; i<wr1_csg_brfaces_count; ++i, ++j)
        wrm->csg_brfaces_array[j] = wr1->csg_brfaces_array[i];
    for (uint32 i=0, j=wr2_csg_brfaces_start; i<wr2_csg_brfaces_count; ++i, ++j)
        wrm->csg_brfaces_array[j] = wr2->csg_brfaces_array[i];
    // NOTE: it is possible to end up with brush faces from door blockables
    //       where the brush was just distant enough from surrounding terrain
    //       that it made a face. that is always an error (it will be a jorge
    //       face); but for our purposes it would create a csg_brushes array
    //       index that is out of bounds (because we truncate to actual brushes
    //       for sanity when merging). so if we find any such entries in the
    //       brfaces array, just
    for (uint32 i=0; i<wrm_csg_brfaces_count; ++i) {
        int32 brface = wrm->csg_brfaces_array[i];
        int32 br_id = CSG_BRFACE_GET_BRUSH_INDEX(brface);
        assert_format(br_id<=br_id_max,
            "brface %u has invalid brush id %d (do you have a door blockable creating faces?)",
            i, br_id);
    }

    // wrm csg_brush_plane_count := ...well...
    //
    // This array has one entry per brush. For simplicity, we would like that
    // every brush be wholly in one or the other wr (or neither). But the
    // reality is the csg_brush_plane* arrays can keep data from brushes that
    // did not end up in the worldrep! So if one wr has a zero count for a
    // brush, we keep the other one; if both have a nonzero count, then we just
    // hope (and assert) that they are identical, and copy it through.
    //
    // Note that as a result, we need to interleave the csg_brush_planes arrays
    // together, keeping each contiguous set of planes contiguous. So we do that
    // here at the same time.
    //
    // Neither csg_brush_plane_count_array nor csg_brush_planes_arrays need any
    // other fixup.
    //
    // Yes, "csg_brush_plane_count_count" would be confusing, so I am calling it
    // "csg_brush_plane_sum_count" instead.

    // csg_brush_plane_count arrays will be dovetailed together.
    uint32 wr1_csg_brush_plane_sum_count = arrlenu32(wr1->csg_brush_plane_count_array);
    uint32 wr2_csg_brush_plane_sum_count = arrlenu32(wr2->csg_brush_plane_count_array);
    // NOTE: When merging, we just truncate both arrays to only cover brush ids
    //       actually in use (see discussion at br_id_max above).
    if (wr1_csg_brush_plane_sum_count>(uint32)br_id_max)
        wr1_csg_brush_plane_sum_count = (uint32)br_id_max;
    if (wr2_csg_brush_plane_sum_count>(uint32)br_id_max)
        wr2_csg_brush_plane_sum_count = (uint32)br_id_max;
    assert(wr1_csg_brush_plane_sum_count==wr2_csg_brush_plane_sum_count);
    uint32 wrm_csg_brush_plane_sum_count = wr1_csg_brush_plane_sum_count;
    // NOTE: We don't know in advance how many csg_brush_planes entries will be
    //       in the result, so we append one by one instead of setting the
    //       array length up in advance.
    {
        // we need cursors for the csg_brush_planes arrays
        uint32 wr1_cursor = 0;
        uint32 wr2_cursor = 0;
        arrsetlen(wrm->csg_brush_plane_count_array, wrm_csg_brush_plane_sum_count);
        for (uint32 i=0; i<wrm_csg_brush_plane_sum_count; ++i) {
            int32 wr1_sum = wr1->csg_brush_plane_count_array[i];
            int32 wr2_sum = wr2->csg_brush_plane_count_array[i];
            if (wr1_sum!=0) {
                if (wr2_sum!=0) {
                    // Brush is in both wrs. Awkward.
                    // NOTE: in fact this is most likely not because it is in
                    //       both wrs, but because the csg code is sloppy about
                    //       clearing its memory, and the csg brush array is
                    //       persistent during a single dromed run.
                    assert_format(wr1_sum==wr2_sum, "Brush %u has different count of csg planes in both wrs!", i);
                    // NOTE: it seems an area brush can sometimes be in both
                    //       csg_brush lists with essentially randomly different
                    //       data? wild. skip them, and all non-terrain brushes.
                    LGBRLISTBrush *b1 = &brushes1[i];
                    // NOTE: we dont need to look at b2, because we have already
                    //       confirmed both BRLISTs are bit-identical.
                    if (LGBRUSH_GET_TYPE(*b1)==BRTYPE_TERRAIN) {
                        // Make sure the brush planes are--if not bit-identical
                        // (that can fail, which is a little surprising!), then at
                        // least very close to each other.
                        for (int32 j=0; j<wr1_sum; ++j) {
                            LGWRCSGPlane p1 = wr1->csg_brush_planes_array[wr1_cursor+j];
                            LGWRCSGPlane p2 = wr2->csg_brush_planes_array[wr2_cursor+j];
                            const double epsilon = 1.0e-10;
                            double da = fabs(p1.a-p2.a);
                            double db = fabs(p1.b-p2.b);
                            double dc = fabs(p1.c-p2.c);
                            double dd = fabs(p1.d-p2.d);
                            if (da>epsilon || db>epsilon || dc>epsilon || dd>epsilon) {
                                abort_format("Brush %u has different csg planes in both wrs!", i);
                                break;
                            }
                        }
                    }
                    // If we didn't abort, then the brush planes are the same
                    // in both wrs, so we fall through to copying it from wr1.
                    // We advance the wr2 cursor past these planes.
                    wr2_cursor += wr2_sum;
                }
                // Brush is in wr1. Append its planes.
                wrm->csg_brush_plane_count_array[i] = wr1_sum;
                for (int32 j=0; j<wr1_sum; ++j) {
                    arrput(wrm->csg_brush_planes_array,
                        wr1->csg_brush_planes_array[wr1_cursor++]);
                }
            } else {
                // Brush is either in wr2, or not in either wr. Append its planes (if any).
                wrm->csg_brush_plane_count_array[i] = wr2_sum;
                for (int32 j=0; j<wr2_sum; ++j) {
                    arrput(wrm->csg_brush_planes_array,
                        wr2->csg_brush_planes_array[wr2_cursor++]);
                }
            }
        }
    }

    // wrm csg_brush_surfaceref_count := ...well...
    //
    // This array has one entry per brush. For simplicity, we require that
    // every brush be wholly in one or the other wr (or neither). Unlike the
    // csg_brush_planes arrays (which can even include area brushes!), this
    // only seems to keep the brushes that actually made it into the worldrep.
    //
    // Note that as a result, we need to interleave the csg_brush_surfacerefs arrays
    // together, keeping each contiguous set of planes contiguous. So we do that
    // here at the same time.
    //
    // Also, we need to fixup the cell ids here, because later we would not know
    // which entries came from wr1 and which from wr2.
    //
    // Yes, "csg_brush_surfaceref_count_count" would be confusing, so I am calling it
    // "csg_brush_surfaceref_sum_count" instead.

    // csg_brush_surfaceref_count arrays will be dovetailed together.
    uint32 wr1_csg_brush_surfaceref_sum_count = arrlenu32(wr1->csg_brush_surfaceref_count_array);
    uint32 wr2_csg_brush_surfaceref_sum_count = arrlenu32(wr2->csg_brush_surfaceref_count_array);
    // NOTE: When merging, we just truncate both arrays to only cover brush ids
    //       actually in use (see discussion at br_id_max above).
    if (wr1_csg_brush_surfaceref_sum_count>(uint32)br_id_max)
        wr1_csg_brush_surfaceref_sum_count = (uint32)br_id_max;
    if (wr2_csg_brush_surfaceref_sum_count>(uint32)br_id_max)
        wr2_csg_brush_surfaceref_sum_count = (uint32)br_id_max;
    assert(wr1_csg_brush_surfaceref_sum_count==wr2_csg_brush_surfaceref_sum_count);
    uint32 wrm_csg_brush_surfaceref_sum_count = wr1_csg_brush_surfaceref_sum_count;
    // NOTE: We don't know in advance how many csg_brush_surfacerefs entries
    //       will be in the result, so we append one by one instead of setting
    //       the array length up in advance.
    {
        // we need cursors for the csg_brush_surfacerefs arrays
        uint32 wr1_cursor = 0;
        uint32 wr2_cursor = 0;
        uint32 wrm_cursor = 0;
        int16 wr1_cell_fixup = wr1_cell_start;
        int16 wr2_cell_fixup = wr2_cell_start;
        arrsetlen(wrm->csg_brush_surfaceref_count_array, wrm_csg_brush_surfaceref_sum_count);
        for (uint32 i=0; i<wrm_csg_brush_surfaceref_sum_count; ++i) {
            int32 wr1_sum = wr1->csg_brush_surfaceref_count_array[i];
            int32 wr2_sum = wr2->csg_brush_surfaceref_count_array[i];
            if (wr1_sum!=0) {
                if (wr2_sum!=0) {
                    // Brush is in both wrs. Slam that AZ-5!
                    abort_format("Brush %u has different surfacerefs in both wrs!", i);
                }
                // Brush is in wr1. Append its surfacerefs. And fixup the cells.
                wrm->csg_brush_surfaceref_count_array[i] = wr1_sum;
                for (int32 j=0; j<wr1_sum; ++j) {
                    arrput(wrm->csg_brush_surfacerefs_array,
                        wr1->csg_brush_surfacerefs_array[wr1_cursor++]);
                    wrm->csg_brush_surfacerefs_array[wrm_cursor].cell += wr1_cell_fixup;
                    ++wrm_cursor;
                }
            } else {
                // Brush is either in wr2, or not in either wr. Append its surfacerefs (if any).
                wrm->csg_brush_surfaceref_count_array[i] = wr2_sum;
                for (int32 j=0; j<wr2_sum; ++j) {
                    arrput(wrm->csg_brush_surfacerefs_array,
                        wr2->csg_brush_surfacerefs_array[wr2_cursor++]);
                    wrm->csg_brush_surfacerefs_array[wrm_cursor].cell += wr2_cell_fixup;
                    ++wrm_cursor;
                }
            }
        }
    }

/*
    // wrm xxxxxx := [wr1 xxxxxx] [wr2 xxxxxx]
    uint32 wr1_xxxxxx_count = arrlenu32(wr1->xxxxxx_array);
    uint32 wr2_xxxxxx_count = arrlenu32(wr2->xxxxxx_array);
    assert(wr1_xxxxxx_count==wr1_cell_count);
    assert(wr2_xxxxxx_count==wr2_cell_count);
    uint32 wrm_xxxxxx_count = wr1_xxxxxx_count+wr2_xxxxxx_count;
    uint32 wr1_xxxxxx_start = 0;
    uint32 wr2_xxxxxx_start = wr1_xxxxxx_start+wr1_xxxxxx_count;
    arrsetlen(wrm->xxxxxx_array, wrm_xxxxxx_count);
    for (uint32 i=0, j=wr1_xxxxxx_start; i<wr1_xxxxxx_count; ++i, ++j)
        wrm->xxxxxx_array[j] = wr1->xxxxxx_array[i];
    for (uint32 i=0, j=wr2_xxxxxx_start; i<wr2_xxxxxx_count; ++i, ++j)
        wrm->xxxxxx_array[j] = wr2->xxxxxx_array[i];
*/
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

        // TODO: how does NewDark's "dynamic light" checkbox on AnimLights complicate this?
        // Fixup light and animlight indexes.
        if (i>=wr1_cell_start && i<wr1_cell_end) {
            for (uint32 j=0, jend=arrlenu32(cell->light_index_array); j<jend; ++j) {
                if (j==0) continue; // light_index_array[0] is the light count!
                uint16 index = cell->light_index_array[j];
                index = FIXUP_WR1_LIGHT_INDEX(index);
                cell->light_index_array[j] = index;
            }
            for (uint32 j=0, jend=arrlenu32(cell->animlight_array); j<jend; ++j) {
                uint16 index = cell->animlight_array[j];
                index = FIXUP_WR1_LIGHT_INDEX(index);
                cell->animlight_array[j] = index;
            }
        } else if (i>=wr2_cell_start && i<wr2_cell_end) {
            for (uint32 j=0, jend=arrlenu32(cell->light_index_array); j<jend; ++j) {
                if (j==0) continue; // light_index_array[0] is the light count!
                uint16 index = cell->light_index_array[j];
                index = FIXUP_WR2_LIGHT_INDEX(index);
                cell->light_index_array[j] = index;
            }
            for (uint32 j=0, jend=arrlenu32(cell->animlight_array); j<jend; ++j) {
                uint16 index = cell->animlight_array[j];
                index = FIXUP_WR2_LIGHT_INDEX(index);
                cell->animlight_array[j] = index;
            }
        } else {
            // Nothing to fixup.
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
        if (node_fixup) {
            uint32 parent_index = BSP_GET_PARENT(node);
            if (parent_index==BSP_INVALID) {
                parent_index = 0;
            } else {
                parent_index += node_fixup;
            }
            BSP_SET_PARENT(node, parent_index);
        }
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

    // Fixup animlighttocell:
    for (uint32 i=0; i<wrm_animlighttocell_count; ++i) {
        int16 cell_fixup;
        if (i>=wr1_animlighttocell_start && i<wr1_animlighttocell_end) {
            cell_fixup = wr1_cell_start;
        } else if (i>=wr2_animlighttocell_start && i<wr2_animlighttocell_end) {
            cell_fixup = wr2_cell_start;
        } else {
            cell_fixup = 0;
        }
        wrm->animlight_to_cell_array[i].cell_index += cell_fixup;
    }

    // merged weather := [dbfile1 weather] [dbfile2 weather]
    LGDBVersion weather_version;
    LGCellWeather *weatherm = NULL;
    {
        DBTagBlock *tagblock1 = dbfile_get_tag(dbfile1, TAG_WEATHER);
        DBTagBlock *tagblock2 = dbfile_get_tag(dbfile2, TAG_WEATHER);
        weather_version = tagblock1->version;
        LGCellWeather *weather1 = weather_load_from_tagblock(tagblock1);
        LGCellWeather *weather2 = weather_load_from_tagblock(tagblock2);
        uint32 weather1_count = arrlenu32(weather1);
        uint32 weather2_count = arrlenu32(weather2);
        assert(weather1_count==(uint32)wr1_cell_count);
        assert(weather2_count==(uint32)wr2_cell_count);
        uint32 weatherm_count = weather1_count+weather2_count;
        uint32 weather1_start = 0;
        uint32 weather2_start = weather1_start+weather1_count;
        arrsetlen(weatherm, weatherm_count);
        for (uint32 i=0, j=weather1_start; i<weather1_count; ++i, ++j)
            weatherm[j] = weather1[i];
        for (uint32 i=0, j=weather2_start; i<weather2_count; ++i, ++j)
            weatherm[j] = weather2[i];
        arrfree(weather1);
        arrfree(weather2);
    }

    // merged AnimLight props := ...well...
    //
    // AnimLight props are stored per-object; since the objects in both input
    // files are required to be identical, we take AnimLight properties from
    // dbfile1 by default, but override them with AnimLight properties from
    // dbfile2 for lights which affected wr2 and not wr1. This can be determined
    // by the light_array_index: it will be >=0 for AnimLights that reached
    // cells. If there are any AnimLights that reached cells in both wr1 and
    // wr2, there isn't a simple way to deal with that, so we just disallow it.
    //
    // NOTE: because the two arrays are being dovetailed and not concatenated,
    //       we also have to apply fixups right now, because later we won't
    //       know which AnimLight comes from dbfile1 and which from dbfile2.
    LGDBVersion animlight_version;
    int old_animlight_version;
    LGAnimLightProp80 *alm = NULL;
    {
        DBTagBlock *tagblock1 = dbfile_get_tag(dbfile1, TAG_PROP_ANIMLIGHT);
        DBTagBlock *tagblock2 = dbfile_get_tag(dbfile2, TAG_PROP_ANIMLIGHT);
        animlight_version = tagblock1->version;
        old_animlight_version = (DBTAGBLOCK_PROP_ITEM_SIZE(tagblock1)==76);
        LGAnimLightProp80 *al1 = animlight_prop_load_from_tagblock(tagblock1);
        LGAnimLightProp80 *al2 = animlight_prop_load_from_tagblock(tagblock2);
        assert(arrlen(al1)==arrlen(al2));
        arrsetlen(alm, arrlen(al1));
        uint16 lighttocell1_fixup = wr1_animlighttocell_start;
        uint16 lighttocell2_fixup = wr2_animlighttocell_start;
        for (uint32 i=0, iend=arrlenu32(alm); i<iend; ++i) {
            int16 al1_index = al1[i].prop.x.animation.light_array_index;
            int16 al2_index = al2[i].prop.x.animation.light_array_index;
            if (al1_index==-1) {
                if (al2_index==-1) {
                    // Doesnt reach any cells in either wr; just copy it from 1.
                    alm[i] = al1[i];
                    // Make sure there's nothing to fixup.
                    LGAnimLightAnimation *anim = &alm[i].prop.x.animation;
                    assert(anim->lighttocell_count==0);
                } else {
                    // Only reaches cells in wr2.
                    alm[i] = al2[i];
                    // Fixup light and cell indexes.
                    LGAnimLightAnimation *anim = &alm[i].prop.x.animation;
                    anim->lighttocell_start += lighttocell2_fixup;
                    int16 index = anim->light_array_index;
                    index = FIXUP_WR2_LIGHT_INDEX(index);
                    anim->light_array_index = index;
                }
            } else {
                if (al2_index==-1) {
                    // Only reaches cells in wr1.
                    alm[i] = al1[i];
                    // Fixup light and cell indexes.
                    LGAnimLightAnimation *anim = &alm[i].prop.x.animation;
                    anim->lighttocell_start += lighttocell1_fixup;
                    int16 index = anim->light_array_index;
                    index = FIXUP_WR1_LIGHT_INDEX(index);
                    anim->light_array_index = index;
                } else {
                    abort_format("AnimLight ObjID %d is in both worldreps!",
                        alm[i].header.id);
                }
            }
        }
        arrfree(al1);
        arrfree(al2);
    }

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
        || tag_name_eq_str(src_tagblock->key, TAG_WEATHER)
        || tag_name_eq_str(src_tagblock->key, TAG_PROP_ANIMLIGHT))
            continue;

        DBTagBlock dest_tagblock = {0};
        dbtagblock_copy(&dest_tagblock, src_tagblock);
        hmputs(dbfile_out->tagblock_hash, dest_tagblock);
    }

    // Write the merged worldrep to the output.
    {
        DBTagBlock tagblock = {0};
        wr_save_to_tagblock(&tagblock, wrm);
        hmputs(dbfile_out->tagblock_hash, tagblock);
    }

    // Write the merged Weather to the output.
    {
        DBTagBlock tagblock = {0};
        tagblock.key = tag_name_from_str(TAG_WEATHER);
        tagblock.version = weather_version;
        DBTAGBLOCK_WRITE_ARRAY(&tagblock, weatherm);
        hmputs(dbfile_out->tagblock_hash, tagblock);
    }

    // Write the merged AnimLight prop to the output.
    {
        DBTagBlock tagblock = {0};
        tagblock.key = tag_name_from_str(TAG_PROP_ANIMLIGHT);
        tagblock.version = animlight_version;
        if (old_animlight_version) {
            LGAnimLightProp76 *alm_old = NULL;
            arrsetlen(alm_old, arrlen(alm));
            for (uint32 i=0, iend=arrlenu32(alm_old); i<iend; ++i) {
                memcpy(&alm_old[i], &alm[i], sizeof(LGAnimLightProp76));
            }
            DBTAGBLOCK_WRITE_ARRAY(&tagblock, alm_old);
            arrfree(alm_old);
        } else {
            DBTAGBLOCK_WRITE_ARRAY(&tagblock, alm);
        }
        hmputs(dbfile_out->tagblock_hash, tagblock);
    }

    return dbfile_out;
}

/** AIPATH stuff */

typedef struct AIPathDB {
    int32 initialized;
    LGAIPATHCellID cell_count;
    LGAIPATHCellv3_4 *cell_array;
    LGAIPATHCellID plane_count;
    LGAIPATHCellPlane *plane_array;
    LGAIPATHVertexID vertex_count;
    LGAIPATHVertex *vertex_array;
    LGAIPATHCell2CellLinkID link_count;
    LGAIPATHCellLink *link_array;
    LGAIPATHCell2VertexLinkID cell_vertex_count;
    LGAIPATHCell2VertexLink *cell_vertex_array;
    uint32 objhint_count;
    /* TODO: more stuff! */
} AIPathDB;

void aipathdb_load_from_tagblock(struct AIPathDB *aipathdb, DBTagBlock *tagblock) {
    MEM_ZERO(*aipathdb);

    // TODO: we could read v3_3 if we want, but newdark does v3_4.
    assert(tagblock->version.major==3 && tagblock->version.minor==4);
    char *pread = tagblock->data;
    char *pend = pread+tagblock->size;

    MEM_READ(aipathdb->initialized, pread);
    if (! aipathdb->initialized)
        return;

    int32 obsolete;
    MEM_READ(obsolete, pread);

    MEM_READ(aipathdb->cell_count, pread);
    MEM_READ_ARRAY(aipathdb->cell_array, (aipathdb->cell_count+1), pread);

    MEM_READ(aipathdb->plane_count, pread);
    MEM_READ_ARRAY(aipathdb->plane_array, (aipathdb->plane_count+1), pread);

    MEM_READ(aipathdb->vertex_count, pread);
    MEM_READ_ARRAY(aipathdb->vertex_array, (aipathdb->vertex_count+1), pread);

    MEM_READ(aipathdb->link_count, pread);
    MEM_READ_ARRAY(aipathdb->link_array, (aipathdb->link_count+1), pread);

    MEM_READ(aipathdb->cell_vertex_count, pread);
    MEM_READ_ARRAY(aipathdb->cell_vertex_array, (aipathdb->cell_vertex_count+1), pread);

    MEM_READ(aipathdb->objhint_count, pread);

    // TODO: more fields!
}

/** BRLIST stuff */

LGBRLISTBrush *brlist_load_from_tagblock(DBTagBlock *tagblock) {
    assert(tagblock->version.major==1 && tagblock->version.minor==0);
    char *pread = tagblock->data;
    char *pend = pread+tagblock->size;

    LGBRLISTBrush *brushes = NULL;
    LGBRLISTBrush brush;

    size_t bare_size = sizeof(brush)-sizeof(brush.faces);
    size_t face_size = sizeof(brush.faces[0]);
    while (pread<pend) {
        // Read one brush
        MEM_ZERO(brush);
        MEM_READ_SIZE(&brush, bare_size, pread);
        int face_count = LGBRUSH_GET_FACE_COUNT(brush);
        if (face_count>0)
            MEM_READ_SIZE(brush.faces, face_count*face_size, pread);
        arrput(brushes, brush);
    }
    assert(pread==pend);

    return brushes;
}

int16 brlist_get_id_max(LGBRLISTBrush *brushes) {
    int16 br_id_max = -1;
    for (uint32 i=0, iend=arrlenu32(brushes); i<iend; ++i) {
        int16 br_id = brushes[i].br_id;
        if (br_id>br_id_max)
            br_id_max = br_id;
    }
    return br_id_max;
}

LGBRLISTBrush *brlist_sorted_by_id(LGBRLISTBrush *brlist) {
    LGBRLISTBrush *brushes = NULL;
    int16 id_max = brlist_get_id_max(brlist);
    arrsetlen(brushes, id_max+1);
    LGBRLISTBrush null_brush = {0};
    null_brush.br_id = -1;
    null_brush.media = -1;
    // Set all entries to an invalid brush.
    for (uint32 i=0, iend=arrlenu32(brushes); i<iend; ++i)
        brushes[i] = null_brush;
    // Copy all the brushes over.
    for (uint32 i=0, iend=arrlenu32(brlist); i<iend; ++i) {
        LGBRLISTBrush *b = &brlist[i];
        int16 id = b->br_id;
        brushes[id] = *b;
    }
    // Ensure no ids are invalid -- except 0.
    for (uint32 i=0, iend=arrlenu32(brushes); i<iend; ++i) {
        if (i==0 && brushes[i].br_id!=-1)
            abort_message("BRLIST contained brush with id 0");
        if (i!=0 && brushes[i].br_id==-1)
            abort_format("BRLIST is missing brush for id %u", i);
    }
    return brushes;
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

/** Property stuff */

LGAnimLightProp80 *animlight_prop_load_from_tagblock(DBTagBlock *tagblock) {
    assert(tag_name_eq_str(tagblock->key, TAG_PROP_ANIMLIGHT));
    LGAnimLightProp80 *array = NULL;
    assert(DBTAGBLOCK_PROP_VERSION_MAJOR(tagblock)==2
        && DBTAGBLOCK_PROP_VERSION_MINOR(tagblock)==0
        && (DBTAGBLOCK_PROP_ITEM_SIZE(tagblock)==76
            || DBTAGBLOCK_PROP_ITEM_SIZE(tagblock)==80));

    if (DBTAGBLOCK_PROP_ITEM_SIZE(tagblock)==76) {
        LGAnimLightProp76 *array_old = NULL;
        DBTAGBLOCK_READ_ARRAY(array_old, tagblock);
        arrsetlen(array, arrlen(array_old));
        for (uint32 i=0, iend=arrlenu32(array); i<iend; ++i) {
            memcpy(&array[i], &array_old[i], sizeof(LGAnimLightProp76));
            array[i].prop.is_dynamic = 0;
        }
        arrfree(array_old);
    } else {
        DBTAGBLOCK_READ_ARRAY(array, tagblock);
    }
    return array;
}

LGLightProp *light_prop_load_from_tagblock(DBTagBlock *tagblock) {
    assert(tag_name_eq_str(tagblock->key, TAG_PROP_LIGHT));
    LGLightProp *array = NULL;
    assert(DBTAGBLOCK_PROP_VERSION_MAJOR(tagblock)==2
        && DBTAGBLOCK_PROP_VERSION_MINOR(tagblock)==0
        && DBTAGBLOCK_PROP_ITEM_SIZE(tagblock)==sizeof(LGLight));
    DBTAGBLOCK_READ_ARRAY(array, tagblock);
    return array;
}

LGPositionProp *position_prop_load_from_tagblock(DBTagBlock *tagblock) {
    assert(tag_name_eq_str(tagblock->key, TAG_PROP_POSITION));
    LGPositionProp *array = NULL;
    assert(DBTAGBLOCK_PROP_VERSION_MAJOR(tagblock)==2
        && DBTAGBLOCK_PROP_VERSION_MINOR(tagblock)==1
        && DBTAGBLOCK_PROP_ITEM_SIZE(tagblock)==sizeof(LGPosition));
    DBTAGBLOCK_READ_ARRAY(array, tagblock);
    return array;
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
    // top.mis bottom.mis a b c d -o out.mis
    // double strtod(const char *nptr, char **endptr);
    if (argc!=8
    || strcmp(argv[6], "-o")!=0) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }

    char *in_filename[2];
    for (int i=0; i<2; ++i)
        in_filename[i] = argv[i];
    LGWRPlane split_plane;
    split_plane.normal.x = (float)atof(argv[2]);
    split_plane.normal.y = (float)atof(argv[3]);
    split_plane.normal.z = (float)atof(argv[4]);
    split_plane.distance = (float)atof(argv[5]);
    if (sqrt(vdot(split_plane.normal, split_plane.normal))<0.01) {
        abort_message("Plane normal looks messed up!");
    }
    char *out_filename = argv[7];

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
    int maxdepth = 99999; // was: 2;
    LGWRBSPNode *node_array = wr->bsp_node_array;
    struct frame {
        LGWRBSPNode *node;
        int depth;
    };
    struct frame *queue = NULL;
    int cursor = 0;
    struct frame frame;
    frame.node = &node_array[0];
    frame.depth = 0;
    arrput(queue, frame);
    while (cursor<arrlen(queue)) {
    //for (uint32 i=0, iend=arrlenu32(node_array); i<iend; ++i) {
    //    LGWRBSPNode *node = &node_array[i];
        frame = queue[cursor++];
        LGWRBSPNode *node = frame.node;
        int i = (int)(node-node_array);
        if (frame.depth<maxdepth) {
            if (! BSP_IS_LEAF(node)) {
                struct frame child;
                if (node->inside_index!=BSP_INVALID) {
                    child.node = &node_array[node->inside_index];
                    child.depth = frame.depth+1;
                    arrput(queue, child);
                }
                if (node->outside_index!=BSP_INVALID)
                    child.node = &node_array[node->outside_index];
                    child.depth = frame.depth+1;
                    arrput(queue, child);
            }
        }

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

int do_dump_wr(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *in_filename = argv[0];
    DBFile *dbfile = dbfile_load(in_filename);
    DBTagBlock *wr_tagblock = dbfile_get_wr_tagblock(dbfile);
    WorldRep *wr = wr_load_from_tagblock(wr_tagblock);

    LGBRLISTBrush *brlist = brlist_load_from_tagblock(
        dbfile_get_tag(dbfile, TAG_BRLIST));
    int16 br_id_max = brlist_get_id_max(brlist);
    arrfree(brlist);

    const char *format_name[] = { "WR", "WRRGB", "WREXT" };
    printf("format: %s\n", format_name[wr->format]);
    printf("lightmap_bpp: %d\n", wr->lightmap_format.lightmap_bpp);
    printf("lightmap_2x_modulation: %d\n", wr->lightmap_format.lightmap_2x_modulation);
    printf("lightmap_scale: %f\n", wr->lightmap_format.lightmap_scale);
    printf("flags: 0x%08x\n", wr->flags);
    for (uint32 i=0, iend=arrlenu32(wr->cell_array); i<iend; ++i) {
        WorldRepCell *cell = &wr->cell_array[i];
        printf("CELL %u:\n", i);
        printf("\tnum_vertices: %u\n", cell->header.num_vertices);
        printf("\tnum_polys: %u\n", cell->header.num_polys);
        printf("\tnum_render_polys: %u\n", cell->header.num_render_polys);
        printf("\tnum_portal_polys: %u\n", cell->header.num_portal_polys);
        printf("\tnum_planes: %u\n", cell->header.num_planes);
        printf("\tmedium: %u\n", cell->header.medium);
        printf("\tflags: 0x%02x\n", cell->header.flags);
        printf("\tportal_vertex_list: %d\n", cell->header.portal_vertex_list);
        printf("\tnum_vlist: %u\n", cell->header.num_vlist);
        printf("\tnum_anim_lights: %u\n", cell->header.num_anim_lights);
        printf("\tmotion_index: %u\n", cell->header.motion_index);
        printf("\tsphere_center: %f %f %f \n", cell->header.sphere_center.x, cell->header.sphere_center.y, cell->header.sphere_center.z);
        printf("\tsphere_radius: %f\n", cell->header.sphere_radius);
        printf("\tlightmaps_size: %u\n", cell->lightmaps_size);
        for (uint32 j=0, jend=arrlenu32(cell->vertex_array); j<jend; ++j) {
            LGVector v = cell->vertex_array[j];
            printf("\tVERTEX %u: %f %f %f\n", j, v.x, v.y, v.z);
        }
        for (uint32 j=0, jend=arrlenu32(cell->poly_array); j<jend; ++j) {
            LGWRPoly *p = &cell->poly_array[j];
            printf("\tPOLY %u:\n", j);
            printf("\t\tflags: 0x%02x\n", p->flags);
            printf("\t\tnum_vertices: %u\n", p->num_vertices);
            printf("\t\tplaneid: %u\n", p->planeid);
            printf("\t\tclut_id: %u\n", p->clut_id);
            printf("\t\tdestination: %d\n", p->destination);
            printf("\t\tmotion_index: %u\n", p->motion_index);
            printf("\t\tpadding: %u\n", p->padding);
        }
        for (uint32 j=0, jend=arrlenu32(cell->renderpoly_array); j<jend; ++j) {
            LGWRRenderPoly *rp = &cell->renderpoly_array[j];
            printf("\tRENDERPOLY %u:\n", j);
            printf("\t\ttex_u: %f %f %f\n", rp->tex_u.x, rp->tex_u.y, rp->tex_u.z);
            printf("\t\ttex_v: %f %f %f\n", rp->tex_v.x, rp->tex_v.y, rp->tex_v.z);
            printf("\t\tu_base: %u\n", rp->u_base);
            printf("\t\tv_base: %u\n", rp->v_base);
            printf("\t\ttexture_id: %u\n", rp->texture_id);
            printf("\t\ttexture_anchor: %u\n", rp->texture_anchor);
            printf("\t\tcached_surface: %u\n", rp->cached_surface);
            printf("\t\ttexture_mag: %f\n", rp->texture_mag);
            printf("\t\tcenter: %f %f %f\n", rp->center.x, rp->center.y, rp->center.z);
        }
        for (uint32 j=0, jend=arrlenu32(cell->renderpoly_ext_array); j<jend; ++j) {
            LGWREXTRenderPoly *rp = &cell->renderpoly_ext_array[j];
            printf("\tRENDERPOLY %u:\n", j);
            printf("\t\ttex_u: %f %f %f\n", rp->tex_u.x, rp->tex_u.y, rp->tex_u.z);
            printf("\t\ttex_v: %f %f %f\n", rp->tex_v.x, rp->tex_v.y, rp->tex_v.z);
            printf("\t\tu_base: %f\n", rp->u_base);
            printf("\t\tv_base: %f\n", rp->v_base);
            printf("\t\ttexture_id: %u\n", rp->texture_id);
            printf("\t\tcached_surface: %u\n", rp->cached_surface);
            printf("\t\ttexture_mag: %f\n", rp->texture_mag);
            printf("\t\tcenter: %f %f %f\n", rp->center.x, rp->center.y, rp->center.z);
        }
        for (uint32 j=0, jend=arrlenu32(cell->index_array); j<jend; ++j) {
            uint8 index = cell->index_array[j];
            printf("\tINDEX %u: %u\n", j, index);
        }
        for (uint32 j=0, jend=arrlenu32(cell->plane_array); j<jend; ++j) {
            LGWRPlane p = cell->plane_array[j];
            printf("\tPLANE %u: %f %f %f %f\n", j, p.normal.x, p.normal.x, p.normal.z, p.distance);
        }
        for (uint32 j=0, jend=arrlenu32(cell->animlight_array); j<jend; ++j) {
            int16 animlight = cell->animlight_array[j];
            printf("\tANIMLIGHT %u: %d\n", j, animlight);
        }
        for (uint32 j=0, jend=arrlenu32(cell->lightmapinfo_array); j<jend; ++j) {
            LGWRLightMapInfo *l = &cell->lightmapinfo_array[j];
            printf("\tLIGHTMAPINFO %u:\n", j);
            printf("\t\tu_base: %d\n", l->u_base);
            printf("\t\tv_base: %d\n", l->v_base);
            printf("\t\tpadded_width: %d\n", l->padded_width);
            printf("\t\theight: %u\n", l->height);
            printf("\t\twidth: %u\n", l->width);
            printf("\t\tdata_ptr: 0x%08x\n", l->data_ptr);
            printf("\t\tdynamic_light_ptr: 0x%08x\n", l->dynamic_light_ptr);
            printf("\t\tanim_light_bitmask: 0x%08x\n", l->anim_light_bitmask);
        }
        for (uint32 j=0, jend=arrlenu32(cell->light_index_array); j<jend; ++j) {
            int16 index = cell->light_index_array[j];
            printf("\tLIGHT_INDEX %u: %d\n", j, index);
        }
    }
    for (uint32 i=0, iend=arrlenu32(wr->bsp_extraplane_array); i<iend; ++i) {
        LGWRPlane p = wr->bsp_extraplane_array[i];
        printf("BSP_EXTRA_PLANE %u: %f %f %f %f\n", i, p.normal.x, p.normal.x, p.normal.z, p.distance);
    }
    for (uint32 i=0, iend=arrlenu32(wr->bsp_node_array); i<iend; ++i) {
        LGWRBSPNode *node = &wr->bsp_node_array[i];
        printf("BSP_NODE %u:\n", i);
        printf("\tparent_index: %08x (parent: %u, flags %02x)\n",
            node->parent_index, BSP_GET_PARENT(node), BSP_GET_FLAGS(node));
        printf("\tplane_cell_id: %d\n", node->plane_cell_id);
        printf("\tplane_id: %d\n", node->plane_id);
        if (BSP_IS_LEAF(node)) {
            printf("\tcell_id: %u\n", node->cell_id);
            printf("\tpad4: %u\n", node->pad4);
        } else {
            printf("\tinside_index: %u\n", node->inside_index);
            printf("\toutside_index: %u\n", node->outside_index);
        }
    }
    for (uint32 i=0, iend=arrlenu32(wr->cell_weatherzones_array); i<iend; ++i) {
        uint8 v = wr->cell_weatherzones_array[i];
        printf("CELL_WEATHERZONE %u: %u\n", i, v);
    }
    for (uint32 i=0, iend=arrlenu32(wr->cell_renderoptions_array); i<iend; ++i) {
        uint8 v = wr->cell_renderoptions_array[i];
        printf("CELL_RENDEROPTIONS %u: 0x%02x\n", i, v);
    }
    printf("num_static_lights: %u\n", wr->num_static_lights);
    printf("num_dynamic_lights: %u\n", wr->num_dynamic_lights);
    for (uint32 i=0, iend=arrlenu32(wr->light_array); i<iend; ++i) {
        LGWRRGBLight *l = &wr->light_array[i];
        printf("LIGHT %u:\n", i);
        printf("\tlocation: %f %f %f\n", l->location.x, l->location.y, l->location.z);
        printf("\tdirection: %f %f %f\n", l->direction.x, l->direction.y, l->direction.z);
        printf("\tbright: %f %f %f\n", l->bright.x, l->bright.y, l->bright.z);
        printf("\tinner: %f\n", l->inner);
        printf("\touter: %f\n", l->outer);
        printf("\tradius: %f\n", l->radius);
    }
    for (uint32 i=0, iend=arrlenu32(wr->animlight_to_cell_array); i<iend; ++i) {
        LGWRAnimLightToCell *l = &wr->animlight_to_cell_array[i];
        printf("ANIMLIGHT_TO_CELL %u:\n", i);
        printf("\tcell_index: %u\n", l->cell_index);
        printf("\tpos_in_cell_palette: %u\n", l->pos_in_cell_palette);
        printf("\tpad0: %u\n", l->pad0);
    }
    for (uint32 i=0, iend=arrlenu32(wr->csg_brfaces_array); i<iend; ++i) {
        int32 v = wr->csg_brfaces_array[i];
        int32 brush = CSG_BRFACE_GET_BRUSH_INDEX(v);
        int32 face = CSG_BRFACE_GET_FACE_INDEX(v);
        if (brush>br_id_max)
            printf("!!!! excessive brush id !!!! ");
        printf("CSG_BRFACES %u: brush %d face %d (0x%08x)\n", i, brush, face, v);
    }
    for (uint32 i=0, iend=arrlenu32(wr->csg_brush_plane_count_array); i<iend; ++i) {
        int32 count = wr->csg_brush_plane_count_array[i];
        if ((int32)i>br_id_max)
            printf("*"); // excessive br_id
        printf("CSG_BRUSH_PLANE_COUNT %u: %d\n", i, count);
    }
    int cursor=0;
    for (uint32 i=0, iend=arrlenu32(wr->csg_brush_plane_count_array); i<iend; ++i) {
        int32 count = wr->csg_brush_plane_count_array[i];
        while (count--) {
            LGWRCSGPlane p = wr->csg_brush_planes_array[cursor];
            printf("CSG_BRUSH_PLANE %u (brush %u): %f %f %f %f\n",
                cursor, i, p.a, p.b, p.c, p.d);
            ++cursor;
        }
    }
    for (uint32 i=0, iend=arrlenu32(wr->csg_brush_surfaceref_count_array); i<iend; ++i) {
        int32 v = wr->csg_brush_surfaceref_count_array[i];
        if ((int32)i>br_id_max)
            printf("*"); // excessive br_id
        printf("CSG_BRUSH_SURFACEREF_COUNT %u: %d\n", i, v);
    }
    for (uint32 i=0, iend=arrlenu32(wr->csg_brush_surfacerefs_array); i<iend; ++i) {
        LGWRCSGSurfaceRef *r = &wr->csg_brush_surfacerefs_array[i];
        printf("CSG_BRUSH_SURFACEREF %u:\n", i);
        printf("\tcell: %d\n", r->cell);
        printf("\tsurface: %u\n", r->surface);
        printf("\tbrush_face: %u\n", r->brush_face);
        printf("\tvertex: %d\n", r->vertex);
    }

    wr_free(&wr);
    dbfile = dbfile_free(dbfile);
    return 0;
}

int do_dump_aipath(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *in_filename = argv[0];
    DBFile *dbfile = dbfile_load(in_filename);
    DBTagBlock *tagblock = dbfile_get_tag(dbfile, TAG_AIPATH);
    AIPathDB aipathdb;
    aipathdb_load_from_tagblock(&aipathdb, tagblock);

    printf("initialized: %u\n", aipathdb.initialized);
    printf("Cell count: %u\n", aipathdb.cell_count);
    printf("Plane count: %u\n", aipathdb.plane_count);
    printf("Vertex count: %u\n", aipathdb.vertex_count);
    printf("Link count: %u\n", aipathdb.link_count);
    printf("Cell vertex count: %u\n", aipathdb.cell_vertex_count);
    for (LGAIPATHCellID i=0, iend=aipathdb.cell_count; i<iend; ++i) {
        LGAIPATHCellv3_4 *cell = &aipathdb.cell_array[i];
        printf("Cell %u:\n", i);
        printf("\tfirstVertex: %u\n", cell->firstVertex);
        printf("\tfirstCell: %u\n", cell->firstCell);
        printf("\tplane: %u\n", cell->plane);
        printf("\tnext: %u\n", cell->next);
        printf("\tbestNeighbor: %u\n", cell->bestNeighbor);
        printf("\tlinkFromNeighbor: %u\n", cell->linkFromNeighbor);
        printf("\tvertexCount: %u\n", (uint32)cell->vertexCount);
        printf("\tpathFlags: 0x%02x\n", (uint32)cell->pathFlags);
        printf("\tcellCount: %u\n", (uint32)cell->cellCount);
        printf("\twrapFlags: 0x%02x\n", (uint32)cell->wrapFlags);
        printf("\tcenter: %f %f %f\n", cell->center.x, cell->center.y, cell->center.z);
        printf("\tflags: 0x%08x\n", cell->flags);
    }
    for (LGAIPATHCellID i=0, iend=aipathdb.plane_count; i<iend; ++i) {
        LGAIPATHCellPlane *plane = &aipathdb.plane_array[i];
        printf("Plane %u: %f %f %f %f\n", i, plane->normal.x, plane->normal.y, plane->normal.z, plane->constant);
    }
    for (LGAIPATHVertexID i=0, iend=aipathdb.vertex_count; i<iend; ++i) {
        LGAIPATHVertex *vertex = &aipathdb.vertex_array[i];
        printf("Vertex %u: %f %f %f, info %d\n", i, vertex->pt.x, vertex->pt.y, vertex->pt.z, vertex->ptInfo);
    }
    for (LGAIPATHCell2CellLinkID i=0, iend=aipathdb.link_count; i<iend; ++i) {
        LGAIPATHCellLink *link = &aipathdb.link_array[i];
        printf("Link %u:\n", i);
        printf("\tdest: %u\n", link->dest);
        printf("\tvertex_1: %u\n", link->vertex_1);
        printf("\tvertex_2: %u\n", link->vertex_2);
        printf("\tokBits: 0x%02x\n", (uint32)link->okBits);
        printf("\tcost: %u\n", (uint32)link->cost);
    }
    for (LGAIPATHCell2VertexLinkID i=0, iend=aipathdb.cell_vertex_count; i<iend; ++i) {
        LGAIPATHCell2VertexLink *cell_vertex = &aipathdb.cell_vertex_array[i];
        printf("Cell vertex %u: %u\n", i, cell_vertex->id);
    }
    printf("objhint_count: %u\n", aipathdb.objhint_count);
    // TODO: more

    dbfile = dbfile_free(dbfile);
    return 0;
}

int do_dump_brlist(int argc, char **argv, struct command *cmd) {
    if (argc!=1) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    char *in_filename = argv[0];
    DBFile *dbfile = dbfile_load(in_filename);
    DBTagBlock *tagblock = dbfile_get_tag(dbfile, TAG_BRLIST);
    LGBRLISTBrush *brlist = brlist_load_from_tagblock(tagblock);

    static const char *brush_type_s[] = {
        "terrain",
        "light",
        "area",
        "object",
        "flow",
        "room",
    };
    static const char *brush_shape_s[] = {
        "cube",
        "cylinder",
        "pyramid",
        "apex pyramid",
        "dodecahedron",
        "wedge",
    };
    static const char *brush_align_s[] = {
        "by sides",
        "by vertices",
    };
    static const char *brush_medium_s[] = {
        "solid",
        "air",
        "water",
        "flood",
        "evaporate",
        "solid->water",
        "solid->air",
        "air->solid",
        "water->solid",
        "blockable",
    };

    for (uint32 i=0, iend=arrlenu32(brlist); i<iend; ++i) {
        LGBRLISTBrush br = brlist[i];
        printf("Brush %d, time %u:\n", (int)br.br_id, i);
        int type = LGBRUSH_GET_TYPE(br);
        assert(type!=BRTYPE_INVALID);
        printf("\ttype: %d %s\n", type, brush_type_s[type]);
        if (type==BRTYPE_TERRAIN) {
            int shape = LGBRUSH_GET_TERR_SHAPE(br);
            assert(shape!=BRSHAPE_INVALID);
            printf("\tshape: %d %s\n", type, brush_shape_s[shape]);
            int sides = LGBRUSH_GET_TERR_SIDES(br);
            printf("\tsides: %d\n", sides);
            int medium = LGBRUSH_GET_MEDIUM(br);
            printf("\tmedium: %d %s\n", medium, brush_medium_s[medium]);
        }
        int align = LGBRUSH_GET_ALIGN(br);
        printf("\talign: %d %s\n", type, brush_align_s[align]);
        printf("\tposition: %f %f %f\n", br.pos.x, br.pos.y, br.pos.z);
        printf("\tfacing: %04x %04x %04x\n", br.ang.x, br.ang.y, br.ang.z);
        printf("\tsize: %f %f %f\n", br.sz.x, br.sz.y, br.sz.z);
        // TODO: other fields!
        int face_count = LGBRUSH_GET_FACE_COUNT(br);
        printf("\tface_count: %d\n", face_count);
        for (uint32 j=0, jend=face_count; j<jend; ++j) {
            LGBRLISTFace *face = &br.faces[j];
            printf("\t\t%u: tex %d\n", j, (int)face->tx_id);
            // TODO: other face details!
        }
    }

    arrfree(brlist);
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
    wr_save_to_tagblock(&wr_tagblock2, wr);
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
    LGAnimLightProp80 *props = animlight_prop_load_from_tagblock(
        dbfile_get_tag(dbfile, TAG_PROP_ANIMLIGHT));
    uint32 count = arrlenu32(props);
    dump("%u animlights:\n", count);
    for (uint32 i=0; i<count; ++i) {
        LGAnimLightProp80 *prop = &props[i];
        dump("%4u:\n", i);
        dump("    id: %d\n", prop->header.id);
        dump("    size: %u\n", prop->header.size);
        LGAnimLight80 *animlight = &(props[i].prop);
        LGBaseLight *base = &(animlight->x.base);
        LGAnimLightAnimation *animation = &(animlight->x.animation);
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

    dbfile = dbfile_free(dbfile);
    return 0;
}

int do_dump_cell_lights(int argc, char **argv, struct command *cmd) {
    // Dump all the brush lights, light objects, and anim lights reaching
    // the given cell.
    if (argc!=2) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }
    uint32 cell_id = (uint32)atoi(argv[1]);

    char *in_filename = argv[0];
    DBFile *dbfile = dbfile_load(in_filename);
    DBTagBlock *wr_tagblock = dbfile_get_wr_tagblock(dbfile);
    WorldRep *wr = wr_load_from_tagblock(wr_tagblock);

    // LGBRLISTBrush *brlist = brlist_load_from_tagblock(
    //     dbfile_get_tag(dbfile, TAG_BRLIST));
    //int16 br_id_max = brlist_get_id_max(brlist);
    //arrfree(brlist);

    DBTagBlock *tagblock_position = dbfile_get_tag(dbfile, TAG_PROP_POSITION);
    LGPositionProp *position_prop_array = position_prop_load_from_tagblock(tagblock_position);
    DBTagBlock *tagblock_light = dbfile_get_tag(dbfile, TAG_PROP_LIGHT);
    LGLightProp *light_prop_array = light_prop_load_from_tagblock(tagblock_light);
    // DBTagBlock *tagblock_animlight = dbfile_get_tag(dbfile, TAG_PROP_ANIMLIGHT);
    // LGAnimLightProp80 *animlight_prop_array = animlight_prop_load_from_tagblock(tagblock_animlight);

    // Build some lookup tables.

    typedef struct ObjIdToIndex {
        int32 key;
        int32 value;
    } ObjIdToIndex;

    ObjIdToIndex *positionindex_by_objid = NULL;
    hmdefault(positionindex_by_objid, -1);
    for (uint32 i=0, iend=arrlenu32(position_prop_array); i<iend; ++i) {
        hmput(positionindex_by_objid, position_prop_array[i].header.id, i);
    }

    ObjIdToIndex *lightindex_by_objid = NULL;
    hmdefault(lightindex_by_objid, -1);
    for (uint32 i=0, iend=arrlenu32(light_prop_array); i<iend; ++i) {
        hmput(lightindex_by_objid, light_prop_array[i].header.id, i);
    }

    typedef struct PositionToLightObjId {
        LGVector key;
        int32 value;
    } PositionToLightObjId;

    PositionToLightObjId *lightobjid_by_position = NULL;
    hmdefault(lightobjid_by_position, 0);
    for (uint32 li=0, liend=arrlenu32(light_prop_array); li<liend; ++li) {
        int32 objid = light_prop_array[li].header.id;
        int32 pos_index = hmget(positionindex_by_objid, objid);
        assert(pos_index!=-1);
        LGVector pos = position_prop_array[pos_index].prop.loc.vec;
        hmput(lightobjid_by_position, pos, objid);
    }

    // Now let's figure out which light objects are seeing this cell.

    assert(cell_id<arrlenu32(wr->cell_array));
    WorldRepCell *cell = &wr->cell_array[cell_id];

    printf("CELL %u, %u static lights:\n", cell_id, arrlenu32(cell->light_index_array));
    for (uint32 i=0, iend=arrlenu32(cell->light_index_array); i<iend; ++i) {
        int16 index = cell->light_index_array[i];
        LGWRRGBLight *l = &wr->light_array[index];

        int32 light_objid = hmget(lightobjid_by_position, l->location);
        //assert(light_objid!=0);

        printf("%3d. light_index:%-4d\tobjid:%-4d\tpos:(%f %f %f) bright:(%f %f %f) radius:%f\n",
            i, index, light_objid,
            l->location.x, l->location.y, l->location.z,
            l->bright.x, l->bright.y, l->bright.z,
            l->radius);
    }

    wr_free(&wr);
    dbfile = dbfile_free(dbfile);
    return 0;
}

static float32 fcl_cell_radius;
static LGVector fcl_cell_center;
static LGWRRGBLight *fcl_light_array;

static int fcl_compare_light_indexes(const int16 *a, const int16 *b) {
    // Sunlight index is always first.
    if (*a==0) return -1;
    if (*b==0) return 1;
    // Okay let's calculate distances!
    LGVector loc_a = fcl_light_array[*a].location;
    LGVector loc_b = fcl_light_array[*b].location;
    float dist_sq_a = vdist_sq(loc_a, fcl_cell_center);
    float dist_sq_b = vdist_sq(loc_b, fcl_cell_center);
    if (dist_sq_a<dist_sq_b)
        return -1;
    else if (dist_sq_a>dist_sq_b)
        return 1;
    else
        return 0;
}

static int fcl_light_reaches_cell(int16 a) {
    // Sunlight index always counts.
    if (a==0) return 1;
    // Check the light radius. A zero (=infinite) radius always counts.
    float light_rad = fcl_light_array[a].radius;
    float light_rad_sq = light_rad*light_rad;
    if (light_rad==0.0) return 1;
    // Okay let's calculate distances!
    LGVector loc = fcl_light_array[a].location;
    float dist_sq = vdist_sq(loc, fcl_cell_center);
    float cell_rad_sq = fcl_cell_radius*fcl_cell_radius;
    if (dist_sq>(cell_rad_sq+light_rad_sq))
        return 0;
    return 1;
}

DBFile *dbfile_fixup_cell_lights(DBFile *dbfile)
{
    // Load the worldrep.
    WorldRep *wr = wr_load_from_tagblock(dbfile_get_wr_tagblock(dbfile));

    for (uint32 i=0, iend=arrlenu32(wr->cell_array); i<iend; ++i) {
        WorldRepCell *cell = &wr->cell_array[i];

        // Comparator needs to know the light array and the cell center.
        fcl_cell_radius = cell->header.sphere_radius;
        fcl_light_array = wr->light_array;
        fcl_cell_center = cell->header.sphere_center;

        // BEWARE: light_index_array[0] -- yes, even on disk -- is the number
        //         of indices (excluding itself).

        // Sort light indexes (except sunlight) by increasing distance.
        int16 *arr = cell->light_index_array;
        uint32 count = arrlenu32(arr);
        qsort(arr+1, count-1, sizeof(*arr), fcl_compare_light_indexes);

        // Okay we are gonna filter out any lights that can't reach the cell.
        int16 *r = &arr[1];
        int16 *w = r;
        for (uint32 l=1; l<count; ++l) {
            if (fcl_light_reaches_cell(*r))
                *w++ = *r;
            ++r;
        }
        uint32 new_count = (uint32)(w-arr);
        if (new_count<count) {
            dump("Cell %d: light table reduced from %u to %u\n",
                i, count, new_count);
            arrsetlen(arr, new_count);
            arr[0] = (int16)(new_count-1);
        }
    }

    // Write the output file.

    DBFile *dbfile_out = calloc(1, sizeof(DBFile));
    dbfile_out->version = (LGDBVersion){ 0, 1 };

    // Copy all other tagblocks from the first file into the output.
    for (int i=0, iend=dbfile_tag_count(dbfile); i<iend; ++i) {
        DBTagBlock *src_tagblock = dbfile_tag_at_index(dbfile, i);

        // Skip the tagblocks that we modified.
        if (tag_name_eq_str(src_tagblock->key, TAG_WR)
        || tag_name_eq_str(src_tagblock->key, TAG_WRRGB)
        || tag_name_eq_str(src_tagblock->key, TAG_WREXT))
            continue;

        DBTagBlock dest_tagblock = {0};
        dbtagblock_copy(&dest_tagblock, src_tagblock);
        hmputs(dbfile_out->tagblock_hash, dest_tagblock);
    }

    // Write the modified worldrep to the output.
    {
        DBTagBlock tagblock = {0};
        wr_save_to_tagblock(&tagblock, wr);
        hmputs(dbfile_out->tagblock_hash, tagblock);
    }

    return dbfile_out;
}

int do_fixup_cell_lights(int argc, char **argv, struct command *cmd) {
    // Sort each cell's light_index_array by ascending distance (keeping
    // sunlight at the top if it is present).
    //
    // in.mis -o out.mis
    if (argc!=3
    || strcmp(argv[1], "-o")!=0) {
        abort_format("Usage: %s %s", cmd->s, cmd->args);
    }

    char *in_filename = argv[0];
    char *out_filename = argv[2];

    dump("File: \"%s\"\n", in_filename);
    DBFile *dbfile = dbfile_load(in_filename);
    DBFile *dbfile_out = dbfile_fixup_cell_lights(dbfile);
    dbfile_save(dbfile_out, out_filename);
    dump("Wrote: \"%s\"\n", out_filename);

    // and clean up maybe?

    dbfile = dbfile_free(dbfile);
    dbfile_out = dbfile_free(dbfile_out);
    dump("Ok.\n");
    return 0;
}

struct command all_commands[] = {
    { "help", do_help,                              "[command]",            "List available commands; show help for a command." },
#ifndef TOOL_FIXUP_CELL_LIGHTS
    { "tag_list", do_tag_list,                      "file.mis",             "List all tagblocks." },
    { "tag_dump", do_tag_dump,                      "file.mis tag",         "Dump tagblock to file." },
    { "fam_list", do_fam_list,                      "file.mis",             "List loaded families." },
    { "tex_list", do_tex_list,                      "file.mis",             "List all textures." },
    { "test_worldrep", do_test_worldrep,            "file.mis",             "Test reading and writing (to memory) the worldrep." },
    { "test_write_minimal", do_test_write_minimal,  "input.mis",            "Test writing a minimal dbfile." },
    { "merge", do_merge,                            "top.mis bottom.mis a b c d -o out.mis",  "Merge two worldreps separated by the plane ax+by+cz+d=0." },
#endif
    { "fixup_cell_lights", do_fixup_cell_lights,    "file.mis -o out.mis",  "trim cell lights for perf boost; fix bad object lighting." },
#ifndef TOOL_FIXUP_CELL_LIGHTS
    { "dump_aipath", do_dump_aipath,                "file.mis",             "dump the AIPATH pathfinding db to stdout." },
    { "dump_brlist", do_dump_brlist,                "file.mis",             "dump the BRLIST to stdout." },
    { "dump_bsp", do_dump_bsp,                      "file.mis -o out.dot",  "dump the BSP tree to graphviz .DOT." },
    { "dump_obj", do_dump_obj,                      "file.mis -o out.obj",  "dump the WR and BSP to wavefront .OBJ." },
#endif
    { "dump_wr", do_dump_wr,                        "file.mis",             "dump the WR to stdout." },
    { "dump_animlight", do_dump_animlight,          "file.mis",             "dump animlight table to stdout." },
    { "dump_wrlight", do_dump_wrlight,              "file.mis",             "dump WR light data table." },
    { "dump_cell_lights", do_dump_cell_lights,      "file.mis cellid",      "dump cell light info." },
#ifndef TOOL_FIXUP_CELL_LIGHTS
    { "bsp_sanity_check", do_bsp_sanity_check,      "file.mis",             "do a BSP sanity check." },
#endif
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
    dump("Misdeed version 0.1.0 alpha, by vfig.\n");
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
