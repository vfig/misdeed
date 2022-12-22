#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int8_t int8;
typedef int16_t int16;
typedef int32_t int32;
typedef int64_t int64;
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;
typedef uint64_t uint64;
typedef float float32;

/** .MIS data types **/

#pragma pack(push, 1)

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
    uint32 data_size;
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
    int32 portal_vertex_list;
    uint16 num_vlist;
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

#pragma pack(pop)

/** Caution: misdeeds ahead! **/

#define assert(condition) \
    if (!(condition)) { \
        fprintf(stderr, "Assertion failed, line %d: %s\n", __LINE__, #condition); \
        abort(); \
    }

void dump(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
}

typedef struct MisInfo {
    char filename[1024];
    size_t data_size;
    char *data;

    uint32 toc_count;
    LGDBTOCEntry *toc;
} MisInfo;

#define MAX_CELLS 32678UL           // Imposed by Dromed
#define MAX_VERTICES (256UL*1024UL) // Imposed by Dromed
#define MAX_FACES (256UL*1024UL)    // Rough guess
#define MAX_FACE_INDICES 32UL       // Imposed by Dromed
#define MAX_INDICES (MAX_FACE_INDICES*MAX_FACES)

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

typedef struct WorldRep {
    uint32 cell_count;
    WorldRepCell cells[MAX_CELLS];
} WorldRep;

int mis_get_chunk(MisInfo *mis, char *tag, char **pdata, uint32 *psize, LGDBVersion *pversion) {
    for (uint32 i=0; i<mis->toc_count; ++i) {
        LGDBTOCEntry *toc = &mis->toc[i];
        if (strncmp(toc->name, tag, 12)==0) {
            uint32 offset = toc->offset;
            LGDBChunkHeader *chunk = (LGDBChunkHeader *)(mis->data+offset);
            assert(strncmp(chunk->name, tag, 12)==0);
            offset += sizeof(LGDBChunkHeader);
            *pdata = (mis->data+offset);
            *psize = toc->data_size;
            *pversion = chunk->version;
            return 1;
        }
    }
    *pdata = 0;
    *psize = 0;
    memset(pversion, 0, sizeof(*pversion));
    return 0;
}

MisInfo *load_mis(char *filename) {
    MisInfo *mis = (MisInfo *)malloc(sizeof(MisInfo));
    char *end = strncpy(mis->filename, filename, sizeof(mis->filename)-1);
    *end = 0;

    // Load the whole file into memory:
    FILE *file = fopen(filename, "rb");
    fseek(file, 0, SEEK_END);
    mis->data_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    mis->data = malloc(mis->data_size);
    assert(mis->data);
    size_t n = fread(mis->data, mis->data_size, 1, file);
    assert(n==1);
    fclose(file);

    // Read the TOC:
    LGDBFileHeader *header = (LGDBFileHeader *)(mis->data);
    char DEADBEEF[4] = {0xDE,0xAD,0xBE,0xEF};
    assert(memcmp(header->deadbeef, DEADBEEF, sizeof(DEADBEEF))==0);
    assert(header->version.major==0);
    assert(header->version.minor==1);
    mis->toc_count = *(uint32 *)(mis->data+header->table_offset);
    mis->toc = (LGDBTOCEntry *)(mis->data+header->table_offset+sizeof(uint32));
    #if 0
    dump("INDEX\tCHUNK\t\tOFFSET\t\tSIZE\n");
    for (uint32 i=0; i<mis->toc_count; ++i) {
        LGDBTOCEntry *toc = &mis->toc[i];
        dump("%d\t%-12s\t0x%08lx\t0x%08lx\n", i, toc->name, toc->offset, toc->data_size);
    }
    #endif

    return mis;
}

float32 _wrext_lightmap_scale_factor(int32 lightmap_scale) {
    int32 value = lightmap_scale;
    int32 sign = (value>=0 ? 1 : -1);
    if (value==0) value = 1;
    int32 exponent = (int32)log2f((float)abs(value));
    return powf(2.0f, (float)(sign*exponent));
}

uint32 bit_count(uint32 v) {
    int c = 0;
    while (v) {
        if (v&1) ++c;
        v >>= 1;
    }
    return c;
}

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

void mis_read_wrext(MisInfo *mis) {
    char *wrext;
    uint32 wrext_size;
    LGDBVersion wrext_version;
    assert(mis_get_chunk(mis, "WREXT", &wrext, &wrext_size, &wrext_version));
    assert(wrext_version.major==0 && wrext_version.minor==30);
    dump("WREXT found.\n");
    char *p = wrext;

    LGWREXTHeader *header = (LGWREXTHeader *)wrext; p += sizeof(LGWREXTHeader);

    dump("WREXT chunk:\n");
    dump("  version: %d.%d\n", wrext_version.major, wrext_version.minor);
    dump("  unknown0: 0x%08x\n", header->unknown0);
    dump("  unknown1: 0x%08x\n", header->unknown1);
    dump("  unknown2: 0x%08x\n", header->unknown2);
    dump("  lightmap_format: %ld\n", header->lightmap_format);
    dump("  lightmap_scale: 0x%08x\n", header->lightmap_scale);
    dump("  data_size: %lu\n", header->data_size);
    dump("  cell_count: %lu\n", header->cell_count);

    // WR lightmap data is 8bpp
    // WRRGB is 16bpp (xB5G5R5)
    // WREXT can be 8, 16, or 32bpp.
    int lightmap_bpp;
    int lightmap_2x_modulation = 0;
    float lightmap_scale = 1.0;
    switch (wrext_version.minor) {
    case 23: lightmap_bpp = 6; break;
    case 24: lightmap_bpp = 16; break;
    case 30:
        lightmap_scale = _wrext_lightmap_scale_factor(header->lightmap_scale);
        switch (header->lightmap_format) {
        case 0: lightmap_bpp = 16; break;
        case 1: lightmap_bpp = 32; break;
        case 2:
            lightmap_bpp = 32;
            lightmap_2x_modulation = 1;
            break;
        default: abort();
        }
        break;
    default: abort();
    }

    dump("\tCELL\tVERTS\tPOLYS\tLIGHTMAP_SIZE\n");
    for (uint32 cell_index=0; cell_index<header->cell_count; ++cell_index) {
        WorldRepCell cell;
        p = wrext_read_cell(p, &cell, lightmap_bpp);
        dump("\t%lu\t%lu\t%lu\t%lu\n",
            cell_index,
            cell.header->num_vertices,
            cell.header->num_polys,
            cell.lightmaps_size);
    }
}

int main(int argc, char *argv[]) {
    MisInfo *mis = load_mis("miss20_low.mis");
    mis_read_wrext(mis);
}
