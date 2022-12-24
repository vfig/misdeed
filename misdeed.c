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
    if (!(condition)) { \
        fprintf(stderr, "Assertion failed, line %d: %s\n", __LINE__, #condition); \
        fflush(stderr); \
        abort(); \
    }

#define assert_message(condition, message) \
    if (!(condition)) { \
        fprintf(stderr, "Assertion failed, line %d: %s\n", __LINE__, #condition); \
        fprintf(stderr, "\t%s\n", message); \
        fflush(stderr); \
        abort(); \
    }

#define assert_format(condition, fmt, ...) \
    if (!(condition)) { \
        fprintf(stderr, "Assertion failed, line %d: %s\n", __LINE__, #condition); \
        fprintf(stderr, "\t" fmt "\n", __VA_ARGS__); \
        fflush(stderr); \
        abort(); \
    }

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

#define FILENAME_SIZE 1024
typedef struct {
    char value[FILENAME_SIZE];
} FileName;

static FileName *filename_copy_str(FileName *dest, const char *src) {
    strncpy(dest->value, src, FILENAME_SIZE);
    assert(dest->value[FILENAME_SIZE-1]==0);
    return dest;
}

static FileName *filename_append_str(FileName *dest, const char *src) {
    assert(strlen(dest->value)<FILENAME_SIZE-strlen(src));
    strcat(dest->value, src);
    return dest;
}

typedef struct DBTagBlockName {
    char value[12];
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
    strncpy(dest.value, src, (sizeof dest.value)/(sizeof dest.value[0]));
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

    FILE *file = fopen(temp_filename.value, "wb");
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
        memcpy(entry.name, tagblock->key.value, sizeof(entry.name));
        entry.offset = (uint32)ftell(file);
        entry.data_size = tagblock->size;
        arrput(toc_array, entry);

        LGDBChunkHeader chunk_header = {0};
        memcpy(chunk_header.name, tagblock->key.value, sizeof(chunk_header.name));
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
    rename(temp_filename.value, filename);
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

// TODO: cleanup:
#if 0
typedef struct MisChunk {
    char name[12];
    LGDBVersion version;
    uint32 size;
    char *data;
} MisChunk;

typedef struct MisInfo {
    char filename[1024];
    size_t data_size;
    char *data;

    uint32 chunk_count;
    MisChunk chunks[MAX_CHUNKS];
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
    for (uint32 i=0; i<mis->chunk_count; ++i) {
        MisChunk *chunk = &(mis->chunks[i]);
        if (strncmp(chunk->name, tag, 12)==0) {
            *pdata = chunk->data;
            *psize = chunk->size;
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
    strncpy(mis->filename, filename, sizeof(mis->filename)-1);
    mis->filename[sizeof(mis->filename)-1] = 0;

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
    assert(memcmp(header->deadbeef, DEADBEEF, sizeof(DEADBEEF))==0);
    assert(header->version.major==0);
    assert(header->version.minor==1);

    mis->chunk_count = *(uint32 *)(mis->data+header->table_offset);
    LGDBTOCEntry *toc = (LGDBTOCEntry *)(mis->data+header->table_offset+sizeof(uint32));
    for (uint32 i=0; i<mis->chunk_count; ++i) {
        LGDBTOCEntry *entry = &toc[i];
        MisChunk *chunk = &(mis->chunks[i]);
        memcpy(chunk, entry->name, sizeof(chunk->name));
        chunk->size = entry->data_size;
        LGDBChunkHeader *chunk_header = (LGDBChunkHeader *)(mis->data+entry->offset);
        chunk->version = chunk_header->version;
        assert(memcmp(entry->name, chunk_header->name, sizeof(entry->name))==0);
        uint32 offset = entry->offset+sizeof(LGDBChunkHeader);
        chunk->data = (mis->data+offset);
    }

    #if 1
    dump("INDEX\tCHUNK\t\tOFFSET\t\tSIZE\n");
    for (uint32 i=0; i<mis->chunk_count; ++i) {
        MisChunk *chunk = &(mis->chunks[i]);
        uint32 offset = (uint32)(chunk->data-mis->data);
        dump("%d\t%-12s\t0x%08lx\t0x%08lx\n", i, chunk->name, offset, chunk->size);
    }
    #endif

    return mis;
}

#if 0
int write_mis(MisInfo *mis, char *filename) {
    char temp_filename[1024];
    strcpy(temp_filename, filename);
    strcat(temp_filename, "~tmp");
    FILE *file = fopen(temp_filename, "wb");
    size_t n;

    // TODO: version
    LGDBFileHeader header = {0};
    header.version.major = 0;
    header.version.minor = 1;
    memcpy(header.deadbeef, DEADBEEF, sizeof(DEADBEEF));

    n = fwrite(&header, sizeof(header), 1, file); assert(n==1);
    for (int i=0; i<mis->chunk_count; ++i) {
        MisChunk *chunk = &(mis->chunks[i]);

// typedef struct MisChunk {
//     char name[12];
//     LGDBVersion version;
//     uint32 size;
//     uint32 offset;
//     char *data;
// } MisChunk;

// typedef struct MisInfo {
//     char filename[1024];
//     size_t data_size;
//     char *data;

//     uint32 chunk_count;
//     MisChunk chunks[MAX_CHUNKS];
// } MisInfo;

    }

    ...

    uint32 toc_offset = (uint32)ftell(file);
    write_toc();
    fseek(file, 0, SEEK_SET);
    n = fwrite(&toc_offset, sizeof(toc_offset), 1, file); assert(n==1);
    fclose(file);
    rename(temp_filename, filename);
}
#endif

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

#if 0
MisInfo *copy_only_worldrep(MisInfo *in) {
    MisInfo *out = (MisInfo *)malloc(sizeof(MisInfo));
    memset(out, 0, sizeof(MisInfo);

.................

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

    out->data_size = sizeof(LGDBFileHeader);

    char *wrext;
    uint32 wrext_size;
    LGDBVersion wrext_version;
    assert(mis_get_chunk(mis, "WREXT", &wrext, &wrext_size, &wrext_version));

    ++out->chunk_count;
    out->data_size += sizeof(LGDBChunkHeader)+wrext_size;

    out->data_size += sizeof(uint32);
    out->data_size += out->chunk_count*sizeof(LGDBTOCEntry);


    typedef struct MisInfo {
        char filename[1024];
        size_t data_size;
        char *data;

        uint32 chunk_count;
        MisChunk chunks[MAX_CHUNKS];
} MisInfo;

}
#endif

#endif

int main(int argc, char *argv[]) {
    //MisInfo *mis = load_mis("miss20_low.mis");
    //mis_read_wrext(mis);
    //write_mis(mis, "deed.mis", keep_only_worldrep);

    DBFile *dbfile = dbfile_load("e:/dev/thief/T2FM/test_misdeed/part1.mis");
    dbfile_save(dbfile, "e:/dev/thief/T2FM/test_misdeed/out.mis");
    dbfile = dbfile_free(dbfile);
    dump("Ok.");
}
