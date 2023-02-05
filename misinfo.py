from struct import calcsize, unpack_from
from zipfile import ZipFile
import os, sys

TARGET_X = 74.00
TARGET_Y = -73.45
TARGET_Z = -50.23
# TARGET_X = 1.07
# TARGET_Y = 3.73
# TARGET_Z = 18.07
PRECISION = 0.1

def read_position_prop(buf, where_stack):
    PROP_HEADER_STRUCT = "<II"      # objid, size
    PROP_POSITION_STRUCT = "<fffhhHHH"   # x, y, z, cell, pad, rx, ry, rz
    cursor = 0
    def read(fmt):
        nonlocal cursor
        result = unpack_from(fmt, buf, cursor)
        cursor += calcsize(fmt)
        return result
    while cursor<len(buf):
        (objid, size) = read(PROP_HEADER_STRUCT)
        assert(size==calcsize(PROP_POSITION_STRUCT))
        (x,y,z,cell,pad,rx,ry,rz) = read(PROP_POSITION_STRUCT)
        if (abs(x-TARGET_X)<PRECISION
        and abs(y-TARGET_Y)<PRECISION
        and abs(z-TARGET_Z)<PRECISION):
            print("In", "/".join(where_stack))
            print(f"\tobject {objid} position: {x},{y},{z}")
    assert(cursor==len(buf))

def read_mis_data(buf, where_stack):
    DBFILE_HEADER_STRUCT = "<III256xI" # toc offset, version_major, version_minor, DEADBEEF
    TOC_HEADER_STRUCT = "<I"    # entry count
    TOC_ENTRY_STRUCT = "<12sII" # name, offset, data_size
    TAGBLOCK_HEADER_STRUCT = "<12sII4x" # name, version_major, version_minor
    cursor = 0
    def read(fmt):
        nonlocal cursor
        result = unpack_from(fmt, buf, cursor)
        cursor += calcsize(fmt)
        return result
    (toc_offset, version_major, version_minor, deadbeef) = read(DBFILE_HEADER_STRUCT)
    assert(deadbeef==0xEFBEADDE)
    cursor = toc_offset
    (toc_count,) = read(TOC_HEADER_STRUCT)
    for i in range(toc_count):
        (tag_name, tag_offset, tag_size) = read(TOC_ENTRY_STRUCT)
        if tag_name==b"P$Position\x00\x00":
            chunk_start = tag_offset+calcsize(TAGBLOCK_HEADER_STRUCT)
            chunk_end = chunk_start+tag_size
            chunk = buf[chunk_start:chunk_end]
            read_position_prop(chunk, where_stack)

def read_mis(filename, where_stack, in_zip=None):
    openfn = in_zip.open if in_zip else open
    mode = "r" if in_zip else "rb"
    with openfn(filename, mode) as f:
        buf = f.read()
    try:
        read_mis_data(buf, where_stack)
    except:
        import traceback
        print("In", "/".join(where_stack))
        traceback.print_exc()

def do_files(filelist, where_stack, in_zip=None):
    for filename in filelist:
        if filename.lower().endswith(".mis"):
            where_stack.append(filename)
            read_mis(filename, where_stack, in_zip=in_zip)
            where_stack.pop()
        elif filename.lower().endswith(".zip"):
            if in_zip==None:
                where_stack.append(filename)
                try:
                    with ZipFile(filename, "r") as zf:
                        do_files(zf.namelist(), where_stack, in_zip=zf)
                except:
                    import traceback
                    print("In", "/".join(where_stack))
                    traceback.print_exc()
                where_stack.pop()

def main(args):
    do_files(os.listdir(), [])

if __name__=="__main__":
    main(sys.argv)
