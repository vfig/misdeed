meta:
  id: mis_newdark
  file-extension: mis
  endian: le
  bit-endian: le

seq:
  - id: header
    type: lg_dbfileheader_t

types:
  dummy:
    doc: This type is intentionally left blank.

  lg_vector_t:
    seq:
      - id: x
        type: f4
      - id: y
        type: f4
      - id: z
        type: f4

  lg_version_t:
    seq:
      - id: major
        type: u4
      - id: minor
        type: u4

  lg_dbfileheader_t:
    seq:
      - id: table_offset
        type: u4
      - id: version
        type: lg_version_t
      - id: pad
        size: 256
      - id: deadbeef
        size: 4
        contents: [0xde, 0xad, 0xbe, 0xef]

  lg_dbtocentry_t: # 20 bytes
    seq:
      - id: name
        type: str
        size: 12
        terminator: 0
        encoding: ASCII
      - id: offset
        type: u4
      - id: data_size
        type: u4
    instances:
      chunk_header:
        io: _root._io
        pos: offset
        type: lg_dbchunkheader_t
      chunk:
        io: _root._io
        pos: offset + 24 # sizeof(lg_dbchunkheader_t)
        size: data_size
        type:
          switch-on: chunk_header.name
          cases:
            '"WREXT"': lg_wrext_t
            _: dummy

  lgdbtocentries_array_t:
    seq:
      - id: items
        type: lg_dbtocentry_t
        repeat: eos

  lg_dbtocheader_t:
    seq:
      - id: entry_count
        type: u4
      - id: entries
        type: lgdbtocentries_array_t
        size: entry_count * 20 # sizeof(lg_dbtocentry_t)

  lg_dbchunkheader_t: # 24 bytes
    seq:
      - id: name
        type: str
        size: 12
        terminator: 0
        encoding: ASCII
      - id: version
        type: lg_version_t
      - id: pad
        type: u4

  lg_wrext_t:
    seq:
      - id: header
        type: lg_wrextheader_t
      - id: cells
        type: lg_wrcell_t
        repeat: expr
        repeat-expr: 1 # header.cell_count

  lg_wrextheader_t:
    seq:
      - id: unknown0
        type: u4
      - id: unknown1
        type: u4
      - id: unknown2
        type: u4
      - id: lightmap_format
        type: u4  # 0: 16 bit; 1: 32 bit; 2: 32 bit 2x
      - id: lightmap_scale
        type: s4  # 0: 1x; 2: 2x; 4: 4x; -2: 0.5x; -4: 0.25x
                  # non power of two values may be stored in
                  # here; just ignore all but the highest bit,
                  # and use the sign bit to determine if it
                  # is a multiply or a divide.
      - id: data_size
        type: u4
      - id: cell_count
        type: u4

  lg_wrcellheader_t:
    seq:
      - id: num_vertices
        type: u1
      - id: num_polys
        type: u1
      - id: num_render_polys
        type: u1
      - id: num_portal_polys
        type: u1
      - id: num_planes
        type: u1
      - id: medium
        type: u1
      - id: flags
        type: u1
      - id: portal_vertex_list
        type: s4
      - id: num_vlist
        type: u2
      - id: num_anim_lights
        type: u1
      - id: motion_index
        type: u1
      - id: sphere_center
        type: lg_vector_t
      - id: sphere_radius
        type: f4

  lg_wrcell_t:
    seq:
      - id: header
        type: lg_wrcellheader_t
      - id: vertices
        type: lg_vector_t
        repeat: expr
        repeat-expr: header.num_vertices
      - id: polys
        type: lg_wrpoly_t
        repeat: expr
        repeat-expr: header.num_polys
      - id: render_polys
        type: lg_wrextrenderpoly_t
        repeat: expr
        repeat-expr: header.num_render_polys
      - id: index_count
        type: u4
      - id: index_list
        type: u1
        repeat: expr
        repeat-expr: index_count
      - id: plane_list
        type: lg_wrplane_t
        repeat: expr
        repeat-expr: header.num_planes
      - id: anim_lights
        type: u2
        repeat: expr
        repeat-expr: header.num_anim_lights
      - id: light_list
        type: lg_wrlightmapinfo_t
        repeat: expr
        repeat-expr: header.num_render_polys
      - id: lightmaps
        type: u4 # actually, hmm, how to do this?
      # TODO: the rest

  lg_wrpoly_t:
    seq:
      - id: flags
        type: u1
      - id: num_vertices
        type: u1
      - id: planeid
        type: u1
      - id: clut_id
        type: u1
      - id: destination
        type: u2
      - id: motion_index
        type: u1
      - id: padding
        type: u1

  lg_wrrenderpoly_t:
    seq:
      - id: tex_u
        type: lg_vector_t
      - id: tex_v
        type: lg_vector_t
      - id: u_base
        type: u2
      - id: v_base
        type: u2
      - id: texture_id
        type: u1
      - id: texture_anchor
        type: u1
      - id: cached_surface
        type: u2
      - id: texture_mag
        type: f4
      - id: center
        type: lg_vector_t

  lg_wrextrenderpoly_t:
    seq:
      - id: tex_u
        type: lg_vector_t
      - id: tex_v
        type: lg_vector_t
      - id: u_base
        type: f4  # Changed in WREXT
      - id: v_base
        type: f4  # Changed in WREXT
      - id: texture_id
        type: u2  # Changed in WREXT (texture_anchor removed)
      - id: cached_surface
        type: u2  ## TODO: jk has this as texture_anchor!
      - id: texture_mag
        type: f4
      - id: center
        type: lg_vector_t

  lg_wrplane_t:
    seq:
      - id: normal
        type: lg_vector_t
      - id: distance
        type: f4

  lg_wrlightmapinfo_t:
    seq:
      - id: u_base
        type: s2
      - id: v_base
        type: s2
      - id: padded_width
        type: s2
      - id: height
        type: u1
      - id: width
        type: u1
      - id: data_ptr
        type: u4  # Always zero on disk
      - id: dynamic_light_ptr
        type: u4  # Always zero on disk
      - id: anim_light_bitmask
        type: u4

# # WR lightmap data is uint8; WRRGB is uint16 (xB5G5R5)
# LGWRLightmap8Bit = uint8
# LGWRRGBLightmap16Bit = uint16
# LGWRRGBLightmap32Bit = uint32

instances:
  toc:
    pos: header.table_offset
    type: lg_dbtocheader_t
