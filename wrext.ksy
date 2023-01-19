meta:
  id: wrext
  file-extension: wrext_suffix
  endian: le

seq:
  - id: unknown0
    type: u1
  - id: num_light
    type: s4
  - id: num_dyn
    type: s4
  - id: light_data
    type: lg_wrext_light_t
    repeat: expr
    repeat-expr: num_light
  - id: light_this
    type: lg_wrext_light_t
    repeat: expr
    repeat-expr: 32

  - id: num_animlight_to_cell
    type: s4
  - id: animlight_to_cell
    type: lg_wr_animlighttocell_t
    repeat: expr
    repeat-expr: num_animlight_to_cell

  - id: csg_num_cells
    type: s4
  - id: csg_brfaces
    type: lg_wr_csg_brface_t
    repeat: expr
    # for each cell in csg_num_cells, there are cell[i]->num_render_poly entries!!
    # but whotf knows how to express that in kaitai?
    repeat-expr: 6

  - id: csg_num_brushes
    type: s4
  - id: csg_num_brush_faces
    type: s4
    repeat: expr
    repeat-expr: csg_num_brushes
    # for each brush in csg_num_brushes, there are csg_num_brush_faces[i] entries!!
    # but whotf knows how to express that in kaitai?
  - id: csg_brush_faces
    type: lg_csgplane_t
    repeat: expr
    repeat-expr: 6

  - id: ref_count
    type: s4
    repeat: expr
    repeat-expr: csg_num_brushes

  - id: cruft
    type: u1
    repeat: eos

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

  lg_csgplane_t:
    seq:
      - id: a
        type: f8
      - id: b
        type: f8
      - id: c
        type: f8
      - id: d
        type: f8

  lg_wrext_light_t:
    seq:
      - id: location
        type: lg_vector_t
      - id: direction
        type: lg_vector_t
      - id: bright
        type: lg_vector_t # TODO: ? bright/hue/sat ?
      - id: inner
        type: f4
      - id: outer
        type: f4
      - id: radius
        type: f4

  lg_wr_animlighttocell_t:
    seq:
      - id: cell_index
        type: u2
      - id: pos_in_cell_palette
        type: u1
      - id: pad0
        type: u1

  lg_wr_csg_brface_t:
    seq:
      - id: brface
        type: s4
    instances:
      csg_brush_index:
        value: (brface >> 8)
      face_index:
        value: (brface & 0xff)
