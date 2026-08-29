3D Vector Calculus API
----------------------

```mermaid

%%{init: { 'theme':'neo',  "class" : {"hideEmptyMembersBox": true} } }%%

classDiagram

vector_3D_t <|-- gradient_3D_t : is a

class scalar_3D_t{
    operator(.grad.) (scalar_3D_t) gradient_3D_t
    operator(+) (scalar_3D_t, scalar_3D_t) scalar_3D_t
    operator(*) (real, scalar_3D_t) scalar_3D_t
    operator(*) (scalar_3D_t, real) scalar_3D_t
    operator(*) (integer, scalar_3D_t) scalar_3D_t
    operator(*) (scalar_3D_t, integer) scalar_3D_t
    values() real(:,:,:)
    grid(component : integer, coordinate : integer) real(:)
    consistent() : logical 
    to_faces(direction : integer) real(:,:,:)
    to_file(name : character(len=:)) file_t
    scalar_3D_t(initializer : scalar_3D_initializer_i, order : integer, cells : integer(:), x_min : real(:), x_max : real(:)) scalar_3D_t
    scalar_3D_t(initializer : scalar_3D_initializer_i, mold : scalar_3D_t) scalar_3D_t
}

class vector_3D_t{
    operator(.div.) (vector_3D_t) : divergence_3D_t
    operator(.dot.) (vector_3D_t, vector_3D_t) : scalar_3D_t
    operator(*)(scalar_3D_t, vector_3D_t) vector_3D_t
    operator(*)(vector_3D_t, scalar_3D_t) vector_3D_t
    grid(component : integer, coordinate : integer) real(:)
    values(direction : integer) real(:,:,:)
    consistent() : logical 
    to_centers_extended() real(:,:,:,:)
    to_file(name : character(len=:)) file_t
    vector_3D_t(initializer : vector_3D_initializer_i, order : integer, cells : integer(:), x_min : real(:), x_max : real(:))
    vector_3D_t(initializer : vector_3D_initializer_i, mold : vector_3D_t)
    vector_3D_t(initializer : vector_3D_initializer_i, mold : scalar_3D_t)
}

class divergence_3D_t{
    assignment(=)(scalar_3D_t, divergence_3D_t)
    divergence_3D_t(initializer : vector_3D_initializer_i, order : integer, cells : integer(:), x_min : real(:), x_max : real(:))
    divergence_3D_t(initializer : vector_3D_initializer_i, mold : vector_3D_t)
}
