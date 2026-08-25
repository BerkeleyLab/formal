3D Vector Calculus API
----------------------

```mermaid

%%{init: { 'theme':'neo',  "class" : {"hideEmptyMembersBox": true} } }%%

classDiagram

vector_3D_t <|-- gradient_3D_t : is a

class scalar_3D_t{
    operator(.grad.) (scalar_3D_t) gradient_3D_t
    operator(+) (scalar_3D_t, scalar_3D_t) scalar_3D_t
    operator(*) (double precision, scalar_3D_t) scalar_3D_t
    operator(*) (scalar_3D_t, double precision) scalar_3D_t
    operator(*) (integer, scalar_3D_t) scalar_3D_t
    operator(*) (scalar_3D_t, integer) scalar_3D_t
    values() double precision(:,:,:)
    grid(component : integer, coordinate : integer) double precision(:)
    consistent() : logical 
    to_faces(direction : integer) double precision(:,:,:)
    to_file(name : character(len=:)) file_t
    scalar_3D_t(initializer : scalar_3D_initializer_i, order : integer, cells : integer(:), x_min : double precision(:), x_max : double precision(:)) scalar_3D_t
    scalar_3D_t(initializer : scalar_3D_initializer_i, mold : scalar_3D_t) scalar_3D_t
}

class vector_3D_t{
    operator(.div.) (vector_3D_t) : divergence_3D_t
    operator(.dot.) (vector_3D_t, vector_3D_t) : scalar_3D_t
    operator(*)(scalar_3D_t, vector_3D_t) vector_3D_t
    operator(*)(vector_3D_t, scalar_3D_t) vector_3D_t
    grid(component : integer, coordinate : integer) double precision(:)
    values(direction : integer) double precision(:,:,:)
    consistent() : logical 
    to_centers_extended() double precision(:,:,:,:)
    to_file(name : character(len=:)) file_t
    vector_3D_t(initializer : vector_3D_initializer_i, order : integer, cells : integer(:), x_min : double precision(:), x_max : double precision(:))
    vector_3D_t(initializer : vector_3D_initializer_i, mold : vector_3D_t)
    vector_3D_t(initializer : vector_3D_initializer_i, mold : scalar_3D_t)
}

class divergence_3D_t{
    assignment(=)(scalar_3D_t, divergence_3D_t)
    divergence_3D_t(initializer : vector_3D_initializer_i, order : integer, cells : integer(:), x_min : double precision(:), x_max : double precision(:))
    divergence_3D_t(initializer : vector_3D_initializer_i, mold : vector_3D_t)
}
