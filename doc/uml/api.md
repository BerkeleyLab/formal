User API: 3D Vector Calculus
----------------------------

```mermaid

%%{init: { 'theme':'neo',  "class" : {"hideEmptyMembersBox": true} } }%%

classDiagram

class scalar_3D_t
class vector_3D_t
class gradient_3D_t
class divergence_3D_t

tensor_3D_t <|-- scalar_3D_t : is a
tensor_3D_t <|-- vector_3D_t : is a
tensor_3D_t <|-- divergence_3D_t : is a

vector_3D_t <|-- gradient_3D_t : is a

class scalar_3D_t{
    + operator(.grad.) gradient_3D_t
}

class vector_3D_t{
    + operator(.div.) divergence_3D_t
}

class gradient_3D_t{
   + weights
}
