module formal_m 
  !! This module contains all public Formal entities. For descriptions of the
  !! public procedures bound to the derived types below, see the interface
  !! bodies in the corresponding module (e.g., tensors_1D_m).  Please see the
  !! programs in this `example` subdirectory for demonstrations of how to use
  !! the entities in this module.

  use tensors_1D_m, only : &
     scalar_1D_t     & ! discrete 1D scalar field derived type
    ,vector_1D_t     & ! discrete 1D vector field derived type
    ,gradient_1D_t   & ! result of `.grad. s` for a scalar_1D_t s
    ,divergence_1D_t & ! result of `.div. v` for a vector_1D_t v
    ,laplacian_1D_t  & ! result of `.laplacian. s` for a scalar_1D_t s
    ,scalar_1D_initializer_i  & ! scalar_1D_t initializer abstract interface
    ,vector_1D_initializer_i    ! vector_1D_t initializar abstract interface

  use mimetic_operators_1D_m, only : &
     gradient_operator_1D_t & ! matrix operator defining a 1D mimetic gradient
    ,divergence_operator_1D_t ! matrix operator defining a 1D mimetic divergence

  implicit none

end module formal_m
