! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module formal_m 
  !! This module contains all public Formal entities. For descriptions of the
  !! public procedures bound to the derived types below, see the interface
  !! bodies in the corresponding module (e.g., tensors_1D_m).  Please see the
  !! programs in the `example` subdirectory for demonstrations of how to use
  !! the entities in this module.

  use tensors_1D_m, only : &
     scalar_1D_t     & ! discrete 1D scalar field derived type
    ,vector_1D_t     & ! discrete 1D vector field derived type
    ,gradient_1D_t   & ! result of `.grad. s` for a scalar_1D_t s
    ,divergence_1D_t & ! result of `.div. v` for a vector_1D_t v
    ,laplacian_1D_t  & ! result of `.laplacian. s` for a scalar_1D_t s
    ,scalar_1D_initializer_i & ! scalar_1D_t initializer abstract interface
    ,vector_1D_initializer_i & ! vector_1D_t initializar abstract interface
    ,d_dx   & ! scalar_1D_t spatial derivative
    ,d2_dx2   ! scalar_1D_t spatial derivative

  use tensors_2D_m, only : &
     scalar_2D_t & ! discrete 2D scalar field derived type
    ,vector_2D_t & ! discrete 2D vector field derived type
    ,divergence_2D_t & ! discrete 2D divergence field derived type
    ,gradient_2D_t & ! result of `.grad. s` for a scalar_2D_t s
    ,scalar_2D_initializer_i & ! scalar_2D_t initializer abstract interface
    ,vector_2D_initializer_i & ! vector_2D_t initializar abstract interface
    ,divergence_2D_initializer_i ! divergence_2D_t initializar abstract interface

  use tensors_3D_m, only : &
     scalar_3D_t & ! discrete 3D scalar field derived type
    ,vector_3D_t & ! discrete 3D vector field derived type
    ,divergence_3D_t & ! discrete 3D divergence field derived type
    ,gradient_3D_t & ! result of `.grad. s` for a scalar_3D_t s
    ,scalar_3D_initializer_i & ! scalar_3D_t initializer abstract interface
    ,vector_3D_initializer_i & ! vector_3D_t initializar abstract interface
    ,divergence_3D_initializer_i ! divergence_2D_t initializar abstract interface

  use differential_operators_1D_m, only : &
     gradient_operator_1D_t & ! matrix operator defining a 1D mimetic gradient
    ,divergence_operator_1D_t ! matrix operator defining a 1D mimetic divergence

  use interpolator_1D_m, only : &
     centers_to_faces_1D_t & ! 1D mimetic interpolator producing cell-centered values from face-centered values
    ,faces_to_centers_1D_t   ! 1D mimetic interpolator producing face-centered values from cell-centered values

  implicit none

end module formal_m
