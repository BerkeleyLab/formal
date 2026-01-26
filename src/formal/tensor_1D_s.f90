! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(tensors_1D_m) tensor_1D_s
  implicit none
contains

  ! PURPOSE: To construct a new tensor_1D_t object by assigning each argument to a corresponding
  !          corresponding component of the new object.
  ! KEYWORDS: 1D tensor constructor
  ! CONTEXT: Constructors for child types assign this function's result to to the child object's parent component.

  module procedure construct_1D_tensor_from_components
    tensor_1D%values_ = values
    tensor_1D%x_min_  = x_min
    tensor_1D%x_max_  = x_max
    tensor_1D%cells_  = cells 
    tensor_1D%order_  = order
  end procedure

  ! PURPOSE: To provide a uniform cell width along the x-coordinate spatial direction.
  ! KEYWORDS: abcissa, mesh spacing
  ! CONTEXT: Use this function to produce cell widths for uniform 1D meshes.

  module procedure dx
    dx = (self%x_max_ - self%x_min_)/self%cells_
  end procedure


end submodule tensor_1D_s
