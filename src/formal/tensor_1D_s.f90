! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) tensor_1D_s
  use julienne_m, only : call_julienne_assert_
  implicit none
contains

  module procedure is_cell_centered
    is_cell_centered = size(self%values_) == self%cells_
  end procedure

  module procedure is_face_centered
    is_face_centered = size(self%values_) == self%cells_ + 1
  end procedure

  module procedure is_cell_centers_extended
    is_cell_centers_extended = size(self%values_) == self%cells_ + 2
  end procedure

  module procedure construct_1D_tensor_from_components
    tensor_1D%values_ = values
    tensor_1D%x_min_  = x_min
    tensor_1D%x_max_  = x_max
    tensor_1D%cells_  = cells 
    tensor_1D%order_  = order
  end procedure

  module procedure dx
    dx = (self%x_max_ - self%x_min_)/self%cells_
  end procedure

  module procedure cells
    cells = self%cells_
  end procedure

end submodule tensor_1D_s
