! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) tensor_2D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.all.) &
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.greaterThan.) &
    ,operator(.within.)
  implicit none

contains

  module procedure construct_2D_tensor_from_components
    tensor_2D%values_ = values
    tensor_2D%cells_ = cells
    tensor_2D%x_min_ = x_min
    tensor_2D%x_max_ = x_max
    tensor_2D%order_ = order
  end procedure

  module procedure tensor_2D_consistent
    call_julienne_assert(allocated(self%values_))
    call_julienne_assert(.all. ([size(self%cells_), size(self%x_min_), size(self%x_max_)] .equalsExpected. space_dimension))
    call_julienne_assert(.all. (self%x_max_ .greaterThan. self%x_min_))
    self_consistent = .true.
  end procedure

  module procedure tensor_2D_conformable
    call_julienne_assert( tensor_2D_consistent(self) )
    call_julienne_assert( tensor_2D_consistent(tensor_2D) )
    call_julienne_assert(.all. (shape(self%cells_) .equalsExpected. shape(tensor_2D%cells_)))
    call_julienne_assert(.all. (shape(self%order_) .equalsExpected. shape(tensor_2D%order_)))
    call_julienne_assert(.all. (shape(self%x_min_) .equalsExpected. shape(tensor_2D%x_min_)))
    call_julienne_assert(.all. (shape(self%x_max_) .equalsExpected. shape(tensor_2D%x_max_)))
    call_julienne_assert(.all. (self%cells_ .equalsExpected. tensor_2D%cells_))
    call_julienne_assert(.all. (self%order_ .equalsExpected. tensor_2D%order_))
    call_julienne_assert(.all. (self%x_min_ .approximates.   tensor_2D%x_min_ .within. 0D0))
    call_julienne_assert(.all. (self%x_max_ .approximates.   tensor_2D%x_max_ .within. 0D0))
    conformable = .true.
  end procedure

end submodule
