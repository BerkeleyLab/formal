! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) gradient_1D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.isAtLeast.) &
    ,operator(.within.)
  implicit none

  real, parameter :: real_equivalence = 1E-5

contains

  module procedure gradient_1D_weights

    integer face
    real, allocatable :: skin(:)

    select case(self%order_)
    case(2)
      skin = [3/8E0, 9/8E0]
    case(4)
      skin = [227/641E0, 941/766E0, 811/903E0, 1373/1348E0, 1401/1400E0, 36343/36342E0, 943491/943490E0]
    case default
      error stop "unsupported order"
    end select

    associate(depth => size(skin))
      call_julienne_assert(self%cells_ .isAtLeast. 2*depth)
      weights = [skin, [(1E0, face = 1, self%cells_ + 1 - 2*depth)], skin(depth:1:-1) ]
    end associate

    call_julienne_assert(size(weights) .equalsExpected. self%cells_ + 1)

  end procedure

  module procedure dot
    call_julienne_assert(size(gradient_1D%values_) .equalsExpected. size(vector_1D%values_))
    call_julienne_assert(gradient_1D%order_ .equalsExpected. vector_1D%order_)
    call_julienne_assert(gradient_1D%cells_ .equalsExpected. vector_1D%cells_)
    call_julienne_assert(gradient_1D%x_min_ .approximates.    vector_1D%x_min_ .within. real_equivalence)
    call_julienne_assert(gradient_1D%x_max_ .approximates.    vector_1D%x_max_ .within. real_equivalence)

    vector_dot_gradient_1D%tensor_1D_t = tensor_1D_t(   &
       values = gradient_1D%values_ * vector_1D%values_ &
      ,x_min  = gradient_1D%x_min_ &
      ,x_max  = gradient_1D%x_max_ &
      ,cells  = gradient_1D%cells_ &
      ,order  = gradient_1D%order_ &
    )
    vector_dot_gradient_1D%weights_ = gradient_1D%weights()
  end procedure

end submodule gradient_1D_s
