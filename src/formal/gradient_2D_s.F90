! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) gradient_2D_s
  use julienne_m, only : call_julienne_assert_
  use tensors_1D_m, only : divergence_operator_1D_t
  implicit none

contains

  module procedure gradient_2D_postmultiply_constant

     call_julienne_assert(lhs%consistent())

     product%vector_2D_t = vector_2D_t( &
        tensor_2D_t(values = lhs%values_ * rhs, cells = lhs%cells_, x_min = lhs%x_min_, x_max = lhs%x_max_, order = lhs%order_) &
       ,divergence_operator_1D_t(k=lhs%order_, cells=lhs%cells_, dx=(lhs%x_max_ - lhs%x_min_)/lhs%cells_) &
     )

     call_julienne_assert(product%consistent())

  end procedure

  module procedure gradient_2D_premultiply_constant
    product = rhs * lhs
  end procedure

end submodule gradient_2D_s