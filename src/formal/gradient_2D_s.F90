! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) gradient_2D_s
  use julienne_m, only : call_julienne_assert_
  use tensors_1D_m, only : divergence_operator_1D_t
  implicit none

contains

  module procedure construct_2D_gradient_from_components

    call_julienne_assert(size(divergence_operator_1D) .equalsExpected. space_dimension)
    call_julienne_assert(tensor_2D%tensor_2D_consistent())

    gradient_2D%tensor_2D_t = tensor_2D
    gradient_2D%divergence_operator_1D_ = divergence_operator_1D

    call_julienne_assert(vector_2D%consistent())
  end procedure

  module procedure gradient_2D_postmultiply_constant

     call_julienne_assert(lhs%consistent())

     !     points = [points_2D_t(lhs%points_(x_dir,1,1,1)%values_*rhs), points_2D_t(lhs%points_(y_dir,1,1,1)%values_*rhs)] &

     associate(test_dummy => reshape([1],[1,1]))

     product%vector_2D_t = vector_2D_t( &
        !tensor_2D_t( &
        construct_2D_tensor_from_components( &
           points = reshape([points_2D_t(test_dummy*rhs), points_2D_t(test_dummy*rhs)], shape=[space_dimension,1,1,1]) &
          ,cells = lhs%cells_ &
          ,x_min = lhs%x_min_ &
          ,x_max = lhs%x_max_ &
          ,order = lhs%order_ &
        ) &
       ,divergence_operator_1D_t(k=lhs%order_, cells=lhs%cells_, dx=(lhs%x_max_ - lhs%x_min_)/lhs%cells_) &
     )

     end associate

     call_julienne_assert(product%consistent())

  end procedure

  module procedure gradient_2D_premultiply_constant
    product = rhs * lhs
  end procedure

end submodule gradient_2D_s