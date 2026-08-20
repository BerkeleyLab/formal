! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_3D_m) gradient_3D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.also.) &
    ,operator(.equalsExpected.) &
    ,operator(.isAtLeast.) &
    ,operator(.isAtMost.)
  use tensors_2D_m, only : x_dir, y_dir
  implicit none

contains

  module procedure construct_3D_gradient_from_components

    call_julienne_assert(size(divergence_operator_1D) .equalsExpected. space_dimension)
    call_julienne_assert(tensor_3D%tensor_3D_consistent())

    gradient_3D%tensor_3D_t = tensor_3D
    gradient_3D%divergence_operator_1D_ = divergence_operator_1D

    call_julienne_assert(gradient_3D%consistent())
  end procedure

  module procedure gradient_3D_postmultiply_constant

     call_julienne_assert(lhs%consistent())

     product%vector_3D_t = vector_3D_t( &
        tensor_3D_t( &
           points = reshape( &
              source = [ &
                 points_3D_t(lhs%points_(x_dir,1,1,1)%values_*rhs) &
                ,points_3D_t(lhs%points_(y_dir,1,1,1)%values_*rhs) &
                ,points_3D_t(lhs%points_(z_dir,1,1,1)%values_*rhs) &
              ] &
             ,shape = [space_dimension,1,1,1] &
           ) &
          ,cells = lhs%cells_ &
          ,x_min = lhs%x_min_ &
          ,x_max = lhs%x_max_ &
          ,order = lhs%order_ &
        ) &
       ,divergence_operator_1D_t(k=lhs%order_, cells=lhs%cells_, dx=(lhs%x_max_ - lhs%x_min_)/lhs%cells_) &
     )

     call_julienne_assert(product%consistent())

  end procedure

  module procedure gradient_3D_premultiply_constant
    product = rhs * lhs
  end procedure

end submodule gradient_3D_s