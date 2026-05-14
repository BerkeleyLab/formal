! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) vector_2D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.all.) &
    ,operator(.equalsExpected.) &
    ,operator(.greaterThan.) &
    ,operator(.isAtLeast.)
  use tensors_1D_m, only : faces_1D
  implicit none

contains

  module procedure construct_2D_vector_from_function

    call_julienne_assert(.all. ([size(cells), size(x_min), size(x_max)] .equalsExpected. space_dimension))
    call_julienne_assert(.all. (x_max .greaterThan. x_min))
    call_julienne_assert(.all. (cells .isAtLeast. 2*order))

    associate( &
       x1 => faces_1D(x_min(1), x_max(1), cells(1)) &
      ,x2 => faces_1D(x_min(2), x_max(2), cells(2)) &
    )
      allocate(vector_2D%values_(cells(1)+1, cells(2)+1, space_dimension))

      do concurrent(integer :: i=1:cells(1)+1, j=1:cells(2)+1, dir=1:space_dimension) default(none) shared(vector_2D, x1, x2)
        vector_2D%values_(i,j,:) = initializer(x1(i), x2(j))
      end do
    end associate

    vector_2D%order_ = order
    vector_2D%x_min_ = x_min
    vector_2D%x_max_ = x_max
    vector_2D%cells_ = cells
  end procedure

end submodule vector_2D_s
