! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) scalar_2D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.all.) &
    ,operator(.equalsExpected.) &
    ,operator(.greaterThan.) &
    ,operator(.isAtLeast.)
  use tensors_1D_m, only : cell_centers_extended_1D
  implicit none

contains

  module procedure construct_2D_scalar_from_function

    call_julienne_assert(.all. ([size(cells), size(x_min), size(x_max)] .equalsExpected. space_dimension))
    call_julienne_assert(.all. (x_max .greaterThan. x_min))
    call_julienne_assert(.all. (cells .isAtLeast. 2*order))

    associate( &
       x1 => cell_centers_extended_1D(x_min(1), x_max(1), cells(1)) &
      ,x2 => cell_centers_extended_1D(x_min(2), x_max(2), cells(2)) &
    )
      allocate(scalar_2D%values_(cells(1)+2, cells(2)+2,1))

      do concurrent(integer :: i=1:cells(1)+2, j=1:cells(2)+2) default(none) shared(scalar_2D, x1, x2)
        scalar_2D%values_(i,j,1) = initializer(x1(i), x2(j))
      end do
    end associate

    scalar_2D%order_ = order
    scalar_2D%x_min_ = x_min
    scalar_2D%x_max_ = x_max
    scalar_2D%cells_ = cells
    scalar_2D%gradient_operator_1D_ = gradient_operator_1D_t(k=order, dx=(x_max - x_min)/cells, cells=cells)
  end procedure

  module procedure grad

    integer c

    associate(dx => (self%x_max_ - self%x_min_)/self%cells_)

!     gradient_2D%tensor_1D_t = tensor_1D_t(self%gradient_operator_1D_ .x. self%values_, self%x_min_, self%x_max_, cells=self%cells_, order=self%order_)

      gradient_2D%divergence_operator_1D_ = divergence_operator_1D_t(self%order_, dx, self%cells_)

      !check_corbino_castillo_eq_17: &
      !!associate(p => gradient_1D%weights(), b => [-1D0, [(0D0, c = 1, self%cells_)], 1D0])
      !!  call_julienne_assert((.all. (matmul(transpose(self%gradient_operator_1D_%assemble()), p) .approximates. b/dx .within. 2D-3)))
      !end associate check_corbino_castillo_eq_17
    end associate

  end procedure

end submodule scalar_2D_s
