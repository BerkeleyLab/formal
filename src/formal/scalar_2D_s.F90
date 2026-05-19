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
  use tensors_1D_m, only : cell_centers_extended_1D, scalar_1D_t
  use julienne_m, only : string_t, operator(.csv.)
  implicit none

contains

  module procedure scalar_2D_values
    scalar_values = self%values_(:,:,1,1,1,1)
  end procedure

  module procedure scalar_2D_grid
    associate(scalar_1D => scalar_1D_t( &
       constant = 0D0 &
      ,cells = self%cells_(direction) &
      ,x_min = self%x_min_(direction) &
      ,x_max = self%x_max_(direction) &
      ,order = self%order_ &
    ))
      scalar_grid_1D = scalar_1D%grid()
    end associate
  end procedure

  module procedure construct_2D_scalar_from_function

    call_julienne_assert(.all. ([size(cells), size(x_min), size(x_max)] .equalsExpected. space_dimension))
    call_julienne_assert(.all. (x_max .greaterThan. x_min))
    call_julienne_assert(.all. (cells .isAtLeast. 2*order))

    associate(x => cell_centers_extended_1D(x_min(1), x_max(1), cells(1)), y => cell_centers_extended_1D(x_min(2), x_max(2), cells(2)))
      scalar_2D%tensor_2D_t = tensor_2D_t( &
         values = reshape(initializer(x,y), shape=[size(x),size(y),1,1,1,1]) &
        ,cells = cells , x_min = x_min, x_max = x_max, order = order &
      )
      scalar_2D%gradient_operator_1D_ = gradient_operator_1D_t(k=order, dx=(x_max - x_min)/cells, cells=cells)
    end associate
  end procedure

  module procedure construct_2D_scalar_from_mold
    scalar_2D = scalar_2D_t(initializer, cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)
  end procedure

  module procedure scalar_2D_gradient

    integer c

    gradient_2D%x_min_ = self%x_min_
    gradient_2D%x_max_ = self%x_max_
    gradient_2D%cells_ = self%cells_
    gradient_2D%order_ = self%order_

    allocate(gradient_2D%values_(self%cells_(1)+1, self%cells_(2)+1, space_dimension, 1, 1, 1))

#if HAVE_DO_CONCURRENT_TYPE_SPEC_SUPPORT && HAVE_LOCALITY_SPECIFIER_SUPPORT
    gradient_x_component: &
    do concurrent(integer :: j=1:size(gradient_2D%values_,2)) default(none) shared(gradient_2D, self)
      gradient_2D%values_(:,j,1,1,1,1) = self%gradient_operator_1D_(1) .x. self%values_(:,j,1,1,1,1)
    end do gradient_x_component

    gradient_y_component: &
    do concurrent(integer :: i=1:size(gradient_2D%values_,1)) default(none) shared(gradient_2D, self)
      gradient_2D%values_(i,:,2,1,1,1) = self%gradient_operator_1D_(2) .x. self%values_(i,:,1,1,1,1)
    end do gradient_y_component
#else
    block
    integer i, j
    gradient_x_component: &
    do concurrent(j=1:size(gradient_2D%values_,2))
      gradient_2D%values_(:,j,1,1,1,1) = self%gradient_operator_1D_(1) .x. self%values_(:,j,1,1,1,1)
    end do gradient_x_component

    gradient_y_component: &
    do concurrent(i=1:size(gradient_2D%values_,1))
      gradient_2D%values_(i,:,2,1,1,1) = self%gradient_operator_1D_(2) .x. self%values_(i,:,1,1,1,1)
    end do gradient_y_component
    end block
#endif

    associate(dx => (self%x_max_ - self%x_min_)/self%cells_)
      gradient_2D%divergence_operator_1D_ = divergence_operator_1D_t(self%order_, dx, self%cells_)
     !check_corbino_castillo_eq_17: &
     !associate(p => gradient_1D%weights(), b => [-1D0, [(0D0, c = 1, self%cells_)], 1D0])
     !  call_julienne_assert((.all. (matmul(transpose(self%gradient_operator_1D_%assemble()), p) .approximates. b/dx .within. 2D-3)))
     !end associate check_corbino_castillo_eq_17
    end associate

  end procedure

  module procedure scalar_2D_to_file
    type(string_t), allocatable :: lines(:)
    integer i, j, l

    associate(x => self%grid(1), y => self%grid(2), header => [string_t("x,y,scalar")])
      associate(num_blank_lines => size(y)-1)
        allocate(lines(size(header) + size(self%values_) + num_blank_lines))
      end associate
      lines(1:size(header)) = header
      l = size(header)
      do j = 1, size(y)
        do i = 1, size(x)
          l = l + 1
          lines(l) = .csv. string_t([x(i), y(j), self%values_(i,j,1,1,1,1)])
        end do
        if (j/=size(y)) then
          l = l + 1
          lines(l) = ""
        end if
      end do
    end associate

    file = file_t(lines)
  end procedure

end submodule scalar_2D_s