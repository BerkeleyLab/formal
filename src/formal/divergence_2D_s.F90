! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) divergence_2D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.all.) &
    ,operator(.equalsExpected.) &
    ,operator(.greaterThan.) &
    ,operator(.isAtLeast.)
  use tensors_1D_m, only : divergence_1D_t, cell_centers_1D
  use julienne_m, only : string_t, operator(.csv.)
  implicit none

contains

  module procedure divergence_2D_values

    call_julienne_assert(self%consistent())

    divergence_values = self%values_(:,:,1,1,1,1)

  end procedure

  module procedure divergence_2D_grid

    call_julienne_assert(self%consistent())

    construct_prototype: &
    associate(divergence_1D => divergence_1D_t( &
       constant = 0D0 &
      ,cells = self%cells_(direction) &
      ,x_min = self%x_min_(direction) &
      ,x_max = self%x_max_(direction) &
      ,order = self%order_ &
    ))
      divergence_grid_1D = divergence_1D%grid()
    end associate construct_prototype
  end procedure

  module procedure construct_2D_divergence_from_function

    define_grid: &
    associate( &
       x => cell_centers_1D(x_min(1), x_max(1), cells(1)) &
      ,y => cell_centers_1D(x_min(2), x_max(2), cells(2)) &
    )
      divergence_2D%tensor_2D_t = tensor_2D_t( &
         values = reshape(initializer(x,y), shape=[size(x),size(y),1,1,1,1]) &
        ,cells = cells , x_min = x_min, x_max = x_max, order = order &
      )
    end associate define_grid

    call_julienne_assert(divergence_2D%consistent())

  end procedure

  module procedure construct_2D_divergence_from_vector_mold

    call_julienne_assert(mold%consistent())

    divergence_2D = divergence_2D_t(initializer, cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)

    call_julienne_assert(divergence_2D%consistent())
    call_julienne_assert(divergence_2D%conformable(mold))

  end procedure

  module procedure divergence_2D_to_file
    type(string_t), allocatable :: lines(:)
    integer i, j, l

    call_julienne_assert(self%consistent())

    associate(x => self%grid(1), y => self%grid(2), header => [string_t("x,y,divergence")])
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

end submodule divergence_2D_s