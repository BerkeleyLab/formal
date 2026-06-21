! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) scalar_product_2D_s
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

  module procedure scalar_product_2D_values

    call_julienne_assert(self%consistent())

    divergences = self%points_(1,1,1,1)%values_(:,:)

  end procedure

  module procedure scalar_product_2D_grid

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

  module procedure construct_2D_scalar_product_from_function

    define_grid: &
    associate( &
       x => cell_centers_1D(x_min(1), x_max(1), cells(1)) &
      ,y => cell_centers_1D(x_min(2), x_max(2), cells(2)) &
    )
      allocate(scalar_product_2D%tensor_2D_t%points_(1,1,1,1))
      scalar_product_2D%tensor_2D_t%points_(1,1,1,1)%values_ = initializer(x,y)
      scalar_product_2D%tensor_2D_t%cells_ = cells
      scalar_product_2D%tensor_2D_t%x_min_ = x_min
      scalar_product_2D%tensor_2D_t%x_max_ = x_max
      scalar_product_2D%tensor_2D_t%order_ = order
    end associate define_grid

    call_julienne_assert(scalar_product_2D%consistent())

  end procedure

  module procedure construct_2D_scalar_product_from_vector_mold

    call_julienne_assert(mold%consistent())

    scalar_product_2D = scalar_product_2D_t(initializer, cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)

    call_julienne_assert(scalar_product_2D%consistent())
    call_julienne_assert(scalar_product_2D%conformable(mold))

  end procedure

  module procedure scalar_product_2D_postmultiply_constant

     call_julienne_assert(lhs%consistent())

     allocate(product%tensor_2D_t%points_(1,1,1,1))
     product%points_(1,1,1,1)%values_ = lhs%points_(1,1,1,1)%values_ * rhs
     product%cells_ = lhs%cells_
     product%x_min_ = lhs%x_min_
     product%x_max_ = lhs%x_max_
     product%order_ = lhs%order_

     call_julienne_assert(product%consistent())

  end procedure

  module procedure scalar_product_2D_premultiply_constant
    product = rhs * lhs
  end procedure

  module procedure scalar_product_2D_to_file
    type(string_t), allocatable :: lines(:)
    integer i, j, l
    double precision, allocatable :: x(:), y(:)

    call_julienne_assert(self%consistent())

    associate( &
       header => [string_t("x, y, " // name)] &
      ,x => self%grid(x_dir) &
      ,y => self%grid(y_dir) &
    )
      associate(num_points => size(x)*size(y))

        associate(num_blank_lines => size(y)-1)
          allocate(lines(size(header) +  num_points + num_blank_lines))
        end associate

        associate(scalars => self%values())

          call_julienne_assert(.all. (shape(scalars) .equalsExpected. [size(x), size(y)]))

          lines(1:size(header)) = header
          l = size(header)

          do j = 1, size(y)
            do i = 1, size(x)
              l = l + 1
              lines(l) = .csv. string_t([x(i), y(j), scalars(i,j)])
            end do
            if (j/=size(y)) then
              l = l + 1
              lines(l) = ""
            end if
          end do

        end associate
      end associate
    end associate

    file = file_t(lines)
  end procedure scalar_product_2D_to_file

end submodule scalar_product_2D_s