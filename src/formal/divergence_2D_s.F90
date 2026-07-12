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
    divergences = self%points_(1,1,1,1)%values_(:,:)
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
      allocate(divergence_2D%tensor_2D_t%points_(1,1,1,1))
      divergence_2D%tensor_2D_t%points_(1,1,1,1)%values_ = initializer(x,y)
      divergence_2D%tensor_2D_t%cells_ = cells
      divergence_2D%tensor_2D_t%x_min_ = x_min
      divergence_2D%tensor_2D_t%x_max_ = x_max
      divergence_2D%tensor_2D_t%order_ = order
    end associate define_grid

    call_julienne_assert(divergence_2D%consistent())

  end procedure

  module procedure divergence_2D_minus_divergence
    call_julienne_assert(lhs%conformable(rhs))

    difference%tensor_2D_t =  tensor_2D_t( &
       points = reshape([points_2D_t(lhs%points_(1,1,1,1)%values_ - rhs%points_(1,1,1,1)%values_)], shape = [1,1,1,1]) &
      ,cells  = lhs%cells_ &
      ,x_min  = lhs%x_min_ &
      ,x_max  = lhs%x_max_ &
      ,order  = lhs%order_ &
    )
    call_julienne_assert(difference%consistent())

  end procedure

  module procedure construct_2D_divergence_from_vector_mold

    call_julienne_assert(mold%consistent())

    divergence_2D = divergence_2D_t(initializer, cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)

    call_julienne_assert(divergence_2D%consistent())
    call_julienne_assert(divergence_2D%conformable(mold))

  end procedure

  module procedure divergence_2D_minus_scalar

     call_julienne_assert(lhs%conformable(rhs))

     allocate(difference%points_(1,1,1,1))
     allocate(difference%points_(1,1,1,1)%values_(rhs%cells_(x_dir)+2, rhs%cells_(y_dir)+2))

     associate( &
        x_last => size(rhs%points_(1,1,1,1)%values_,x_dir) - 1 &
       ,y_last => size(rhs%points_(1,1,1,1)%values_,y_dir) - 1 &
     )
       difference%points_(1,1,1,1)%values_(2:x_last-1, 2:y_last-1) = & ! internal points
              lhs%points_(1,1,1,1)%values_ &
            - rhs%points_(1,1,1,1)%values_(2:x_last-1, 2:y_last-1)
       difference%points_(1,1,1,1)%values_(1,:)      = - rhs%points_(1,1,1,1)%values_(1,:)      ! x_min boundary
       difference%points_(1,1,1,1)%values_(x_last,:) = - rhs%points_(1,1,1,1)%values_(x_last,:) ! x_max boundary
       difference%points_(1,1,1,1)%values_(:,1)      = - rhs%points_(1,1,1,1)%values_(:,1)      ! y_min boundary
       difference%points_(1,1,1,1)%values_(:,y_last) = - rhs%points_(1,1,1,1)%values_(:,y_last) ! y_max boundary
     end associate

     difference%cells_ = lhs%cells_
     difference%x_min_ = lhs%x_min_
     difference%x_max_ = lhs%x_max_
     difference%order_ = lhs%order_

     call_julienne_assert(difference%consistent())

  end procedure

  module procedure divergence_2D_postmultiply_constant
    lhs_x_rhs%tensor_2D_t =  tensor_2D_t( &
       points = reshape([points_2D_t(lhs%points_(1,1,1,1)%values_ * rhs)], shape = [1,1,1,1]) &
      ,cells  = lhs%cells_ &
      ,x_min  = lhs%x_min_ &
      ,x_max  = lhs%x_max_ &
      ,order  = lhs%order_ &
    )
  end procedure

  module procedure divergence_2D_premultiply_constant
    lhs_x_rhs = rhs * lhs
  end procedure

  module procedure divergence_2D_to_file
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
  end procedure divergence_2D_to_file

end submodule divergence_2D_s