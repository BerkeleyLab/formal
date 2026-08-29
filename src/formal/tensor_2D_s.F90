! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) tensor_2D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.approximates.) &
    ,operator(.csv.) &
    ,operator(.equalsExpected.) &
    ,operator(.isAtLeast.) &
    ,operator(.isAtMost.) &
    ,operator(.greaterThan.) &
    ,operator(.within.)
  implicit none

contains

  module procedure construct_2D_tensor_from_components
    tensor_2D%points_ = points
    tensor_2D%cells_ = cells
    tensor_2D%x_min_ = x_min
    tensor_2D%x_max_ = x_max
    tensor_2D%order_ = order
  end procedure

  module procedure tensor_2D_consistent

    call_julienne_assert(allocated(self%points_))

    associate(tensor_2D_rank => self%tensor_rank())
      call_julienne_assert( (tensor_2D_rank .isAtLeast. 0) .also. (tensor_2D_rank .isAtMost. max_tensor_rank) )
    end associate

    do concurrent(integer :: i=1:size(self%points_,1), j=1:size(self%points_,2), k=1:size(self%points_,3), m=1:size(self%points_,4))
      call_julienne_assert(allocated(self%points_(i,j,k,m)%values_))
    end do

    call_julienne_assert(.all. ([size(self%cells_), size(self%x_min_), size(self%x_max_)] .equalsExpected. space_dimension))
    call_julienne_assert(.all. (self%x_max_ .greaterThan. self%x_min_))
    self_consistent = .true.
  end procedure

  module procedure tensor_rank

    integer d

    associate(points_shape => shape(self%points_))
      associate(points_dims => size(points_shape))
        do d = 1, points_dims
          if (points_shape(d) /= 1) then
            call_julienne_assert(points_shape(d) .equalsExpected. space_dimension)
          else
            if (d < points_dims) then
              call_julienne_assert(.all. (points_shape(d+1:) .equalsExpected. 1))
            end if
            my_rank = d - 1
            return
          end if
          my_rank = d
        end do
      end associate
    end associate

  end procedure

  module procedure tensor_2D_conformable
    call_julienne_assert( tensor_2D_consistent(self) )
    call_julienne_assert( tensor_2D_consistent(tensor_2D) )
    call_julienne_assert(.all. (shape(self%cells_) .equalsExpected. shape(tensor_2D%cells_)))
    call_julienne_assert(.all. (shape(self%order_) .equalsExpected. shape(tensor_2D%order_)))
    call_julienne_assert(.all. (shape(self%x_min_) .equalsExpected. shape(tensor_2D%x_min_)))
    call_julienne_assert(.all. (shape(self%x_max_) .equalsExpected. shape(tensor_2D%x_max_)))
    call_julienne_assert(.all. (self%cells_ .equalsExpected. tensor_2D%cells_))
    call_julienne_assert(.all. (self%order_ .equalsExpected. tensor_2D%order_))
    call_julienne_assert(.all. (self%x_min_ .approximates.   tensor_2D%x_min_ .within. 0E0))
    call_julienne_assert(.all. (self%x_max_ .approximates.   tensor_2D%x_max_ .within. 0E0))
    conformable = .true.
  end procedure

end submodule tensor_2D_s