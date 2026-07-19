! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) scalar_2D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.all.) &
    ,operator(.csv.) &
    ,operator(//) &
    ,operator(.expect.) &
    ,operator(.equalsExpected.) &
    ,operator(.greaterThan.) &
    ,operator(.isAtLeast.) &
    ,string_t
  use tensors_1D_m, only : cell_centers_extended_1D, scalar_1D_t
  use interpolator_1D_m, only : centers_to_faces_1D_t
  implicit none

contains

  module procedure scalar_2D_consistent
    call_julienne_assert(self%tensor_2D_consistent())
    call_julienne_assert(.all. (self%cells_ .isAtLeast. 2*self%order_))
    call_julienne_assert(size(self%gradient_operator_1D_) .equalsExpected. space_dimension)
    self_consistent = .true.
  end procedure

  module procedure construct_2D_scalar_from_components

    call_julienne_assert(size(gradient_operator_1D) .equalsExpected. space_dimension)

    scalar_2D%tensor_2D_t = tensor_2D
    scalar_2D%gradient_operator_1D_ = gradient_operator_1D

    call_julienne_assert(scalar_2D%consistent())

  end procedure

  module procedure scalar_2D_values
    call_julienne_assert(self%consistent())
    values = self%points_(1,1,1,1)%values_(:,:)
  end procedure

  module procedure scalar_2D_grid
    call_julienne_assert(self%consistent())
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

  module procedure scalar_2D_postmultiply_double

    call_julienne_assert(lhs%consistent())

    lhs_x_rhs =  scalar_2D_t(  &
       tensor_2D_t( &
          points = reshape([points_2D_t(lhs%points_(1,1,1,1)%values_ * rhs)], shape = [1,1,1,1]) &
         ,cells  = lhs%cells_  &
         ,x_min  = lhs%x_min_  &
         ,x_max  = lhs%x_max_  &
         ,order  = lhs%order_  &
       ) &
      ,gradient_operator_1D_t( &
          k = lhs%order_       &
         ,dx = (lhs%x_max_ - lhs%x_min_)/lhs%cells_ &
         ,cells  = lhs%cells_  &
    )  )

    call_julienne_assert(lhs_x_rhs%consistent())
  end procedure

  module procedure scalar_2D_postmultiply_integer

    call_julienne_assert(lhs%consistent())

    lhs_x_rhs =  scalar_2D_t(  &
       tensor_2D_t( &
          points = reshape([points_2D_t(lhs%points_(1,1,1,1)%values_ * rhs)], shape = [1,1,1,1]) &
         ,cells  = lhs%cells_  &
         ,x_min  = lhs%x_min_  &
         ,x_max  = lhs%x_max_  &
         ,order  = lhs%order_  &
       ) &
      ,gradient_operator_1D_t( &
          k = lhs%order_       &
         ,dx = (lhs%x_max_ - lhs%x_min_)/lhs%cells_ &
         ,cells  = lhs%cells_  &
    )  )

    call_julienne_assert(lhs_x_rhs%consistent())
  end procedure

  module procedure scalar_2D_premultiply_double
    lhs_x_rhs =  rhs * lhs
  end procedure

  module procedure scalar_2D_premultiply_integer
    lhs_x_rhs =  rhs * lhs
  end procedure

  module procedure scalar_2D_plus_scalar

    call_julienne_assert(rhs%conformable(lhs))

    lhs_plus_rhs =  scalar_2D_t(  &
       tensor_2D_t( &
          points = reshape([points_2D_t(lhs%points_(1,1,1,1)%values_ + rhs%points_(1,1,1,1)%values_)], shape = [1,1,1,1]) &
         ,cells  = lhs%cells_  &
         ,x_min  = lhs%x_min_  &
         ,x_max  = lhs%x_max_  &
         ,order  = lhs%order_  &
       ) &
      ,gradient_operator_1D_t( &
          k = lhs%order_       &
         ,dx = (lhs%x_max_ - lhs%x_min_)/lhs%cells_ &
         ,cells  = lhs%cells_  &
    )  )

    call_julienne_assert(lhs_plus_rhs%consistent())
  end procedure

  module procedure construct_2D_scalar_from_function

    associate( &
       x => cell_centers_extended_1D(x_min(x_dir), x_max(x_dir), cells(x_dir)) &
      ,y => cell_centers_extended_1D(x_min(y_dir), x_max(y_dir), cells(y_dir)) &
    )
      scalar_2D%tensor_2D_t = tensor_2D_t( &
         points = reshape([points_2D_t(initializer(x,y))], shape=[1,1,1,1]), cells = cells , x_min = x_min, x_max = x_max, order = order &
      )
      scalar_2D%gradient_operator_1D_ = gradient_operator_1D_t(k=order, dx=(x_max - x_min)/cells, cells=cells)
    end associate

    call_julienne_assert(scalar_2D%consistent())

  end procedure

  module procedure construct_2D_scalar_from_mold
    call_julienne_assert(mold%consistent())
    scalar_2D = scalar_2D_t(initializer, cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)
    call_julienne_assert(scalar_2D%consistent())
  end procedure

  module procedure scalar_2D_gradient

    integer c, i, j

    call_julienne_assert(self%consistent())

    gradient_2D%x_min_ = self%x_min_
    gradient_2D%x_max_ = self%x_max_
    gradient_2D%cells_ = self%cells_
    gradient_2D%order_ = self%order_

    allocate(gradient_2D%points_(space_dimension,1,1,1))
    allocate(gradient_2D%points_(x_dir,1,1,1)%values_(self%cells_(x_dir)+1, self%cells_(y_dir)+2))
    allocate(gradient_2D%points_(y_dir,1,1,1)%values_(self%cells_(x_dir)+2, self%cells_(y_dir)+1))

    gradient_x_component: &
    do concurrent(integer :: j=1:size(self%points_(1,1,1,1)%values_,y_dir)) default(none) shared(gradient_2D, self)
      gradient_2D%points_(x_dir,1,1,1)%values_(:,j) = self%gradient_operator_1D_(x_dir) .x. self%points_(1,1,1,1)%values_(:,j)
    end do gradient_x_component

    gradient_y_component: &
    do concurrent(integer :: i=1:size(self%points_(1,1,1,1)%values_,x_dir)) default(none) shared(gradient_2D, self)
      gradient_2D%points_(y_dir,1,1,1)%values_(i,:) = self%gradient_operator_1D_(y_dir) .x. self%points_(1,1,1,1)%values_(i,:)
    end do gradient_y_component

    associate(dx => (self%x_max_ - self%x_min_)/self%cells_)
      gradient_2D%divergence_operator_1D_ = divergence_operator_1D_t(self%order_, dx, self%cells_)

     !check_corbino_castillo_eq_17: &
     !associate(p => gradient_1D%weights(), b => [-1D0, [(0D0, c = 1, self%cells_)], 1D0])
     !  call_julienne_assert((.all. (matmul(transpose(self%gradient_operator_1D_%assemble()), p) .approximates. b/dx .within. 2D-3)))
     !end associate check_corbino_castillo_eq_17
    end associate

    call_julienne_assert(gradient_2D%consistent())

  end procedure

  module procedure scalar_2D_assign_divergence

     call_julienne_assert(rhs%consistent())

     if (allocated(lhs%points_)) deallocate(lhs%points_)
     allocate(lhs%points_(1,1,1,1))
     if (allocated(lhs%points_(1,1,1,1)%values_)) deallocate(lhs%points_(1,1,1,1)%values_)
     allocate(lhs%points_(1,1,1,1)%values_(rhs%cells_(x_dir)+2, rhs%cells_(y_dir)+2))

     associate( &
        x_last => size(rhs%points_(1,1,1,1)%values_,x_dir) - 1 &
       ,y_last => size(rhs%points_(1,1,1,1)%values_,y_dir) - 1 &
     )
       lhs%points_(1,1,1,1)%values_(2:x_last-1, 2:y_last-1) = rhs%points_(1,1,1,1)%values_(2:x_last-1, 2:y_last-1) ! internal points
       lhs%points_(1,1,1,1)%values_(1         ,  :        ) = 0D0 ! x_min boundary
       lhs%points_(1,1,1,1)%values_(  x_last  ,  :        ) = 0D0 ! x_max boundary
       lhs%points_(1,1,1,1)%values_( :        , 1         ) = 0D0 ! y_min boundary
       lhs%points_(1,1,1,1)%values_( :        ,   y_last  ) = 0D0 ! y_max boundary
     end associate

     lhs%cells_ = rhs%cells_
     lhs%x_min_ = rhs%x_min_
     lhs%x_max_ = rhs%x_max_
     lhs%order_ = rhs%order_

     call_julienne_assert(lhs%consistent())
     call_julienne_assert(lhs%conformable(rhs))

  end procedure


  module procedure scalar_2D_to_file
    type(string_t), allocatable :: lines(:)
    integer i, j, l, m, n, p, q
    double precision, allocatable :: x(:), y(:)

    call_julienne_assert(self%consistent())

    associate( &
       header => [string_t("x, y, " // name)] &
      ,num_points => sum( [( [( [( [( size(self%points_(m,n,p,q)%values_), m = 1,size(self%points_,1) )] &
                         ,n = 1,size(self%points_,2) )], p = 1, size(self%points_,3) )], q = 1,size(self%points_,4) )] ) &
      ,x => self%grid(x_dir) &
      ,y => self%grid(y_dir) &
    )
      call_julienne_assert(num_points .equalsExpected. size(x)*size(y))

      associate(num_blank_lines => size(y)-1)
        allocate(lines(size(header) +  num_points + num_blank_lines))
      end associate

      lines(1:size(header)) = header
      l = size(header)

      do j = 1, size(y)
        do i = 1, size(x)
          l = l + 1
          lines(l) = .csv. string_t( [x(i), y(j), [( [( [( [( self%points_(m,n,p,q)%values_(i,j), m = 1,size(self%points_,1) )] &
                                    ,n = 1,size(self%points_,2) )], p = 1, size(self%points_,3) )], q = 1,size(self%points_,4) )] ])
        end do
        if (j/=size(y)) then
          l = l + 1
          lines(l) = ""
        end if
      end do
    end associate

    file = file_t(lines)
  end procedure

  module procedure scalar_2D_to_faces

    call_julienne_assert(self%consistent())

    construct_interpolator_array: &
    associate(interpolator => centers_to_faces_1D_t(order=self%order_, cells=self%cells_, dx=(self%x_max_ - self%x_min_)/self%cells_))

      select case(direction)

      case(x_dir)

        allocate(scalars(self%cells_(x_dir)+1, self%cells_(y_dir)+2))

        interpolate_centers_to_x_faces: &
        do concurrent(integer :: j = 1:size(self%points_(1,1,1,1)%values_,y_dir))
          scalars(:,j) = interpolator(x_dir)%face_values(self%points_(1,1,1,1)%values_(:,j))
        end do interpolate_centers_to_x_faces

      case(y_dir)

        allocate(scalars(self%cells_(x_dir)+2, self%cells_(y_dir)+1))

        interpolate_centers_to_y_faces: &
        do concurrent(integer :: i = 1:size(self%points_(1,1,1,1)%values_,x_dir))
          scalars(i,:) = interpolator(y_dir)%face_values(self%points_(1,1,1,1)%values_(i,:))
        end do interpolate_centers_to_y_faces

      case default
        error stop "scalar_2D_to_faces in scalar_2D_s: invalid direction"
      end select

    end associate construct_interpolator_array

  end procedure scalar_2D_to_faces

end submodule scalar_2D_s