! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) vector_2D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(//) &
    ,operator(.all.) &
    ,operator(.approximates.) &
    ,operator(.csv.) &
    ,operator(.equalsExpected.) &
    ,operator(.expect.) &
    ,operator(.greaterThan.) &
    ,operator(.isAtLeast.) &
    ,operator(.within.) &
    ,string_t
  use tensors_1D_m, only : cell_centers_1D, faces_1D, vector_1D_t, gradient_operator_1D_t
  use interpolator_1D_m, only : centers_to_faces_1D_t, faces_to_centers_1D_t

  implicit none

contains

  module procedure construct_2D_vector_from_components

    call_julienne_assert(size(divergence_operator_1D) .equalsExpected. space_dimension)
    call_julienne_assert(tensor_2D%tensor_2D_consistent())

    vector_2D%tensor_2D_t = tensor_2D
    vector_2D%divergence_operator_1D_ = divergence_operator_1D

    call_julienne_assert(vector_2D%consistent())
  end procedure

  module procedure vector_2D_consistent
    call_julienne_assert(self%tensor_2D_consistent())
    call_julienne_assert(.all. (self%cells_ .isAtLeast. 2*self%order_ + 1))
    call_julienne_assert(size(self%divergence_operator_1D_) .equalsExpected. space_dimension)
    self_consistent = .true.
  end procedure

  module procedure vector_2D_conformable_vector
    call_julienne_assert(vector_2D_consistent(self))
    call_julienne_assert(vector_2D_consistent(vector_2D))
    call_julienne_assert(self%tensor_2D_conformable(vector_2D))
    conformable = .true.
  end procedure

  module procedure vector_2D_conformable_scalar
    call_julienne_assert(vector_2D_consistent(self))
    call_julienne_assert(scalar_2D_consistent(scalar_2D))
    call_julienne_assert(self%tensor_2D_conformable(scalar_2D))
    conformable = .true.
  end procedure

  module procedure construct_2D_vector_from_function

   associate( &
       x_centers => cell_centers_1D(x_min(x_dir), x_max(x_dir), cells(x_dir)) &
      ,y_centers => cell_centers_1D(x_min(y_dir), x_max(y_dir), cells(y_dir)) &
      ,x_faces   =>        faces_1D(x_min(x_dir), x_max(x_dir), cells(x_dir)) &
      ,y_faces   =>        faces_1D(x_min(y_dir), x_max(y_dir), cells(y_dir)) &
    )
      associate(vectors_x => initializer(x_faces,y_centers), vectors_y => initializer(x_faces,y_centers))
        vector_2D%tensor_2D_t = tensor_2D_t( &
          points = reshape(  &
            source = [points_2D_t(vectors_x(:,:,x_dir)), points_2D_t(vectors_x(:,:,y_dir))] &
           ,shape  = [space_dimension,1,1,1] &
          ) &
          ,cells = cells , x_min = x_min, x_max = x_max, order = order &
        )
        block
          integer dir
            vector_2D%divergence_operator_1D_ = [( &
               divergence_operator_1D_t(k = order, dx = ((x_max(dir)-x_min(dir))/cells(dir)), cells = cells(dir)) &
              ,dir = 1, space_dimension &
            )]
        end block
      end associate
    end associate

    call_julienne_assert( vector_2D%consistent() )

  end procedure

  module procedure construct_2D_vector_from_vector_mold
    call_julienne_assert( mold%consistent() )
    vector_2D = vector_2D_t(initializer, cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)
    call_julienne_assert( vector_2D%conformable(mold) )
  end procedure

  module procedure construct_2D_vector_from_scalar_mold
    call_julienne_assert( mold%consistent() )
    vector_2D = vector_2D_t(initializer, cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)
    call_julienne_assert( vector_2D%conformable(mold) )
  end procedure

  module procedure vector_2D_to_file
    type(string_t), allocatable :: lines(:)
    integer i, j, l

    call_julienne_assert(self%consistent())

    associate( &
       header => [string_t("x,y,vector_x,vector_y")] &
      ,x => cell_centers_1D(self%x_min_(x_dir), self%x_max_(x_dir), self%cells_(x_dir)) &
      ,y => cell_centers_1D(self%x_min_(y_dir), self%x_max_(y_dir), self%cells_(y_dir)) &
      ,vectors => self%at_cell_centers() &
    )
      associate(num_blank_lines => size(y)-1)
        allocate(lines(size(header) + size(vectors)/space_dimension + num_blank_lines))
      end associate
      lines(1:size(header)) = header
      l = size(header)
      do j = 1, size(y)
        do i = 1, size(x)
          l = l + 1 
          lines(l) = .csv. string_t([x(i), y(j), vectors(i,j,:)])
        end do
        if (j/=size(y)) then
          l = l + 1 
          lines(l) = ""
        end if
      end do
    end associate

    file = file_t(lines)
  end procedure

  module procedure vector_2D_grid

    select case(description(coordinate, component))
    case("x coordinate of x components")
      vector_grid_1D =        faces_1D(x_min = self%x_min_(x_dir), x_max = self%x_max_(x_dir), cells = self%cells_(x_dir))
    case("y coordinate of x components")
      vector_grid_1D = cell_centers_1D(x_min = self%x_min_(y_dir), x_max = self%x_max_(y_dir), cells = self%cells_(x_dir))
    case("x coordinate of y components")
      vector_grid_1D = cell_centers_1D(x_min = self%x_min_(x_dir), x_max = self%x_max_(x_dir), cells = self%cells_(x_dir))
    case("y coordinate of y components")
      vector_grid_1D =        faces_1D(x_min = self%x_min_(y_dir), x_max = self%x_max_(y_dir), cells = self%cells_(x_dir))
    case default
      error stop "vector_2D_grid: invalid coordinate or component"
    end select

  contains   

    pure function description(coordinate, component) result(point_cloud)
      integer, intent(in) :: component, coordinate
      character(len=:), allocatable :: point_cloud
      !point_cloud = merge("x" // ,"y", coordinate==x_dir) // " coordinate of " // merge("x" // ,"y", component==y_dir) // " components"
      point_cloud = "x"
    end function

  end procedure
     
  module procedure vector_2D_divergence

    call_julienne_assert(self%consistent())

    divergence_2D%x_min_ = self%x_min_
    divergence_2D%x_max_ = self%x_max_
    divergence_2D%cells_ = self%cells_
    divergence_2D%order_ = self%order_

    allocate(divergence_2D%points_(1, 1, 1, 1))
    allocate(divergence_2D%points_(1, 1, 1, 1)%values_(self%cells_(x_dir), self%cells_(y_dir)))

    associate(v_x => divergence_2D%points_(x_dir,1,1,1)%values_, v_y => divergence_2D%points_(y_dir,1,1,1)%values_)

      divergence_x_term: &
      do concurrent(integer :: j = 1:size(v_x,y_dir)) default(none) shared(divergence_2D, self, v_x)
        associate(padded_divergence => self%divergence_operator_1D_(x_dir) .x. v_x(:,j))
          divergence_2D%points_(1,1,1,1)%values_(:,j) = padded_divergence(2:size(padded_divergence)-1)
        end associate
      end do divergence_x_term

      add_y_term: &
      do concurrent(integer :: i = 1:size(v_y,x_dir)) default(none) shared(divergence_2D, self, v_y)
        associate(padded_divergence => self%divergence_operator_1D_(y_dir) .x. v_y(i,:))
          divergence_2D%points_(1,1,1,1)%values_(i,:) = &
            divergence_2D%points_(1,1,1,1)%values_(i,:) + padded_divergence(2:size(padded_divergence)-1)
        end associate
      end do add_y_term

    end associate

    call_julienne_assert(divergence_2D%conformable(self))

  end procedure

  module procedure vector_2D_at_cell_centers

    double precision, allocatable :: x_components(:,:), y_components(:,:)

    call_julienne_assert(self%consistent())

    ! values at cell centers extended to include boundaries only along the direction/component to be interpolated
    allocate(x_components(self%cells_(x_dir)+2, self%cells_(y_dir)  ))
    allocate(y_components(self%cells_(x_dir)  , self%cells_(y_dir)+2))

    construct_interpolator_array: &
    associate(interpolator => faces_to_centers_1D_t(order=self%order_, cells=self%cells_, dx=(self%x_max_ - self%x_min_)/self%cells_))

      interpolate_x_faces_to_centers_extended: &
      do concurrent(integer :: j = 1:size(self%points_(x_dir,1,1,1)%values_,y_dir))
        x_components(:,j) = interpolator(x_dir)%center_values_extended(self%points_(x_dir,1,1,1)%values_(:,j))
      end do interpolate_x_faces_to_centers_extended

      interpolate_y_faces_to_centers_extended: &
      do concurrent(integer :: i = 1:size(self%points_(y_dir,1,1,1)%values_,x_dir))
        y_components(i,:) = interpolator(y_dir)%center_values_extended(self%points_(y_dir,1,1,1)%values_(i,:))
      end do interpolate_y_faces_to_centers_extended

    end associate construct_interpolator_array

    ! trim boundaries because edges lack tangential components and corners lack all commponents:
    allocate(vectors(self%cells_(x_dir), self%cells_(y_dir), space_dimension))
    vectors(:,:,x_dir) = x_components(2:self%cells_(x_dir)+1,  :                    )
    vectors(:,:,y_dir) = y_components( :                    , 2:self%cells_(y_dir)+1)

  end procedure

  module procedure vector_2D_dot_vector

    call_julienne_assert(lhs%conformable(rhs))

    associate( &
      lhs_centers => lhs%at_cell_centers() &
     ,rhs_centers => rhs%at_cell_centers() &
    )
      call_julienne_assert(.all. (shape(lhs_centers) .equalsExpected. shape(rhs_centers)))

      product%tensor_2D_t = tensor_2D_t( &
         points = reshape([points_2D_t(lhs_centers(:,:,x_dir) * rhs_centers(:,:,x_dir) + lhs_centers(:,:,y_dir) * rhs_centers(:,:,y_dir))], [1,1,1,1]) &
        ,cells = lhs%cells_ &
        ,x_min = lhs%x_min_ &
        ,x_max = lhs%x_max_ &
        ,order = lhs%order_ &
      )
    end associate

  end procedure

end submodule vector_2D_s