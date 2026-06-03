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
  use tensors_1D_m, only : faces_1D, vector_1D_t, gradient_operator_1D_t
  use interpolator_1D_m, only : centers_to_faces_1D_t, faces_to_centers_1D_t

  implicit none

  integer, parameter :: x_dir=1, y_dir=2

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

   define_grid: &
    associate( &
       x => faces_1D(x_min(1), x_max(1), cells(1)) &
      ,y => faces_1D(x_min(2), x_max(2), cells(2)) &
    )
      define_parent_tensor: &
      associate(vector_values => initializer(x,y))
        vector_2D%tensor_2D_t = tensor_2D_t( &
           values = reshape(vector_values, shape=[shape(vector_values),1,1,1]) &
          ,cells = cells, x_min = x_min, x_max = x_max, order = order &
        )
      end associate define_parent_tensor

      define_divergence_operators: &
      block
        integer dir
        vector_2D%divergence_operator_1D_ = &
          [(divergence_operator_1D_t(k=order, dx=((x_max(dir)-x_min(dir))/cells(dir)), cells=cells(dir)), dir=1,space_dimension)]
      end block define_divergence_operators

    end associate define_grid

    call_julienne_assert( vector_2D%consistent() )

  end procedure

  module procedure construct_2D_vector_from_vector_mold

    call_julienne_assert( mold%consistent() )

    define_grid: &
    associate( &
      x => faces_1D(mold%x_min_(1), mold%x_max_(1), mold%cells_(1)) &
     ,y => faces_1D(mold%x_min_(2), mold%x_max_(2), mold%cells_(2)) &
    )
      define_parent_tensor: &
      associate(vector_values => initializer(x,y))
        vector_2D%tensor_2D_t = tensor_2D_t( &
           values = reshape(vector_values, shape=[shape(vector_values),1,1,1]) &
          ,cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_ &
        )
      end associate define_parent_tensor

      define_divergence_operators: &
      block
        integer dir
        vector_2D%divergence_operator_1D_ = [( &
           divergence_operator_1D_t(k=mold%order_, dx=((mold%x_max_(dir)-mold%x_min_(dir))/mold%cells_(dir)), cells=mold%cells_(dir)) &
          ,dir = 1, space_dimension &
        )]
      end block define_divergence_operators

    end associate define_grid

    call_julienne_assert( vector_2D%conformable(mold) )

  end procedure

  module procedure construct_2D_vector_from_scalar_mold
    integer dir

    call_julienne_assert( mold%consistent() )

    define_grid: &
    associate( &
       x => faces_1D(mold%x_min_(1), mold%x_max_(1), mold%cells_(1)) &
      ,y => faces_1D(mold%x_min_(2), mold%x_max_(2), mold%cells_(2)) &
    )
      define_parent_tensor: &
      associate(vector_values => initializer(x,y))
        vector_2D%tensor_2D_t = tensor_2D_t( &
           values = reshape(vector_values, shape=[shape(vector_values),1,1,1]) &
          ,cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_ &
        )
      end associate define_parent_tensor

      define_divergence_operators: &
      block
        integer dir
        vector_2D%divergence_operator_1D_ = [( &
          divergence_operator_1D_t(k=mold%order_, dx=((mold%x_max_(dir)-mold%x_min_(dir))/mold%cells_(dir)), cells=mold%cells_(dir)) &
          ,dir = 1, space_dimension &
        )]
      end block define_divergence_operators

    end associate define_grid

    call_julienne_assert( vector_2D%conformable(mold) )

  end procedure

  module procedure vector_2D_values

    call_julienne_assert(self%consistent())

    vector_values = self%values_(:,:,:,1,1,1)

  end procedure

  module procedure vector_2D_to_file
    type(string_t), allocatable :: lines(:)
    integer i, j, l

    call_julienne_assert(self%consistent())

    associate(x => self%grid(x_dir), y => self%grid(y_dir), header => [string_t("x,y,vector_x,vector_y")])
      associate(num_blank_lines => size(y)-1)
        allocate(lines(size(header) + size(self%values_)/space_dimension + num_blank_lines))
      end associate
      lines(1:size(header)) = header
      l = size(header)
      do j = 1, size(y)
        do i = 1, size(x)
          l = l + 1 
          lines(l) = .csv. string_t([x(i), y(j), self%values_(i,j,1:space_dimension,1,1,1)])
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
    associate(vector_1D => vector_1D_t( &
       constant = 0D0 &
      ,cells = self%cells_(direction) &
      ,x_min = self%x_min_(direction) &
      ,x_max = self%x_max_(direction) &
      ,order = self%order_ &
    ))
      vector_grid_1D = vector_1D%grid()
    end associate
  end procedure

  module procedure vector_2D_divergence

    call_julienne_assert(self%consistent())

    divergence_2D%x_min_ = self%x_min_
    divergence_2D%x_max_ = self%x_max_
    divergence_2D%cells_ = self%cells_
    divergence_2D%order_ = self%order_

    allocate(divergence_2D%values_(self%cells_(x_dir), self%cells_(y_dir), 1, 1, 1, 1))

    divergence_x_term: &
    do concurrent(integer :: j=1:size(divergence_2D%values_,y_dir)) default(none) shared(divergence_2D, self)
      associate(padded_divergence => self%divergence_operator_1D_(x_dir) .x. self%values_(:,j,x_dir,1,1,1))
        divergence_2D%values_(:,j,1,1,1,1) = padded_divergence(2:size(padded_divergence)-1)
      end associate
    end do divergence_x_term

    add_y_term: &
    do concurrent(integer :: i=1:size(divergence_2D%values_,x_dir)) default(none) shared(divergence_2D, self)
      associate(padded_divergence => self%divergence_operator_1D_(y_dir) .x. self%values_(i,:,y_dir,1,1,1))
        divergence_2D%values_(i,:,1,1,1,1) = divergence_2D%values_(i,:,1,1,1,1) + padded_divergence(2:size(padded_divergence)-1)
      end associate
    end do add_y_term

    call_julienne_assert(divergence_2D%conformable(self))

  end procedure

  module procedure vector_2D_dot_vector

    call_julienne_assert(lhs%conformable(rhs))
    call_julienne_assert((.expect. lhs%is_face_centered()) // " lhs%cells_ = " // (.csv. string_t(lhs%cells_)) // ", shape(lhs%values) = " // (.csv. string_t(shape(lhs%values_))))

    allocate(product%values_(lhs%cells_(x_dir)+2,lhs%cells_(y_dir)+2,1,1,1,1))
      ! allocate space for 2D cell centers extended to include boundaries

    construct_interpolator: &
    associate(interpolator => faces_to_centers_1D_t(order=lhs%order_, cells=lhs%cells_, dx=(lhs%x_max_ - lhs%x_min_)/lhs%cells_))

      interpolate_x_faces_to_centers_extended: &
      do concurrent(integer :: j=1:size(lhs%values_,y_dir))
        product%values_(:,j,1,1,1,1) = &
            interpolator(x_dir)%center_values_extended(lhs%values_(:,j,x_dir,1,1,1)) &
          * interpolator(x_dir)%center_values_extended(rhs%values_(:,j,x_dir,1,1,1))
      end do interpolate_x_faces_to_centers_extended

      add_interpolated_y_faces: &
      do concurrent(integer :: i=1:size(lhs%values_,x_dir))
        product%values_(i,:,1,1,1,1) = product%values_(i,:,1,1,1,1) &
          + interpolator(y_dir)%center_values_extended(lhs%values_(i,:,y_dir,1,1,1)) &
          * interpolator(y_dir)%center_values_extended(rhs%values_(i,:,y_dir,1,1,1))
      end do add_interpolated_y_faces

    end associate construct_interpolator

    product = scalar_2D_t( &
       tensor_2D_t(values=product%values_, cells=lhs%cells_, x_min=lhs%x_min_, x_max=lhs%x_max_, order=lhs%order_) &
      ,gradient_operator_1D_t(k = lhs%order_, dx = (lhs%x_max_ - lhs%x_min_)/lhs%cells_, cells = lhs%cells_) &
    )

    call_julienne_assert((.expect. product%is_cell_centers_extended()) // " product%cells_ = " // (.csv. string_t(product%cells_)) // ", shape(product%values) = " // (.csv. string_t(shape(product%values_))))
  end procedure

end submodule vector_2D_s