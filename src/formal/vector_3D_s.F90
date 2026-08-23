! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_3D_m) vector_3D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(//) &
    ,operator(.all.) &
    ,operator(.csv.) &
    ,operator(.equalsExpected.) &
    ,operator(.expect.) &
    ,operator(.greaterThan.) &
    ,operator(.isAtLeast.) &
    ,string_t
  use tensors_1D_m, only : cell_centers_extended_1D, faces_1D
  use tensors_2D_m, only : x_dir, y_dir
  use interpolator_1D_m, only : faces_to_centers_1D_t

  implicit none

contains

  module procedure construct_3D_vector_from_components

    call_julienne_assert(size(divergence_operator_1D) .equalsExpected. space_dimension)
    call_julienne_assert(tensor_3D%tensor_3D_consistent())

    vector_3D%tensor_3D_t = tensor_3D
    vector_3D%divergence_operator_1D_ = divergence_operator_1D

    call_julienne_assert(vector_3D%consistent())
  end procedure

  module procedure vector_3D_consistent
    call_julienne_assert(self%tensor_3D_consistent())
    call_julienne_assert(.all. (self%cells_ .isAtLeast. 2*self%order_ + 1))
    call_julienne_assert(size(self%divergence_operator_1D_) .equalsExpected. space_dimension)
    self_consistent = .true.
  end procedure

  module procedure vector_3D_values
    vector_values = self%points_(direction,1,1,1)%values_
  end procedure

  module procedure construct_3D_vector_from_function

   associate( &
       x_ccext => cell_centers_extended_1D(x_min(x_dir), x_max(x_dir), cells(x_dir)) &
      ,y_ccext => cell_centers_extended_1D(x_min(y_dir), x_max(y_dir), cells(y_dir)) &
      ,z_ccext => cell_centers_extended_1D(x_min(z_dir), x_max(z_dir), cells(z_dir)) &
      ,x_faces =>        faces_1D(x_min(x_dir), x_max(x_dir), cells(x_dir)) &
      ,y_faces =>        faces_1D(x_min(y_dir), x_max(y_dir), cells(y_dir)) &
      ,z_faces =>        faces_1D(x_min(z_dir), x_max(z_dir), cells(z_dir)) &
    )
      associate( &
         vectors_x => initializer(x_faces,y_ccext,z_ccext) &
        ,vectors_y => initializer(x_ccext,y_faces,z_ccext) &
        ,vectors_z => initializer(x_ccext,y_ccext,z_faces) &
      )
        vector_3D%tensor_3D_t = tensor_3D_t( &
          points = reshape(  &
            source = [points_3D_t(vectors_x(:,:,:,x_dir)), points_3D_t(vectors_y(:,:,:,y_dir)), points_3D_t(vectors_z(:,:,:,z_dir))] &
           ,shape  = [space_dimension,1,1,1] &
          ) &
          ,cells = cells , x_min = x_min, x_max = x_max, order = order &
        )
        block
          integer dir
            vector_3D%divergence_operator_1D_ = [( &
               divergence_operator_1D_t(k = order, dx = ((x_max(dir)-x_min(dir))/cells(dir)), cells = cells(dir)) &
              ,dir = 1, space_dimension &
            )]
        end block
      end associate
    end associate

    call_julienne_assert( vector_3D%consistent() )

  end procedure

  module procedure construct_3D_vector_from_vector_mold
    call_julienne_assert( mold%consistent() )
    vector_3D = vector_3D_t(initializer, cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)
    call_julienne_assert( vector_3D%conformable(mold) )
  end procedure

  module procedure construct_3D_vector_from_scalar_mold
    call_julienne_assert( mold%consistent() )
    vector_3D = vector_3D_t(initializer, cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)
    call_julienne_assert( vector_3D%conformable(mold) )
  end procedure

  module procedure vector_3D_grid

    select case(description(coordinate, component))
    case("x coordinate of x components")
      vector_grid_1D =        faces_1D(x_min = self%x_min_(x_dir), x_max = self%x_max_(x_dir), cells = self%cells_(x_dir))
    case("y coordinate of x components")
      vector_grid_1D = cell_centers_extended_1D(x_min = self%x_min_(y_dir), x_max = self%x_max_(y_dir), cells = self%cells_(y_dir))
    case("x coordinate of y components")
      vector_grid_1D = cell_centers_extended_1D(x_min = self%x_min_(x_dir), x_max = self%x_max_(x_dir), cells = self%cells_(x_dir))
    case("y coordinate of y components")
      vector_grid_1D =        faces_1D(x_min = self%x_min_(y_dir), x_max = self%x_max_(y_dir), cells = self%cells_(y_dir))
    case default
      error stop "vector_3D_grid: invalid coordinate or component"
    end select

  contains   

    pure function description(coordinate, component) result(point_cloud)
      integer, intent(in) :: coordinate, component
      character(len=:), allocatable :: point_cloud
      point_cloud = merge("x" ,"y", coordinate==x_dir) // " coordinate of " // merge("x" ,"y", component==y_dir) // " components"
    end function

  end procedure
     
  module procedure vector_3D_divergence

    double precision, dimension(self%cells_(x_dir)+2, self%cells_(y_dir)+2, self%cells_(z_dir)+2) :: div_x_term, div_y_term, div_z_term

    call_julienne_assert(self%consistent())

    divergence_3D%x_min_ = self%x_min_
    divergence_3D%x_max_ = self%x_max_
    divergence_3D%cells_ = self%cells_
    divergence_3D%order_ = self%order_

    allocate(divergence_3D%points_(1, 1, 1, 1))
    allocate(divergence_3D%points_(1, 1, 1, 1)%values_(self%cells_(x_dir), self%cells_(y_dir), self%cells_(z_dir)))

    associate(v_x => self%points_(x_dir,1,1,1)%values_)
      do concurrent(integer :: j = 1:size(v_x,y_dir), k = 1:size(v_x,z_dir)) default(none) shared(divergence_3D, self, v_x, div_x_term)
        div_x_term(:,j,k) = self%divergence_operator_1D_(x_dir) .x. v_x(:,j,k)
      end do
    end associate

    associate(v_y => self%points_(y_dir,1,1,1)%values_)
      do concurrent(integer :: i = 1:size(v_y,x_dir), k = 1:size(v_y,z_dir)) default(none) shared(divergence_3D, self, v_y, div_y_term)
        div_y_term(i,:,k) = self%divergence_operator_1D_(y_dir) .x. v_y(i,:,k)
      end do
    end associate

    associate(v_z => self%points_(z_dir,1,1,1)%values_)
      do concurrent(integer :: i = 1:size(v_z,x_dir), j = 1:size(v_z,y_dir)) default(none) shared(divergence_3D, self, v_z, div_z_term)
        div_z_term(i,j,:) = self%divergence_operator_1D_(z_dir) .x. v_z(i,j,:)
      end do
    end associate

    divergence_3D%points_(1, 1, 1, 1)%values_ = &
        div_x_term(2:size(div_x_term,x_dir)-1, 2:size(div_x_term,y_dir)-1, 2:size(div_x_term,z_dir)-1) &
      + div_y_term(2:size(div_y_term,x_dir)-1, 2:size(div_y_term,y_dir)-1, 2:size(div_y_term,z_dir)-1) &
      + div_z_term(2:size(div_z_term,x_dir)-1, 2:size(div_z_term,y_dir)-1, 2:size(div_z_term,z_dir)-1) 

    call_julienne_assert(divergence_3D%conformable(self))

  end procedure

  module procedure vector_3D_to_centers_extended

    call_julienne_assert(self%consistent())

    allocate(vectors(self%cells_(x_dir)+2, self%cells_(y_dir)+2, self%cells_(z_dir)+2, space_dimension)) ! values at cell centers extended to include boundaries

    construct_interpolator_array: &
    associate(interpolator => faces_to_centers_1D_t(order=self%order_, cells=self%cells_, dx=(self%x_max_ - self%x_min_)/self%cells_))

      interpolate_x_faces_to_centers_extended: &
      do concurrent(integer :: j = 1:size(self%points_(x_dir,1,1,1)%values_,y_dir), k = 1:size(self%points_(x_dir,1,1,1)%values_,z_dir))
        vectors(:,j,k,x_dir) = interpolator(x_dir)%center_values_extended(self%points_(x_dir,1,1,1)%values_(:,j,k))
      end do interpolate_x_faces_to_centers_extended

      interpolate_y_faces_to_centers_extended: &
      do concurrent(integer :: i = 1:size(self%points_(y_dir,1,1,1)%values_,x_dir), k = 1:size(self%points_(y_dir,1,1,1)%values_,z_dir))
        vectors(i,:,k,y_dir) = interpolator(y_dir)%center_values_extended(self%points_(y_dir,1,1,1)%values_(i,:,k))
      end do interpolate_y_faces_to_centers_extended

      interpolate_z_faces_to_centers_extended: &
      do concurrent(integer :: i = 1:size(self%points_(z_dir,1,1,1)%values_,x_dir), j = 1:size(self%points_(z_dir,1,1,1)%values_,y_dir))
        vectors(i,j,:,z_dir) = interpolator(z_dir)%center_values_extended(self%points_(z_dir,1,1,1)%values_(i,j,:))
      end do interpolate_z_faces_to_centers_extended

    end associate construct_interpolator_array

  end procedure

  module procedure vector_3D_postmultiply_scalar_3D

    call_julienne_assert(vector_3D%conformable(scalar_3D))

    associate( &
       scalar_x => scalar_3D%to_faces(x_dir) &
      ,scalar_y => scalar_3D%to_faces(y_dir) &
      ,scalar_z => scalar_3D%to_faces(z_dir) &
    )
      call_julienne_assert(.all. (shape(scalar_x) .equalsExpected. shape(vector_3D%points_(x_dir,1,1,1)%values_)))
      call_julienne_assert(.all. (shape(scalar_y) .equalsExpected. shape(vector_3D%points_(y_dir,1,1,1)%values_)))
      call_julienne_assert(.all. (shape(scalar_z) .equalsExpected. shape(vector_3D%points_(z_dir,1,1,1)%values_)))

      vector_x_scalar = vector_3D_t( &
         tensor_3D_t( &
            points = reshape( &
               source = [ points_3D_t(vector_3D%points_(x_dir,1,1,1)%values_ * scalar_x) &
                         ,points_3D_t(vector_3D%points_(y_dir,1,1,1)%values_ * scalar_y) &
                         ,points_3D_t(vector_3D%points_(z_dir,1,1,1)%values_ * scalar_z)] &
              ,shape  = [space_dimension,1,1,1] &
            ) &
           ,cells = vector_3D%cells_ &
           ,x_min = vector_3D%x_min_ &
           ,x_max = vector_3D%x_max_ &
           ,order = vector_3D%order_ &
         ) &
        ,divergence_operator_1D_t( &
           k = vector_3D%order_ &
          ,dx = (vector_3D%x_max_ - vector_3D%x_min_)/vector_3D%cells_ &
          ,cells = vector_3D%cells_ &
      )  )
    end associate

  end procedure

  module procedure vector_3D_premultiply_scalar_3D
    scalar_x_vector = vector_3D * scalar_3D
  end procedure

  module procedure vector_3D_dot_vector

    call_julienne_assert(lhs%conformable(rhs))

    associate( &
      lhs_ => lhs%to_centers_extended() &
     ,rhs_ => rhs%to_centers_extended() &
    )
      call_julienne_assert(.all. (shape(lhs_) .equalsExpected. shape(rhs_)))

      scalar_3D = scalar_3D_t( &
         tensor_3D_t( &
            points = reshape([points_3D_t(lhs_(:,:,:,x_dir) * rhs_(:,:,:,x_dir) + lhs_(:,:,:,y_dir) * rhs_(:,:,:,y_dir) + lhs_(:,:,:,z_dir) * rhs_(:,:,:,z_dir))], [1,1,1,1]) &
           ,cells = lhs%cells_ &
           ,x_min = lhs%x_min_ &
           ,x_max = lhs%x_max_ &
           ,order = lhs%order_ &
         ) &
        ,gradient_operator_1D_t( &
           k = lhs%order_ &
          ,dx = (lhs%x_max_ - lhs%x_min_)/lhs%cells_ &
          ,cells = lhs%cells_ &
      )  )
    end associate

  end procedure

  module procedure vector_3D_to_file
    type(string_t), allocatable :: lines(:)
    integer i, j, k, l

    call_julienne_assert(self%consistent())

    associate( &
       header => [string_t("x, y, z, " // name)] &
      ,x => cell_centers_extended_1D(self%x_min_(x_dir), self%x_max_(x_dir), self%cells_(x_dir)) &
      ,y => cell_centers_extended_1D(self%x_min_(y_dir), self%x_max_(y_dir), self%cells_(y_dir)) &
      ,z => cell_centers_extended_1D(self%x_min_(z_dir), self%x_max_(z_dir), self%cells_(z_dir)) &
      ,vectors => self%to_centers_extended() &
    )
      allocate(lines(size(header) +  size(x)*size(y)))

      call_julienne_assert(.all. (shape(vectors) .equalsExpected. [size(x), size(y), size(z), space_dimension]))

      lines(1:size(header)) = header
      l = size(header)

      do k = 1, size(z)
        do j = 1, size(y)
          do i = 1, size(x)
            l = l + 1
            lines(l) = .csv. string_t([x(i), y(j), z(k), vectors(i,j,k,:)])
          end do
        end do
      end do

    end associate

    file = file_t(lines)
  end procedure

end submodule vector_3D_s