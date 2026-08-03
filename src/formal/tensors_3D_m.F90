! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module tensors_3D_m
  !! Define public 3D scalar and vector abstractions and associated mimetic gradient,
  !! divergence, and Laplacian operators as detailed by Corbino & Castillo (2020)
  !! https://doi.org/10.1016/j.cam.2019.06.042.
  use differential_operators_1D_m, only : gradient_operator_1D_t, divergence_operator_1D_t
  use julienne_m, only : file_t 
  implicit none

  private
  public :: scalar_3D_t
  public :: vector_3D_t
  public :: gradient_3D_t
  public :: divergence_3D_t
  public :: scalar_3D_initializer_i
  public :: vector_3D_initializer_i
  public :: divergence_3D_initializer_i
  public :: x_dir
  public :: y_dir
  public :: z_dir

  integer, parameter :: space_dimension = 3, max_tensor_rank = 4, x_dir = 1, y_dir = 2, z_dir = 3

  abstract interface

    pure function scalar_3D_initializer_i(x,y,z) result(f)
      !! Sampling function for initializing a scalar_3D_t object
      implicit none
      double precision, intent(in) :: x(:), y(:), z(:)
      double precision f(size(x),size(y),size(z))
    end function

    pure function divergence_3D_initializer_i(x,y,z) result(f)
      !! Sampling function for initializing a divergence_3D_t object
      implicit none
      double precision, intent(in) :: x(:), y(:), z(:)
      double precision f(size(x),size(y),size(z))
    end function

    pure function vector_3D_initializer_i(x,y,z) result(v)
      !! Sampling function for initializing a vector_3D_t object
      import space_dimension
      implicit none
      double precision, intent(in) :: x(:), y(:), z(:)
      double precision v(size(x),size(y),size(z),space_dimension)
    end function

  end interface

  type points_3D_t
    double precision, allocatable :: values_(:,:,:) !! tensor component values at 3D locations
  end type

  type tensor_3D_t
    !! Encapsulate the components that are common to all 3D tensors.
    !! Child types define the operations supported by each child, including
    !! gradient (.grad.) for scalars and divergence (.div.) for vectors.
    private
    type(points_3D_t), allocatable :: points_(:,:,:,:) !! tensor values indexable up to rank 4
    double precision x_min_(space_dimension) !! domain lower boundary
    double precision x_max_(space_dimension) !! domain upper boundary
    integer cells_(space_dimension) !! number of grid cells spanning the domain
    integer order_ !! order of accuracy of mimetic discretization
  contains
    generic :: conformable => tensor_3D_conformable
    procedure, non_overridable, private :: tensor_rank
    procedure, non_overridable, private :: tensor_3D_consistent
    procedure, non_overridable, private :: tensor_3D_conformable
  end type

  interface tensor_3D_t

    pure module function construct_3D_tensor_from_components(points, cells, x_min, x_max, order) result(tensor_3D)
      implicit none
      type(points_3D_t), intent(in) :: points(:,:,:,:) !! tensor values at 3D spatial locations
      double precision, intent(in) :: x_min(:) !! domain lower boundary
      double precision, intent(in) :: x_max(:) !! domain upper boundary
      integer, intent(in) :: cells(:) !! number of grid cells spanning the domain
      integer, intent(in) :: order !! order of accuracy of mimetic discretization
      type(tensor_3D_t) tensor_3D
    end function

  end interface

  type, extends(tensor_3D_t) :: scalar_3D_t
    !! Encapsulate scalar values at cell centers and boundaries
    private
    type(gradient_operator_1D_t) gradient_operator_1D_(space_dimension)
  contains
    generic :: assignment(=) => scalar_3D_assign_divergence
    generic :: operator(.grad.) => scalar_3D_gradient
    generic :: operator(*) => scalar_3D_postmultiply_double, scalar_3D_premultiply_double &
                             ,scalar_3D_postmultiply_integer, scalar_3D_premultiply_integer
    generic :: operator(+) => scalar_3D_plus_scalar
    generic :: values => scalar_3D_values
    generic :: grid => scalar_3D_grid
    generic :: consistent => scalar_3D_consistent
    generic :: to_faces => scalar_3D_to_faces
    generic :: to_file => scalar_3D_to_file
    procedure, non_overridable, private :: scalar_3D_assign_divergence
    procedure, non_overridable, private :: scalar_3D_to_file
    procedure, non_overridable, private :: scalar_3D_to_faces
    procedure, non_overridable, private :: scalar_3D_gradient
    procedure, non_overridable, private :: scalar_3D_values
    procedure, non_overridable, private :: scalar_3D_grid
    procedure, non_overridable, private :: scalar_3D_consistent
    procedure, non_overridable, private :: scalar_3D_postmultiply_double, scalar_3D_postmultiply_integer
    procedure, non_overridable, private :: scalar_3D_plus_scalar
    procedure, non_overridable, private, pass(rhs) :: scalar_3D_premultiply_double, scalar_3D_premultiply_integer
  end type

  interface scalar_3D_t

    pure module function construct_3D_scalar_from_function(initializer, order, cells, x_min, x_max) result(scalar_3D)
      !! Result is a collection of cell-centered-extended values with a corresponding mimetic gradient operator
      implicit none
      procedure(scalar_3D_initializer_i), pointer :: initializer
      integer, intent(in) :: order !! order of accuracy
      integer, intent(in) :: cells(:) !! number of grid cells spanning each spatial direction
      double precision, intent(in) :: x_min(:) !! grid location minima
      double precision, intent(in) :: x_max(:) !! grid location maxima
      type(scalar_3D_t) scalar_3D
    end function

    pure module function construct_3D_scalar_from_mold(initializer, mold) result(scalar_3D)
      !! Result is a 3D scalar field using a mold for all components other than the field values
      implicit none
      procedure(scalar_3D_initializer_i), pointer :: initializer
      type(scalar_3D_t), intent(in) :: mold
      type(scalar_3D_t) scalar_3D
    end function

    pure module function construct_3D_scalar_from_components(tensor_3D, gradient_operator_1D) result(scalar_3D)
      !! Result is a 3D scalar field using a mold for all components other than the field values
      implicit none
      type(tensor_3D_t), intent(in) :: tensor_3D
      type(gradient_operator_1D_t), intent(in) :: gradient_operator_1D(:)
      type(scalar_3D_t) scalar_3D
    end function

  end interface

  type, extends(tensor_3D_t) :: vector_3D_t
    !! Encapsulate 3D vector values at cell faces (of unit area for 3D) and corresponding operators
    private
    type(divergence_operator_1D_t) divergence_operator_1D_(space_dimension)
  contains
    generic :: grid => vector_3D_grid
    generic :: values => vector_3D_values
    generic :: consistent => vector_3D_consistent
    generic :: to_centers_extended => vector_3D_to_centers_extended
    generic :: operator(.div.) => vector_3D_divergence
    generic :: operator(.dot.) => vector_3D_dot_vector
    generic :: operator(*) => vector_3D_postmultiply_scalar, vector_3D_premultiply_scalar
    generic :: to_file => vector_3D_to_file
    procedure, non_overridable, private :: vector_3D_to_file
    procedure, non_overridable, private :: vector_3D_grid
    procedure, non_overridable, private :: vector_3D_values
    procedure, non_overridable, private :: vector_3D_divergence
    procedure, non_overridable, private :: vector_3D_consistent
    procedure, non_overridable, private :: vector_3D_to_centers_extended
    procedure, non_overridable, private :: vector_3D_dot_vector
    procedure, non_overridable, private :: vector_3D_postmultiply_scalar
    procedure, non_overridable, private, pass(vector_3D) :: vector_3D_premultiply_scalar
  end type

  interface vector_3D_t

    pure module function construct_3D_vector_from_components(tensor_3D, divergence_operator_1D) result(vector_3D)
      !! Result is a 3D gradient with values initialized by the provided 3D tensor grandparent
      implicit none
      type(tensor_3D_t), intent(in) :: tensor_3D
      type(divergence_operator_1D_t), intent(in) :: divergence_operator_1D(:)
      type(vector_3D_t) vector_3D
    end function

    pure module function construct_3D_vector_from_function(initializer, order, cells, x_min, x_max) result(vector_3D)
      !! Result is a 3D vector with values initialized by the provided procedure pointer sampled on the specified
      !! number of evenly spaced cells covering [x_min, x_max]
      implicit none
      procedure(vector_3D_initializer_i), pointer :: initializer
      integer, intent(in) :: order !! order of accuracy
      integer, intent(in) :: cells(:) !! number of grid cells spanning each spatial direction
      double precision, intent(in) :: x_min(:) !! grid location minima
      double precision, intent(in) :: x_max(:) !! grid location maxima
      type(vector_3D_t) vector_3D
    end function

    pure module function construct_3D_vector_from_vector_mold(initializer, mold) result(vector_3D)
      !! Result is a 3D vector with values initialized by the provided procedure pointer sampled on the 
      !! same grid as the mold
      implicit none
      procedure(vector_3D_initializer_i), pointer :: initializer
      type(vector_3D_t), intent(in) :: mold
      type(vector_3D_t) vector_3D
    end function

    pure module function construct_3D_vector_from_scalar_mold(initializer, mold) result(vector_3D)
      !! Result is a 3D vector with values initialized by the provided procedure pointer sampled on the 
      !! face-centered grid corresponding to the cell-centered grid of the mold
      implicit none
      procedure(vector_3D_initializer_i), pointer :: initializer
      type(scalar_3D_t), intent(in) :: mold
      type(vector_3D_t) vector_3D
    end function
    
  end interface

  type, extends(vector_3D_t) :: gradient_3D_t
    !! A 3D mimetic gradient vector field abstraction with a public method that produces corresponding numerical quadrature weights
  contains
    generic :: operator(*) => gradient_3D_premultiply_constant, gradient_3D_postmultiply_constant
    procedure, private, non_overridable :: gradient_3D_postmultiply_constant
    procedure, private, non_overridable, pass(rhs) :: gradient_3D_premultiply_constant
  end type

  interface gradient_3D_t

    pure module function construct_3D_gradient_from_components(tensor_3D, divergence_operator_1D) result(gradient_3D)
      !! Result is a 3D gradient with values initialized by the provided 3D tensor (grand)parent
      implicit none
      type(tensor_3D_t), intent(in) :: tensor_3D
      type(divergence_operator_1D_t), intent(in) :: divergence_operator_1D(:)
      type(gradient_3D_t) gradient_3D
    end function

  end interface

  type, extends(tensor_3D_t) :: divergence_3D_t
    !! A 3D mimetic divergence field abstraction with a public method that produces corresponding numerical quadrature weights
  contains
    generic :: values => divergence_3D_values
    generic :: grid => divergence_3D_grid
    generic :: consistent => tensor_3D_consistent
    generic :: operator(*) => divergence_3D_premultiply_constant, divergence_3D_postmultiply_constant
    generic :: operator(-) => divergence_3D_minus_scalar, divergence_3D_minus_divergence
    generic :: to_file => divergence_3D_to_file
    procedure, non_overridable, private :: divergence_3D_to_file
    procedure, private, non_overridable :: divergence_3D_values
    procedure, private, non_overridable :: divergence_3D_grid
    procedure, private, non_overridable :: divergence_3D_minus_scalar
    procedure, private, non_overridable :: divergence_3D_postmultiply_constant
    procedure, private, non_overridable :: divergence_3D_minus_divergence
    procedure, private, non_overridable, pass(rhs) :: divergence_3D_premultiply_constant
  end type

  interface divergence_3D_t

    pure module function construct_3D_divergence_from_function(initializer, order, cells, x_min, x_max) result(divergence_3D)
      !! Result is a 3D scalar product initialized by sampling the initializer at cell centers defined by the other arguments
      implicit none
      procedure(scalar_3D_initializer_i), pointer, intent(in) :: initializer
      integer, intent(in) :: order !! order of accuracy
      integer, intent(in) :: cells(:) !! number of grid cells spanning each spatial direction
      double precision, intent(in) :: x_min(:) !! grid location minima
      double precision, intent(in) :: x_max(:) !! grid location maxima
      type(divergence_3D_t) divergence_3D
    end function

    pure module function construct_3D_divergence_from_vector_mold(initializer, mold) result(divergence_3D)
      !! Result is a 3D scalar product initialized by sampling the initializer on cell centers defined by the mold
      implicit none
      procedure(divergence_3D_initializer_i), pointer, intent(in) :: initializer
      type(vector_3D_t), intent(in) :: mold
      type(divergence_3D_t) divergence_3D
    end function

  end interface

  interface

    pure module function tensor_3D_consistent(self) result(self_consistent)
      !! Assert self-consistent tensor component shapes and values except for the value of the values_ component
      implicit none
      class(tensor_3D_t), intent(in) :: self
      logical self_consistent
    end function

    pure module function tensor_3D_conformable(self, tensor_3D) result(conformable)
      !! Assert same-shaped self & vector_3D components
      implicit none
      class(tensor_3D_t), intent(in) :: self, tensor_3D
      logical conformable
    end function

    pure module subroutine scalar_3D_assign_divergence(lhs, rhs)
      !! Assign 3D divergence to 3D scalar at internal points
      implicit none
      class(scalar_3D_t), intent(inout) :: lhs
      type(divergence_3D_t), intent(in) :: rhs
    end subroutine

    pure module function scalar_3D_consistent(self) result(self_consistent)
      !! Assert components allocated and self-consistent, including sufficient accuracy for gradient operator
      implicit none
      class(scalar_3D_t), intent(in) :: self
      logical self_consistent
    end function

    pure module function scalar_3D_to_faces(self, direction) result(scalars)
      !! Result is the scalar values interpolated form centers-extended to faces along the requested direction
      implicit none
      class(scalar_3D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: scalars(:,:,:)
    end function

    pure module function scalar_3D_values(self) result(values)
      !! Scalar values getter
      implicit none
      class(scalar_3D_t), intent(in) :: self
      double precision, allocatable :: values(:,:,:)
    end function

    pure module function scalar_3D_grid(self, direction) result(scalar_grid_1D)
      !! Result array contains scalar grid locations along the requested spatial direction
      implicit none
      class(scalar_3D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: scalar_grid_1D(:)
    end function

    pure module function scalar_3D_postmultiply_double(lhs, rhs) result(lhs_x_rhs)
      !! Result is product of the scalar_3D_t lhs and double-precision (constant) rhs
      implicit none
      class(scalar_3D_t), intent(in) :: lhs
      double precision, intent(in) :: rhs
      type(scalar_3D_t) lhs_x_rhs
    end function

    pure module function scalar_3D_postmultiply_integer(lhs, rhs) result(lhs_x_rhs)
      !! Result is product of the scalar_3D_t lhs and double-precision (constant) rhs
      implicit none
      class(scalar_3D_t), intent(in) :: lhs
      integer, intent(in) :: rhs
      type(scalar_3D_t) lhs_x_rhs
    end function

    pure module function scalar_3D_premultiply_double(lhs, rhs) result(lhs_x_rhs)
      !! Result is product of the scalar_3D_t rhs and double-precision (constant) lhs
      implicit none
      class(scalar_3D_t), intent(in) :: rhs
      double precision, intent(in) :: lhs
      type(scalar_3D_t) lhs_x_rhs
    end function

    pure module function scalar_3D_premultiply_integer(lhs, rhs) result(lhs_x_rhs)
      !! Result is product of the scalar_3D_t rhs and double-precision (constant) lhs
      implicit none
      class(scalar_3D_t), intent(in) :: rhs
      integer, intent(in) :: lhs
      type(scalar_3D_t) lhs_x_rhs
    end function

    pure module function scalar_3D_plus_scalar(lhs, rhs) result(lhs_plus_rhs)
      !! Result is product of the scalar_3D_t lhs and double-precision (constant) rhs
      implicit none
      class(scalar_3D_t), intent(in) :: lhs, rhs
      type(scalar_3D_t) lhs_plus_rhs
    end function

    pure module function vector_3D_grid(self, component, coordinate) result(vector_grid_1D)
      !! Result contains the grid locations along the requested coordinate for the requested vector component
      implicit none
      class(vector_3D_t), intent(in) :: self
      integer, intent(in) :: component, coordinate
      double precision, allocatable :: vector_grid_1D(:) !! grid points along the requested coordinate direction
    end function

    pure module function vector_3D_values(self, direction) result(vector_values)
      !! Result contains the vector values for the component designated by "direction"
      implicit none
      class(vector_3D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: vector_values(:,:,:) 
    end function

    pure module function divergence_3D_grid(self, direction) result(divergence_grid_1D)
      !! Result array contains divergence grid locations along the requested spatial direction
      implicit none
      class(divergence_3D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: divergence_grid_1D(:) !! grid points along the requested coordinate direction
    end function

    pure module function vector_3D_to_centers_extended(self) result(vectors)
      !! Vector values getter
      implicit none
      class(vector_3D_t), intent(in) :: self
      double precision, allocatable :: vectors(:,:,:,:)
    end function

    pure module function vector_3D_consistent(self) result(self_consistent)
      !! Assert internal self consistency, including sufficient accuracy for divergence operator
      implicit none
      class(vector_3D_t), intent(in) :: self
      logical self_consistent
    end function

    pure module function divergence_3D_values(self) result(divergences)
      !! Vector values getter
      implicit none
      class(divergence_3D_t), intent(in) :: self
      double precision, allocatable :: divergences(:,:,:)
    end function

    pure module function scalar_3D_gradient(self) result(gradient_3D)
      !! Result is mimetic gradient of the scalar_3D_t "self"
      implicit none
      class(scalar_3D_t), intent(in) :: self
      type(gradient_3D_t) gradient_3D
    end function

    pure module function vector_3D_divergence(self) result(divergence_3D)
      !! Result is mimetic divergence of the 3D vector self
      implicit none
      class(vector_3D_t), intent(in) :: self
      type(divergence_3D_t) divergence_3D
    end function

    pure module function vector_3D_postmultiply_scalar(vector_3D, scalar_3D) result(vector_x_scalar)
      !! Result is product of the 3D vector and scalar arguments
      implicit none
      class(vector_3D_t), intent(in) :: vector_3D
      type(scalar_3D_t), intent(in) :: scalar_3D
      type(vector_3D_t) vector_x_scalar
    end function

    pure module function vector_3D_premultiply_scalar(scalar_3D, vector_3D) result(scalar_x_vector)
      !! Result is product of the 3D vector and scalar arguments
      implicit none
      class(vector_3D_t), intent(in) :: vector_3D
      type(scalar_3D_t), intent(in) :: scalar_3D
      type(vector_3D_t) scalar_x_vector
    end function

    pure module function vector_3D_dot_vector(lhs, rhs) result(scalar_3D)
      !! Result is scalar product of the 3D-vector arguments
      implicit none
      class(vector_3D_t), intent(in) :: lhs, rhs
      type(scalar_3D_t) scalar_3D
    end function

    pure module function gradient_3D_postmultiply_constant(lhs, rhs) result(product)
      !! Result is product of the gradient_3D_t lhs and the constant rhs
      implicit none
      class(gradient_3D_t), intent(in) :: lhs
      double precision, intent(in) :: rhs
      type(gradient_3D_t) product
    end function

    pure module function gradient_3D_premultiply_constant(lhs, rhs) result(product)
      !! Result is product of the gradient_3D_t rhs and the constant lhs
      implicit none
      class(gradient_3D_t), intent(in) :: rhs
      double precision, intent(in) :: lhs
      type(gradient_3D_t) product
    end function

    pure module function divergence_3D_postmultiply_constant(lhs, rhs) result(lhs_x_rhs)
      !! Result is product of the divergence_3D_t lhs and the constant rhs
      implicit none
      class(divergence_3D_t), intent(in) :: lhs
      double precision, intent(in) :: rhs
      type(divergence_3D_t) lhs_x_rhs
    end function

    pure module function divergence_3D_premultiply_constant(lhs, rhs) result(lhs_x_rhs)
      !! Result is product of the constant rhs and the lhs divergence_3D_t
      implicit none
      class(divergence_3D_t), intent(in) :: rhs
      double precision, intent(in) :: lhs
      type(divergence_3D_t) lhs_x_rhs
    end function

    pure module function divergence_3D_minus_divergence(lhs, rhs) result(difference)
      !! Result is the pointwise difference between the lhs and rhs
      implicit none
      class(divergence_3D_t), intent(in) :: lhs, rhs
      type(divergence_3D_t) difference
    end function

    pure module function divergence_3D_minus_scalar(lhs, rhs) result(difference)
      !! Result is the pointwise difference between the lhs and rhs
      implicit none
      class(divergence_3D_t), intent(in) :: lhs
      class(scalar_3D_t), intent(in) :: rhs
      type(scalar_3D_t) difference
    end function

    pure module function scalar_3D_to_file(self, name) result(file)
      !! Result is a file_t object containing the grid points and the corresponding scalar values
      implicit none
      class(scalar_3D_t), intent(in) :: self
      character(len=*), intent(in) :: name
      type(file_t) file
    end function

    pure module function vector_3D_to_file(self, name) result(file)
      !! Result is a file_t object containing the grid points and the corresponding vector values
      implicit none
      class(vector_3D_t), intent(in) :: self
      character(len=*), intent(in) :: name
      type(file_t) file
    end function

    pure module function divergence_3D_to_file(self, name) result(file)
      !! Result is a file_t object containing the grid points and the corresponding 3D scalar-product values
      implicit none
      class(divergence_3D_t), intent(in) :: self
      character(len=*), intent(in) :: name
      type(file_t) file
    end function

    pure module function tensor_rank(self) result(my_rank)
      !! Result is number of spatial dimensions of non-unit size
      implicit none
      class(tensor_3D_t), intent(in) :: self
      integer my_rank
    end function

  end interface

end module tensors_3D_m
