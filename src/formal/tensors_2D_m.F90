! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module tensors_2D_m
  !! Define public 2D scalar and vector abstractions and associated mimetic gradient,
  !! divergence, and Laplacian operators as detailed by Corbino & Castillo (2020)
  !! https://doi.org/10.1016/j.cam.2019.06.042.
  use differential_operators_1D_m, only : gradient_operator_1D_t, divergence_operator_1D_t
  use julienne_m, only : file_t 
  implicit none

  private
  public :: scalar_2D_t
  public :: vector_2D_t
  public :: gradient_2D_t
  public :: divergence_2D_t
  public :: scalar_2D_initializer_i
  public :: vector_2D_initializer_i
  public :: divergence_2D_initializer_i

  integer, parameter :: space_dimension = 2

  abstract interface

    pure function scalar_2D_initializer_i(x,y) result(f)
      !! Sampling function for initializing a scalar_2D_t object
      implicit none
      double precision, intent(in) :: x(:), y(:)
      double precision f(size(x),size(y))
    end function

    pure function divergence_2D_initializer_i(x,y) result(f)
      !! Sampling function for initializing a scalar_2D_t object
      implicit none
      double precision, intent(in) :: x(:), y(:)
      double precision f(size(x),size(y))
    end function

    pure function vector_2D_initializer_i(x,y) result(v)
      !! Sampling function for initializing a vector_2D_t object
      import space_dimension
      implicit none
      double precision, intent(in) :: x(:), y(:)
      double precision v(size(x),size(y),space_dimension)
    end function

  end interface

  type tensor_2D_t
    !! Encapsulate the components that are common to all 2D tensors.
    !! Child types define the operations supported by each child, including
    !! gradient (.grad.) for scalars and divergence (.div.) for vectors.
    private
    double precision, allocatable :: values_(:,:, :,:,:,:) !! tensor components for rank<=4 at 2D locations
    double precision x_min_(space_dimension) !! domain lower boundary
    double precision x_max_(space_dimension) !! domain upper boundary
    integer cells_(space_dimension) !! number of grid cells spanning the domain
    integer order_ !! order of accuracy of mimetic discretization
  contains
    procedure, non_overridable, private :: tensor_2D_consistent
    procedure, non_overridable, private :: tensor_2D_conformable
  end type

  interface tensor_2D_t

    pure module function construct_2D_tensor_from_components(values, cells, x_min, x_max, order) result(tensor_2D)
      implicit none
      double precision, intent(in) :: values(:,:, :,:,:,:) !! tensor components at 2D spatial locations
      double precision, intent(in) :: x_min(:) !! domain lower boundary
      double precision, intent(in) :: x_max(:) !! domain upper boundary
      integer, intent(in) :: cells(:) !! number of grid cells spanning the domain
      integer, intent(in) :: order !! order of accuracy of mimetic discretization
      type(tensor_2D_t) tensor_2D
    end function

  end interface

  type, extends(tensor_2D_t) :: scalar_2D_t
    !! Encapsulate scalar values at cell centers and boundaries
    private
    type(gradient_operator_1D_t) gradient_operator_1D_(space_dimension)
  contains
    generic :: operator(.grad.) => scalar_2D_gradient
    generic :: values => scalar_2D_values
    generic :: grid => scalar_2D_grid
    generic :: to_file => scalar_2D_to_file
    generic :: consistent => scalar_2D_consistent
    generic :: conformable => scalar_2D_conformable_scalar
    procedure, non_overridable, private :: scalar_2D_to_file
    procedure, non_overridable, private :: scalar_2D_gradient
    procedure, non_overridable, private :: scalar_2D_values
    procedure, non_overridable, private :: scalar_2D_grid
    procedure, non_overridable, private :: scalar_2D_consistent
    procedure, non_overridable, private :: scalar_2D_conformable_scalar
  end type

  interface scalar_2D_t

    pure module function construct_2D_scalar_from_function(initializer, order, cells, x_min, x_max) result(scalar_2D)
      !! Result is a collection of cell-centered-extended values with a corresponding mimetic gradient operator
      implicit none
      procedure(scalar_2D_initializer_i), pointer :: initializer
      integer, intent(in) :: order !! order of accuracy
      integer, intent(in) :: cells(:) !! number of grid cells spanning each spatial direction
      double precision, intent(in) :: x_min(:) !! grid location minima
      double precision, intent(in) :: x_max(:) !! grid location maxima
      type(scalar_2D_t) scalar_2D
    end function

    pure module function construct_2D_scalar_from_mold(initializer, mold) result(scalar_2D)
      !! Result is a 2D scalar field using a mold for all components other than the field values
      implicit none
      procedure(scalar_2D_initializer_i), pointer :: initializer
      type(scalar_2D_t), intent(in) :: mold
      type(scalar_2D_t) scalar_2D
    end function

  end interface

  type, extends(tensor_2D_t) :: vector_2D_t
    !! Encapsulate 2D vector values at cell faces (of unit area for 2D) and corresponding operators
    private
    type(divergence_operator_1D_t) divergence_operator_1D_(space_dimension)
  contains
    generic :: values => vector_2D_values
    generic :: to_file => vector_2D_to_file
    generic :: grid => vector_2D_grid
    generic :: consistent => vector_2D_consistent
    generic :: conformable => vector_2D_conformable_vector, vector_2D_conformable_scalar
    generic :: operator(.div.) => vector_2D_divergence
    procedure, non_overridable, private :: vector_2D_values
    procedure, non_overridable, private :: vector_2D_to_file
    procedure, non_overridable, private :: vector_2D_grid
    procedure, non_overridable, private :: vector_2D_divergence
    procedure, non_overridable, private :: vector_2D_consistent
    procedure, non_overridable, private :: vector_2D_conformable_vector
    procedure, non_overridable, private :: vector_2D_conformable_scalar
  end type

  interface vector_2D_t

    pure module function construct_2D_vector_from_function(initializer, order, cells, x_min, x_max) result(vector_2D)
      !! Result is a 2D vector with values initialized by the provided procedure pointer sampled on the specified
      !! number of evenly spaced cells covering [x_min, x_max]
      implicit none
      procedure(vector_2D_initializer_i), pointer :: initializer
      integer, intent(in) :: order !! order of accuracy
      integer, intent(in) :: cells(:) !! number of grid cells spanning each spatial direction
      double precision, intent(in) :: x_min(:) !! grid location minima
      double precision, intent(in) :: x_max(:) !! grid location maxima
      type(vector_2D_t) vector_2D
    end function

    pure module function construct_2D_vector_from_vector_mold(initializer, mold) result(vector_2D)
      !! Result is a 2D vector with values initialized by the provided procedure pointer sampled on the 
      !! same grid as the mold
      implicit none
      procedure(vector_2D_initializer_i), pointer :: initializer
      type(vector_2D_t), intent(in) :: mold
      type(vector_2D_t) vector_2D
    end function

    pure module function construct_2D_vector_from_scalar_mold(initializer, mold) result(vector_2D)
      !! Result is a 2D vector with values initialized by the provided procedure pointer sampled on the 
      !! face-centered grid corresponding to the cell-centered grid of the mold
      implicit none
      procedure(vector_2D_initializer_i), pointer :: initializer
      type(scalar_2D_t), intent(in) :: mold
      type(vector_2D_t) vector_2D
    end function
    
  end interface

  type, extends(vector_2D_t) :: gradient_2D_t
    !! A 2D mimetic gradient vector field abstraction with a public method that produces corresponding numerical quadrature weights
  end type

  type, extends(tensor_2D_t) :: divergence_2D_t
    !! A 2D mimetic divergence field abstraction with a public method that produces corresponding numerical quadrature weights
  contains
    generic :: values => divergence_2D_values
    generic :: grid => divergence_2D_grid
    generic :: to_file => divergence_2D_to_file
    generic :: consistent => tensor_2D_consistent
    generic :: conformable => tensor_2D_conformable
    generic :: operator(*) => divergence_2D_premultiply_constant, divergence_2D_postmultiply_constant
    procedure, private, non_overridable :: divergence_2D_values
    procedure, private, non_overridable :: divergence_2D_grid
    procedure, private, non_overridable :: divergence_2D_to_file
    procedure, private, non_overridable, private :: divergence_2D_postmultiply_constant
    procedure, private, non_overridable, pass(rhs) :: divergence_2D_premultiply_constant
  end type

  interface divergence_2D_t

    pure module function construct_2D_divergence_from_function(initializer, order, cells, x_min, x_max) result(divergence_2D)
      !! Result is a 2D divergence initialized by sampling the initializer at cell centers defined by the other arguments
      implicit none
      procedure(scalar_2D_initializer_i), pointer, intent(in) :: initializer
      integer, intent(in) :: order !! order of accuracy
      integer, intent(in) :: cells(:) !! number of grid cells spanning each spatial direction
      double precision, intent(in) :: x_min(:) !! grid location minima
      double precision, intent(in) :: x_max(:) !! grid location maxima
      type(divergence_2D_t) divergence_2D
    end function

    pure module function construct_2D_divergence_from_vector_mold(initializer, mold) result(divergence_2D)
      !! Result is a 2D divergence initialized by sampling the initializer on cell centers defined by the mold
      implicit none
      procedure(divergence_2D_initializer_i), pointer, intent(in) :: initializer
      type(vector_2D_t), intent(in) :: mold
      type(divergence_2D_t) divergence_2D
    end function

  end interface

  interface

    pure module function tensor_2D_consistent(self) result(self_consistent)
      !! Assert self-consistent tensor component shapes and values except for the value of the values_ component
      implicit none
      class(tensor_2D_t), intent(in) :: self
      logical self_consistent
    end function

    pure module function tensor_2D_conformable(self, tensor_2D) result(conformable)
      !! Assert same-shaped self & vector_2D components
      implicit none
      class(tensor_2D_t), intent(in) :: self, tensor_2D
      logical conformable
    end function

    pure module function scalar_2D_consistent(self) result(self_consistent)
      !! Assert components allocated and self-consistent, including sufficient accuracy for gradient operator
      implicit none
      class(scalar_2D_t), intent(in) :: self
      logical self_consistent
    end function

    pure module function scalar_2D_conformable_scalar(self, scalar_2D) result(conformable)
      !! Assert the arguments' components are conformable, self-consistent, and consistent with each other
      implicit none
      class(scalar_2D_t), intent(in) :: self, scalar_2D
      logical conformable
    end function

    pure module function vector_2D_consistent(self) result(self_consistent)
      !! Assert components allocated and self-consistent, including sufficient accuracy for divergence operator
      implicit none
      class(vector_2D_t), intent(in) :: self
      logical self_consistent
    end function

    pure module function vector_2D_conformable_vector(self, vector_2D) result(conformable)
      !! Assert the arguments' components are conformable, self-consistent, and consistent with each other
      implicit none
      class(vector_2D_t), intent(in) :: self, vector_2D
      logical conformable
    end function

    pure module function vector_2D_conformable_scalar(self, scalar_2D) result(conformable)
      !! Assert components allocated and self-consistent
      implicit none
      class(vector_2D_t), intent(in) :: self
      class(scalar_2D_t), intent(in) :: scalar_2D
      logical conformable
    end function

    pure module function scalar_2D_values(self) result(scalar_values)
      !! Scalar values getter
      implicit none
      class(scalar_2D_t), intent(in) :: self
      double precision, allocatable :: scalar_values(:,:)
    end function

    pure module function scalar_2D_grid(self, direction) result(scalar_grid_1D)
      !! Result array contains scalar grid locations along the requested spatial direction
      implicit none
      class(scalar_2D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: scalar_grid_1D(:)
    end function

    pure module function vector_2D_grid(self, direction) result(vector_grid_1D)
      !! Result array contains scalar grid locations along the requested spatial direction
      implicit none
      class(vector_2D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: vector_grid_1D(:) !! grid points along the requested coordinate direction
    end function

    pure module function divergence_2D_grid(self, direction) result(divergence_grid_1D)
      !! Result array contains divergence grid locations along the requested spatial direction
      implicit none
      class(divergence_2D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: divergence_grid_1D(:) !! grid points along the requested coordinate direction
    end function

    pure module function vector_2D_values(self) result(vector_values)
      !! Vector values getter
      implicit none
      class(vector_2D_t), intent(in) :: self
      double precision, allocatable :: vector_values(:,:,:)
    end function

    pure module function divergence_2D_values(self) result(divergence_values)
      !! Vector values getter
      implicit none
      class(divergence_2D_t), intent(in) :: self
      double precision, allocatable :: divergence_values(:,:)
    end function

    pure module function scalar_2D_gradient(self) result(gradient_2D)
      !! Result is mimetic gradient of the scalar_2D_t "self"
      implicit none
      class(scalar_2D_t), intent(in) :: self
      type(gradient_2D_t) gradient_2D
    end function

    pure module function vector_2D_divergence(self) result(divergence_2D)
      !! Result is mimetic divergence of the vector_2D_t "self"
      implicit none
      class(vector_2D_t), intent(in) :: self
      type(divergence_2D_t) divergence_2D
    end function

    pure module function divergence_2D_postmultiply_constant(lhs, rhs) result(product)
      !! Result is product of the divergence_2D_t lhs and the constant rhs
      implicit none
      class(divergence_2D_t), intent(in) :: lhs
      double precision, intent(in) :: rhs
      type(divergence_2D_t) product
    end function

    pure module function divergence_2D_premultiply_constant(lhs, rhs) result(product)
      !! Result is product of the divergence_2D_t rhs and the constant lhs
      implicit none
      class(divergence_2D_t), intent(in) :: rhs
      double precision, intent(in) :: lhs
      type(divergence_2D_t) product
    end function

    pure module function scalar_2D_to_file(self) result(file)
      !! Result is a file_t object containing the grid points and the corresponding scalar values
      implicit none
      class(scalar_2D_t), intent(in) :: self
      type(file_t) file
    end function

    pure module function divergence_2D_to_file(self) result(file)
      !! Result is a file_t object containing the grid points and the corresponding divergence values
      implicit none
      class(divergence_2D_t), intent(in) :: self
      type(file_t) file
    end function

    pure module function vector_2D_to_file(self) result(file)
      !! Result is a file_t object containing the grid points and the corresponding vector components
      implicit none
      class(vector_2D_t), intent(in) :: self
      type(file_t) file
    end function

  end interface

end module tensors_2D_m
