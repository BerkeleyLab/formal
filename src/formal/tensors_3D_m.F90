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

  integer, parameter :: space_dimension = 3

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

  type tensor_3D_t
    !! Encapsulate the components that are common to all 3D tensors.
    !! Child types define the operations supported by each child, including
    !! gradient (.grad.) for scalars and divergence (.div.) for vectors.
    private
    double precision, allocatable :: values_(:,:,:, :,:,:,:) !! tensor components for rank<=4 at 3D locations
    double precision x_min_(space_dimension) !! domain lower boundary
    double precision x_max_(space_dimension) !! domain upper boundary
    integer cells_(space_dimension) !! number of grid cells spanning the domain
    integer order_ !! order of accuracy of mimetic discretization
  end type

  interface tensor_3D_t

    pure module function construct_3D_tensor_from_components(values, cells, x_min, x_max, order) result(tensor_3D)
      implicit none
      double precision, intent(in) :: values(:,:,:, :,:,:,:) !! tensor components for rank<=4 at 3D locations
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
    generic :: operator(.grad.) => scalar_3D_gradient
    generic :: values => scalar_3D_values
    generic :: grid => scalar_3D_grid
    generic :: to_file => scalar_3D_to_file
    procedure, non_overridable, private :: scalar_3D_to_file
    procedure, non_overridable, private :: scalar_3D_gradient
    procedure, non_overridable, private :: scalar_3D_values
    procedure, non_overridable, private :: scalar_3D_grid
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

  end interface

  type, extends(tensor_3D_t) :: vector_3D_t
    !! Encapsulate 3D vector values at cell faces (of unit area for 3D) and corresponding operators
    private
    type(divergence_operator_1D_t) divergence_operator_1D_(space_dimension)
  contains
    generic :: values => vector_3D_values
    generic :: to_file => vector_3D_to_file
    generic :: grid => vector_3D_grid
    generic :: operator(.div.) => vector_3D_divergence
    procedure, non_overridable, private :: vector_3D_values
    procedure, non_overridable, private :: vector_3D_to_file
    procedure, non_overridable, private :: vector_3D_grid
    procedure, non_overridable, private :: vector_3D_divergence
  end type

  interface vector_3D_t

    pure module function construct_3D_vector_from_function(initializer, order, cells, x_min, x_max) result(vector_3D)
      !! Result is a 3D vector with values initialized by the provided procedure pointer sampled on the faces of
      !! the specified number of evenly spaced cells covering [x_min, x_max]
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
  end type

  type, extends(tensor_3D_t) :: divergence_3D_t
    !! A 3D mimetic divergence field abstraction with a public method that produces corresponding numerical quadrature weights
  contains
    generic :: values => divergence_3D_values
    generic :: grid => divergence_3D_grid
    generic :: to_file => divergence_3D_to_file
    procedure, private, non_overridable :: divergence_3D_values
    procedure, private, non_overridable :: divergence_3D_grid
    procedure, private, non_overridable :: divergence_3D_to_file
  end type

  interface divergence_3D_t

    pure module function construct_3D_divergence_from_function(initializer, order, cells, x_min, x_max) result(divergence_3D)
      !! Result is a 3D divergence initialized by sampling the initializer at cell centers defined by the other arguments
      implicit none
      procedure(scalar_3D_initializer_i), pointer, intent(in) :: initializer
      integer, intent(in) :: order !! order of accuracy
      integer, intent(in) :: cells(:) !! number of grid cells spanning each spatial direction
      double precision, intent(in) :: x_min(:) !! grid location minima
      double precision, intent(in) :: x_max(:) !! grid location maxima
      type(divergence_3D_t) divergence_3D
    end function

    pure module function construct_3D_divergence_from_vector_mold(initializer, mold) result(divergence_3D)
      !! Result is a 3D divergence initialized by sampling the initializer on cell centers defined by the mold
      implicit none
      procedure(divergence_3D_initializer_i), pointer, intent(in) :: initializer
      type(vector_3D_t), intent(in) :: mold
      type(divergence_3D_t) divergence_3D
    end function

  end interface

  interface

    pure module function scalar_3D_values(self) result(scalar_values)
      !! Scalar values getter
      class(scalar_3D_t), intent(in) :: self
      double precision, allocatable :: scalar_values(:,:,:)
    end function

    pure module function scalar_3D_grid(self, direction) result(scalar_grid_1D)
      !! Result contains scalar grid locations along the requested spatial direction
      class(scalar_3D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: scalar_grid_1D(:)
    end function

    pure module function vector_3D_grid(self, direction) result(vector_grid_1D)
      !! Result contains scalar grid locations along the requested spatial direction
      class(vector_3D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: vector_grid_1D(:) !! grid points along one the requested coordinate direction
    end function

    pure module function vector_3D_values(self) result(vector_values)
      !! Vector values getter
      class(vector_3D_t), intent(in) :: self
      double precision, allocatable :: vector_values(:,:,:,:)
    end function

    pure module function scalar_3D_gradient(self) result(gradient_3D)
      !! Result is the mimetic gradient of the scalar_3D_t "self"
      implicit none
      class(scalar_3D_t), intent(in) :: self
      type(gradient_3D_t) gradient_3D
    end function

    pure module function vector_3D_divergence(self) result(divergence_3D)
      !! Result is mimetic divergence of the scalar_3D_t "self"
      implicit none
      class(vector_3D_t), intent(in) :: self
      type(divergence_3D_t) divergence_3D
    end function

    pure module function scalar_3D_to_file(self) result(file)
      !! Result is a file_t object containing the grid points and the corresponding scalar values
      implicit none
      class(scalar_3D_t), intent(in) :: self
      type(file_t) file
    end function

    pure module function vector_3D_to_file(self) result(file)
      !! Result is a file_t object containing the grid points and the corresponding vector components
      implicit none
      class(vector_3D_t), intent(in) :: self
      type(file_t) file
    end function

    pure module function divergence_3D_grid(self, direction) result(divergence_grid_1D)
      !! Result array contains divergence grid locations along the requested spatial direction
      class(divergence_3D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: divergence_grid_1D(:) !! grid points along the requested coordinate direction
    end function

    pure module function divergence_3D_values(self) result(divergence_values)
      !! Vector values getter
      class(divergence_3D_t), intent(in) :: self
      double precision, allocatable :: divergence_values(:,:,:)
    end function

    pure module function divergence_3D_to_file(self) result(file)
      !! Result is a file_t object containing the grid points and the corresponding divergence values
      implicit none
      class(divergence_3D_t), intent(in) :: self
      type(file_t) file
    end function

  end interface

end module tensors_3D_m
