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
  public :: scalar_2D_initializer_i
  public :: vector_2D_initializer_i

  integer, parameter :: space_dimension = 2

  abstract interface

    pure function scalar_2D_initializer_i(x,y) result(f)
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
    double precision, allocatable :: values_(:,:,:,:,:,:) !! tensor components for rank<=4 at 2D locations
    double precision x_min_(space_dimension) !! domain lower boundary
    double precision x_max_(space_dimension) !! domain upper boundary
    integer cells_(space_dimension) !! number of grid cells spanning the domain
    integer order_ !! order of accuracy of mimetic discretization
  end type

  interface tensor_2D_t

    pure module function construct_2D_tensor_from_components(values, cells, x_min, x_max, order) result(tensor_2D)
      implicit none
      double precision, intent(in) :: values(:,:,:,:,:,:) !! tensor components at 2D spatial locations
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
    procedure, non_overridable, private :: scalar_2D_to_file
    procedure, non_overridable, private :: scalar_2D_gradient
    procedure, non_overridable, private :: scalar_2D_values
    procedure, non_overridable, private :: scalar_2D_grid
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
    procedure, non_overridable, private :: vector_2D_values
    procedure, non_overridable, private :: vector_2D_to_file
    procedure, non_overridable, private :: vector_2D_grid
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

  interface

    pure module function scalar_2D_values(self) result(scalar_values)
      !! Scalar values getter
      class(scalar_2D_t), intent(in) :: self
      double precision, allocatable :: scalar_values(:,:)
    end function

    pure module function scalar_2D_grid(self, direction) result(scalar_grid_1D)
      !! Result array contains scalar grid locations along the requested spatial direction
      class(scalar_2D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: scalar_grid_1D(:)
    end function

    pure module function vector_2D_grid(self, direction) result(vector_grid_1D)
      !! Result array contains scalar grid locations along the requested spatial direction
      class(vector_2D_t), intent(in) :: self
      integer, intent(in) :: direction
      double precision, allocatable :: vector_grid_1D(:) !! grid points along one the requested coordinate direction
    end function

    pure module function vector_2D_values(self) result(vector_values)
      !! Vector values getter
      class(vector_2D_t), intent(in) :: self
      double precision, allocatable :: vector_values(:,:,:)
    end function

    pure module function scalar_2D_gradient(self) result(gradient_2D)
      !! Result is mimetic gradient of the scalar_2D_t "self"
      implicit none
      class(scalar_2D_t), intent(in) :: self
      type(gradient_2D_t) gradient_2D
    end function

    pure module function scalar_2D_to_file(self) result(file)
      !! Result is a file_t object containing the grid points and the corresponding scalar values
      implicit none
      class(scalar_2D_t), intent(in) :: self
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
