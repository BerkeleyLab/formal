! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "formal-language-support.F90"

module tensors_2D_m
  !! Define public 2D scalar and vector abstractions and associated mimetic gradient,
  !! divergence, and Laplacian operators as detailed by Corbino & Castillo (2020)
  !! https://doi.org/10.1016/j.cam.2019.06.042.
  use differential_operators_1D_m, only : gradient_operator_1D_t, divergence_operator_1D_t
    
  implicit none

  private

  public :: scalar_2D_t
  public :: vector_2D_t
  public :: gradient_2D_t
  public :: scalar_2D_initializer_i
  public :: vector_2D_initializer_i

  integer, parameter :: space_dimension = 2

  abstract interface

    pure function scalar_2D_initializer_i(x1, x2) result(f)
      !! Sampling function for initializing a scalar_2D_t object
      implicit none
      double precision, intent(in) :: x1, x2
      double precision, allocatable :: f
    end function

    pure function vector_2D_initializer_i(x1, x2 ) result(v)
      !! Sampling function for initializing a vector_2D_t object
      import space_dimension
      implicit none
      double precision, intent(in) :: x1, x2
      double precision v(space_dimension)
    end function

  end interface

  type tensor_2D_t
    !! Encapsulate the components that are common to all 2D tensors.
    !! Child types define the operations supported by each child, including
    !! gradient (.grad.) for scalars and divergence (.div.) for vectors.
    private
    double precision x_min_(space_dimension) !! domain lower boundary
    double precision x_max_(space_dimension) !! domain upper boundary
    integer cells_(space_dimension) !! number of grid cells spanning the domain
    integer order_ !! order of accuracy of mimetic discretization
    double precision, allocatable :: values_(:,:,:) !! tensor components at spatial locations
  end type

  type, extends(tensor_2D_t) :: scalar_2D_t
    !! Encapsulate scalar values at cell centers and boundaries
    private
    type(gradient_operator_1D_t) gradient_operator_1D_(space_dimension)
  contains
    generic :: operator(.grad.) => grad
    generic :: values => scalar_2D_values
    procedure, non_overridable, private :: grad
    procedure, non_overridable, private :: scalar_2D_values
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

  end interface

  type, extends(tensor_2D_t) :: vector_2D_t
    !! Encapsulate 2D vector values at cell faces (of unit area for 2D) and corresponding operators
    private
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

  end interface

  type, extends(vector_2D_t) :: gradient_2D_t
    !! A 2D mimetic gradient vector field abstraction with a public method that produces corresponding numerical quadrature weights
    type(divergence_operator_1D_t) divergence_operator_1D_(space_dimension)
  end type

  interface

    pure module function scalar_2D_values(self) result(scalar_values)
      !! Scalar values getter
      class(scalar_2D_t), intent(in) :: self
      double precision, allocatable :: scalar_values(:,:)
    end function

    pure module function grad(self) result(gradient_2D)
      !! Result is mimetic gradient of the scalar_2D_t "self"
      implicit none
      class(scalar_2D_t), intent(in) :: self
      type(gradient_2D_t) gradient_2D
    end function

  end interface

end module tensors_2D_m
