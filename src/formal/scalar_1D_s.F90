! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) scalar_1D_s
  use julienne_m, only : &
    call_julienne_assert_ &
   ,julienne_assert &
   ,operator(//) &
   ,operator(.all.) &
   ,operator(.approximates.) &
   ,operator(.equalsExpected.) &
   ,operator(.csv.) &
   ,operator(.isAtLeast.) &
   ,operator(.greaterThan.) &
   ,operator(.within.) &
   ,string_t
  implicit none

contains


#ifndef __GFORTRAN__

  ! PURPOSE: To construct a new scalar_1D_t object by assigning each argument to a corresponding
  !          corresponding component of the new object.
  ! KEYWORDS: 1D scalar field constructor
  ! CONTEXT: Invoke this constructor with a pointer associated with a function to be sampled at a set
  !          of uniformly-spaced cell centers along one spatial dimension bounded by x_min and x_max.

  module procedure construct_1D_scalar_from_function
    call_julienne_assert(x_max .greaterThan. x_min)
    call_julienne_assert(cells .isAtLeast. 2*order)

    associate(values => initializer(scalar_1D_grid_locations(x_min, x_max, cells)))
      scalar_1D%tensor_1D_t = tensor_1D_t(values, x_min, x_max, cells, order)
    end associate
    scalar_1D%gradient_operator_1D_ = gradient_operator_1D_t(k=order, dx=(x_max - x_min)/cells, cells=cells)
  end procedure

#else

  ! PURPOSE: To construct a new scalar_1D_t object by assigning each argument to a corresponding
  !          corresponding component of the new object.
  ! KEYWORDS: 1D scalar field constructor
  ! CONTEXT: Invoke this constructor with a pointer associated with a function to be sampled at a set
  !          of uniformly-spaced cell centers along one spatial dimension bounded by x_min and x_max.

  pure module function construct_1D_scalar_from_function(initializer, order, cells, x_min, x_max) result(scalar_1D)
    procedure(scalar_1D_initializer_i), pointer :: initializer
    integer, intent(in) :: order !! order of accuracy
    integer, intent(in) :: cells !! number of grid cells spanning the domain
    double precision, intent(in) :: x_min !! grid location minimum
    double precision, intent(in) :: x_max !! grid location maximum
    type(scalar_1D_t) scalar_1D

    call_julienne_assert(x_max .greaterThan. x_min)
    call_julienne_assert(cells .isAtLeast. 2*order)

    associate(values => initializer(scalar_1D_grid_locations(x_min, x_max, cells)))
      scalar_1D%tensor_1D_t = tensor_1D_t(values, x_min, x_max, cells, order)
    end associate
    scalar_1D%gradient_operator_1D_ = gradient_operator_1D_t(k=order, dx=(x_max - x_min)/cells, cells=cells)
  end function

  pure function cell_center_locations(x_min, x_max, cells) result(x)
    double precision, intent(in) :: x_min, x_max
    integer, intent(in) :: cells
    double precision, allocatable:: x(:)
    integer cell

    associate(dx => (x_max - x_min)/cells)
      x = x_min + dx/2. + [((cell-1)*dx, cell = 1, cells)]
    end associate
  end function

#endif

  ! PURPOSE: To compute mimetic approximations to the gradient of scalar fields.
  ! KEYWORDS: gradient, differential operator
  ! CONTEXT: Invoke this function via the unary .grad. operator with a right-hand-side, scalar-field operand.

  module procedure grad

    integer c

    associate(dx => (self%x_max_ - self%x_min_)/self%cells_)
      associate(G => gradient_operator_1D_t(self%order_, dx, self%cells_))
        gradient_1D%tensor_1D_t = tensor_1D_t(G .x. self%values_, self%x_min_, self%x_max_, cells=self%cells_, order=self%order_)
        gradient_1D%divergence_operator_1D_ = divergence_operator_1D_t(self%order_, dx, self%cells_)
        check_corbino_castillo_eq_17: &
        associate(p => gradient_1D%weights(), b => [-1D0, [(0D0, c = 1, self%cells_)], 1D0])
          call_julienne_assert((.all. (matmul(transpose(G%assemble()), p) .approximates. b/dx .within. 2D-3)))
        end associate check_corbino_castillo_eq_17
      end associate
    end associate

  end procedure

  ! PURPOSE: To compute mimetic approximations to the Laplacian of a scalar field.
  ! KEYWORDS: Laplacian, differential operator
  ! CONTEXT: Invoke this function via the unary .laplacian. operator with a right-hand-side, scalar-field operand.

  module procedure laplacian

#ifndef __GFORTRAN__
    laplacian_1D%divergence_1D_t = .div. (.grad. self)
#else
    laplacian_1D%divergence_1D_t = div(grad(self))
#endif

    associate(divergence_operator_1D => divergence_operator_1D_t(self%order_, (self%x_max_ - self%x_min_)/self%cells_, self%cells_))
      laplacian_1D%boundary_depth_ = divergence_operator_1D%submatrix_A_rows() + 1
    end associate

  end procedure


  ! PURPOSE: To provide the cell-centered values of scalar quantities.
  ! KEYWORDS: cell centers, staggered grid, scalar field
  ! CONTEXT: Invoke this function via the "values" generic binding to produce discrete scalar values.

  module procedure scalar_1D_values
    cell_centers_extended_values = self%values_
  end procedure

  pure function scalar_1D_grid_locations(x_min, x_max, cells) result(x)
    double precision, intent(in) :: x_min, x_max
    integer, intent(in) :: cells
    double precision, allocatable:: x(:)
    integer cell

    associate(dx => (x_max - x_min)/cells)
      x = [x_min, cell_center_locations(x_min, x_max, cells), x_max]
    end associate
  end function

  ! PURPOSE: To provide the staggered-grid locations at which scalar values are stored: cell centers plus domain boundaries.
  ! KEYWORDS: staggered grid, scalar field, cell centers
  ! CONTEXT: Invoke this function via the "grid" generic binding to produce discrete scalar locations for
  !          initialization-function sampling, printing, or plotting.

  module procedure scalar_1D_grid
    cell_centers_extended  = scalar_1D_grid_locations(self%x_min_, self%x_max_, self%cells_)
  end procedure

end submodule scalar_1D_s
