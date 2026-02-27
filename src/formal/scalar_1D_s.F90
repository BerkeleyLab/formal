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

  ! PURPOSE: Constructs a scalar_1D_t object by evaluating a user-provided initializer function on
  !          the extended grid (boundary + cell-center locations) and storing the resulting values
  !          along with grid metadata and a pre-built gradient operator for the specified order of
  !          accuracy and cell count.
  ! KEYWORDS: scalar_1D, construction, initializer, structured-grid, staggered-grid, gradient-operator,
  !           finite-difference, mimetic, cell-centered, boundary-values
  ! CONTEXT: This procedure constructs a scalar_1D_t object in the formal library's mimetic
  !          finite-difference framework. The scalar field is initialized on an extended grid
  !          consisting of the two domain boundary points plus all cell-center locations, yielding
  !          m+2 values where m is the number of cells. The gradient_operator_1D_t is pre-built and
  !          stored within the scalar object so that subsequent calls to the .grad. operator can
  !          apply it without reconstruction. Assertions verify that x_max > x_min and that the cell
  !          count is at least 2*order to support the mimetic stencil width. This version is compiled
  !          for non-gfortran compilers; gfortran uses an alternate definition below due to
  !          differences in procedure pointer handling in module procedure definitions.
  module procedure construct_1D_scalar_from_function
    call_julienne_assert(x_max .greaterThan. x_min)
    call_julienne_assert(cells .isAtLeast. 2*order)

    associate(values => initializer(scalar_1D_grid_locations(x_min, x_max, cells)))
      scalar_1D%tensor_1D_t = tensor_1D_t(values, x_min, x_max, cells, order)
    end associate
    scalar_1D%gradient_operator_1D_ = gradient_operator_1D_t(k=order, dx=(x_max - x_min)/cells, cells=cells)
  end procedure
  ! END CODE CHUNK

#else

  ! PURPOSE: Constructs a scalar_1D_t object by evaluating a user-provided initializer function on
  !          the extended grid (boundary + cell-center locations) and storing the resulting values
  !          along with grid metadata and a pre-built gradient operator for the specified order of
  !          accuracy and cell count. This is the gfortran-specific variant with an explicit function
  !          signature.
  ! KEYWORDS: scalar_1D, construction, initializer, structured-grid, staggered-grid, gradient-operator,
  !           finite-difference, mimetic, cell-centered, boundary-values, gfortran
  ! CONTEXT: This function is the gfortran-specific variant of construct_1D_scalar_from_function in
  !          the formal library's mimetic finite-difference framework. It provides the same
  !          functionality as the non-gfortran module procedure but uses an explicit function
  !          signature rather than a module procedure definition, working around gfortran limitations
  !          with procedure pointer arguments in module procedure definitions. The scalar field is
  !          initialized on an extended grid of m+2 values and the gradient_operator_1D_t is
  !          pre-built and stored for subsequent .grad. operator applications. Assertions verify
  !          that x_max > x_min and that the cell count is at least 2*order.
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
  ! END CODE CHUNK

  ! PURPOSE: Computes the cell-center x-coordinates for a uniform 1D grid given the domain bounds
  !          and number of cells, returning an array of cell-center locations offset by half a cell
  !          width from x_min.
  ! KEYWORDS: grid, cell-center, uniform-mesh, 1D, structured-grid, staggered-grid, utility, gfortran
  ! CONTEXT: This helper function is only compiled under gfortran and provides cell-center coordinate
  !          computation needed by the scalar_1D submodule. It constructs a uniform grid with cell
  !          width dx = (x_max - x_min)/cells and places each cell center at x_min + dx/2 + (cell-1)*dx
  !          using an implied do loop. Other compilers use a cell_center_locations function defined
  !          elsewhere. This function is used by scalar_1D_grid_locations to build the extended grid
  !          array that includes both boundary points and cell centers.
  pure function cell_center_locations(x_min, x_max, cells) result(x)
    double precision, intent(in) :: x_min, x_max
    integer, intent(in) :: cells
    double precision, allocatable:: x(:)
    integer cell

    associate(dx => (x_max - x_min)/cells)
      x = x_min + dx/2. + [((cell-1)*dx, cell = 1, cells)]
    end associate
  end function
  ! END CODE CHUNK

#endif

  ! PURPOSE: Computes the discrete gradient of the scalar_1D field by applying the mimetic gradient
  !          operator to the stored values, producing a gradient_1D_t object that carries the
  !          node-centered gradient values, grid metadata, a pre-built divergence operator, and
  !          verified quadrature weights satisfying the Corbino & Castillo (2020) Eq. 17 identity.
  ! KEYWORDS: gradient, mimetic, operator-application, Corbino-Castillo, structured-grid,
  !           staggered-grid, scalar_1D, gradient_1D, divergence-operator, quadrature-weights,
  !           summation-by-parts, verification
  ! CONTEXT: This procedure implements the .grad. operator for scalar_1D_t objects in the formal
  !          library's mimetic finite-difference framework. It constructs a gradient_operator_1D_t
  !          from the scalar's order and grid spacing, applies it to the scalar's m+2 extended values
  !          to produce m+1 node-centered gradient values, and stores a divergence_operator_1D_t
  !          within the resulting gradient_1D_t for subsequent use in compound expressions like
  !          .div. (.grad. f). After construction, an assertion verifies the Corbino & Castillo (2020)
  !          Eq. 17 identity relating the transpose of the assembled gradient matrix, the gradient
  !          quadrature weights, and the boundary vector b = [-1, 0, ..., 0, 1], ensuring the
  !          discrete operator satisfies the required summation-by-parts property.
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
  ! END CODE CHUNK

  ! PURPOSE: Computes the discrete Laplacian of the scalar_1D field by composing the divergence and
  !          gradient operators, and determines the boundary depth at which the Laplacian has
  !          reduced-order accuracy.
  ! KEYWORDS: laplacian, divergence, gradient, div-grad, mimetic, operator-composition,
  !           structured-grid, staggered-grid, scalar_1D, boundary-depth, reduced-order
  ! CONTEXT: This procedure implements the .laplacian. operator for scalar_1D_t objects in the formal
  !          library's mimetic finite-difference framework. It computes the Laplacian as the
  !          composition .div. (.grad. self), with a compiler-conditional workaround for gfortran
  !          that calls div(grad(self)) as named function calls rather than using the overloaded
  !          operator syntax. The resulting laplacian_1D_t wraps a divergence_1D_t and additionally
  !          stores the boundary depth where the Laplacian exhibits reduced-order accuracy, computed
  !          as the number of rows in the divergence operator's upper boundary block A plus one. This
  !          boundary depth information is used by convergence tests to separately assess interior
  !          and boundary error behavior.
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
  ! END CODE CHUNK

  ! PURPOSE: Returns the extended cell-centered values stored in a scalar_1D_t object, including both
  !          boundary values and interior cell-center values.
  ! KEYWORDS: scalar_1D, accessor, cell-centered-values, extended-values, getter, boundary-values
  ! CONTEXT: This procedure is a simple accessor that exposes the internally stored m+2 extended
  !          values from a scalar_1D_t object in the formal library. The extended values include the
  !          two domain boundary values at x_min and x_max plus the m interior cell-center values.
  !          These values are used by other operators and test functions for computation and
  !          verification.
  module procedure scalar_1D_values
    cell_centers_extended_values = self%values_
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Computes the extended grid locations for a scalar_1D field, consisting of the two domain
  !          boundary points bracketing the cell-center locations, returning an array of m+2 values.
  ! KEYWORDS: grid, cell-center, boundary-points, extended-grid, uniform-mesh, 1D, structured-grid,
  !           staggered-grid, utility
  ! CONTEXT: This private helper function constructs the extended grid array used by the scalar_1D
  !          submodule in the formal library. The extended grid places the domain boundary points
  !          x_min and x_max at the first and last positions, with the m cell-center locations from
  !          cell_center_locations filling the interior. This m+2 layout matches the scalar_1D_t
  !          value storage convention where boundary ghost values bookend the cell-centered data.
  !          This function is called by construct_1D_scalar_from_function during initialization and
  !          by scalar_1D_grid to provide grid coordinates.
  pure function scalar_1D_grid_locations(x_min, x_max, cells) result(x)
    double precision, intent(in) :: x_min, x_max
    integer, intent(in) :: cells
    double precision, allocatable:: x(:)
    integer cell

    associate(dx => (x_max - x_min)/cells)
      x = [x_min, cell_center_locations(x_min, x_max, cells), x_max]
    end associate
  end function
  ! END CODE CHUNK

  ! PURPOSE: Returns the extended grid locations for a scalar_1D_t object by delegating to the
  !          scalar_1D_grid_locations helper function.
  ! KEYWORDS: grid, cell-center, extended-grid, accessor, scalar_1D, structured-grid, staggered-grid,
  !           getter
  ! CONTEXT: This procedure provides access to the extended grid coordinates associated with a
  !          scalar_1D_t object in the formal library. It delegates to scalar_1D_grid_locations,
  !          passing the stored domain bounds and cell count. The returned array of m+2 locations
  !          includes the boundary points and cell centers. Test functions and other operators use
  !          this accessor to retrieve grid coordinates for constructing spatially-varying expected
  !          values when verifying operator results.
  module procedure scalar_1D_grid
    cell_centers_extended  = scalar_1D_grid_locations(self%x_min_, self%x_max_, self%cells_)
  end procedure
  ! END CODE CHUNK

end submodule scalar_1D_s
