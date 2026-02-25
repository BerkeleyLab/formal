! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) divergence_1D_s
  use julienne_m, only : &
     call_julienne_assert_ & 
    ,operator(.equalsExpected.) &
    ,operator(.isAtLeast.)
  implicit none

contains

#ifdef __GFORTRAN__

  ! PURPOSE: Computes the cell-center x-coordinates for a uniform 1D grid given the domain bounds
  !          and number of cells, returning an array of cell-center locations offset by half a cell
  !          width from x_min.
  ! KEYWORDS: grid, cell-center, uniform-mesh, 1D, structured-grid, staggered-grid, utility, gfortran
  ! CONTEXT: This helper function is only compiled under gfortran and provides cell-center coordinate
  !          computation needed by the divergence_1D submodule. It constructs a uniform grid with cell
  !          width dx = (x_max - x_min)/cells and places each cell center at x_min + dx/2 + (cell-1)*dx
  !          using an implied do loop. Other compilers may provide this functionality through a different
  !          code path or type-bound procedure. This function is used by divergence_1D_grid to return
  !          the grid coordinates associated with a divergence_1D_t object.
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

  ! PURPOSE: Computes the element-wise product of a scalar_1D field's interior values with a
  !          divergence_1D field's cell-centered values, producing a new scalar_x_divergence_1D_t
  !          object that carries both the multiplied values and the divergence quadrature weights.
  ! KEYWORDS: scalar-multiplication, divergence, operator-overloading, premultiply, scalar_1D,
  !           divergence_1D, structured-grid, staggered-grid, quadrature-weights, interior-values
  ! CONTEXT: This procedure implements the left-multiplication of a scalar_1D_t by a divergence_1D_t
  !          in the formal library's operator overloading framework. The scalar field has two extra
  !          boundary values compared to the divergence field's cell-centered values, so the scalar's
  !          interior slice (index 2 through size-1) is multiplied element-wise with the divergence
  !          values. The result inherits the scalar field's grid metadata and the divergence field's
  !          quadrature weights, with a compiler-conditional call to either weights() or
  !          divergence_1D_weights() to handle gfortran naming differences. Assertions verify that
  !          the scalar field has exactly two more values than the divergence field and that the
  !          resulting weights array has the expected size.
  module procedure premultiply_scalar_1D
    call_julienne_assert(size(scalar_1D%values_) .equalsExpected. size(divergence_1D%values_) + 2)
    scalar_x_divergence_1D%tensor_1D_t = &
       tensor_1D_t(scalar_1D%values_(2:size(scalar_1D%values_)-1) * divergence_1D%values_, scalar_1D%x_min_, scalar_1D%x_max_, scalar_1D%cells_, scalar_1D%order_)
#ifndef __GFORTRAN__
    scalar_x_divergence_1D%weights_ = divergence_1D%weights() 
#else
    scalar_x_divergence_1D%weights_ = divergence_1D%divergence_1D_weights() 
#endif
    call_julienne_assert(size(scalar_x_divergence_1D%weights_) .equalsExpected. size(divergence_1D%values_)+2)
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Delegates right-multiplication of a divergence_1D field by a scalar_1D field to the
  !          premultiply_scalar_1D procedure, ensuring commutativity of scalar-divergence
  !          multiplication.
  ! KEYWORDS: scalar-multiplication, divergence, operator-overloading, postmultiply, commutativity,
  !           scalar_1D, divergence_1D, delegation
  ! CONTEXT: This procedure implements the right-multiplication form (divergence * scalar) by
  !          delegating to premultiply_scalar_1D (scalar * divergence) in the formal library's
  !          operator overloading framework. Since scalar-divergence multiplication is commutative,
  !          this thin wrapper avoids duplicating the multiplication logic and ensures both operator
  !          orderings produce identical results.
  module procedure postmultiply_scalar_1D
    scalar_x_divergence_1D = premultiply_scalar_1D(scalar_1D, divergence_1D) 
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Returns the cell-centered divergence values stored in a divergence_1D_t object.
  ! KEYWORDS: divergence, accessor, cell-centered-values, divergence_1D, getter
  ! CONTEXT: This procedure is a simple accessor that exposes the internally stored cell-centered
  !          divergence values from a divergence_1D_t object. It is used by test functions and other
  !          operators in the formal library to retrieve computed divergence data for comparison
  !          against analytical expectations or for use in compound operator expressions.
  module procedure divergence_1D_values
    cell_centered_values = self%values_
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Returns the cell-center x-coordinates of the 1D grid associated with a divergence_1D_t
  !          object by delegating to the cell_center_locations helper function.
  ! KEYWORDS: grid, cell-center, accessor, divergence_1D, structured-grid, staggered-grid, getter
  ! CONTEXT: This procedure provides access to the grid coordinates associated with a divergence_1D_t
  !          object in the formal library. It delegates to the cell_center_locations function, passing
  !          the stored domain bounds and cell count. Test functions use this accessor to retrieve
  !          grid coordinates for constructing spatially-varying expected values when verifying
  !          divergence operator results.
  module procedure divergence_1D_grid
    cell_centers = cell_center_locations(self%x_min_, self%x_max_, self%cells_)
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Computes the mimetic quadrature weights for a divergence_1D_t object, returning an
  !          array of m+2 weights where m is the number of cells, with boundary skin weights that
  !          ensure discrete conservation properties and interior weights of 1.0.
  ! KEYWORDS: quadrature-weights, mimetic, divergence, boundary-weights, Corbino-Castillo,
  !           structured-grid, staggered-grid, 2nd-order, 4th-order, conservation, summation-by-parts
  ! CONTEXT: This procedure computes the quadrature weights used for discrete integration involving
  !          divergence fields in the formal library's mimetic finite-difference framework. The
  !          weights follow the formulation of Corbino & Castillo (2020) Eqs. 14-15 and 19, where
  !          boundary "skin" weights deviate from unity to maintain discrete conservation and
  !          summation-by-parts properties. For 2nd-order discretizations the skin is empty (all
  !          weights are 1.0), while for 4th-order discretizations an 8-element skin array provides
  !          the boundary corrections. The skin is mirrored symmetrically at both domain boundaries.
  !          Assertions verify that the grid has sufficient cells to accommodate the skin depth on
  !          both sides and that the resulting weights array has the expected size of cells+2.
  !          Unsupported orders trigger an error stop.
  module procedure divergence_1D_weights
      integer c 

      double precision, allocatable :: skin(:)

      select case(self%order_)
      case(2)
        skin = [double precision::]
      case(4)
        skin = [1D0, 2186/1943D0, 1992/2651D0, 1993/1715D0, 649/674D0, 699/700D0, 18170/18171D0, 471744/471745D0]
      case default
        error stop "unsupported order"
      end select

      associate(depth => size(skin))
        weights = [skin, [(1D0, c = depth+1, self%cells_+2-depth)], skin(depth:1:-1) ] ! m+2 values, where m = self%cells_
      end associate                                                                    ! cf. Corbino & Castillo (2020) Eqs. 14-15 & 19

      call_julienne_assert(self%cells_ .isAtLeast. 2*size(skin))
      call_julienne_assert(size(weights) .equalsExpected. self%cells_+2)
  end procedure
  ! END CODE CHUNK

end submodule divergence_1D_s
