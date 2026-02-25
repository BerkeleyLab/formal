! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) gradient_1D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.isAtLeast.) &
    ,operator(.within.)
  implicit none

  double precision, parameter :: double_equivalence = 1D-15

contains

  ! PURPOSE: Computes the mimetic quadrature weights for a gradient_1D_t object, returning an array
  !          of m+1 weights where m is the number of cells, with boundary skin weights that ensure
  !          discrete conservation properties and interior weights of 1.0.
  ! KEYWORDS: quadrature-weights, mimetic, gradient, boundary-weights, structured-grid, staggered-grid,
  !           2nd-order, 4th-order, conservation, summation-by-parts, accessor
  ! CONTEXT: This procedure computes the quadrature weights used for discrete integration involving
  !          gradient fields in the formal library's mimetic finite-difference framework. The gradient
  !          lives on a staggered-grid with m+1 node-centered values, so the weights array has size
  !          m+1. Boundary "skin" weights deviate from unity to maintain discrete conservation and
  !          summation-by-parts properties. For 2nd-order discretizations the skin has 2 elements,
  !          while for 4th-order discretizations the skin has 7 elements. The skin is mirrored
  !          symmetrically at both domain boundaries with unity-valued weights filling the interior.
  !          Assertions verify that the grid has sufficient cells to accommodate the skin depth on
  !          both sides and that the resulting weights array has the expected size of cells+1.
  !          Unsupported orders trigger an error stop.
  module procedure gradient_1D_weights

    integer face
    double precision, allocatable :: skin(:)

    select case(self%order_)
    case(2)
      skin = [3/8D0, 9/8D0]
    case(4)
      skin = [227/641D0, 941/766D0, 811/903D0, 1373/1348D0, 1401/1400D0, 36343/36342D0, 943491/943490D0]
    case default
      error stop "unsupported order"
    end select

    associate(depth => size(skin))
      call_julienne_assert(self%cells_ .isAtLeast. 2*depth)
      weights = [skin, [(1D0, face = 1, self%cells_ + 1 - 2*depth)], skin(depth:1:-1) ]
    end associate

    call_julienne_assert(size(weights) .equalsExpected. self%cells_ + 1)

  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Computes the element-wise dot product of a vector_1D field with a gradient_1D field,
  !          producing a new vector_dot_gradient_1D_t object that carries both the multiplied values
  !          and the gradient quadrature weights.
  ! KEYWORDS: dot-product, gradient, vector_1D, operator-overloading, structured-grid, staggered-grid,
  !           mimetic, quadrature-weights, element-wise-multiplication
  ! CONTEXT: This procedure implements the dot product of a vector_1D_t with a gradient_1D_t in the
  !          formal library's operator overloading framework. Both fields must live on the same
  !          staggered-grid with matching sizes, orders, cell counts, and domain bounds, which are
  !          verified via assertions. The element-wise product of the two fields' node-centered values
  !          forms the resulting tensor, which inherits the gradient field's grid metadata and
  !          quadrature weights. The quadrature weights are retrieved via a compiler-conditional call
  !          to either weights() or gradient_1D_weights() to handle gfortran naming differences. The
  !          resulting vector_dot_gradient_1D_t object can then be passed to the volume integration
  !          operator .SSS. as part of compound expressions such as .SSS. (v .dot. .grad. f) * dV.
  module procedure dot

    call_julienne_assert(size(gradient_1D%values_) .equalsExpected. size(vector_1D%values_))
    call_julienne_assert(gradient_1D%order_ .equalsExpected. vector_1D%order_)
    call_julienne_assert(gradient_1D%cells_ .equalsExpected. vector_1D%cells_)
    call_julienne_assert(gradient_1D%x_min_ .approximates.    vector_1D%x_min_ .within. double_equivalence)
    call_julienne_assert(gradient_1D%x_max_ .approximates.    vector_1D%x_max_ .within. double_equivalence)

    vector_dot_gradient_1D%tensor_1D_t = tensor_1D_t(   &
       values = gradient_1D%values_ * vector_1D%values_ &
      ,x_min  = gradient_1D%x_min_ &
      ,x_max  = gradient_1D%x_max_ &
      ,cells  = gradient_1D%cells_ &
      ,order  = gradient_1D%order_ &
    )
#ifndef __GFORTRAN__
    vector_dot_gradient_1D%weights_ = gradient_1D%weights()
#else
    vector_dot_gradient_1D%weights_ = gradient_1D%gradient_1D_weights()
#endif
  end procedure
  ! END CODE CHUNK

end submodule gradient_1D_s
