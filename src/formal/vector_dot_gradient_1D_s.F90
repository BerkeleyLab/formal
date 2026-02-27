! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) vector_dot_gradient_1D_s
  use julienne_m, only: call_julienne_assert_, operator(.equalsExpected.)
  implicit none

contains

  ! PURPOSE: Computes the discrete volume integral of a vector_dot_gradient_1D_t field by performing
  !          a weighted sum of the node-centered values using the mimetic quadrature weights inherited
  !          from the gradient field.
  ! KEYWORDS: volume-integral, quadrature, mimetic, vector-dot-gradient, structured-grid,
  !           staggered-grid, weighted-sum, summation-by-parts, node-centered
  ! CONTEXT: This procedure implements the .SSS. volume integration operator for
  !          vector_dot_gradient_1D_t objects in the formal library's mimetic finite-difference
  !          framework. The vector_dot_gradient_1D_t type represents the element-wise dot product of
  !          a vector field with a gradient field, and carries both the m+1 node-centered product
  !          values and the m+1 mimetic quadrature weights inherited from the gradient field. Unlike
  !          the scalar_x_divergence_1D volume integral which requires zero-padding at the
  !          boundaries, this integral is a direct weighted sum because the gradient field's
  !          node-centered values and weights are defined at all m+1 face locations without zero
  !          boundary rows. An assertion verifies that the weights and values arrays have the same
  !          size. This integration is used in compound expressions such as
  !          .SSS. (v .dot. .grad. f) * dV within the extended Gauss divergence theorem test.
  module procedure volume_integrate_vector_dot_grad_scalar_1D
    call_julienne_assert(size(integrand%weights_ ) .equalsExpected. size(integrand%values_))
    integral  = sum(integrand%weights_ * integrand%values_)
  end procedure
  ! END CODE CHUNK

end submodule vector_dot_gradient_1D_s
