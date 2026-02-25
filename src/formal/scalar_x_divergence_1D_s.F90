! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) scalar_x_divergence_1D_s
  use julienne_m, only : call_julienne_assert_, operator(.equalsExpected.)
  implicit none

contains

  ! PURPOSE: Computes the discrete volume integral of a scalar_x_divergence_1D_t field by performing
  !          a weighted sum of the cell-centered values zero-padded at the boundaries, using the
  !          mimetic quadrature weights inherited from the divergence field.
  ! KEYWORDS: volume-integral, quadrature, mimetic, scalar-divergence-product, structured-grid,
  !           staggered-grid, weighted-sum, boundary-padding, summation-by-parts
  ! CONTEXT: This procedure implements the .SSS. volume integration operator for
  !          scalar_x_divergence_1D_t objects in the formal library's mimetic finite-difference
  !          framework. The scalar_x_divergence_1D_t type represents the element-wise product of a
  !          scalar field with a divergence field, and carries both the m cell-centered product values
  !          and the m+2 mimetic quadrature weights inherited from the divergence field. The integral
  !          is computed as the dot product of the weights with the values zero-padded at both
  !          boundaries, consistent with the mimetic divergence operator's structure where the first
  !          and last rows are zero. An assertion verifies that the weights array has exactly two more
  !          elements than the values array. This integration is used in compound expressions such as
  !          .SSS. (f * .div. v) * dV within the extended Gauss divergence theorem test.
  module procedure volume_integrate_scalar_x_divergence_1D
    call_julienne_assert(size(integrand%weights_ ) .equalsExpected. size(integrand%values_)+2)
    integral  = sum(integrand%weights_ * [0D0, integrand%values_, 0D0])
  end procedure
  ! END CODE CHUNK

end submodule scalar_x_divergence_1D_s
