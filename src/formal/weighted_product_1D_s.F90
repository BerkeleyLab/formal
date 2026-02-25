! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(tensors_1D_m) weighted_product_s
  implicit none

contains

  ! PURPOSE: Computes the discrete surface integral of a weighted_product_1D_t field by summing the
  !          stored boundary-weighted product values that were computed as dx * B * v * f by the
  !          weighted_premultiply procedure.
  ! KEYWORDS: surface-integral, quadrature, mimetic, Corbino-Castillo, weighted-product,
  !           boundary-operator, summation, summation-by-parts
  ! CONTEXT: This procedure implements the .SS. surface integration operator for
  !          weighted_product_1D_t objects in the formal library's mimetic finite-difference
  !          framework. The stored values already incorporate the boundary operator B from Corbino &
  !          Castillo (2020) Eq. 7 and the cell width dx, so the surface integral reduces to a
  !          simple summation of all stored values. This represents the discrete surface integral
  !          term in the extended Gauss divergence theorem, appearing in expressions such as
  !          .SS. (f .x. (v .dot. dA)).
  module procedure surface_integrate_vector_x_scalar_1D
   integral = sum(integrand%values_)    
  end procedure
  ! END CODE CHUNK

end submodule weighted_product_s
