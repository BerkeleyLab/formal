! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(tensors_1D_m) weighted_product_s
  implicit none

contains

  ! PURPOSE: To perform mimetic surface integration of a scalar/vector product.
  ! KEYWORDS: double integral, surface integral, flux
  ! CONTEXT: Invoke this function in expressions of the form -.SS. (f .x. (v .dot. dA)) 
  !          with a vector_1D_t v, a scalar_1D_t f, and a differential area dA.

  module procedure surface_integrate_vector_x_scalar_1D
   integral = sum(integrand%values_)    
  end procedure

end submodule weighted_product_s
