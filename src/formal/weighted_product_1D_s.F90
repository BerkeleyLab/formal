! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(tensors_1D_m) weighted_product_s
  implicit none

contains

  module procedure surface_integrate_vector_x_scalar_1D
   integral = sum(integrand%values_)    
  end procedure

end submodule weighted_product_s
