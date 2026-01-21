! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) scalar_x_divergence_1D_s
  use julienne_m, only : call_julienne_assert_, operator(.equalsExpected.)
  implicit none

contains

  ! PURPOSE: To perform mimetic volume integration of a scalar/divergence dot product.
  ! KEYWORDS: triple integral, volume integral
  ! CONTEXT: Invoke this function in expressions of the form  .SSS. (f * .div. v) * dV
  !          with a vector_1D_t v, a scalar f, and a differential volume dV.

  module procedure volume_integrate_scalar_x_divergence_1D
    call_julienne_assert(size(integrand%weights_ ) .equalsExpected. size(integrand%values_)+2)
    integral  = sum(integrand%weights_ * [0D0, integrand%values_, 0D0])
  end procedure

end submodule scalar_x_divergence_1D_s
