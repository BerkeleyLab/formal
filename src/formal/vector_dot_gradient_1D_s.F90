! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) vector_dot_gradient_1D_s
  use julienne_m, only: call_julienne_assert_, operator(.equalsExpected.)
  implicit none

contains

  ! PURPOSE: To perform mimetic volume integration of a vector/scalar-gradient dot product.
  ! KEYWORDS: triple integral, volume integral
  ! CONTEXT: Invoke this function in expressions of the form .SSS. (v .dot. .grad. f) * dV
  !          with a vector_1D_t v, a scalar f, and a differential volume dV.

  module procedure volume_integrate_vector_dot_grad_scalar_1D
    call_julienne_assert(size(integrand%weights_ ) .equalsExpected. size(integrand%values_))
    integral  = sum(integrand%weights_ * integrand%values_)
  end procedure

end submodule vector_dot_gradient_1D_s
