! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(tensors_1D_m) dyad_1D_s
  implicit none
contains

  module procedure dyad_over_integer
    ratio%tensor_1D_t = tensor_1D_t(self%tensor_1D_t%values_/numerator, self%x_min_, self%x_max_, self%cells_, order = self%order_)
  end procedure

end submodule dyad_1D_s
