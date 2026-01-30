! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) dyad_1D_s
  use julienne_m, only : call_julienne_assert_
  implicit none
contains

  module procedure dyad_over_integer
    ratio%tensor_1D_t = tensor_1D_t(self%tensor_1D_t%values_/numerator, self%x_min_, self%x_max_, self%cells_, order = self%order_)
    ratio%divergence_operator_1D_ = self%divergence_operator_1D_
  end procedure

  module procedure construct_1D_dyad_from_components
    dyad_1D%tensor_1D_t = tensor_1D
    dyad_1D%divergence_operator_1D_ = divergence_operator_1D
  end procedure

  module procedure div_dyad

    integer center
   
#ifdef NAGFOR
    associate(D => self%divergence_operator_1D_)
#else
    associate(D => (self%divergence_operator_1D_))
#endif
      call_julienne_assert(size(self%values_))
      associate(Dv => D .x. self%values_)
        divergence_1D%tensor_1D_t = tensor_1D_t(Dv(2:size(Dv)-1), self%x_min_, self%x_max_, self%cells_, self%order_)
        error stop "To Do: interploate the dyad's divergence to the cell faces and construct a vector_1D_t"
#if ASSERTIONS
        associate( &
           q  => divergence_1D%weights() &
          ,dx => (self%x_max_ - self%x_min_)/self%cells_ &
          ,b => [-1D0, [(0D0, center = 1, self%cells_-1)], 1D0] &
        )
          call_julienne_assert(.all. ([size(Dv), size(q)] .equalsExpected. self%cells_+2))
          call_julienne_assert((.all. (matmul(transpose(D%assemble()), q) .approximates. b/dx .within. double_equivalence)))
            ! Check D^T * a = b_{m+1},  Eq. (19), Corbino & Castillo (2020)
        end associate
#endif
      end associate
    end associate

  end procedure

end submodule dyad_1D_s
