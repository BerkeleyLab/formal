! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_2D_m) vector_2D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.all.) &
    ,operator(.equalsExpected.) &
    ,operator(.greaterThan.) &
    ,operator(.isAtLeast.)
  use tensors_1D_m, only : faces_1D
  use differential_operators_1D_m, only : divergence_operator_1D_t
  implicit none

contains

  module procedure construct_2D_vector_from_function

    integer dir

    call_julienne_assert(.all. ([size(cells), size(x_min), size(x_max)] .equalsExpected. space_dimension))
    call_julienne_assert(.all. (x_max .greaterThan. x_min))
    call_julienne_assert(.all. (cells .isAtLeast. 2*order))

    associate(x => faces_1D(x_min(1), x_max(1), cells(1)), y => faces_1D(x_min(2), x_max(2), cells(2)))
      vector_2D%tensor_2D_t = tensor_2D_t(values = initializer(x,y), cells = cells, x_min = x_min, x_max = x_max, order = order)
      vector_2D%divergence_operator_1D_ = [(divergence_operator_1D_t(k=order, dx=((x_max(dir)-x_min(dir))/cells(dir)), cells=cells(dir)), dir=1,space_dimension)]
    end associate
  end procedure

  module procedure construct_2D_vector_from_vector_mold
    integer dir

    call_julienne_assert(.all. ([size(mold%cells_), size(mold%x_min_), size(mold%x_max_)] .equalsExpected. space_dimension))
    call_julienne_assert(.all. (mold%x_max_ .greaterThan. mold%x_min_))
    call_julienne_assert(.all. (mold%cells_ .isAtLeast. 2*mold%order_))

    associate(x => faces_1D(mold%x_min_(1), mold%x_max_(1), mold%cells_(1)), y => faces_1D(mold%x_min_(2), mold%x_max_(2), mold%cells_(2)))
      vector_2D%tensor_2D_t = tensor_2D_t(values = initializer(x,y), cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)
      vector_2D%divergence_operator_1D_ = [(divergence_operator_1D_t(k=mold%order_, dx=((mold%x_max_(dir)-mold%x_min_(dir))/mold%cells_(dir)), cells=mold%cells_(dir)), dir=1,space_dimension)]
    end associate
  end procedure

  module procedure construct_2D_vector_from_scalar_mold
    integer dir

    call_julienne_assert(.all. ([size(mold%cells_), size(mold%x_min_), size(mold%x_max_)] .equalsExpected. space_dimension))
    call_julienne_assert(.all. (mold%x_max_ .greaterThan. mold%x_min_))
    call_julienne_assert(.all. (mold%cells_ .isAtLeast. 2*mold%order_))

    associate(x => faces_1D(mold%x_min_(1), mold%x_max_(1), mold%cells_(1)), y => faces_1D(mold%x_min_(2), mold%x_max_(2), mold%cells_(2)))
      vector_2D%tensor_2D_t = tensor_2D_t(values = initializer(x,y), cells = mold%cells_, x_min = mold%x_min_, x_max = mold%x_max_, order = mold%order_)
      vector_2D%divergence_operator_1D_ = [(divergence_operator_1D_t(k=mold%order_, dx=((mold%x_max_(dir)-mold%x_min_(dir))/mold%cells_(dir)), cells=mold%cells_(dir)), dir=1,space_dimension)]
    end associate
  end procedure

  module procedure vector_2D_values
    vector_values = self%values_        
  end procedure

end submodule vector_2D_s