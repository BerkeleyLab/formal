! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) scalar_1D_s
  use julienne_m, only : &
    call_julienne_assert_ &
   ,julienne_assert &
   ,operator(//) &
   ,operator(.all.) &
   ,operator(.approximates.) &
   ,operator(.equalsExpected.) &
   ,operator(.csv.) &
   ,operator(.isAtLeast.) &
   ,operator(.greaterThan.) &
   ,operator(.within.) &
   ,string_t
  use interpolator_1D_m, only : faces_to_centers_1d_t
  implicit none

contains


#ifndef __GFORTRAN__

  module procedure construct_1D_scalar_from_function
    call_julienne_assert(x_max .greaterThan. x_min)
    call_julienne_assert(cells .isAtLeast. 2*order)

    associate(values => initializer(cell_centers_extended_1D(x_min, x_max, cells)))
      scalar_1D%tensor_1D_t = tensor_1D_t(values, x_min, x_max, cells, order)
    end associate
    scalar_1D%gradient_operator_1D_ = gradient_operator_1D_t(k=order, dx=(x_max - x_min)/cells, cells=cells)
  end procedure

#else

  pure module function construct_1D_scalar_from_function(initializer, order, cells, x_min, x_max) result(scalar_1D)
    procedure(scalar_1D_initializer_i), pointer :: initializer
    integer, intent(in) :: order !! order of accuracy
    integer, intent(in) :: cells !! number of grid cells spanning the domain
    double precision, intent(in) :: x_min !! grid location minimum
    double precision, intent(in) :: x_max !! grid location maximum
    type(scalar_1D_t) scalar_1D

    call_julienne_assert(x_max .greaterThan. x_min)
    call_julienne_assert(cells .isAtLeast. 2*order)

    associate(values => initializer(cell_centers_extended_1D(x_min, x_max, cells)))
      scalar_1D%tensor_1D_t = tensor_1D_t(values, x_min, x_max, cells, order)
    end associate
    scalar_1D%gradient_operator_1D_ = gradient_operator_1D_t(k=order, dx=(x_max - x_min)/cells, cells=cells)
  end function

  pure function cell_center_locations(x_min, x_max, cells) result(x)
    double precision, intent(in) :: x_min, x_max
    integer, intent(in) :: cells
    double precision, allocatable:: x(:)
    integer cell

    associate(dx => (x_max - x_min)/cells)
      x = x_min + dx/2. + [((cell-1)*dx, cell = 1, cells)]
    end associate
  end function

#endif

  module procedure construct_1D_scalar_constant

    integer i

    call_julienne_assert(x_max .greaterThan. x_min)
    call_julienne_assert(cells .isAtLeast. 2*order)

    scalar_1D = scalar_1D_t( tensor_1D_t( &
         values = [(constant, i = 1, size(cell_centers_extended_1D(x_min, x_max, cells)))] &
        ,x_min = x_min &
        ,x_max = x_max &
        ,cells = cells &
        ,order = order &
    )   )
  end procedure

  module procedure divide_by_integer
    ratio%tensor_1D_t = tensor_1D_t( &
      values = self%values_/denominator, x_min = self%x_min_, x_max = self%x_max_, cells = self%cells_, order = self%order_ &
    )
    ratio%gradient_operator_1D_ = gradient_operator_1D_t( &
      k = self%order_, dx = (self%x_max_ - self%x_min_)/self%cells_, cells = self%cells_ &
    )
  end procedure

  pure logical function conformable(lhs, rhs)
    type(scalar_1D_t), intent(in) :: lhs, rhs
    call_julienne_assert(size(lhs%values_) .equalsExpected. size(rhs%values_))
    call_julienne_assert(.all.([lhs%cells_,lhs%order_] .equalsExpected. [rhs%cells_,rhs%order_]))
    call_julienne_assert(.all.([lhs%x_min_,lhs%x_max_] .approximates. [rhs%x_min_,rhs%x_max_] .within. 1D-08))
    conformable = .true.
  end function

  module procedure subtract_scalar_1D
    call_julienne_assert(conformable(lhs,rhs))
    difference%gradient_operator_1D_ = lhs%gradient_operator_1D_
    difference%tensor_1D_t = &
      tensor_1D_t(values =  lhs%values_ - rhs%values_, x_min = rhs%x_min_, x_max = rhs%x_max_, cells = rhs%cells_, order = rhs%order_)
  end procedure

  module procedure add_scalar_1D
    call_julienne_assert(conformable(lhs,rhs))
    total%gradient_operator_1D_ = lhs%gradient_operator_1D_
    total%tensor_1D_t = &
      tensor_1D_t(values =  lhs%values_ + rhs%values_, x_min = rhs%x_min_, x_max = rhs%x_max_, cells = rhs%cells_, order = rhs%order_)
  end procedure

  module procedure premultiply_double
    lhs_x_rhs%gradient_operator_1D_ = rhs%gradient_operator_1D_
    lhs_x_rhs%tensor_1D_t = &
      tensor_1D_t(values = lhs*rhs%values_, x_min = rhs%x_min_, x_max = rhs%x_max_, cells = rhs%cells_, order = rhs%order_)
  end procedure

  module procedure premultiply_integer
    lhs_x_rhs%gradient_operator_1D_ = rhs%gradient_operator_1D_
    lhs_x_rhs%tensor_1D_t = &
      tensor_1D_t(values = lhs*rhs%values_, x_min = rhs%x_min_, x_max = rhs%x_max_, cells = rhs%cells_, order = rhs%order_)
  end procedure

  module procedure postmultiply_double
    lhs_x_rhs%gradient_operator_1D_ = lhs%gradient_operator_1D_
    lhs_x_rhs%tensor_1D_t = &
      tensor_1D_t(values = rhs*lhs%values_, x_min = lhs%x_min_, x_max = lhs%x_max_, cells = lhs%cells_, order = lhs%order_)
  end procedure

  module procedure postmultiply_integer
    lhs_x_rhs%gradient_operator_1D_ = lhs%gradient_operator_1D_
    lhs_x_rhs%tensor_1D_t = &
      tensor_1D_t(values = rhs*lhs%values_, x_min = lhs%x_min_, x_max = lhs%x_max_, cells = lhs%cells_, order = lhs%order_)
  end procedure

  module procedure exponentiate
    power%tensor_1D_t = tensor_1D_t( &
      values = self%values_**exponent, x_min = self%x_min_, x_max = self%x_max_, cells = self%cells_, order = self%order_ &
    )
    power%gradient_operator_1D_ = gradient_operator_1D_t( &
      k = self%order_, dx = (self%x_max_ - self%x_min_)/self%cells_, cells = self%cells_ &
    )
  end procedure

  module procedure construct_1D_scalar_from_parent

    call_julienne_assert(tensor_1D%is_cell_centers_extended())

    scalar_1D%tensor_1D_t = tensor_1D_t( &
      values = tensor_1D%values_, x_min = tensor_1D%x_min_, x_max = tensor_1D%x_max_, cells = tensor_1D%cells_, order = tensor_1D%order_ &
    )
    scalar_1D%gradient_operator_1D_ = gradient_operator_1D_t( &
      k = tensor_1D%order_, dx = (tensor_1D%x_max_-tensor_1D%x_min_)/tensor_1D%cells_, cells = tensor_1D%cells_ &
    )
  end procedure

  module procedure grad

    integer c

    associate(dx => (self%x_max_ - self%x_min_)/self%cells_)
      gradient_1D%tensor_1D_t = tensor_1D_t(self%gradient_operator_1D_ .x. self%values_, self%x_min_, self%x_max_, cells=self%cells_, order=self%order_)
      gradient_1D%divergence_operator_1D_ = divergence_operator_1D_t(self%order_, dx, self%cells_)
      check_corbino_castillo_eq_17: &
      associate(p => gradient_1D%weights(), b => [-1D0, [(0D0, c = 1, self%cells_)], 1D0])
        call_julienne_assert((.all. (matmul(transpose(self%gradient_operator_1D_%assemble()), p) .approximates. b/dx .within. 2D-3)))
      end associate check_corbino_castillo_eq_17
    end associate

  end procedure

  module procedure d_dx

    associate( &
       dx => (self%x_max_ - self%x_min_)/self%cells_ &
      ,interpolator => faces_to_centers_1D_t(order=self%order_, cells=self%cells_, dx=(self%x_max_ - self%x_min_)/self%cells_) &
    )
      dself_dx%gradient_operator_1D_ = gradient_operator_1D_t(self%order_, dx, self%cells_)
      associate(tensor_1D => &
        tensor_1D_t(dself_dx%gradient_operator_1D_ .x. self%values_, self%x_min_, self%x_max_, cells=self%cells_, order=self%order_) &
      ) 
        dself_dx%tensor_1D_t = &
          tensor_1D_t(interpolator%center_values(tensor_1D%values_), self%x_min_, self%x_max_, cells=self%cells_, order=self%order_)
      end associate
    end associate

    call_julienne_assert(dself_dx%is_cell_centers_extended())

  end procedure

  module procedure d2_dx2
    d2_self_dx2 = d_dx(d_dx(self))
  end procedure

  module procedure laplacian

#ifndef __GFORTRAN__
    laplacian_1D%divergence_1D_t = .div. (.grad. self)
#else
    laplacian_1D%divergence_1D_t = div(grad(self))
#endif

    associate(divergence_operator_1D => divergence_operator_1D_t(self%order_, (self%x_max_ - self%x_min_)/self%cells_, self%cells_))
      laplacian_1D%boundary_depth_ = divergence_operator_1D%submatrix_A_rows() + 1
    end associate

  end procedure

  module procedure scalar_1D_values
    cell_centers_extended_values = self%values_
  end procedure

  module procedure scalar_1D_grid
    cell_centers_extended  = cell_centers_extended_1D(self%x_min_, self%x_max_, self%cells_)
  end procedure

end submodule scalar_1D_s