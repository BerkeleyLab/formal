module claude_integrand_operands_m
  implicit none

contains

  pure function scalar(x) result(f)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: f(:)
    f = (x**2)/2
  end function

  pure function vector(x) result(v)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: v(:)
    v = x
  end function

end module claude_integrand_operands_m


program claude_code
  use formal_m, only: &
      scalar_1D_t, &
      vector_1D_t, &
      scalar_1D_initializer_i, &
      vector_1D_initializer_i
  use claude_integrand_operands_m, only: scalar, vector
  implicit none

  procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer
  procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer

  scalar_1D_initializer => scalar
  vector_1D_initializer => vector

  associate( &
      f => scalar_1D_t(scalar_1D_initializer, order=4, cells=200, x_min=0D0, x_max=1D0), &
      v => vector_1D_t(vector_1D_initializer, order=4, cells=200, x_min=0D0, x_max=1D0)  &
  )
    associate( &
        dV => f%dV(), &
        dA => v%dA()  &
    )
      associate( &
          term1  => .SSS. (v .dot. .grad. f) * dV,         &
          term2  => .SSS. (f * .div. v) * dV,              &
          term3  => .SS.  (f .x. (v .dot. dA))             &
      )
        associate(residual => term1 + term2 - term3)
          print *, "Extended Gauss Divergence Theorem residual: ", residual
        end associate
      end associate
    end associate
  end associate

end program claude_code
