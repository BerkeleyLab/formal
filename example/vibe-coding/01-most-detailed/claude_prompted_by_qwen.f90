module claude_prompted_by_qwen_integrand_operands_m
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

end module

program claude_prompted_by_qwen_target
  use formal_m, only : scalar_1D_t, vector_1D_t, scalar_1D_initializer_i, vector_1D_initializer_i
  use claude_prompted_by_qwen_integrand_operands_m, only : scalar, vector
  implicit none

  procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => scalar
  procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer => vector

  integrand_factors: &
  associate( &
     f => scalar_1D_t(scalar_1D_initializer, order=4, cells=200, x_min=0D0, x_max=1D0) &
    ,v => vector_1D_t(vector_1D_initializer, order=4, cells=200, x_min=0D0, x_max=1D0) &
  )
    differential_elements: &
    associate( &
       dV => f%dV() &
      ,dA => v%dA() &
    )
      volume_integral: &
      associate( &
          volume_term => .SSS. (v .dot. (.grad. f)) * dV &
      )
      end associate volume_integral
    end associate differential_elements
  end associate integrand_factors

end program claude_prompted_by_qwen_target
