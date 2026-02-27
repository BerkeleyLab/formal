MODULE qwen_integrand_operands_m
  IMPLICIT NONE

CONTAINS

  PURE FUNCTION scalar(x) RESULT(f)
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
    DOUBLE PRECISION, DIMENSION(SIZE(x)) :: f

    f = (x**2)/2
  END FUNCTION scalar

  PURE FUNCTION vector(x) RESULT(v)
    DOUBLE PRECISION, DIMENSION(:), INTENT(IN) :: x
    DOUBLE PRECISION, DIMENSION(SIZE(x)) :: v

    v = x
  END FUNCTION vector

END MODULE qwen_integrand_operands_m


PROGRAM qwen_code
  USE formal_m, ONLY: scalar_1D_t, vector_1D_t, &
                      scalar_1D_initializer_i, vector_1D_initializer_i
  USE qwen_integrand_operands_m, ONLY: scalar, vector

  IMPLICIT NONE

  PROCEDURE(scalar_1D_initializer_i), POINTER :: scalar_1D_initializer => NULL()
  PROCEDURE(vector_1D_initializer_i), POINTER :: vector_1D_initializer => NULL()

  TYPE(scalar_1D_t) :: f
  TYPE(vector_1D_t) :: v
  TYPE(tensor_1D_t) :: dV, dA
  TYPE(vector_dot_gradient_1D_t) :: v_dot_grad_f
  DOUBLE PRECISION :: term1, term2, term3, residual

  ! Associate the procedure pointers with the functions
  scalar_1D_initializer => scalar
  vector_1D_initializer => vector

  ! Define f and v using the user-defined structure constructors
  ASSOCIATE ( &
    order => 4, &
    cells => 200, &
    x_min => 0D0, &
    x_max => 1D0 &
  )
    f = scalar_1D_t(scalar_1D_initializer, order=order, cells=cells, x_min=x_min, x_max=x_max)
    v = vector_1D_t(vector_1D_initializer, order=order, cells=cells, x_min=x_min, x_max=x_max)

    ! Define the differential volume dV and differential area dA
    dV = f%dV()
    dA = v%dA()

    ! Compute the first term in the extended Gauss divergence theorem: ∫ (v · grad(f)) dV
    v_dot_grad_f = v .dot. (.grad. f)
    term1 = .SSS. (v_dot_grad_f * dV)

    ! Compute the second term in the extended Gauss divergence theorem: ∫ (f · div(v)) dV
    term2 = .SSS. (f * (.div. v) * dV)

    ! Compute the third term in the extended Gauss divergence theorem: ∫ (f · (v · dA))
    term3 = .SS. (f * (v .x. dA))

    ! Compute and print the residual
    residual = term1 + term2 - term3
    PRINT *, 'Residual:', residual

  END ASSOCIATE

END PROGRAM qwen_code
