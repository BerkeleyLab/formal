module time_integration_m
  implicit none

#ifndef HAVE_TEMPLATE_SUPPORT
  logical prevent_empty_module
#else

  requirement unary_op_r(op, T, U)

    type, deferred :: T
    type, deferred :: U

    !deferred interface
    interface
      function op(x) result(y)
        type(T), intent(in) :: x
        type(U) :: y
      end function
    end interface

  end requirement

  requirement binary_op_r(op, T, U, V)

    type, deferred :: T
    type, deferred :: U
    type, deferred :: V

    !deferred interface
    interface
      function op(x,y) result(z)
        type(T), intent(in) :: x
        type(U), intent(in) :: y
        type(V) :: z
      end function
    end interface

  end requirement

  template runge_kutta_4th_order_t(    &
     TFA, TFB, TFC, TFD     & ! types
    ,tmult, rtmult, taplus, tcplus, rhs & ! functions
  )
     type, deferred :: TFA
     type, deferred :: TFB
     type, deferred :: TFC
     type, deferred :: TFD

     require :: binary_op_r(integer,     TFC, TFC, tmult) ! report lfortran bug on intrinsic types
     require :: binary_op_r(    TFC,       R, TFB, rtmult)
     require :: binary_op_r(    TFA,     TFB, TFA, taplus)
     require :: binary_op_r(    TFC,     TFC, TFC, tcplus)
     require :: binary_op_r(    TFA,     TFD, TFC, rhs)
     require :: binary_op_r(      R, integer,   R, rdiv)

     interface operator(+)
       module procedure taplus, tcplus
     end interface

     interface operator(*)
       module procedure rtmult, tmult
     end interface

     interface operator(/)
       module procedure rdiv
     end interface

  contains

    pure function step(s, v, dt) result(s_next)
      type(TFA), intent(in) :: s
      type(TFD), intent(in) :: v
      type(R)  , intent(in) :: dt
      type(TFA) s_next
 
      associate(k1 => rhs(s, v))
      associate(k2 => rhs(s + k1*(dt/2), v))
      associate(k3 => rhs(s + k2*(dt/2), v))
      associate(k4 => rhs(s + k3* dt, v))
        s_next = s + (k1 + 2*k2 + 2*k3 + k4) * (dt/6)
       !s_next = s + k1*(dt/6) + k2*(dt/3) + k3*(dt/3) + k4*(dt/6)
      end associate; end associate; end associate; end associate
    end function

  end template

#endif

end module
