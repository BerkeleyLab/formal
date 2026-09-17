module time_integration_m
  implicit none

#ifndef HAVE_TEMPLATE_SUPPORT
  integer dummy
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
     R, TFA, TFB, TFC, TFD     & ! types
    ,rmult, rtmult, tplus, rhs & ! functions
  )
     type, deferred :: R
     type, deferred :: TFA
     type, deferred :: TFB
     type, deferred :: TFC
     type, deferred :: TFD

     require :: binary_op_r(integer,     TFC, TFC, tmult)
     require :: binary_op_r(      R, integer,   R, rdiv)
     require :: binary_op_r(      R,     TFC, TFB, rtmult)
     require :: binary_op_r(    TFA,     TFB, TFA, taplus)
     require :: binary_op_r(    TFC,     TFC, TFC, tcplus)
     require :: binary_op_r(    TFA,     TFD, TFC, rhs)

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
      type(R),   intent(in) :: dt
      type(TFA) s_next
 
      associate(k1 => rhs(s, v)) 
        associate(k2 => rhs(s + (dt/2)*k1, v)) 
          associate(k3 => rhs(s + (dt/2)*k2, v)) 
            associate(k4 => rhs(s + dt*k3, v)) 
              s_next = s + (dt/6)*(k1 + 2*k2 + 2*k3 + k4) 
            end associate
          end associate
        end associate
      end associate
  end function

  end template

#endif

end module
