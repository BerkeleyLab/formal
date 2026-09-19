module time_integration_m
  implicit none

#ifndef HAVE_TEMPLATE_SUPPORT
  logical prevent_empty_module
#else

  private
  public :: runge_kutta_4th_order_t
  public :: binary_operator_r

  requirement binary_operator_r(T, U, V, op)

#if defined(__LFORTRAN__) && (__lfortran_major__ < 1) && (__lfortran_minor__ < 1) && (__lfortran_patch_level__ < 1)
    type, deferred :: T
    type, deferred :: U
    type, deferred :: V

    interface
#else
    deferred type :: T, U, V

    deferred interface
#endif
      pure function op(x,y) result(z)
        type(T), intent(in) :: x
        type(U), intent(in) :: y
        type(V) :: z
      end function
    end interface

  end requirement

#if defined(__LFORTRAN__) && (__lfortran_major__ < 1) && (__lfortran_minor__ < 1) && (__lfortran_patch_level__ < 1)
  template runge_kutta_4th_order_t( &
     TFA, TFB, TFC, TFD, R                    & ! types
    ,tmult, rtmult, taplus, tcplus, rhs, rdiv & ! functions
   )
     type, deferred :: TFA
     type, deferred :: TFB
     type, deferred :: TFC
     type, deferred :: TFD
     type, deferred :: R
#else
  template runge_kutta_4th_order_t{ &
     TFA, TFB, TFC, TFD, R                    & ! types
    ,tmult, rtmult, taplus, tcplus, rhs, rdiv & ! functions
   }
     deferred type :: TFA, TFB, TFC, TFD, R
#endif

     require :: binary_operator_r(integer,     TFC, TFC, tmult)
     require :: binary_operator_r(    TFC,       R, TFB, rtmult)
     require :: binary_operator_r(    TFA,     TFB, TFA, taplus)
     require :: binary_operator_r(    TFC,     TFC, TFC, tcplus)
     require :: binary_operator_r(    TFA,     TFD, TFC, rhs)
     require :: binary_operator_r(      R, integer,   R, rdiv)

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
      end associate; end associate; end associate; end associate
    end function

  end template

#endif

end module
