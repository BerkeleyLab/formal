module explicit_euler_m
  implicit none

#ifndef HAVE_TEMPLATE_SUPPORT
  logical prevent_empty_module
#else

  private
  public :: explicit_euler_tmpl
  public :: binary_operator_r
  public :: assign_incremented_s_r

  requirement binary_operator_r(T, U, V, op)
#if defined(__LFORTRAN__) && (__lfortran_major__ < 1) && (__lfortran_minor__ < 67)
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

  requirement defined_assignment_r(lhs_t, rhs_t, assign)
#if defined(__LFORTRAN__) && (__lfortran_major__ < 1) && (__lfortran_minor__ < 67)
    type, deferred :: lhs_t
    type, deferred :: rhs_t

    interface
#else
    deferred type :: lhs, rhs

    deferred interface
#endif
      elemental subroutine assign(lhs, rhs)
        type(lhs_t), intent(out) :: lhs
        type(rhs_t), intent(in) :: rhs
      end subroutine

    end interface
  end requirement

#if defined(__LFORTRAN__) && (__lfortran_major__ < 1) && (__lfortran_minor__ < 67)
  template explicit_euler(s_t, v_t, dt_t, rhs_t, rhs_dt_t, rhs_f, rhs_dt_plus_s_t, rhs_times_dt_f, rhs_dt_plus_s_f, define_s_next_s)
    type, deferred :: s_t
    type, deferred :: v_t
    type, deferred :: dt_t
    type, deferred :: rhs_t
    type, deferred :: rhs_dt_t
    type, deferred :: rhs_dt_plus_s_t
    type, deferred :: s_next_t
#else
  template explicit_euler_tmpl{s_t, v_t, dt_t, rhs_t, s_next_t, rhs_f, define_s_next_s}
    deferred type :: s_t, v_t, dt_t, rhs_t, s_next_t
#endif

     require :: binary_operator_r(     s_t,  v_t,           rhs_t,           rhs_f)
     require :: binary_operator_r(   rhs_t, dt_t,        rhs_dt_t,  rhs_times_dt_f)
     require :: binary_operator_r(rhs_dt_t,  s_t, rhs_dt_plus_s_t, rhs_dt_plus_s_f)

     require :: defined_assignment_r(   s_next_t, rhs_dt_plus_s_t, define_s_next_s)

     interface operator(*)
       module procedure rhs_times_dt_f
     end interface

     interface operator(+)
       module procedure rhs_dt_plus_s_f
     end interface

     interface assignment(=)
       module procedure define_s_next_s
     end interface

  contains

    pure function euler_step(s, v, dt) result(s_next)
      type(s_t), intent(in) :: s
      type(v_t), intent(in) :: v
      type(dt_t), intent(in) :: dt
      type(s_next_t) s_next
      s_next = rhs_f(s, v) * dt + s 
    end function

  end template

#endif

end module explicit_euler_m
