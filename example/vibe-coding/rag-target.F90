! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

! PROMPT:
! You are a member of the committee that drafted the Fortran 2018 standard and you are
! familiar with vector calculus.
!
! Write a module named rag_integrand_operands_m containing one function named "scalar" 
! and one function named "vector".  Both functions should have the "pure" attribute and
! should have one double precision, one-dimensional (1D) assumed-shape dummy array argument
! named "x" and a deferred-shape array result.  Name the "scalar" function result "f" and
! define the result to be (x**2)/2 in an array statement.  Name the "vector" function result "v"
! and define the result to be x in an array statement.
!
! Write a main program named "rag_target" that uses the only following entities from the
! "formal_m" module:
!   - the scalar_1D_t and and vector_1D_t types,
!   - the scalar_1D_initializer_i and vector_1D_initializer_i abstract intefaces,
! and only the "scalar" and "vector" functions from the rag_integrand_operands_m module.
!
! Declare two procedure pointers: 
!    - one named "scalar_1D_initializer" associated with the "scalar" function
!      and conforming to the "scalar_1D_initializer_i" abstract interface.  
!    - one named "vector_1D_initializer" associated with the the "vector" function
!      and conforming to the "vector_1D_initializer_i" abstract interface.
!
! Use associate statements when defining named entities that will not change after definition.
! Define "f" as an invocation of the user-defined structure constructor scalar_1D_t with the
! actual arguments 
!   - scalar_1D_initializer
!   - order = 4
!   - cells = 200
!   - x_min = 0D0
!   - x_max = 1D0
! Define "v" as an invocation of the user-defined structure constructor vector_1D_t with the
! actual arguments 
!   - vector_1D_initializer
!   - order = 4
!   - cells = 200
!   - x_min = 0D0
!   - x_max = 1D0
!
! Define the differential volume "dV" by invoking the dV type-bound procedure on f.
! Define the differential area "dA" by invoking the dA type-bound procedure on v.
!
! Define one term in the extended Gauss divergence theorem as a volume integral in
! which the integrand is formed from by the dot product of v with the gradient of f.
! Use the defined operation .SSS. to compute the volume integral.  Use the defined
! operations .dot. and .grad. and to compute the dot product and the gradient, 
! respectively.
!
! Define a second term as a volume integral in which the integrand is formed from the
! product of f with the divergence of v.  Use the defined operation .div. to compute
! the divergence.
!  
! Define a third term as a integral in which the integrand is formed by a weighted
! product of f with the dot product of v and dA.  Use the defined operation .x. for the
! weighted product.

! Compute and print a residual formed by summing the two volume integrals and subtracting
! the area integral.

! END PROMPT

! Print each term in the following residual formed from the extended Gauss-divergence
! theorem using one-dimensional (1D) 4th- (default) and 2nd-order mimetic discretizations:
! `residual = .SSS. (v .dot. .grad. f) * dV +.SSS. (f * .div. v) * dV - .SS. (f .x. (v .dot. dA))`
! where `.SSS.` and `.SS.` are the 1D equivalents of a volume integral over the whole
! domain and a surface integral over a domain boundary of unit area, respectively.

! This file is derived from extended-gauss-divergence.F90 by stripping out all details
! that need not be in the code that we desire for the retreival augmented generation (RAG)
! process to generate. 

module rag_integrand_operands_m
  implicit none
contains

  pure function scalar(x) result(f)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: f(:)
    f = (x**2)/2 ! <-- scalar function
  end function

  pure function vector(x) result(v)
    double precision, intent(in) :: x(:)
    double precision, allocatable :: v(:)
    v = x        ! <-- vector function
  end function

end module

program rag_target

  use formal_m, only : scalar_1D_t, scalar_1D_initializer_i, vector_1D_t, vector_1D_initializer_i
  use rag_integrand_operands_m, only : scalar, vector

  implicit none
  procedure(scalar_1D_initializer_i), pointer :: scalar_1D_initializer => scalar
  procedure(vector_1D_initializer_i), pointer :: vector_1D_initializer => vector

  double precision SSS_v_dot_grad_f_dV, SSS_f_div_v_dV, SS_f_v_dot_dA

        integrand_factors: &
        associate( &
           f => scalar_1D_t(scalar_1D_initializer, order=4, cells=200, x_min=0D0, x_max=1D0) &
          ,v => vector_1D_t(vector_1D_initializer, order=4, cells=200, x_min=0D0, x_max=1D0) &
        )
          differential_volume: &
          associate(dV => f%dV())
              SSS_v_dot_grad_f_dV = .SSS. (v .dot. .grad. f) * dV
              SSS_f_div_v_dV      = .SSS. (f * .div. v) * dV
          end associate differential_volume

          differential_area: &
          associate(dA => v%dA())
              SS_f_v_dot_dA     =  .SS. (f .x. (v .dot. dA))
              print '(26x,a,g0,a)',"sum = ", SSS_v_dot_grad_f_dV  +  SSS_f_div_v_dV - SS_f_v_dot_dA, " (residual)"
          end associate differential_area
        end associate integrand_factors

end program
