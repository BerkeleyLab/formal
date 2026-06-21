! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

module sink_2D_functions_m
  use julienne_m, only : call_julienne_assert_
  implicit none

  integer, parameter :: space_dimension = 2
  double precision, parameter :: pi = acos(-1D0)

contains

  pure function velocity(x,y) result(v)
    double precision, intent(in) :: x(:), y(:)
    double precision v(size(x),size(y),space_dimension), theta
    double precision, parameter :: Q = 1D0
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      associate(r => sqrt(x(i)**2 + y(j)**2))
        call_julienne_assert(r /= 0D0)
        v(i,j,:) = -(Q/(2*pi))*[x(i), y(j)]/(x(i)**2 + y(j)**2)
      end associate
    end do
  end function

  pure function divergence(x,y) result(div_v)
    double precision, intent(in) :: x(:), y(:)
    double precision div_v(size(x),size(y))
    call_julienne_assert(.not. any(x == 0D0 .and. y == 0D0))
    div_v = 0D0
  end function

end module

program sink_2D
  use julienne_m, only : file_t
  use sink_2D_functions_m, only : velocity, divergence, pi
  use formal_m, only : vector_2D_t, divergence_2D_t, divergence_2D_initializer_i, vector_2D_initializer_i
  implicit none

  integer, parameter ::  order = 4
  procedure(vector_2D_initializer_i), pointer :: vector_2D_initializer
  procedure(divergence_2D_initializer_i), pointer :: divergence_2D_initializer

  divergence_2D_initializer => divergence
  vector_2D_initializer => velocity

  associate(v => vector_2D_t(vector_2D_initializer, order=order, cells=[11,11], x_min=[-1D0,-1D0], x_max=[1D0,1D0]))
    associate(div_v => .div. v, expected_divergence => divergence_2D_t(divergence_2D_initializer, mold=v))
      associate(v_file => v%to_file("v"),div_v_file => div_v%to_file(".div. v"), expected_divergence_file => expected_divergence%to_file("expected .div. v"))
        call v_file%write_lines("example/scripts/sink-velocity.csv") 
        call div_v_file%write_lines("example/scripts/sink-divergence.csv") 
        call expected_divergence_file%write_lines("example/scripts/expected-divergence.csv")
      end associate
    end associate
  end associate

end program