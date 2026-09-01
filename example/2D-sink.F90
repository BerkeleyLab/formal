! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

module sink_2D_functions_m
  use julienne_m, only : call_julienne_assert_
  implicit none

  integer, parameter :: space_dimension = 2
  real, parameter :: pi = acos(-1E0)

contains

  pure function velocity(x,y) result(v)
    real, intent(in) :: x(:), y(:)
    real v(size(x),size(y),space_dimension)
    real, parameter :: Q = 1E0
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      associate(r => sqrt(x(i)**2 + y(j)**2))
        call_julienne_assert(r /= 0E0)
        v(i,j,:) = -(Q/(2*pi))*[x(i), y(j)]/(x(i)**2 + y(j)**2)
      end associate
    end do
  end function

  pure function divergence(x,y) result(div_v)
    real, intent(in) :: x(:), y(:)
    real div_v(size(x),size(y))
    call_julienne_assert(.not. any(x == 0E0 .and. y == 0E0))
    div_v = 0E0
  end function

end module

program sink_2D
  use sink_2D_functions_m, only : velocity, divergence
  use formal_m, only : vector_2D_t, divergence_2D_t, divergence_2D_initializer_i, vector_2D_initializer_i
  implicit none

  integer, parameter ::  order = 4
  procedure(vector_2D_initializer_i), pointer :: vector_2D_initializer
  procedure(divergence_2D_initializer_i), pointer :: divergence_2D_initializer

  divergence_2D_initializer => divergence
  vector_2D_initializer => velocity

  associate(v => vector_2D_t(vector_2D_initializer, order=order, cells=[11,11], x_min=[-1E0,-1E0], x_max=[1E0,1E0]))
    associate(div_v => .div. v, expected_divergence => divergence_2D_t(divergence_2D_initializer, mold=v))
      associate(v_file => v%to_file("v"),div_v_file => div_v%to_file(".div. v"), expected_divergence_file => expected_divergence%to_file("expected .div. v"))
        call v_file%write_lines("example/scripts/sink-velocity.csv") 
        call div_v_file%write_lines("example/scripts/sink-divergence.csv") 
        call expected_divergence_file%write_lines("example/scripts/expected-divergence.csv")
      end associate
    end associate
  end associate

end program