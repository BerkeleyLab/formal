! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module velocity_potential_m 
  implicit none

  integer, parameter :: space_dimension = 2

contains

  pure function potential(x,y) result(phi)
    double precision, intent(in) :: x(:), y(:)
    double precision phi(size(x),size(y))
    do concurrent(integer :: j=1:size(y)) default(none) shared(x,y,phi)
      phi(:,j) = atan(y(j)/x)
    end do
  end function

  pure function velocity(x,y) result(grad_phi)
    double precision, intent(in) :: x(:), y(:)
    double precision grad_phi(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      grad_phi(i,j,:) = [-y(j)/(x(i)**2 + y(j)**2), x(i)/(x(i)**2 + y(j)**2)]
    end do
  end function

end module

program vortex_2D
  use julienne_m, only : file_t
  use velocity_potential_m, only : potential, velocity
  use formal_m, only : scalar_2D_t, vector_2D_t, scalar_2D_initializer_i, vector_2D_initializer_i
  implicit none

  integer, parameter ::  order = 4
  double precision, parameter :: pi = acos(-1D0)
  procedure(scalar_2D_initializer_i), pointer :: scalar_2D_initializer
  procedure(vector_2D_initializer_i), pointer :: vector_2D_initializer

  scalar_2D_initializer => potential
  vector_2D_initializer => velocity

  associate(phi => scalar_2D_t(scalar_2D_initializer, order=order, cells=[11,11], x_min=[-pi,-pi], x_max=[pi,pi]))
    associate(  velocity => .grad. phi & 
      ,expected_velocity => vector_2D_t(vector_2D_initializer, mold=phi) &
    ) 
      associate(velocity_potential_file =>               phi%to_file("phi") &
                         ,velocity_file =>          velocity%to_file("velocity") &
                ,expected_velocity_file => expected_velocity%to_file("expected velocity") &
    )
        call velocity_potential_file%write_lines("example/scripts/velocity-potential.csv") 
        call velocity_file%write_lines("example/scripts/velocity.csv") 
        call expected_velocity_file%write_lines("example/scripts/expected-velocity.csv")
      end associate
    end associate
  end associate

end program