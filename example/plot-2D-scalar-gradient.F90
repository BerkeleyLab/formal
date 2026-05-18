! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module scalar_2D_functions_m
  implicit none

  integer, parameter :: space_dimension = 2

contains

  pure function biquadratic(x,y) result(z)
    double precision, intent(in) :: x(:), y(:)
    double precision z(size(x),size(y))
    do concurrent(integer :: j=1:size(y)) default(none) shared(x,y,z)
      z(:,j) = 1 - 2*x + 3*x**2 - x*y(j)/5 + 3*y(j)**2 - 2*y(j)
    end do
  end function

  pure function biquadratic_gradient(x,y) result(gradient)
    double precision, intent(in) :: x(:), y(:)
    double precision gradient(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      gradient(i,j,:) = [-2 + 6*x(i) - y(j)/5, -x(i)/5 + 6*y(j) - 2]
    end do
  end function

end module scalar_2D_functions_m

program scalar_surface
  use julienne_m, only : file_t
  use scalar_2D_functions_m, only : biquadratic, biquadratic_gradient
  use formal_m, only : scalar_2D_t, vector_2D_t, scalar_2D_initializer_i, vector_2D_initializer_i
  implicit none

  procedure(scalar_2D_initializer_i), pointer :: scalar_2D_initializer
  procedure(vector_2D_initializer_i), pointer :: expected_gradient_initializer
  integer, parameter ::  order = 4

  scalar_2D_initializer => biquadratic
  expected_gradient_initializer => biquadratic_gradient

  associate(scalar_2D => scalar_2D_t(scalar_2D_initializer, order=order, cells=[30,20], x_min=[-1D0,1D0], x_max=[9D0,4D0]))
    associate(grad_scalar => .grad. scalar_2D, expected_gradient => vector_2D_t(expected_gradient_initializer, mold=scalar_2D))
      associate(scalar_2D_file => scalar_2D%to_file(), grad_scalar_file => grad_scalar%to_file())
        call scalar_2D_file%write_lines("example/scripts/scalar-surface.csv") 
        call grad_scalar_file%write_lines("example/scripts/gradient-field.csv")
      end associate
    end associate
  end associate

end program scalar_surface