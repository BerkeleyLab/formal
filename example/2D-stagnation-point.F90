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
      phi(:,j) = (x**2 - y(j)**2)/2
    end do
  end function

  pure function potential_gradient(x,y) result(grad_phi)
    double precision, intent(in) :: x(:), y(:)
    double precision grad_phi(size(x),size(y),space_dimension)
    integer d
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      grad_phi(i,j,:) = [x(i), -y(j)]
    end do
  end function

end module

program stagnation_point_2D
  use julienne_m, only : file_t
  use velocity_potential_m, only : potential, potential_gradient
  use formal_m, only : scalar_2D_t, vector_2D_t, scalar_2D_initializer_i, vector_2D_initializer_i
  implicit none

  procedure(scalar_2D_initializer_i), pointer :: scalar_2D_initializer
  procedure(vector_2D_initializer_i), pointer :: vector_2D_initializer

  scalar_2D_initializer => potential
  vector_2D_initializer => potential_gradient

  associate( &
    phi => scalar_2D_t(scalar_2D_initializer, order=4, cells=[20,20], x_min=[-2D0,-2D0], x_max=[2D0,2D0]) &
  )
    associate( &
       v => .grad. phi &
      ,v_exp => vector_2D_t(vector_2D_initializer, mold=phi) &
    )
      associate( &
         velocity_potential_file => phi%to_file("phi") &
        ,velocity_file           => v%to_file("velocity") &
        ,velocity_expected_file  => v_exp%to_file("expected velocity") &
      )
        block 
          character(len=*), parameter :: path = "example/scripts/"
          call velocity_potential_file%write_lines(path // "velocity-potential.csv")
          call velocity_file%write_lines(path // "velocity.csv")
          call velocity_expected_file%write_lines(path // "expected-velocity.csv")
          print *
          print '(a)', "With gnuplot installed, plot the results by setting your present working" 
          print '(a)', "directory to formal/example/scripts and executing the following commands:" // new_line('')
          print '(a)', 'gnuplot -e "base_name=' // "'velocity'"           // '" 2D-vector-field.gnuplot'
          print '(a)', 'gnuplot -e "base_name=' // "'expected-velocity'"  // '" 2D-vector-field.gnuplot'
          print '(a)', 'gnuplot -e "base_name=' // "'velocity-potential'" // '" 2D-scalar-field.gnuplot' // new_line('')
        end block
      end associate
    end associate
  end associate

end program
