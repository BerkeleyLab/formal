! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module fields_m
  implicit none

  integer, parameter :: space_dimension = 2

contains

  pure function scalar_field(x,y) result(gaussian)
    double precision, intent(in) :: x(:), y(:)
    double precision gaussian(size(x),size(y))
    double precision, parameter :: pi = acos(-1D0)
    double precision, parameter :: x0 = 1D0, y0 = 1D0, sigma = pi/5
    do concurrent(integer :: j=1:size(y)) default(none) shared(x,y,gaussian)
      associate(r => sqrt((x-x0)**2 + (y(j)-y0)**2))
        gaussian(:,j) = exp(-(r**2)/(2*sigma**2))
      end associate
    end do
  end function

  pure function stagnation_point_velocity(x,y) result(grad_phi)
    double precision, intent(in) :: x(:), y(:)
    double precision grad_phi(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      grad_phi(i,j,:) = [x(i), -y(j)]
    end do
  end function

end module

program advection_diffusion_2D
  !! Solve the advection-diffusion equation for a passive scalar moving through a static
  !! 2D velocity field.
  use julienne_m, only : file_t
  use fields_m, only : scalar_field, stagnation_point_velocity
  use formal_m, only : scalar_2D_t, vector_2D_t, scalar_2D_initializer_i, vector_2D_initializer_i
  implicit none

  integer, parameter ::  order = 4
  procedure(scalar_2D_initializer_i), pointer :: scalar_2D_initializer
  procedure(vector_2D_initializer_i), pointer :: velocity_2D_initializer
  type(scalar_2D_t) s

  scalar_2D_initializer => scalar_field
  velocity_2D_initializer => stagnation_point_velocity

  s = scalar_2D_t(scalar_2D_initializer, order=4, cells=[20,20], x_min=[-2D0,-2D0], x_max=[2D0,2D0])

  associate(v => vector_2D_t(velocity_2D_initializer, mold=s))

    advance_time: &
    block
      double precision :: dt = 1D-6
      associate(s_half => s + (dt/2) * d_dt(s, v))
        s = s + dt * d_dt(s_half, v)
      end associate
    end block advance_time

    associate( &
       scalar_file   => s%to_file("scalar") &
      ,velocity_file => v%to_file("vector") &
    )
      call   scalar_file%write_lines("example/scripts/scalar-adv-dif.csv")
      call velocity_file%write_lines("example/scripts/velocity-adv-dif.csv")
    end associate

  end associate

contains

  pure function d_dt(s, v) result(ds_dt)
    type(scalar_2D_t), intent(in) :: s
    type(vector_2D_t), intent(in) :: v
    type(scalar_2D_t) ds_dt
    double precision, parameter :: D = 1D0
    !ds_dt = .div. (D * .grad. s) - (v .dot. .grad. s)
    !ds_dt = .div. (D * .grad. s) - .div. (v * s)
    associate(ds_dt => .div.(v * s))
    end associate
  end function

end program
