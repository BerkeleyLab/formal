! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module fields_m
  implicit none

  integer, parameter :: space_dimension = 2

contains

  pure function scalar_field(x,y) result(gaussian)
    real, intent(in) :: x(:), y(:)
    real gaussian(size(x),size(y))
    real, parameter :: pi = acos(-1E0)
    real, parameter :: x0 = -pi/2, y0 = -pi/2, sigma = pi/8
    do concurrent(integer :: j=1:size(y)) default(none) shared(x,y,gaussian)
      associate(r => sqrt((x-x0)**2 + (y(j)-y0)**2))
        gaussian(:,j) = exp(-(r**2)/(2*sigma**2))
      end associate
    end do
  end function

  pure function taylor_green_velocity(x,y) result(velocity)
    real, intent(in) :: x(:), y(:)
    real velocity(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y))
      velocity(i,j,:) = [10*sin(x(i))*cos(y(j)), -10*cos(x(i))*sin(y(j))]
    end do
  end function

end module

program advection_diffusion_2D
  !! Solve the advection-diffusion equation for a passive scalar moving through a
  !! static velocity field define by 2D Taylor-Green vortices.
  use fields_m, only : scalar_field, taylor_green_velocity
  use formal_m, only : scalar_2D_t, vector_2D_t, scalar_2D_initializer_i, vector_2D_initializer_i
  implicit none

  integer, parameter ::  order = 4
  procedure(scalar_2D_initializer_i), pointer :: scalar_2D_initializer
  procedure(vector_2D_initializer_i), pointer :: velocity_2D_initializer
  type(scalar_2D_t) s
  real, parameter :: pi = acos(-1E0)

  scalar_2D_initializer => scalar_field
  velocity_2D_initializer => taylor_green_velocity

  s = scalar_2D_t(scalar_2D_initializer, order=4, cells=[51,51], x_min=[-pi,-pi], x_max=[pi,pi])

  associate(v => vector_2D_t(velocity_2D_initializer, mold=s))

    associate( &
       scalar_file   => s%to_file("scalar") &
      ,velocity_file => v%to_file("velocity") &
    )
      call   scalar_file%write_lines("example/scripts/scalar-initial.csv")
      call velocity_file%write_lines("example/scripts/velocity.csv")
    end associate

    advance_time: &
    block
      real :: dt = 1E-4
      integer step

      do step = 1, 500
        associate(k1 => d_dt(s, v))
          associate(k2 => d_dt(s + (dt/2)*k1, v))
            associate(k3 => d_dt(s + (dt/2)*k2, v))
              associate(k4 => d_dt(s + dt*k3, v))
                s = s + (dt/6)*(k1 + 2*k2 + 2*k3 + k4)
              end associate
            end associate
          end associate
        end associate
      end do
    end block advance_time

    associate(scalar_file => s%to_file("scalar"))
      call scalar_file%write_lines("example/scripts/scalar-final.csv")
    end associate

  end associate

contains

  pure function d_dt(s, v) result(ds_dt)
    type(scalar_2D_t), intent(in) :: s
    type(vector_2D_t), intent(in) :: v
    type(scalar_2D_t) ds_dt
    real, parameter :: D = 0.5E0
    ds_dt = .div. (D * .grad. s) - .div. (v * s)
  end function

end program
