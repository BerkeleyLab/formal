! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module fields_m
  implicit none

  double precision, parameter :: pi = acos(-1D0)

  integer, parameter :: space_dimension = 2

contains

  pure function scalar_field(x,y) result(gaussian)
    double precision, intent(in) :: x(:), y(:)
    double precision gaussian(size(x),size(y))
    double precision, parameter :: x0 = pi/2, y0 = 0D0, sigma = pi/5
    do concurrent(integer :: j=1:size(y)) default(none) shared(x,y,gaussian)
      associate(r => sqrt((x-x0)**2 + (y(j)-y0)**2))
        gaussian(:,j) = exp(-(r**2)/(2*sigma**2))
      end associate
    end do
  end function

  pure function vector_field(x,y) result(potential_vortex)
    double precision, intent(in) :: x(:), y(:)
    double precision potential_vortex(size(x),size(y),space_dimension)
    do concurrent(integer :: i=1:size(x), j=1:size(y)) default(none) shared(x,y,potential_vortex)
      potential_vortex(i,j,:) = [-y(j)/(x(i)**2 + y(j)**2), x(i)/(x(i)**2 + y(j)**2)]
    end do
  end function

end module

program advection_diffusion_2D
  !! Solve the advection-diffusion equation for a passive scalar moving through a static
  !! 2D velocity field.
  use julienne_m, only : file_t
  use fields_m, only : scalar_field, vector_field, pi
  use formal_m, only : scalar_2D_t, vector_2D_t, scalar_2D_initializer_i, vector_2D_initializer_i
  implicit none

  integer, parameter ::  order = 4
  procedure(scalar_2D_initializer_i), pointer :: scalar_initializer
  procedure(vector_2D_initializer_i), pointer :: velocity_initializer
  type(scalar_2D_t) s

  scalar_initializer => scalar_field
  velocity_initializer => vector_field

  s = scalar_2D_t(scalar_initializer, order=order, cells=[10,20], x_min=[-pi,-pi], x_max=[pi,pi])

  associate(v => vector_2D_t(velocity_initializer, mold=s))

    advance_time: &
    block
      double precision :: dt = 1D-6
      !s = s + dt * d_dt(s)
      s = d_dt(s, v)
    end block advance_time

    associate( &
       scalar_file   => s%to_file() &
      ,velocity_file => v%to_file() &
    )
      call   scalar_file%write_lines("example/scripts/scalar.csv")
      call velocity_file%write_lines("example/scripts/velocity.csv")
    end associate

  end associate

contains

  pure function d_dt(s, v) result(ds_dt)
    type(scalar_2D_t), intent(in) :: s
    type(vector_2D_t), intent(in) :: v
    type(scalar_2D_t) ds_dt
    double precision, parameter :: D = 1D0

    associate(grad_s => .grad. s)
      associate( &
         div_D_grad_s => .div. (D * grad_s) &
        ,v_dot_grad_s => v .dot. grad_s &
      )
        ds_dt = s
        !ds_dt = .div. (D * grad_s) - v .dot. grad_s
      end associate
    end associate

  end function

end program