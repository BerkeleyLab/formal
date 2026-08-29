! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module scalar_3D_test_m
  use julienne_m, only : &
     operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.approximates.) &
    ,operator(.within.) &
    ,passing_test &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t &
    ,usher
  use formal_m, only : scalar_3D_t, vector_3D_t, scalar_3D_initializer_i, vector_3D_initializer_i

  implicit none

  type, extends(test_t) :: scalar_3D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  real, parameter :: tolerance = 1E-3
  integer, parameter :: dimensionality = 3

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'The scalar_3D_t derived type'
  end function

  function results() result(test_results)
   type(scalar_3D_test_t) scalar_3D_test
   type(test_result_t), allocatable :: test_results(:)

   test_results = scalar_3D_test%run([ & 
     test_description_t('computing the gradient of a scalar field', usher(check_gradient)) &
   ])
  end function

  pure function rotated_stagnation_point_potential(x,y,z) result(s)
    !! Define a stagnation-point scalar potential in a plane tilted 45 deg from the x-y plane
    real, intent(in), dimension(:) :: x, y, z
    real s(size(x),size(y),size(z))
    real, parameter :: pi = acos(-1E0)
    do concurrent(integer :: j=1:size(y), k=1:size(z)) default(none) shared(x,y,z,s)
      associate(eta => y(j)*cos(pi/4) + z(k)*sin(pi/4)) ! x-eta plane rotated around x axis pi/4 radians from x-y plane
        s(:,j,k) = (x**2 - eta**2)/2 
      end associate
    end do
  end function

  pure function rotated_stagnation_point_velocity(x,y,z) result(gradient)
    !! Define a stagnation-point velocity field as the gradient of the stagantion-point scalar potential:
    !! gradient(x,eta) = [ds/dx,              ds/dy,              ds/dz]
    !!                 = [    x, (ds/deta)(deta/dy), (ds/deta)(deta/dz)]
    !!                 = [    x,   -eta * cos(pi/4),   -eta * sin(pi/4)]
    real, intent(in), dimension(:) :: x, y, z
    real gradient(size(x),size(y),size(z),dimensionality)
    real, parameter :: pi = acos(-1E0)
    do concurrent(integer :: i=1:size(x), j=1:size(y), k=1:size(z)) default(none) shared(gradient,x,y,z)
      associate(eta => y(j)*cos(pi/4) + z(k)*sin(pi/4)) ! x-eta plane rotated around x axis pi/4 radians from x-y plane
        gradient(i,j,k,:) = [x(i), -eta * cos(pi/4), -eta * sin(pi/4)]
      end associate
    end do
  end function

  function check_gradient() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_3D_initializer_i), pointer :: scalar_3D_initializer
    procedure(vector_3D_initializer_i), pointer :: expected_gradient_initializer
    integer order

    scalar_3D_initializer => rotated_stagnation_point_potential
    expected_gradient_initializer => rotated_stagnation_point_velocity
    test_diagnosis = passing_test()

    do order = 2, 4, 2
      associate(scalar_3D => scalar_3D_t(scalar_3D_initializer, order=order, cells=[20,20,20], x_min=[-10E0,-10E0,-10E0], x_max=[10E0,10E0,10E0]))
        associate( &
           grad_scalar => .grad. scalar_3D &
          ,expected_gradient => vector_3D_t(expected_gradient_initializer, mold=scalar_3D) &
        )
          test_diagnosis = test_diagnosis .also. &
            .all. (grad_scalar%to_centers_extended() .approximates. expected_gradient%to_centers_extended() .within. tolerance) &
            // string_t(" for order ") // string_t(order)
        end associate
      end associate
    end do
  end function

end module scalar_3D_test_m