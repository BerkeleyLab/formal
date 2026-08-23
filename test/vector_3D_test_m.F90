! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

module vector_3D_test_m
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(//) &
    ,operator(.all.) &
    ,operator(.also.) &
    ,operator(.approximates.) &
    ,operator(.equalsExpected.) &
    ,operator(.within.) &
    ,passing_test &
    ,string_t &
    ,test_description_t &
    ,test_diagnosis_t &
    ,test_result_t &
    ,test_t &
    ,usher 
  use formal_m, only : &
     scalar_3D_t &
    ,scalar_3D_initializer_i &
    ,vector_3D_t &
    ,vector_3D_initializer_i &
    ,divergence_3D_t &
    ,divergence_3D_initializer_i &
    ,x_dir &
    ,y_dir &
    ,z_dir

  implicit none

  type, extends(test_t) :: vector_3D_test_t
  contains
    procedure, nopass :: subject
    procedure, nopass :: results
  end type

  integer, parameter :: space_dimension = 2
  double precision, parameter :: tolerance = 1D-13
  double precision, parameter :: u_const(*) = [1D0,3D0], v_const(*) = [3D0,4D0], u_dot_v_exact = dot_product(u_const, v_const)

contains

  pure function subject() result(test_subject)
    character(len=:), allocatable :: test_subject
    test_subject = 'The vector_3D_t derived type'
  end function

  function results() result(test_results)
   type(vector_3D_test_t) vector_3D_test
   type(test_result_t), allocatable :: test_results(:)

   test_results = vector_3D_test%run([ &
      test_description_t('computing the divergence of a vector field', usher(check_divergence)) &
     ,test_description_t('computing the dot product of two vector fields', usher(check_dot_product)) &
     ,test_description_t('computing the product of a vector field and a scalar', usher(check_vector_scalar_product)) &
   ])
  end function

  pure function rotated_stagnation_point_potential(x,y,z) result(s)
    !! Define a stagnation-point scalar potential in a plane tilted 45 deg from the x-y plane
    double precision, intent(in), dimension(:) :: x, y, z
    double precision s(size(x),size(y),size(z))
    double precision, parameter :: pi = acos(-1D0)
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
    !!                 = [    x,  -eta * cos(theta),  -eta * sin(theta)]
    double precision, intent(in), dimension(:) :: x, y, z
    double precision, parameter :: pi = acos(-1D0), theta = pi/4
    integer, parameter :: dimensionality = 3
    double precision gradient(size(x),size(y),size(z),dimensionality)
    do concurrent(integer :: i=1:size(x), j=1:size(y), k=1:size(z)) default(none) shared(gradient,x,y,z)
      associate(eta => y(j)*cos(theta) + z(k)*sin(theta)) ! x-eta plane rotated around x axis theta radians from x-y plane
        gradient(i,j,k,:) = [x(i), -eta * cos(theta), -eta * sin(theta)]
      end associate
    end do
  end function

  pure function vs_expected(v,s) result(vsv)
    double precision, intent(in) ::  s(:,:,:), v(:,:,:,:)
    double precision vsv(size(v,1),size(v,2),size(v,3),size(v,4))
    integer, parameter :: dimensionality = 3
    call_julienne_assert(size(v,4) .equalsExpected. dimensionality)
    call_julienne_assert(.all. (shape(v) .equalsExpected. [shape(s),dimensionality]))
    do concurrent(integer :: d=1:size(v,4)) default(none) shared(vsv,s,v)
        vsv(:,:,:,d) = v(:,:,:,d) * s(:,:,:)
    end do
  end function

  pure function velocity_squared(x,y,z) result(v_sq)
    double precision, intent(in), dimension(:) :: x, y, z
    double precision, parameter :: pi = acos(-1D0), theta = pi/4
    integer, parameter :: dimensionality = 3
    double precision v_sq(size(x),size(y),size(z))
    do concurrent(integer :: i=1:size(x), j=1:size(y), k=1:size(z)) default(none) shared(v_sq, x, y, z)
      associate(eta => y(j)*cos(theta) + z(k)*sin(theta)) ! x-eta plane rotated around x axis theta radians from x-y plane
        associate(v => [x(i), -eta * cos(theta), -eta * sin(theta)])
          v_sq(i,j,k) = dot_product(v,v)
        end associate
      end associate
    end do
  end function

  function check_divergence() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(vector_3D_initializer_i), pointer :: vector_3D_initializer
    procedure(divergence_3D_initializer_i), pointer :: expected_divergence_initializer
    integer order

    test_diagnosis = passing_test()
    vector_3D_initializer => rotated_stagnation_point_velocity
    !expected_divergence_initializer => cubic_divergence

    do order = 2, 4, 2
      associate(vector_3D => vector_3D_t(vector_3D_initializer, order, cells=[20,20,20], x_min=[-100D0,-10D0,-10D0], x_max=[10D0,10D0,10D0]))
        associate(div_vector => .div. vector_3D)
          !associate(expected_divergence => divergence_3D_t(expected_divergence_initializer, mold=vector_3D))
            test_diagnosis = test_diagnosis .also. &
              (.all. (div_vector%values() .approximates. 0D0 .within. tolerance)) &
              // string_t(" for order ") // string_t(order)
          !end associate
        end associate
      end associate
    end do
  end function

  function check_dot_product() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(vector_3D_initializer_i), pointer :: v_init
    procedure(scalar_3D_initializer_i), pointer :: v_sq_init
    integer order

    test_diagnosis = passing_test()

    v_init => rotated_stagnation_point_velocity
    v_sq_init => velocity_squared
  
    do order = 2, 4, 2
      associate( &
         v => vector_3D_t(v_init, order, cells=[20,20,20], x_min=[-10D0, -10D0, -10D0], x_max=[10D0, 10D0, 10D0]) &
        ,v_squared => scalar_3D_t(v_sq_init, order, cells=[20,20,20], x_min=[-10D0, -10D0, -10D0], x_max=[10D0, 10D0, 10D0]) &
      )
        associate(v_dot_v => v .dot. v)
          test_diagnosis = test_diagnosis .also. (.all. (v_dot_v%values() .approximates. v_squared%values() .within. tolerance))
        end associate
      end associate
    end do
  end function

  function check_vector_scalar_product() result(test_diagnosis)
    type(test_diagnosis_t) test_diagnosis
    procedure(scalar_3D_initializer_i), pointer :: s_init
    procedure(vector_3D_initializer_i), pointer :: v_init, vs_init
    integer order

    test_diagnosis = passing_test()

    v_init => rotated_stagnation_point_velocity
    s_init => rotated_stagnation_point_potential

    do order = 2, 4, 2
      associate( &
         v => vector_3D_t(v_init, order, cells=[20,20,20], x_min=[-10D0, -10D0, -10D0], x_max=[10D0, 10D0, 10D0]) &
        ,s => scalar_3D_t(s_init, order, cells=[20,20,20], x_min=[-10D0, -10D0, -10D0], x_max=[10D0, 10D0, 10D0]) &
      )
        associate( &
           vs => v * s &
          ,vs_exp_x => v%values(x_dir)* s%values() &
          ,vs_exp_y => v%values(y_dir)* s%values() &
          ,vs_exp_z => v%values(z_dir)* s%values() &
        )
          test_diagnosis = test_diagnosis .also. (.all. (vs%values(x_dir) .approximates. vs_exp_x .within. tolerance))
          test_diagnosis = test_diagnosis .also. (.all. (vs%values(y_dir) .approximates. vs_exp_y .within. tolerance))
          test_diagnosis = test_diagnosis .also. (.all. (vs%values(z_dir) .approximates. vs_exp_z .within. tolerance))
        end associate
      end associate
    end do
  end function

end module vector_3D_test_m