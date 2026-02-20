! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(interpolation_operators_1D_m) interpolation_operators_1D_s
  implicit none

contains

  module procedure c2f_component_constructor

    centers_to_faces_1D%cells_ = cells

    select case(order)
    case(2)
      centers_to_faces_1D%first_ = 1D0
      centers_to_faces_1D%upper_ = reshape([double precision::], [0,3])
      centers_to_faces_1D%inner_ = [1D0,1D0]/2D0
      centers_to_faces_1D%lower_ = reshape([double precision::], [0,3])
      centers_to_faces_1D%final_ = 1D0
    case(4)
      centers_to_faces_1D%first_ = 1D0
      centers_to_faces_1D%upper_ = reshape([-16,  70, 70, -14,   2], [1,5])/112D0
      centers_to_faces_1D%inner_ =              [ -7,  63, 63,  -7]        /112D0
      centers_to_faces_1D%lower_ = reshape([  2, -14, 70,  70, -16], [1,5])/112D0
      centers_to_faces_1D%final_ = 1D0
    case default
      error stop "c2f_component_constructor: unsupported order"
    end select

  end procedure

  module procedure f2c_component_constructor

    faces_to_centers_1D%cells_ = cells

    select case(order)
    case(2)
      faces_to_centers_1D%first_ = 1D0
      faces_to_centers_1D%upper_ = reshape([double precision::], [0,3])
      faces_to_centers_1D%inner_ = [1D0,1D0]/2D0
      faces_to_centers_1D%lower_ = reshape([double precision::], [0,3])
      faces_to_centers_1D%final_ = 1D0
    case(4)
      faces_to_centers_1D%first_ = 1D0
      faces_to_centers_1D%upper_ = reshape([35, 140, -70, 28, -5], [1,5])/128D0
      faces_to_centers_1D%inner_ =         [-8,  72,  72, -8]            /128D0
      faces_to_centers_1D%lower_ = reshape([-5, 28, -70, 140, 35], [1,5])/128D0
      faces_to_centers_1D%final_ = 1D0
    case default
      error stop "f2c_component_constructor: unsupported order"
    end select

  end procedure

end submodule interpolation_operators_1D_s
