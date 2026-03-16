! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(interpolator_1D_m) interpolator_1D_s
  use julienne_m, only : call_julienne_assert_, operator(.equalsExpected.)
  implicit none

contains

  module procedure c2f_constructor

    centers_to_faces_1D%order_ = order
    centers_to_faces_1D%cells_ = cells
    centers_to_faces_1D%dx_    = dx

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

    call_julienne_assert(shape(self%lower_) .equalsExpected. shape(self%upper_))
  end procedure

  module procedure f2c_constructor

    faces_to_centers_1D%order_ = order
    faces_to_centers_1D%cells_ = cells
    faces_to_centers_1D%dx_    = dx

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

    call_julienne_assert(shape(self%lower_) .equalsExpected. shape(self%upper_))
  end procedure

  module procedure interpolate_to_faces
    integer row
    associate( &
       N => size(centers)               , inner_cols => size(self%inner_) &
      ,upper_rows => size(self%upper_,1), upper_cols => size(self%upper_,2) &
      ,lower_rows => size(self%lower_,1), lower_cols => size(self%lower_,2) &
    )
      faces = [        self%first_ * centers(1) &
        ,       matmul(self%upper_ , centers(1:upper_cols)) &
        ,[(dot_product(self%inner_ , centers(row-upper_rows+1 : row-upper_rows+inner_cols)), row = upper_rows+1, N-lower_rows-1)] &
        ,       matmul(self%lower_ , centers(N-lower_cols+1:N)) &
        ,              self%final_ * centers(N) &
      ]
    end associate
  end procedure

end submodule interpolator_1D_s