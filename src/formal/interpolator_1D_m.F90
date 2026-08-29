! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module interpolator_1D_m
  !! Define a sparse matrix storage format tailored to the staggered-grid interpolation matrix
  !! operators detailed by Dumett & Castillo (2022) https://doi.org/10.13140/RG.2.2.26630.14400
  implicit none

  private
  public :: centers_to_faces_1D_t
  public :: faces_to_centers_1D_t

  type, abstract :: interpolator_1D_t
    !! Encapsulate a staggered-grid interpolation matrix with a corresponding matrix-vector product operator
    private
    integer order_, cells_, dx_
    real                 first_
    real, allocatable :: upper_(:,:) 
    real, allocatable :: inner_(:)
    real, allocatable :: lower_(:,:)
    real                 final_
  end type

  type, extends(interpolator_1D_t) :: centers_to_faces_1D_t
  contains
    procedure, non_overridable :: face_values
  end type

  type, extends(interpolator_1D_t) :: faces_to_centers_1D_t
  contains
    procedure, non_overridable :: center_values_extended
  end type

  interface centers_to_faces_1D_t

    elemental module function c2f_constructor(order, cells, dx) result(centers_to_faces_1D)
      !! Construct centers-to-faces interpolation operator
      implicit none
      integer, intent(in) :: order, cells
      real, intent(in) :: dx
      type(centers_to_faces_1D_t) centers_to_faces_1D
    end function

  end interface

  interface faces_to_centers_1D_t

    elemental module function f2c_constructor(order, cells, dx) result(faces_to_centers_1D)
      !! Construct centers-to-faces interpolation operator
      implicit none
      integer, intent(in) :: order, cells
      real, intent(in) :: dx
      type(faces_to_centers_1D_t) faces_to_centers_1D
    end function

  end interface

  interface

    pure module function face_values(self, centers_extended) result(faces)
      !! Interpolate cell-centered values to face-centered values
      implicit none
      class(centers_to_faces_1D_t), intent(in) :: self
      real, intent(in) :: centers_extended(:)
      real, allocatable :: faces(:)
    end function

    pure module function center_values_extended(self, faces) result(centers_extended)
      !! Interpolate face-centered values to cell-centered values
      implicit none
      class(faces_to_centers_1D_t), intent(in) :: self
      real, intent(in) :: faces(:)
      real, allocatable :: centers_extended(:)
    end function

  end interface

end module interpolator_1D_m