! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module interpolation_operators_1D_m
  !! Define a sparse matrix storage format tailored to the staggered-grid interpolation matrix
  !! operators detailed by Dumett & Castillo (2022) https://doi.org/10.13140/RG.2.2.26630.14400
  implicit none

  private
  public :: cells_to_faces_t
  public :: faces_to_cells_t

  type interpolation_operator_1D_t
    !! Encapsulate a staggered-grid interpolation matrix with a corresponding matrix-vector product operator
    private
    integer order
    double precision                 first_
    double precision, allocatable :: upper_(:,:) 
    double precision, allocatable :: inner_(:)
    double precision, allocatable :: lower_(:,:)
    double precision                 final_
  end type

  type, extends(interpolation_operator_1D_t) :: centers_to_faces_1D_t
  end type

  type, extends(interpolation_operator_1D_t) :: faces_to_centers_1D_t
  end type

  interface centers_to_faces_1D_t

    pure module function c2f_component_constructor(order, cells, dx) result(centers_to_faces_1D)
      !! Construct centers-to-faces interpolation operator
      implicit none
      integer, intent(in) :: order, cells
      double precision, intent(in) :: dx
      type(centers_to_faces_1D_t) centers_to_faces_1D
    end function

  end interface

  interface faces_to_centers_1D_t

    pure module function f2c_component_constructor(order, cells, dx) result(faces_to_centers_1D)
      !! Construct faces-to-centers interpolation operator
      implicit none
      integer, intent(in) :: order, cells
      double precision, intent(in) :: dx
      type(faces_to_centers_1D_t) faces_to_centers_1D
    end function

  end interface

end module interpolation_operators_1D_m
