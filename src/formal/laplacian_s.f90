! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(tensors_1D_m) laplacian_s
  implicit none
contains
 
  ! PURPOSE: To report the number of boundary-adjacent locations at which the Laplacian has reduced-order accuracy.
  ! KEYWORDS: Laplacian, boundary, order of accuracy
  ! CONTEXT: Use this function to determine the region of slightly slower convergence for mimetic Laplacian approximations.

  module procedure reduced_order_boundary_depth
    num_nodes = self%boundary_depth_
  end procedure

end submodule laplacian_s
