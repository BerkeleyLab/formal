! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(tensors_1D_m) laplacian_s
  implicit none
contains
 
  ! PURPOSE: Returns the number of boundary-adjacent grid locations at which the mimetic Laplacian
  !          approximation has reduced-order accuracy compared to the interior stencil.
  ! KEYWORDS: laplacian, boundary, order-of-accuracy, mimetic, structured-grid, staggered-grid,
  !           accessor, boundary-depth, reduced-order, getter
  ! CONTEXT: This procedure is a simple accessor that exposes the internally stored boundary depth
  !          from a laplacian_1D_t object in the formal library's mimetic finite-difference framework.
  !          Mimetic Laplacian operators achieve their full design-order accuracy in the grid interior
  !          but exhibit a reduced convergence rate at a finite number of boundary-adjacent nodes. The
  !          returned value indicates the depth of this reduced-accuracy region on each side of the
  !          domain. Test functions such as the Laplacian convergence checks use this accessor to
  !          separate interior and boundary error measurements when verifying convergence rates, since
  !          the boundary region converges at a lower order than the interior.

  module procedure reduced_order_boundary_depth
    num_nodes = self%boundary_depth_
  end procedure
  ! END CODE CHUNK

end submodule laplacian_s
