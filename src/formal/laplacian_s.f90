! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(tensors_1D_m) laplacian_s
  implicit none
contains
 
  module procedure reduced_order_boundary_depth
    num_nodes = self%boundary_depth_
  end procedure

end submodule laplacian_s
