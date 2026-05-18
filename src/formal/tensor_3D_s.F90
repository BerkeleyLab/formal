submodule(tensors_3D_m) tensor_3D_s
  implicit none

contains

  module procedure construct_3D_tensor_from_components
    tensor_3D%values_ = values
    tensor_3D%cells_ = cells
    tensor_3D%x_min_ = x_min
    tensor_3D%x_max_ = x_max
    tensor_3D%order_ = order
  end procedure

end submodule