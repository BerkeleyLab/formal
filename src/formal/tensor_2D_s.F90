submodule(tensors_2D_m) tensor_2D_s
  implicit none

contains

  module procedure construct_2D_tensor_from_components
    tensor_2D%values_ = values
    tensor_2D%cells_ = cells
    tensor_2D%x_min_ = x_min
    tensor_2D%x_max_ = x_max
    tensor_2D%order_ = order
  end procedure

end submodule