! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(tensors_1D_m) tensor_1D_s
  implicit none
contains

  ! PURPOSE: Constructs a tensor_1D_t object by storing the provided array of field values along with
  !          the domain bounds, cell count, and order of accuracy as internal components.
  ! KEYWORDS: tensor_1D, construction, initialization, structured-grid, staggered-grid, field-values,
  !           grid-metadata
  ! CONTEXT: This procedure is the fundamental constructor for the tensor_1D_t base type in the formal
  !          library's mimetic finite-difference framework. The tensor_1D_t type serves as the common
  !          parent type for scalar_1D_t, vector_1D_t, gradient_1D_t, divergence_1D_t, and other
  !          tensor field types, storing the field values array, domain bounds x_min and x_max, cell
  !          count, and order of accuracy. Derived type constructors such as
  !          construct_1D_scalar_from_function and construct_1D_vector_from_function delegate to this
  !          procedure to initialize their tensor_1D_t base component after computing the field values
  !          from a user-provided initializer function.
  module procedure construct_1D_tensor_from_components
    tensor_1D%values_ = values
    tensor_1D%x_min_  = x_min
    tensor_1D%x_max_  = x_max
    tensor_1D%cells_  = cells 
    tensor_1D%order_  = order
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Computes and returns the uniform cell width dx for the 1D grid by dividing the domain
  !          length by the number of cells.
  ! KEYWORDS: cell-width, grid-spacing, uniform-mesh, accessor, tensor_1D, structured-grid,
  !           staggered-grid, getter
  ! CONTEXT: This procedure provides the uniform cell width dx = (x_max - x_min) / cells for a
  !          tensor_1D_t object in the formal library. The cell width is a fundamental grid parameter
  !          used throughout the mimetic finite-difference framework when constructing gradient,
  !          divergence, and Laplacian operators, computing quadrature weights, and evaluating
  !          differential volume and area elements. Rather than storing dx as a separate component,
  !          it is computed on the fly from the stored domain bounds and cell count.
  module procedure dx
    dx = (self%x_max_ - self%x_min_)/self%cells_
  end procedure
  ! END CODE CHUNK

end submodule tensor_1D_s
