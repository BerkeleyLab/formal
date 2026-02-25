! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "formal-language-support.F90"

module tensors_1D_m
  !! Define public 1D scalar and vector abstractions and associated mimetic gradient,
  !! divergence, and Laplacian operators as detailed by Corbino & Castillo (2020)
  !! https://doi.org/10.1016/j.cam.2019.06.042.
  use julienne_m, only : file_t
  use mimetic_operators_1D_m, only : divergence_operator_1D_t, gradient_operator_1D_t
    
  implicit none

  private

  public :: scalar_1D_t
  public :: vector_1D_t
  public :: gradient_1D_t
  public :: laplacian_1D_t
  public :: divergence_1D_t
  public :: scalar_1D_initializer_i
  public :: vector_1D_initializer_i

  abstract interface

    pure function scalar_1D_initializer_i(x) result(f)
      !! Sampling function for initializing a scalar_1D_t object
      implicit none
      double precision, intent(in) :: x(:)
      double precision, allocatable :: f(:)
    end function

    pure function vector_1D_initializer_i(x) result(v)
      !! Sampling function for initializing a vector_1D_t object
      implicit none
      double precision, intent(in) :: x(:)
      double precision, allocatable :: v(:)
    end function

  end interface

  ! PURPOSE: Encapsulates the components common to all 1D tensor field types, including domain bounds,
  !          cell count, order of accuracy, and the array of field values at spatial locations. Child
  !          types define the differential operators supported by each specific tensor kind.
  ! KEYWORDS: tensor_1D, base-type, structured-grid, staggered-grid, mimetic, field-values,
  !           grid-metadata, domain-bounds, cell-count, order-of-accuracy
  ! CONTEXT: This type is the common base for all 1D tensor field types in the formal library's
  !          mimetic finite-difference framework, including scalar_1D_t, vector_1D_t, gradient_1D_t,
  !          divergence_1D_t, vector_dot_gradient_1D_t, scalar_x_divergence_1D_t, weighted_product_1D_t,
  !          and laplacian_1D_t. It stores the domain bounds x_min and x_max, the number of grid
  !          cells, the order of accuracy of the mimetic discretization, and an allocatable array of
  !          field values whose size and meaning depend on the child type (m+2 for cell-centered
  !          extended scalars, m+1 for face-centered vectors, m for cell-centered divergences, etc.).
  !          The type provides private procedures for computing gradient and divergence quadrature
  !          weights and a public dV (aliased to dx) accessor for the differential volume element.
  type tensor_1D_t
    !! Encapsulate the components that are common to all 1D tensors.
    !! Child types define the operations supported by each child, including
    !! gradient (.grad.) for scalars and divergence (.div.) for vectors.
    private
    double precision x_min_ !! domain lower boundary
    double precision x_max_ !! domain upper boundary
    integer cells_          !! number of grid cells spanning the domain
    integer order_          !! order of accuracy of mimetic discretization
    double precision, allocatable :: values_(:) !! tensor components at spatial locations
  contains
    procedure, non_overridable, private :: gradient_1D_weights
    procedure, non_overridable, private :: divergence_1D_weights
    generic :: dV => dx
    procedure, non_overridable :: dx
  end type
  ! END CODE CHUNK

  interface tensor_1D_t

    ! PURPOSE: Constructs a tensor_1D_t object by assigning the provided field values, domain bounds,
    !          cell count, and order of accuracy to the corresponding components.
    ! KEYWORDS: tensor_1D, construction, initialization, field-values, grid-metadata, structured-grid,
    !           staggered-grid
    ! CONTEXT: This interface provides the user-defined constructor for the tensor_1D_t base type in
    !          the formal library. All derived tensor types delegate to this constructor to initialize
    !          their tensor_1D_t base component after computing field values from initializer functions
    !          or operator applications. The implementation in tensor_1D_s performs direct assignment
    !          of the dummy arguments to the corresponding private components.
    pure module function construct_1D_tensor_from_components(values, x_min, x_max, cells, order) result(tensor_1D)
      !! User-defined constructor: result is a 1D tensor defined by assigning the dummy arguments to corresponding components
      implicit none
      double precision, intent(in) :: values(:) !! tensor components at grid locations define by child
      double precision, intent(in) :: x_min     !! grid location minimum
      double precision, intent(in) :: x_max     !! grid location maximum
      integer,          intent(in) :: cells     !! number of grid cells spanning the domain
      integer,          intent(in) :: order     !! order of accuracy
      type(tensor_1D_t) tensor_1D
    end function
    ! END CODE CHUNK

  end interface

  ! PURPOSE: Encapsulates a 1D scalar field defined at cell centers and domain boundaries (m+2
  !          values), along with a pre-built gradient operator, and provides the .grad. and
  !          .laplacian. differential operators.
  ! KEYWORDS: scalar_1D, scalar-field, cell-centered, boundary-values, gradient-operator, laplacian,
  !           mimetic, structured-grid, staggered-grid, operator-overloading
  ! CONTEXT: This type extends tensor_1D_t in the formal library's mimetic finite-difference
  !          framework to represent a scalar field on the extended cell-centered grid (m cell centers
  !          plus 2 boundary values). It stores a gradient_operator_1D_t for efficient application of
  !          the .grad. operator, which maps the m+2 scalar values to m+1 node-centered gradient
  !          values. The .laplacian. operator composes .div. and .grad. to produce the discrete
  !          Laplacian. The type also provides grid and values accessors for retrieving the extended
  !          grid coordinates and field values respectively.
  type, extends(tensor_1D_t) :: scalar_1D_t
    !! Encapsulate scalar values at cell centers and boundaries
    private
    type(gradient_operator_1D_t) gradient_operator_1D_
  contains
    generic :: operator(.grad.) => grad
    generic :: operator(.laplacian.) => laplacian
    generic :: grid   => scalar_1D_grid
    generic :: values => scalar_1D_values
    procedure, non_overridable, private :: grad
    procedure, non_overridable, private :: laplacian
    procedure, non_overridable, private :: scalar_1D_values
    procedure, non_overridable, private :: scalar_1D_grid
  end type
  ! END CODE CHUNK

  interface scalar_1D_t

    ! PURPOSE: Constructs a scalar_1D_t object by evaluating a user-provided initializer function on
    !          the extended grid and storing the resulting values along with a pre-built gradient
    !          operator.
    ! KEYWORDS: scalar_1D, construction, initializer, structured-grid, staggered-grid,
    !           gradient-operator, mimetic, cell-centered, boundary-values
    ! CONTEXT: This interface provides the constructor for scalar_1D_t in the formal library's
    !          mimetic finite-difference framework. The implementation in scalar_1D_s evaluates the
    !          initializer function pointer on the m+2 extended grid (boundary points plus cell
    !          centers), stores the values as the tensor_1D_t base component, and pre-builds a
    !          gradient_operator_1D_t for the specified order and grid spacing. Assertions verify
    !          that x_max > x_min and that the cell count is at least 2*order. A gfortran-specific
    !          variant with an explicit function signature is provided in the implementation.
    pure module function construct_1D_scalar_from_function(initializer, order, cells, x_min, x_max) result(scalar_1D)
      !! Result is a collection of cell-centered-extended values with a corresponding mimetic gradient operator
      implicit none
      procedure(scalar_1D_initializer_i), pointer :: initializer
      integer, intent(in) :: order !! order of accuracy
      integer, intent(in) :: cells !! number of grid cells spanning the domain
      double precision, intent(in) :: x_min !! grid location minimum
      double precision, intent(in) :: x_max !! grid location maximum
      type(scalar_1D_t) scalar_1D
    end function
    ! END CODE CHUNK

  end interface

  ! PURPOSE: Encapsulates a 1D vector field defined at cell faces (m+1 values), along with a
  !          pre-built divergence operator, and provides the .div., .dot., and .x. operators for
  !          divergence, surface-normal dot product, and weighted scalar premultiplication
  !          respectively.
  ! KEYWORDS: vector_1D, vector-field, face-centered, divergence-operator, mimetic, structured-grid,
  !           staggered-grid, operator-overloading, surface-normal, weighted-product
  ! CONTEXT: This type extends tensor_1D_t in the formal library's mimetic finite-difference
  !          framework to represent a vector field on the face-centered staggered-grid (m+1 face
  !          locations including both domain boundaries). It stores a divergence_operator_1D_t for
  !          efficient application of the .div. operator, which maps the m+1 face values to m+2
  !          cell-centered values (with zero boundary rows). The .dot. operator computes the dot
  !          product with a surface normal dS for boundary integrals, and the .x. operator computes
  !          the weighted product with a scalar field using the Corbino & Castillo (2020) Eq. 7
  !          boundary operator. The dA accessor returns the differential area element (unity in 1D).
  !          A compiler-conditional block exposes the gradient_1D_weights procedure under the generic
  !          name weights for the Intel compiler.
  type, extends(tensor_1D_t) :: vector_1D_t
    !! Encapsulate 1D vector values at cell faces (of unit area for 1D) and corresponding operators
    private
    type(divergence_operator_1D_t) divergence_operator_1D_
  contains
    generic :: operator(.x.)   => weighted_premultiply
    generic :: operator(.div.) => div
    generic :: operator(.dot.) => dot_surface_normal
    generic :: grid   => vector_1D_grid
    generic :: values => vector_1D_values
#ifdef __INTEL_COMPILER
    generic :: weights => gradient_1D_weights
#endif
    procedure, non_overridable :: dA
    procedure, non_overridable, pass(vector_1D) :: weighted_premultiply
    procedure, non_overridable, private :: div
    procedure, non_overridable, private :: dot_surface_normal
    procedure, non_overridable, private :: vector_1D_grid
    procedure, non_overridable, private :: vector_1D_values
  end type
  ! END CODE CHUNK

  ! PURPOSE: Encapsulates the result of the weighted product of a vector_1D field and a scalar_1D
  !          field via the Corbino & Castillo (2020) Eq. 7 boundary operator, and provides the .SS.
  !          surface integration operator.
  ! KEYWORDS: weighted-product, boundary-operator, surface-integral, mimetic, Corbino-Castillo,
  !           structured-grid, staggered-grid, operator-overloading
  ! CONTEXT: This type extends tensor_1D_t in the formal library's mimetic finite-difference
  !          framework to represent the result of the .x. weighted premultiplication of a vector_1D_t
  !          with a scalar_1D_t. The stored values are the products dx * B * v * f, where B is the
  !          Corbino & Castillo (2020) boundary operator from Eq. 7. The .SS. operator performs the
  !          surface integral by summing the stored values, implementing the discrete surface
  !          integral term in the extended Gauss divergence theorem.
  type, extends(tensor_1D_t) :: weighted_product_1D_t
  contains
    generic :: operator(.SS.) => surface_integrate_vector_x_scalar_1D
    procedure, non_overridable, private :: surface_integrate_vector_x_scalar_1D
  end type
  ! END CODE CHUNK

  interface vector_1D_t

    ! PURPOSE: Constructs a vector_1D_t object by evaluating a user-provided initializer function on
    !          the face-centered grid and storing the resulting values along with a pre-built
    !          divergence operator.
    ! KEYWORDS: vector_1D, construction, initializer, structured-grid, staggered-grid,
    !           divergence-operator, mimetic, face-centered
    ! CONTEXT: This interface provides the constructor for vector_1D_t from an initializer function
    !          in the formal library's mimetic finite-difference framework. The implementation in
    !          vector_1D_s evaluates the initializer function pointer on the m+1 face-centered grid,
    !          stores the values as the tensor_1D_t base component, and pre-builds a
    !          divergence_operator_1D_t for the specified order and grid spacing. Assertions verify
    !          that x_max > x_min and that the cell count is at least 2*order+1. A gfortran-specific
    !          variant with an explicit function signature is provided in the implementation.
    pure module function construct_1D_vector_from_function(initializer, order, cells, x_min, x_max) result(vector_1D)
      !! Result is a 1D vector with values initialized by the provided procedure pointer sampled on the specified
      !! number of evenly spaced cells covering [x_min, x_max]
      implicit none
      procedure(vector_1D_initializer_i), pointer :: initializer
      integer, intent(in) :: order !! order of accuracy
      integer, intent(in) :: cells !! number of grid cells spanning the domain
      double precision, intent(in) :: x_min !! grid location minimum
      double precision, intent(in) :: x_max !! grid location maximum
      type(vector_1D_t) vector_1D
    end function
    ! END CODE CHUNK

    ! PURPOSE: Constructs a vector_1D_t object from pre-existing tensor_1D_t and
    !          divergence_operator_1D_t components, bypassing initializer function evaluation.
    ! KEYWORDS: vector_1D, construction, component-assembly, tensor_1D, divergence-operator,
    !           structured-grid, staggered-grid
    ! CONTEXT: This interface provides an alternative constructor for vector_1D_t in the formal
    !          library when the field values and divergence operator have already been computed
    !          separately. The implementation in vector_1D_s directly assigns the provided components.
    !          This is used internally when constructing vector fields from intermediate operator
    !          results.
    pure module function construct_from_components(tensor_1D, divergence_operator_1D) result(vector_1D)
      !! Result is a 1D vector with the provided parent component tensor_1D and the provided divergence operator
      type(tensor_1D_t), intent(in) :: tensor_1D
      type(divergence_operator_1D_t), intent(in) :: divergence_operator_1D
      type(vector_1D_t) vector_1D
    end function
    ! END CODE CHUNK

  end interface

  ! PURPOSE: Encapsulates a 1D mimetic gradient vector field at node-centered locations (m+1 values),
  !          extending vector_1D_t with a .dot. operator that computes the element-wise dot product
  !          with another vector_1D field, and a weights accessor for the gradient quadrature weights.
  ! KEYWORDS: gradient_1D, gradient-field, node-centered, mimetic, structured-grid, staggered-grid,
  !           dot-product, quadrature-weights, operator-overloading
  ! CONTEXT: This type extends vector_1D_t in the formal library's mimetic finite-difference
  !          framework to represent the result of applying the .grad. operator to a scalar_1D_t
  !          object. It inherits the divergence operator from vector_1D_t (enabling .div. (.grad. f)
  !          for Laplacian computation) and adds a .dot. operator for computing v .dot. grad(f)
  !          products that produce vector_dot_gradient_1D_t objects suitable for volume integration.
  !          The weights accessor (generic name, with compiler-conditional bindings to
  !          gradient_1D_weights) provides the mimetic quadrature weights for node-centered
  !          integration.
  type, extends(vector_1D_t) :: gradient_1D_t
    !! A 1D mimetic gradient vector field abstraction with a public method that produces corresponding numerical quadrature weights
  contains
    generic :: operator(.dot.) => dot
#ifndef __INTEL_COMPILER
    generic :: weights => gradient_1D_weights
#endif
    procedure, non_overridable, private, pass(gradient_1D) :: dot
  end type
  ! END CODE CHUNK

  ! PURPOSE: Encapsulates the element-wise dot product of a 1D vector field with a 1D gradient field,
  !          carrying both the node-centered product values and the gradient quadrature weights, and
  !          provides the .SSS. volume integration operator.
  ! KEYWORDS: vector-dot-gradient, dot-product, volume-integral, quadrature-weights, mimetic,
  !           structured-grid, staggered-grid, node-centered, operator-overloading
  ! CONTEXT: This type extends tensor_1D_t in the formal library's mimetic finite-difference
  !          framework to represent the result of the .dot. operator applied between a vector_1D_t
  !          and a gradient_1D_t. It stores both the m+1 node-centered product values and the m+1
  !          gradient quadrature weights needed for volume integration. The .SSS. operator computes
  !          the discrete volume integral as a direct weighted sum of the product values, which
  !          appears in compound expressions such as .SSS. (v .dot. .grad. f) * dV within the
  !          extended Gauss divergence theorem test.
  type, extends(tensor_1D_t) :: vector_dot_gradient_1D_t
    !! Result is the dot product of a 1D vector field and a 1D gradient field
    private
    double precision, allocatable :: weights_(:)
  contains
    generic :: operator(.SSS.) => volume_integrate_vector_dot_grad_scalar_1D
    procedure, non_overridable, private, pass(integrand) ::volume_integrate_vector_dot_grad_scalar_1D
  end type
  ! END CODE CHUNK

  ! PURPOSE: Encapsulates a 1D divergence field at cell centers (m values), providing grid and values
  !          accessors, divergence quadrature weights, and multiplication operators for combining with
  !          scalar fields.
  ! KEYWORDS: divergence_1D, divergence-field, cell-centered, mimetic, structured-grid, staggered-grid,
  !           quadrature-weights, scalar-multiplication, operator-overloading
  ! CONTEXT: This type extends tensor_1D_t in the formal library's mimetic finite-difference
  !          framework to represent the result of applying the .div. operator to a vector_1D_t
  !          object. The m cell-centered divergence values (with boundary zeros stripped) are stored
  !          in the inherited values_ array. The type provides grid and values accessors, a weights
  !          accessor for the divergence quadrature weights, and overloaded * operators for
  !          premultiplication and postmultiplication with scalar_1D_t fields, producing
  !          scalar_x_divergence_1D_t objects suitable for volume integration.
  type, extends(tensor_1D_t) :: divergence_1D_t
    !! Encapsulate divergences at cell centers
  contains
    generic :: grid   => divergence_1D_grid
    generic :: values => divergence_1D_values
    generic :: weights => divergence_1D_weights
    generic :: operator(*) => premultiply_scalar_1D, postmultiply_scalar_1D
    procedure, non_overridable, private, pass(divergence_1D) :: premultiply_scalar_1D
    procedure, non_overridable, private :: postmultiply_scalar_1D
    procedure, non_overridable, private :: divergence_1D_values
    procedure, non_overridable, private :: divergence_1D_grid
  end type
  ! END CODE CHUNK

  ! PURPOSE: Encapsulates the element-wise product of a 1D scalar field with a 1D divergence field,
  !          carrying both the cell-centered product values and the divergence quadrature weights, and
  !          provides the .SSS. volume integration operator.
  ! KEYWORDS: scalar-divergence-product, volume-integral, quadrature-weights, mimetic, structured-grid,
  !           staggered-grid, cell-centered, operator-overloading, boundary-padding
  ! CONTEXT: This type extends tensor_1D_t in the formal library's mimetic finite-difference
  !          framework to represent the result of the * operator applied between a scalar_1D_t and a
  !          divergence_1D_t. It stores the m cell-centered product values and the m+2 divergence
  !          quadrature weights. The .SSS. volume integration operator zero-pads the values at both
  !          boundaries before computing the weighted sum, consistent with the zero boundary rows of
  !          the divergence operator. This appears in compound expressions such as
  !          .SSS. (f * .div. v) * dV within the extended Gauss divergence theorem test.
  type, extends(tensor_1D_t) :: scalar_x_divergence_1D_t
    !! product of a 1D scalar field and a 1D divergence field
    private
    double precision, allocatable :: weights_(:)
  contains
    generic :: operator(.SSS.) => volume_integrate_scalar_x_divergence_1D
    procedure, non_overridable, private, pass(integrand) :: volume_integrate_scalar_x_divergence_1D
  end type
  ! END CODE CHUNK

  ! PURPOSE: Encapsulates a 1D discrete Laplacian field computed as the composition of the divergence
  !          and gradient operators, extending divergence_1D_t with a boundary depth indicating where
  !          the Laplacian has reduced-order accuracy.
  ! KEYWORDS: laplacian_1D, laplacian, div-grad, divergence, gradient, mimetic, structured-grid,
  !           staggered-grid, boundary-depth, reduced-order, operator-composition
  ! CONTEXT: This type extends divergence_1D_t in the formal library's mimetic finite-difference
  !          framework to represent the result of applying the .laplacian. operator to a scalar_1D_t
  !          object. The Laplacian is computed as .div. (.grad. f), inheriting all divergence_1D_t
  !          functionality. The additional boundary_depth_ component records the number of cells from
  !          each boundary where the Laplacian exhibits reduced-order accuracy due to the boundary
  !          stencils, computed as the divergence operator's upper block row count plus one. The
  !          reduced_order_boundary_depth accessor provides this information for convergence tests
  !          that separately assess interior and boundary error behavior.
  type, extends(divergence_1D_t) :: laplacian_1D_t
    private
    integer boundary_depth_
  contains
    procedure reduced_order_boundary_depth
  end type
  ! END CODE CHUNK

  interface

    ! PURPOSE: Returns the differential area element dA for the 1D case, which is always 1.0 since
    !          the cross-sectional area of a 1D domain is unity.
    ! KEYWORDS: differential-area, surface-element, 1D, vector_1D, accessor, getter, boundary-integral
    ! CONTEXT: This interface declares the accessor that returns the differential area element for a
    !          vector_1D_t object in the formal library's mimetic finite-difference framework. In one
    !          spatial dimension, the surface bounding each cell is a point with unit area, so dA is
    !          trivially 1.0. This accessor exists to maintain a consistent interface with
    !          higher-dimensional generalizations where dA would be a nontrivial geometric quantity.
    !          It is used in surface integral expressions such as .SS. (f .x. (v .dot. dA)).
    pure module function dA(self)
      !! Result is the grid's discrete surface-area differential for use in surface integrals of the form
      !! .SS. (f .x. (v .dot. dA))
      implicit none
      class(vector_1D_t), intent(in) :: self
      double precision dA
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes and returns the uniform cell width dx for the 1D grid by dividing the domain
    !          length by the number of cells.
    ! KEYWORDS: cell-width, grid-spacing, uniform-mesh, accessor, tensor_1D, structured-grid,
    !           staggered-grid, differential-volume, getter
    ! CONTEXT: This interface declares the accessor that returns the uniform cell width
    !          dx = (x_max - x_min) / cells for a tensor_1D_t object in the formal library. The cell
    !          width serves double duty as the differential volume element dV in 1D (exposed via the
    !          generic binding dV => dx). It is used throughout the mimetic framework when
    !          constructing operators, computing quadrature weights, and evaluating integrals.
    pure module function dx(self)
      !! Result is the uniform cell width
      implicit none
      class(tensor_1D_t), intent(in) :: self
      double precision dx
    end function
    ! END CODE CHUNK

    ! PURPOSE: Returns the extended grid locations for a scalar_1D_t object, consisting of the two
    !          domain boundary points bracketing the cell-center locations (m+2 values).
    ! KEYWORDS: grid, cell-center, boundary-points, extended-grid, scalar_1D, accessor, getter,
    !           structured-grid, staggered-grid
    ! CONTEXT: This interface declares the grid accessor for scalar_1D_t in the formal library. The
    !          implementation in scalar_1D_s delegates to the scalar_1D_grid_locations helper
    !          function, returning an array of m+2 locations that includes x_min, the m cell-center
    !          coordinates, and x_max. These coordinates correspond positionally to the m+2 extended
    !          scalar field values.
    pure module function scalar_1D_grid(self) result(cell_centers_extended)
      !! Result is the array of locations at which 1D scalars are defined: cell centers augmented by spatial boundaries
      implicit none
      class(scalar_1D_t), intent(in) :: self
      double precision, allocatable :: cell_centers_extended(:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Returns the face-centered grid locations for a vector_1D_t object, consisting of m+1
    !          face locations including both domain boundaries.
    ! KEYWORDS: grid, face-centered, vector_1D, accessor, getter, structured-grid, staggered-grid
    ! CONTEXT: This interface declares the grid accessor for vector_1D_t in the formal library. The
    !          implementation in vector_1D_s delegates to the faces helper function, returning an
    !          array of m+1 face locations from x_min to x_max. These coordinates correspond
    !          positionally to the m+1 face-centered vector field values.
    pure module function vector_1D_grid(self) result(cell_faces)
      !! Result is the array of cell face locations (of unit area for 1D) at which 1D vectors are defined
      implicit none
      class(vector_1D_t), intent(in) :: self
      double precision, allocatable :: cell_faces(:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Returns the cell-center grid locations for a divergence_1D_t object, consisting of m
    !          interior cell-center coordinates.
    ! KEYWORDS: grid, cell-center, divergence_1D, accessor, getter, structured-grid, staggered-grid
    ! CONTEXT: This interface declares the grid accessor for divergence_1D_t in the formal library.
    !          The implementation in divergence_1D_s returns an array of m cell-center coordinates,
    !          corresponding to the interior cells where the divergence field is defined. Unlike the
    !          scalar_1D grid which includes boundary points, the divergence grid contains only
    !          interior cell centers because the divergence operator produces zero boundary rows that
    !          are stripped during construction.
    pure module function divergence_1D_grid(self) result(cell_centers)
      !! Result is the array of cell centers at which 1D divergences are defined
      implicit none
      class(divergence_1D_t), intent(in) :: self
      double precision, allocatable :: cell_centers(:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Returns the extended cell-centered values stored in a scalar_1D_t object, including
    !          both boundary values and interior cell-center values (m+2 values).
    ! KEYWORDS: scalar_1D, accessor, cell-centered-values, extended-values, getter, boundary-values
    ! CONTEXT: This interface declares the values accessor for scalar_1D_t in the formal library. The
    !          implementation in scalar_1D_s returns the internally stored m+2 extended values
    !          including the two domain boundary values at x_min and x_max plus the m interior
    !          cell-center values.
    pure module function scalar_1D_values(self) result(cell_centers_extended_values)
      !! Result is an array of 1D scalar values at boundaries and cell centers
      implicit none
      class(scalar_1D_t), intent(in) :: self
      double precision, allocatable :: cell_centers_extended_values(:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Returns the face-centered vector values stored in a vector_1D_t object (m+1 values).
    ! KEYWORDS: vector_1D, accessor, face-centered-values, getter
    ! CONTEXT: This interface declares the values accessor for vector_1D_t in the formal library. The
    !          implementation in vector_1D_s returns the internally stored m+1 face-centered values
    !          including both domain boundary faces and all interior cell faces.
    pure module function vector_1D_values(self) result(face_centered_values)
      !! Result is an array of the 1D vector values at cell faces (of unit area 1D)
      implicit none
      class(vector_1D_t), intent(in) :: self
      double precision, allocatable :: face_centered_values(:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Returns the cell-centered divergence values stored in a divergence_1D_t object (m
    !          values).
    ! KEYWORDS: divergence_1D, accessor, cell-centered-values, getter
    ! CONTEXT: This interface declares the values accessor for divergence_1D_t in the formal library.
    !          The implementation in divergence_1D_s returns the internally stored m cell-centered
    !          divergence values, which are the interior entries of the divergence operator's output
    !          with boundary zeros stripped.
    pure module function divergence_1D_values(self) result(cell_centered_values)
      !! Result is an array of 1D divergences at cell centers
      implicit none
      class(divergence_1D_t), intent(in) :: self
      double precision, allocatable :: cell_centered_values(:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the discrete gradient of the scalar_1D field by applying the mimetic gradient
    !          operator, producing a gradient_1D_t object with node-centered gradient values and
    !          verified quadrature weights satisfying the Corbino & Castillo (2020) Eq. 17 identity.
    ! KEYWORDS: gradient, mimetic, operator-application, Corbino-Castillo, scalar_1D, gradient_1D,
    !           divergence-operator, quadrature-weights, summation-by-parts, verification
    ! CONTEXT: This interface declares the .grad. operator for scalar_1D_t in the formal library. The
    !          implementation in scalar_1D_s constructs a gradient_operator_1D_t, applies it to the
    !          scalar's m+2 extended values to produce m+1 node-centered gradient values, stores a
    !          divergence_operator_1D_t in the result, and verifies the Corbino & Castillo (2020)
    !          Eq. 17 summation-by-parts identity.
    pure module function grad(self) result(gradient_1D)
      !! Result is mimetic gradient of the scalar_1D_t "self"
      implicit none
      class(scalar_1D_t), intent(in) :: self
      type(gradient_1D_t) gradient_1D
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the discrete Laplacian of the scalar_1D field by composing the divergence and
    !          gradient operators, and determines the boundary depth where the Laplacian has
    !          reduced-order accuracy.
    ! KEYWORDS: laplacian, divergence, gradient, div-grad, mimetic, operator-composition, scalar_1D,
    !           laplacian_1D, boundary-depth, reduced-order
    ! CONTEXT: This interface declares the .laplacian. operator for scalar_1D_t in the formal library.
    !          The implementation in scalar_1D_s computes the Laplacian as .div. (.grad. self) and
    !          stores the boundary depth (divergence operator's upper block row count plus one) in
    !          the resulting laplacian_1D_t for use in convergence tests that separately assess
    !          interior and boundary error behavior.
    pure module function laplacian(self) result(laplacian_1D)
      !! Result is mimetic Laplacian of the scalar_1D_t "self"
      implicit none
      class(scalar_1D_t), intent(in) :: self
      type(laplacian_1D_t) laplacian_1D
    end function
    ! END CODE CHUNK

    ! PURPOSE: Returns the number of nodes from the boundary at which the Laplacian exhibits
    !          reduced-order convergence rate (one degree lower than the interior).
    ! KEYWORDS: laplacian_1D, boundary-depth, reduced-order, convergence, accessor, getter
    ! CONTEXT: This interface declares the accessor for the boundary depth of a laplacian_1D_t object
    !          in the formal library. The boundary depth indicates how many cells from each boundary
    !          the Laplacian has reduced-order accuracy due to the boundary stencils in the mimetic
    !          gradient and divergence operators. Convergence tests use this value to partition the
    !          domain into interior and boundary regions for separate error analysis.
    pure module function reduced_order_boundary_depth(self) result(num_nodes)
      !! Result is number of nodes away from the boundary for which convergence rate is one degree lower
      implicit none
      class(laplacian_1D_t), intent(in) :: self
      integer num_nodes
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the discrete divergence of the vector_1D field by applying the mimetic
    !          divergence operator, producing a divergence_1D_t object with cell-centered divergence
    !          values and verified quadrature weights satisfying the Corbino & Castillo (2020) Eq. 19
    !          identity.
    ! KEYWORDS: divergence, mimetic, operator-application, Corbino-Castillo, vector_1D, divergence_1D,
    !           quadrature-weights, summation-by-parts, verification
    ! CONTEXT: This interface declares the .div. operator for vector_1D_t in the formal library. The
    !          implementation in vector_1D_s applies the stored divergence_operator_1D_t to the
    !          vector's m+1 face-centered values, strips the zero boundary entries to yield m
    !          cell-centered divergence values, and verifies the Corbino & Castillo (2020) Eq. 19
    !          summation-by-parts identity D^T * q = b/dx.
    pure module function div(self) result(divergence_1D)
      !! Result is mimetic divergence of the vector_1D_t "self"
      implicit none
      class(vector_1D_t), intent(in) :: self
      type(divergence_1D_t) divergence_1D !! discrete divergence
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the discrete volume integral of a vector_dot_gradient_1D_t field by
    !          performing a weighted sum of the node-centered product values using the mimetic
    !          gradient quadrature weights.
    ! KEYWORDS: volume-integral, quadrature, mimetic, vector-dot-gradient, weighted-sum,
    !           summation-by-parts, node-centered
    ! CONTEXT: This interface declares the .SSS. volume integration operator for
    !          vector_dot_gradient_1D_t in the formal library. The implementation in
    !          vector_dot_gradient_1D_s computes the integral as a direct weighted sum of the m+1
    !          node-centered product values with the gradient quadrature weights. This appears in
    !          compound expressions such as .SSS. (v .dot. .grad. f) * dV within the extended Gauss
    !          divergence theorem test.
    pure module function volume_integrate_vector_dot_grad_scalar_1D(integrand) result(integral)
      !! Result is the mimetic quadrature corresponding to a volume integral of a vector-gradient dot product
      implicit none
      class(vector_dot_gradient_1D_t), intent(in) :: integrand
      double precision integral
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the discrete volume integral of a scalar_x_divergence_1D_t field by
    !          performing a weighted sum of the cell-centered product values (zero-padded at
    !          boundaries) using the mimetic divergence quadrature weights.
    ! KEYWORDS: volume-integral, quadrature, mimetic, scalar-divergence-product, weighted-sum,
    !           summation-by-parts, cell-centered, boundary-padding
    ! CONTEXT: This interface declares the .SSS. volume integration operator for
    !          scalar_x_divergence_1D_t in the formal library. The implementation in
    !          scalar_x_divergence_1D_s zero-pads the m product values at both boundaries and
    !          computes the weighted sum with the m+2 divergence quadrature weights. This appears in
    !          compound expressions such as .SSS. (f * .div. v) * dV within the extended Gauss
    !          divergence theorem test.
    pure module function volume_integrate_scalar_x_divergence_1D(integrand) result(integral)
      !! Result is the mimetic quadrature corresponding to a volume integral of a scalar-divergence product
      implicit none
      class(scalar_x_divergence_1D_t), intent(in) :: integrand
      double precision integral
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the discrete surface integral of a weighted_product_1D_t field by summing
    !          the stored boundary-weighted product values.
    ! KEYWORDS: surface-integral, quadrature, mimetic, Corbino-Castillo, weighted-product,
    !           boundary-operator, summation
    ! CONTEXT: This interface declares the .SS. surface integration operator for
    !          weighted_product_1D_t in the formal library. The implementation sums the stored values
    !          that were computed as dx * B * v * f by the weighted_premultiply procedure, where B is
    !          the Corbino & Castillo (2020) Eq. 7 boundary operator. This represents the surface
    !          integral term in the extended Gauss divergence theorem.
    pure module function surface_integrate_vector_x_scalar_1D(integrand) result(integral)
      !! Result is the mimetic quadrature corresponding to a surface integral of a scalar-vector product
      implicit none
      class(weighted_product_1D_t), intent(in) :: integrand
      double precision integral
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the element-wise dot product of a vector_1D field with a gradient_1D field,
    !          producing a vector_dot_gradient_1D_t object that carries both the node-centered
    !          product values and the gradient quadrature weights for subsequent volume integration.
    ! KEYWORDS: dot-product, vector-gradient, mimetic, node-centered, quadrature-weights,
    !           structured-grid, staggered-grid, operator-overloading
    ! CONTEXT: This interface declares the .dot. operator between a vector_1D_t and a gradient_1D_t
    !          in the formal library. The implementation in gradient_1D_s computes the element-wise
    !          product of the vector and gradient face-centered values and stores the result along
    !          with the gradient quadrature weights in a vector_dot_gradient_1D_t object. The
    !          gradient_1D argument is the passed-object dummy, allowing the syntax
    !          v .dot. (.grad. f) where the gradient result is the dispatching object.
    pure module function dot(vector_1D, gradient_1D) result(vector_dot_gradient_1D)
      !! Result is the mimetic divergence of the vector_1D_t "self"
      implicit none
      class(gradient_1D_t), intent(in) :: gradient_1D
      type(vector_1D_t), intent(in) :: vector_1D
      type(vector_dot_gradient_1D_t) vector_dot_gradient_1D
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the dot product of a vector_1D field with a surface normal differential area
    !          element dS, producing a vector_1D_t object that carries the element-wise product of
    !          the face-centered vector values with dS and inherits the vector's divergence operator.
    ! KEYWORDS: dot-product, surface-normal, differential-area, vector_1D, operator-overloading,
    !           structured-grid, staggered-grid, mimetic, boundary-integral, face-centered
    ! CONTEXT: This interface declares the .dot. operator between a vector_1D_t and a scalar dS in
    !          the formal library. The implementation in vector_1D_s performs element-wise
    !          multiplication of the face-centered vector values with dS and returns a new vector_1D_t
    !          that inherits the divergence operator. In 1D the surface normal dS is a scalar, so
    !          the operation is a simple scaling. This is used in surface integral expressions such
    !          as .SS. (f .x. (v .dot. dA)).
    pure module function dot_surface_normal(vector_1D, dS) result(v_dot_dS)
      !! Result is magnitude of a vector/surface-normal dot product for use in surface integrals of the form
      !! `.SS. (f .x. (v .dot. dA))`
      !! The sign of the dot-product is incorporated into the weights in the weighted multiplication operator(.x.).
      implicit none
      class(vector_1D_t), intent(in) :: vector_1D
      double precision, intent(in) :: dS
      type(vector_1D_t) v_dot_dS
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the weighted product of a scalar_1D field and a vector_1D field using the
    !          mimetic boundary operator B from Corbino & Castillo (2020) Eq. 7, producing a
    !          weighted_product_1D_t suitable for surface integration.
    ! KEYWORDS: weighted-product, boundary-operator, mimetic, Corbino-Castillo, product-rule,
    !           structured-grid, staggered-grid, divergence, gradient, quadrature-weights,
    !           summation-by-parts, scalar_1D, vector_1D
    ! CONTEXT: This interface declares the .x. operator between a scalar_1D_t and a vector_1D_t in
    !          the formal library. The implementation in vector_1D_s assembles the boundary operator
    !          B = Q*D + G^T*P from Corbino & Castillo (2020) Eq. 7 and computes
    !          dx * B * v * f. The result is a weighted_product_1D_t whose values can be surface-
    !          integrated via the .SS. operator to yield the boundary term in the extended Gauss
    !          divergence theorem.
    pure module function weighted_premultiply(scalar_1D, vector_1D) result(weighted_product_1D)
      !! Result is the product of a boundary-weighted vector_1D_t with a scalar_1D_t
      implicit none
      type(scalar_1D_t), intent(in) :: scalar_1D
      class(vector_1D_t), intent(in) :: vector_1D
      type(weighted_product_1D_t) weighted_product_1D
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the gradient quadrature weights for a tensor_1D_t object, returning an array
    !          of m+1 weights used for weighted inner products involving gradient fields on the
    !          node-centered staggered-grid.
    ! KEYWORDS: quadrature-weights, gradient, mimetic, node-centered, structured-grid, staggered-grid,
    !           summation-by-parts, accessor
    ! CONTEXT: This interface declares the gradient quadrature weights accessor for tensor_1D_t in
    !          the formal library. The implementation in weights_1D_s computes the m+1 quadrature
    !          weights for integrating products on the node-centered grid where gradient fields are
    !          defined. These weights appear in the summation-by-parts identities (Corbino & Castillo,
    !          2020, Eq. 17) and in the boundary operator B construction (Eq. 7). The procedure is
    !          bound to tensor_1D_t and exposed via the generic name weights on gradient_1D_t and
    !          (conditionally) vector_1D_t.
    pure module function gradient_1D_weights(self) result(weights)
      !! Result is an array of quadrature coefficients that can be used to compute a weighted
      !! inner product  of a vector_1D_t object and a gradient_1D_t object.
      implicit none
      class(tensor_1D_t), intent(in) :: self
      double precision, allocatable :: weights(:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the divergence quadrature weights for a tensor_1D_t object, returning an
    !          array of m+2 weights used for weighted inner products involving divergence fields on
    !          the cell-centered extended grid.
    ! KEYWORDS: quadrature-weights, divergence, mimetic, cell-centered, structured-grid, staggered-grid,
    !           summation-by-parts, accessor
    ! CONTEXT: This interface declares the divergence quadrature weights accessor for tensor_1D_t in
    !          the formal library. The implementation in weights_1D_s computes the m+2 quadrature
    !          weights for integrating products on the cell-centered extended grid where divergence
    !          fields are defined. These weights appear in the summation-by-parts identities (Corbino
    !          & Castillo, 2020, Eq. 19) and in the boundary operator B construction (Eq. 7). The
    !          procedure is bound to tensor_1D_t and exposed via the generic name weights on
    !          divergence_1D_t.
    pure module function divergence_1D_weights(self) result(weights)
      !! Result is an array of quadrature coefficients that can be used to compute a weighted
      !! inner product  of a scalar_1D_t object and a divergence_1D_t object.
      implicit none
      class(tensor_1D_t), intent(in) :: self
      double precision, allocatable :: weights(:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the element-wise product of a scalar_1D field premultiplied onto a
    !          divergence_1D field, producing a scalar_x_divergence_1D_t that carries the
    !          cell-centered product values and the divergence quadrature weights for subsequent
    !          volume integration.
    ! KEYWORDS: scalar-divergence-product, premultiply, mimetic, cell-centered, quadrature-weights,
    !           structured-grid, staggered-grid, operator-overloading
    ! CONTEXT: This interface declares the * operator with a scalar_1D_t on the left and a
    !          divergence_1D_t on the right in the formal library. The implementation in
    !          divergence_1D_s extracts the interior m cell-center values from the scalar field,
    !          multiplies them element-wise with the divergence values, and stores the result along
    !          with the divergence quadrature weights in a scalar_x_divergence_1D_t. The pass
    !          attribute on divergence_1D makes this a type-bound procedure of divergence_1D_t.
    pure module function premultiply_scalar_1D(scalar_1D, divergence_1D) result(scalar_x_divergence_1D)
      !! Result is the point-wise product of a 1D scalar field and the divergence of a 1D vector field
      implicit none
      type(scalar_1D_t), intent(in) :: scalar_1D
      class(divergence_1D_t), intent(in) :: divergence_1D
      type(scalar_x_divergence_1D_t) scalar_x_divergence_1D
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the element-wise product of a divergence_1D field postmultiplied by a
    !          scalar_1D field, producing a scalar_x_divergence_1D_t that carries the cell-centered
    !          product values and the divergence quadrature weights for subsequent volume integration.
    ! KEYWORDS: scalar-divergence-product, postmultiply, mimetic, cell-centered, quadrature-weights,
    !           structured-grid, staggered-grid, operator-overloading
    ! CONTEXT: This interface declares the * operator with a divergence_1D_t on the left and a
    !          scalar_1D_t on the right in the formal library. The implementation in divergence_1D_s
    !          extracts the interior m cell-center values from the scalar field, multiplies them
    !          element-wise with the divergence values, and stores the result along with the
    !          divergence quadrature weights in a scalar_x_divergence_1D_t. This provides
    !          commutativity of the scalar-divergence product.
    pure module function postmultiply_scalar_1D(divergence_1D, scalar_1D) result(scalar_x_divergence_1D)
      !! Result is the point-wise product of a 1D scalar field and the divergence of a 1D vector field
      implicit none
      class(divergence_1D_t), intent(in) :: divergence_1D
      type(scalar_1D_t), intent(in) :: scalar_1D
      type(scalar_x_divergence_1D_t) scalar_x_divergence_1D
    end function
    ! END CODE CHUNK

  end interface

#ifndef __GFORTRAN__

contains

  ! PURPOSE: Computes the cell-center x-coordinates for a uniform 1D grid given the domain bounds
  !          and number of cells, returning an array of cell-center locations offset by half a cell
  !          width from x_min.
  ! KEYWORDS: grid, cell-center, uniform-mesh, 1D, structured-grid, staggered-grid, utility
  ! CONTEXT: This module-level function is compiled for non-gfortran compilers and provides
  !          cell-center coordinate computation needed throughout the tensors_1D_m module. It
  !          constructs a uniform grid with cell width dx = (x_max - x_min)/cells and places each
  !          cell center at x_min + dx/2 + (cell-1)*dx using an implied do loop. For gfortran, an
  !          identical function is defined locally within the scalar_1D_s submodule. This function
  !          is used by scalar_1D_grid_locations to build the extended grid array that includes both
  !          boundary points and cell centers.
  pure function cell_center_locations(x_min, x_max, cells) result(x)
    double precision, intent(in) :: x_min, x_max
    integer, intent(in) :: cells
    double precision, allocatable:: x(:)
    integer cell

    associate(dx => (x_max - x_min)/cells)
      x = x_min + dx/2. + [((cell-1)*dx, cell = 1, cells)]
    end associate
  end function
  ! END CODE CHUNK

#endif

end module tensors_1D_m
