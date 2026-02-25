! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(tensors_1D_m) vector_1D_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(//) &
    ,operator(.all.) &
    ,operator(.approximates.) &
    ,operator(.csv.) &
    ,operator(.cat.) &
    ,operator(.sv.) &
    ,operator(.equalsExpected.) &
    ,operator(.isAtLeast.) &
    ,operator(.greaterThan.) &
    ,operator(.within.)
  implicit none

   double precision, parameter :: double_equivalence = 2D-4

contains

  ! PURPOSE: Computes the dot product of a vector_1D field with a surface normal differential area
  !          element dS, producing a vector_dot_dS_1D_t object that carries the element-wise product
  !          of the face-centered vector values with dS and inherits the vector's divergence operator.
  ! KEYWORDS: dot-product, surface-normal, differential-area, vector_1D, operator-overloading,
  !           structured-grid, staggered-grid, mimetic, boundary-integral, face-centered
  ! CONTEXT: This procedure implements the dot product of a vector_1D_t with the surface normal
  !          differential area element in the formal library's mimetic finite-difference framework.
  !          In 1D the surface normal dS is a scalar quantity at each face, so the operation is an
  !          element-wise multiplication of the face-centered vector values with dS. The resulting
  !          vector_dot_dS_1D_t object inherits the vector field's grid metadata and divergence
  !          operator, and is used in surface integral expressions such as .SS. (v .dot. dS) within
  !          the extended Gauss divergence theorem test.
  module procedure dot_surface_normal
     v_dot_dS%tensor_1D_t = tensor_1D_t(vector_1D%values_*dS, vector_1D%x_min_, vector_1D%x_max_, vector_1D%cells_, vector_1D%order_)
     v_dot_dS%divergence_operator_1D_ = vector_1D%divergence_operator_1D_
  end procedure
  ! END CODE CHUNK

#ifndef __GFORTRAN__

  ! PURPOSE: Constructs a vector_1D_t object by evaluating a user-provided initializer function on
  !          the face-centered grid locations and storing the resulting values along with grid
  !          metadata and a pre-built divergence operator for the specified order of accuracy and
  !          cell count.
  ! KEYWORDS: vector_1D, construction, initializer, structured-grid, staggered-grid, divergence-operator,
  !           finite-difference, mimetic, face-centered
  ! CONTEXT: This procedure constructs a vector_1D_t object in the formal library's mimetic
  !          finite-difference framework. The vector field is initialized on a face-centered grid
  !          consisting of m+1 face locations (including both domain boundaries), where m is the
  !          number of cells. The divergence_operator_1D_t is pre-built and stored within the vector
  !          object so that subsequent calls to the .div. operator can apply it without
  !          reconstruction. Assertions verify that x_max > x_min and that the cell count is at
  !          least 2*order+1 to support the mimetic stencil width. This version is compiled for
  !          non-gfortran compilers; gfortran uses an alternate definition below due to differences
  !          in procedure pointer handling in module procedure definitions.
  module procedure construct_1D_vector_from_function
    call_julienne_assert(x_max .greaterThan. x_min)
    call_julienne_assert(cells .isAtLeast. 2*order+1)

    associate(values => initializer(faces(x_min, x_max, cells)))
      vector_1D%tensor_1D_t = tensor_1D_t(values, x_min, x_max, cells, order)
    end associate
    vector_1D%divergence_operator_1D_ = divergence_operator_1D_t(k=order, dx=(x_max - x_min)/cells, cells=cells)
  end procedure
  ! END CODE CHUNK

#else

  ! PURPOSE: Constructs a vector_1D_t object by evaluating a user-provided initializer function on
  !          the face-centered grid locations and storing the resulting values along with grid
  !          metadata and a pre-built divergence operator for the specified order of accuracy and
  !          cell count. This is the gfortran-specific variant with an explicit function signature.
  ! KEYWORDS: vector_1D, construction, initializer, structured-grid, staggered-grid, divergence-operator,
  !           finite-difference, mimetic, face-centered, gfortran
  ! CONTEXT: This function is the gfortran-specific variant of construct_1D_vector_from_function in
  !          the formal library's mimetic finite-difference framework. It provides the same
  !          functionality as the non-gfortran module procedure but uses an explicit function
  !          signature rather than a module procedure definition, working around gfortran limitations
  !          with procedure pointer arguments in module procedure definitions. The vector field is
  !          initialized on a face-centered grid of m+1 values and the divergence_operator_1D_t is
  !          pre-built and stored for subsequent .div. operator applications. Assertions verify
  !          that x_max > x_min and that the cell count is at least 2*order+1.
  pure module function construct_1D_vector_from_function(initializer, order, cells, x_min, x_max) result(vector_1D)
    procedure(vector_1D_initializer_i), pointer :: initializer
    integer, intent(in) :: order !! order of accuracy
    integer, intent(in) :: cells !! number of grid cells spanning the domain
    double precision, intent(in) :: x_min !! grid location minimum
    double precision, intent(in) :: x_max !! grid location maximum
    type(vector_1D_t) vector_1D

    call_julienne_assert(x_max .greaterThan. x_min)
    call_julienne_assert(cells .isAtLeast. 2*order+1)

    associate(values => initializer(faces(x_min, x_max, cells)))
      vector_1D%tensor_1D_t = tensor_1D_t(values, x_min, x_max, cells, order)
    end associate
    vector_1D%divergence_operator_1D_ = divergence_operator_1D_t(k=order, dx=(x_max - x_min)/cells, cells=cells)
  end function
  ! END CODE CHUNK

#endif

  ! PURPOSE: Constructs a vector_1D_t object from pre-existing tensor_1D_t and
  !          divergence_operator_1D_t components, bypassing the initializer function evaluation.
  ! KEYWORDS: vector_1D, construction, component-assembly, tensor_1D, divergence-operator,
  !           structured-grid, staggered-grid
  ! CONTEXT: This procedure provides an alternative construction path for vector_1D_t objects in the
  !          formal library when the field values and divergence operator have already been computed
  !          separately. Rather than evaluating an initializer function on the grid, it directly
  !          assigns the provided tensor_1D_t base component and divergence_operator_1D_t. This is
  !          used internally when constructing vector fields from intermediate operator results or
  !          when reconstituting a vector field from its constituent parts.
  module procedure construct_from_components
    vector_1D%tensor_1D_t = tensor_1D
    vector_1D%divergence_operator_1D_ = divergence_operator_1D
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Computes the discrete divergence of the vector_1D field by applying the mimetic
  !          divergence operator to the stored face-centered values, producing a divergence_1D_t
  !          object that carries the cell-centered divergence values (with boundary zeros stripped)
  !          and verified quadrature weights satisfying the Corbino & Castillo (2020) Eq. 19
  !          identity.
  ! KEYWORDS: divergence, mimetic, operator-application, Corbino-Castillo, structured-grid,
  !           staggered-grid, vector_1D, divergence_1D, quadrature-weights, summation-by-parts,
  !           verification
  ! CONTEXT: This procedure implements the .div. operator for vector_1D_t objects in the formal
  !          library's mimetic finite-difference framework. It retrieves the stored
  !          divergence_operator_1D_t and applies it to the vector's m+1 face-centered values to
  !          produce an m+2 result, then strips the zero boundary entries to yield m cell-centered
  !          divergence values stored in the resulting divergence_1D_t. After construction, an
  !          assertion verifies the Corbino & Castillo (2020) Eq. 19 identity D^T * q = b/dx,
  !          where q is the divergence quadrature weights vector, D is the assembled divergence
  !          matrix, and b = [-1, 0, ..., 0, 1], ensuring the discrete operator satisfies the
  !          required summation-by-parts property. A compiler-conditional associate block with extra
  !          parentheses works around a NAG compiler issue with accessing the divergence operator
  !          component.
  module procedure div

    integer center

#ifdef NAGFOR
    associate(D => self%divergence_operator_1D_)
#else
    associate(D => (self%divergence_operator_1D_))
#endif
      associate(Dv => D .x. self%values_)
        divergence_1D%tensor_1D_t = tensor_1D_t(Dv(2:size(Dv)-1), self%x_min_, self%x_max_, self%cells_, self%order_)
        associate( &
           q  => divergence_1D%weights() &
          ,dx => (self%x_max_ - self%x_min_)/self%cells_ &
          ,b => [-1D0, [(0D0, center = 1, self%cells_-1)], 1D0] &
        )
          call_julienne_assert(.all. ([size(Dv), size(q)] .equalsExpected. self%cells_+2))
          call_julienne_assert((.all. (matmul(transpose(D%assemble()), q) .approximates. b/dx .within. double_equivalence)))
            ! Check D^T * a = b_{m+1},  Eq. (19), Corbino & Castillo (2020)
        end associate
      end associate
    end associate

  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Returns the face-centered vector values stored in a vector_1D_t object.
  ! KEYWORDS: vector_1D, accessor, face-centered-values, getter
  ! CONTEXT: This procedure is a simple accessor that exposes the internally stored m+1 face-centered
  !          values from a vector_1D_t object in the formal library. The face-centered values include
  !          both domain boundary faces and all interior cell faces. These values are used by other
  !          operators and test functions for computation and verification.
  module procedure vector_1D_values
    face_centered_values = self%values_
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Computes the face-centered x-coordinates for a uniform 1D grid given the domain bounds
  !          and number of cells, returning an array of m+1 face locations including both domain
  !          boundaries.
  ! KEYWORDS: grid, face-centered, uniform-mesh, 1D, structured-grid, staggered-grid, utility
  ! CONTEXT: This private helper function constructs the face-centered grid array used by the
  !          vector_1D submodule in the formal library. The face grid places x_min at the first
  !          position, x_max at the last position, and m-1 uniformly spaced interior faces between
  !          them, yielding m+1 total locations. This layout corresponds to the staggered-grid
  !          arrangement where vector quantities live at cell faces while scalar quantities live at
  !          cell centers. This function is called by construct_1D_vector_from_function during
  !          initialization and by vector_1D_grid to provide grid coordinates.
  pure function faces(x_min, x_max, cells) result(x)
    double precision, intent(in) :: x_min, x_max
    integer, intent(in) :: cells
    double precision, allocatable:: x(:)
    integer cell

    associate(dx => (x_max - x_min)/cells)
      x = [x_min, x_min + [(cell*dx, cell = 1, cells-1)], x_max]
    end associate
  end function
  ! END CODE CHUNK

  ! PURPOSE: Returns the face-centered grid locations for a vector_1D_t object by delegating to the
  !          faces helper function.
  ! KEYWORDS: grid, face-centered, accessor, vector_1D, structured-grid, staggered-grid, getter
  ! CONTEXT: This procedure provides access to the face-centered grid coordinates associated with a
  !          vector_1D_t object in the formal library. It delegates to the faces function, passing
  !          the stored domain bounds and cell count. The returned array of m+1 face locations
  !          includes both boundary faces and interior faces. Test functions and other operators use
  !          this accessor to retrieve grid coordinates for constructing spatially-varying expected
  !          values when verifying operator results.
  module procedure vector_1D_grid
    cell_faces  = faces(self%x_min_, self%x_max_, self%cells_)
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Computes the weighted product of a vector_1D field and a scalar_1D field using the
  !          mimetic boundary operator B from Corbino & Castillo (2020) Eq. 7, which combines the
  !          divergence and gradient quadrature weights with the assembled operator matrices to form
  !          the discrete analogue of the product rule integration term. Internal helper functions
  !          premultiply_diagonal and postmultiply_diagonal perform efficient diagonal matrix
  !          multiplication.
  ! KEYWORDS: weighted-product, boundary-operator, mimetic, Corbino-Castillo, product-rule,
  !           structured-grid, staggered-grid, divergence, gradient, quadrature-weights,
  !           summation-by-parts, operator-overloading, vector_1D, scalar_1D
  ! CONTEXT: This procedure implements the weighted multiplication of a vector_1D_t with a
  !          scalar_1D_t in the formal library's mimetic finite-difference framework, following
  !          Corbino & Castillo (2020) Eq. 7. The boundary operator B = Q*D + G^T*P combines the
  !          divergence matrix D pre-multiplied by the divergence quadrature weights Q with the
  !          transpose of the gradient matrix G post-multiplied by the gradient quadrature weights P.
  !          The result is computed as dx * B * v * f, where v is the face-centered vector field and
  !          f is the extended cell-centered scalar field. Assertions verify that the vector and
  !          scalar fields are compatible in size, cell count, order, and domain bounds, and that the
  !          assembled operator matrices have the expected dimensions. The internal helper functions
  !          premultiply_diagonal and postmultiply_diagonal efficiently multiply a matrix by a
  !          diagonal matrix represented as a 1D array, using do concurrent with compiler-conditional
  !          syntax variations.
  module procedure weighted_premultiply

                           !! vector values at faces                   scalar values at cell centers + boundaries
    call_julienne_assert(size(vector_1D%values_) .equalsExpected. size(scalar_1D%values_)-1)
    call_julienne_assert(     vector_1D%cells_   .equalsExpected.       scalar_1D%cells_ )
    call_julienne_assert(     vector_1D%order_   .equalsExpected.       scalar_1D%order_ )
    call_julienne_assert(vector_1D%x_min_ .approximates. scalar_1D%x_min_ .within. double_equivalence)
    call_julienne_assert(vector_1D%x_max_ .approximates. scalar_1D%x_max_ .within. double_equivalence)

    associate( &
      q => vector_1D%divergence_1D_weights() &
     ,p => vector_1D%gradient_1D_weights() &
     ,D => vector_1D%divergence_operator_1D_%assemble() &
     ,G => scalar_1D%gradient_operator_1D_%assemble() &
     ,m => scalar_1D%cells_ &
    )
      call_julienne_assert(.all. (shape(G) .equalsExpected. [m+1,m+2]))
      call_julienne_assert(.all. (shape(D) .equalsExpected. [m+2,m+1]))
      associate( &
         QD => premultiply_diagonal(q,D) &
        ,GTP => postmultiply_diagonal(transpose(G),p) &
        ,dx => vector_1D%dx() &
      )
        call_julienne_assert(.all. (shape(QD) .equalsExpected. shape(GTP)))
        associate(B => QD + GTP) ! Eq. (7), Corbino & Castillo (2020)
          weighted_product_1D%tensor_1D_t = &
            tensor_1D_t(dx * matmul(B,vector_1D%values_) * scalar_1D%values_, vector_1D%x_min_, vector_1D%x_max_, vector_1D%cells_, vector_1D%order_)
        end associate
      end associate
    end associate

  contains

    pure function premultiply_diagonal(d,A) result(DA)
      double precision, intent(in) :: d(:), A(:,:)
      double precision, allocatable :: DA(:,:)

      call_julienne_assert(size(d) .equalsExpected. size(A,1))

      allocate(DA, mold=A)

#ifdef HAVE_DO_CONCURRENT_TYPE_SPEC_SUPPORT && HAVE_LOCALITY_SPECIFIER_SUPPORT
      do concurrent(integer :: row = 1 : size(A,1)) default(none) shared(d, A, DA)
        DA(row,:) = d(row) * A(row,:)
      end do
#else
      block
        integer row
        do concurrent(row = 1 : size(A,1))
          DA(row,:) = d(row) * A(row,:)
        end do
      end block
#endif

    end function

    pure function postmultiply_diagonal(A,d) result(AD)
      double precision, intent(in) :: A(:,:), d(:)
      double precision, allocatable :: AD(:,:)

      call_julienne_assert(size(d) .equalsExpected. size(A,2))

      allocate(AD, mold=A)

#ifdef HAVE_DO_CONCURRENT_TYPE_SPEC_SUPPORT && HAVE_LOCALITY_SPECIFIER_SUPPORT
      do concurrent(integer :: column = 1 : size(A,2)) default(none) shared(d, A, AD)
        AD(:,column) = A(:,column) * d(column)
      end do
#else
      block
        integer column
        do concurrent(column = 1 : size(A,2))
          AD(:,column) = A(:,column) * d(column)
        end do
      end block
#endif

    end function

  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Returns the differential area element dA for the 1D case, which is always 1.0 since
  !          the cross-sectional area of a 1D domain is unity.
  ! KEYWORDS: differential-area, surface-element, 1D, vector_1D, accessor, getter, boundary-integral
  ! CONTEXT: This procedure provides the differential area element for a vector_1D_t object in the
  !          formal library's mimetic finite-difference framework. In one spatial dimension, the
  !          surface bounding each cell is a point with unit area, so dA is trivially 1.0. This
  !          accessor exists to maintain a consistent interface with higher-dimensional
  !          generalizations where dA would be a nontrivial geometric quantity. It is used in
  !          surface integral expressions such as .SS. (v .dot. dA) within the extended Gauss
  !          divergence theorem test.
  module procedure dA
    dA = 1D0
  end procedure
  ! END CODE CHUNK

end submodule vector_1D_s
