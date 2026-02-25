! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "formal-language-support.F90"

module mimetic_operators_1D_m
  !! Define sparse matrix storage formats and operators tailored to the one-dimensional (1D) mimetic discretizations
  !! detaild by Corbino & Castillo (2020) https://doi.org/10.1016/j.cam.2019.06.042.
  use julienne_m, only : file_t
  implicit none

  private
  public :: gradient_operator_1D_t
  public :: divergence_operator_1D_t

  ! PURPOSE: Encapsulates the block-structured sparse storage of a 1D mimetic operator matrix
  !          consisting of an upper boundary block A, a repeated interior stencil row M, and a lower
  !          boundary block A', following the Corbino & Castillo (2020) formulation.
  ! KEYWORDS: mimetic, sparse-matrix, block-structure, Corbino-Castillo, structured-grid,
  !           staggered-grid, boundary-block, interior-stencil, base-type
  ! CONTEXT: This type is the base type for gradient_operator_1D_t and divergence_operator_1D_t in
  !          the formal library's mimetic finite-difference framework. Rather than storing the full
  !          dense matrix, it stores only the upper boundary block A, the single interior stencil row
  !          M (which repeats for all interior rows), and the lower boundary block A'. This compact
  !          representation exploits the banded Toeplitz-like structure of mimetic operators where
  !          all interior rows share the same stencil. The to_file_t type-bound procedure provides
  !          serialization of the stored blocks for debugging and output.
  type mimetic_matrix_1D_t
    !! Encapsulate a mimetic matrix with a corresponding matrix-vector product operator
    private
    double precision, allocatable :: upper_(:,:) !! A  submatrix block (cf. Corbino & Castillo, 2020)
    double precision, allocatable :: inner_(:)   !! M  submatrix row   (cf. Corbino & Castillo, 2020)
    double precision, allocatable :: lower_(:,:) !! A' submatrix block (cf. Corbino & Castillo, 2020)
  contains
    procedure, non_overridable :: to_file_t
  end type
  ! END CODE CHUNK

  interface mimetic_matrix_1D_t

    ! PURPOSE: Constructs a mimetic_matrix_1D_t object from the provided upper boundary block A,
    !          interior stencil row M, and lower boundary block A'.
    ! KEYWORDS: mimetic, sparse-matrix, construction, block-structure, Corbino-Castillo,
    !           structured-grid, staggered-grid, boundary-block, interior-stencil
    ! CONTEXT: This interface provides the constructor for the mimetic_matrix_1D_t base type in the
    !          formal library. It accepts the three block components that define a mimetic operator
    !          matrix: the upper boundary block A, the repeated interior stencil row M, and the lower
    !          boundary block A'. The gradient and divergence operator constructors delegate to this
    !          constructor after computing the order-specific stencil coefficients.
    pure module function construct_matrix_operator(upper, inner, lower) result(mimetic_matrix_1D)
      !! Construct discrete operator from matrix blocks
      implicit none
      double precision, intent(in) :: upper(:,:) !! A  submatrix block (cf. Corbino & Castillo, 2020)
      double precision, intent(in) :: inner(:)   !! M  submatrix row   (cf. Corbino & Castillo, 2020)
      double precision, intent(in) :: lower(:,:) !! A' submatrix block (cf. Corbino & Castillo, 2020)
      type(mimetic_matrix_1D_t) mimetic_matrix_1D
    end function
    ! END CODE CHUNK

  end interface

  ! PURPOSE: Encapsulates a 1D mimetic gradient operator that maps from m+2 cell-centered values
  !          (including boundary ghost values) to m+1 node-centered gradient values, extending the
  !          mimetic_matrix_1D_t base type with the order of accuracy k, cell count m, and cell
  !          width dx.
  ! KEYWORDS: mimetic, gradient, operator, sparse-matrix, Corbino-Castillo, structured-grid,
  !           staggered-grid, finite-difference, cell-to-node, extended-type
  ! CONTEXT: This type extends mimetic_matrix_1D_t in the formal library's mimetic finite-difference
  !          framework to represent the gradient operator specifically. It adds the order of accuracy
  !          k, cell count m, and cell width dx as private components needed for matrix-vector
  !          multiplication and assembly. The type provides a generic .x. operator for matrix-free
  !          application of the gradient to a vector and an assemble procedure for constructing the
  !          full dense matrix representation. The gradient operator maps from the m+2 extended
  !          scalar grid (cell centers plus boundary ghost values) to the m+1 node-centered
  !          staggered-grid.
  type, extends(mimetic_matrix_1D_t) :: gradient_operator_1D_t
    !! Encapsulate a 1D mimetic gradient operator
    private
    integer k_ !! order of accuracy
    integer m_ !! number of cells
    double precision dx_ !! cell width
  contains
    generic :: operator(.x.) => gradient_matrix_multiply
    procedure, non_overridable, private :: gradient_matrix_multiply
    generic :: assemble => assemble_gradient
    procedure, non_overridable, private :: assemble_gradient
  end type
  ! END CODE CHUNK

  interface gradient_operator_1D_t

    ! PURPOSE: Constructs a 1D mimetic gradient operator for a given order of accuracy k, cell width
    !          dx, and number of cells, assembling the block-structured operator matrix following
    !          the Corbino & Castillo (2020) formulation.
    ! KEYWORDS: mimetic, gradient, operator-construction, Corbino-Castillo, finite-difference,
    !           structured-grid, staggered-grid, 2nd-order, 4th-order, boundary-block,
    !           interior-stencil
    ! CONTEXT: This interface provides the constructor for gradient_operator_1D_t in the formal
    !          library's mimetic finite-difference framework. The implementation in
    !          gradient_operator_1D_s computes the order-specific upper boundary block A, interior
    !          stencil row M, and lower boundary block A' for the requested order of accuracy, and
    !          stores them along with k, dx, and the cell count. The constructed operator is used by
    !          the .grad. operator applied to scalar_1D_t objects.
    pure module function construct_1D_gradient_operator(k, dx, cells) result(gradient_operator_1D)
      !! Construct a mimetic gradient operator
      implicit none
      integer, intent(in) :: k !! order of accuracy
      double precision, intent(in) :: dx !! step size
      integer, intent(in) :: cells !! number of grid cells
      type(gradient_operator_1D_t) gradient_operator_1D
    end function
    ! END CODE CHUNK

  end interface

  ! PURPOSE: Encapsulates a kth-order 1D mimetic divergence operator that maps from m+1
  !          node-centered values to m+2 cell-centered values (with zero boundary rows), extending
  !          the mimetic_matrix_1D_t base type with the order of accuracy k, cell count m, and cell
  !          width dx.
  ! KEYWORDS: mimetic, divergence, operator, sparse-matrix, Corbino-Castillo, structured-grid,
  !           staggered-grid, finite-difference, node-to-cell, extended-type
  ! CONTEXT: This type extends mimetic_matrix_1D_t in the formal library's mimetic finite-difference
  !          framework to represent the divergence operator specifically. It adds the order of
  !          accuracy k, cell count m, and cell width dx as private components needed for
  !          matrix-vector multiplication and assembly. The type provides a generic .x. operator for
  !          matrix-free application of the divergence to a vector, an assemble procedure for
  !          constructing the full dense matrix representation, and a submatrix_A_rows accessor for
  !          querying the boundary block depth. The divergence operator maps from the m+1
  !          node-centered staggered-grid to the m+2 cell-centered grid with zero-padded boundary
  !          rows.
  type, extends(mimetic_matrix_1D_t) :: divergence_operator_1D_t
    !! Encapsulate kth-order mimetic divergence operator on m_ cells of width dx
    private
    integer k_, m_
    double precision dx_
  contains
    generic :: operator(.x.) => divergence_matrix_multiply
    procedure, non_overridable, private :: divergence_matrix_multiply
    generic :: assemble => assemble_divergence
    procedure, non_overridable, private :: assemble_divergence
    procedure, non_overridable :: submatrix_A_rows
  end type
  ! END CODE CHUNK

  interface divergence_operator_1D_t

    ! PURPOSE: Constructs a 1D mimetic divergence operator for a given order of accuracy k, cell
    !          width dx, and number of cells, assembling the block-structured operator matrix
    !          following the Corbino & Castillo (2020) formulation.
    ! KEYWORDS: mimetic, divergence, operator-construction, Corbino-Castillo, finite-difference,
    !           structured-grid, staggered-grid, 2nd-order, 4th-order, boundary-block,
    !           interior-stencil
    ! CONTEXT: This interface provides the constructor for divergence_operator_1D_t in the formal
    !          library's mimetic finite-difference framework. The implementation in
    !          divergence_operator_1D_s computes the order-specific upper boundary block A, interior
    !          stencil row M, and lower boundary block A' for the requested order of accuracy, and
    !          stores them along with k, dx, and the cell count. The constructed operator is used by
    !          the .div. operator applied to vector_1D_t objects.
    pure module function construct_1D_divergence_operator(k, dx, cells) result(divergence_operator_1D)
      !! Construct a mimetic gradient operator
      implicit none
      integer, intent(in) :: k !! order of accuracy
      double precision, intent(in) :: dx !! step size
      integer, intent(in) :: cells !! number of grid cells
      type(divergence_operator_1D_t) divergence_operator_1D
    end function
    ! END CODE CHUNK

  end interface

  interface

    ! PURPOSE: Returns the number of rows in the upper boundary block submatrix A of the mimetic
    !          divergence operator.
    ! KEYWORDS: mimetic, divergence, boundary-block, submatrix-rows, accessor, getter
    ! CONTEXT: This interface declares the accessor that returns the row count of the upper boundary
    !          block A stored in a divergence_operator_1D_t object. The implementation in
    !          divergence_operator_1D_s queries the allocated upper_ component. The number of rows
    !          in A determines the depth of the boundary region where the divergence stencil differs
    !          from the interior stencil, which is used when partitioning convergence analysis into
    !          boundary and interior regions.
    pure module function submatrix_A_rows(self) result(rows)
      !! Result is number of rows in the A block of the mimetic divergence matrix operator
      implicit none
      class(divergence_operator_1D_t), intent(in) :: self
      integer rows
    end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the matrix-vector product of the mimetic gradient operator with an input
    !          vector, producing the node-centered gradient values.
    ! KEYWORDS: mimetic, gradient, matrix-vector-multiply, operator-application, Corbino-Castillo,
    !           structured-grid, staggered-grid, do-concurrent
    ! CONTEXT: This interface declares the matrix-free application of the mimetic gradient operator
    !          to a vector in the formal library. The implementation in gradient_operator_1D_s
    !          exploits the block structure to apply the upper boundary block, repeated interior
    !          stencil, and lower boundary block separately, avoiding assembly of the full dense
    !          matrix. This procedure backs the generic .x. operator on gradient_operator_1D_t.
    pure module function gradient_matrix_multiply(self, vec) result(matvec_product)
      !! Result is mimetic gradient vector
      implicit none
      class(gradient_operator_1D_t), intent(in) :: self
      double precision, intent(in) :: vec(:)
      double precision, allocatable :: matvec_product(:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Assembles the full dense matrix representation of the mimetic gradient operator,
    !          producing an (m+1) x (m+2) matrix.
    ! KEYWORDS: mimetic, gradient, matrix-assembly, dense-matrix, operator-assembly, structured-grid,
    !           staggered-grid, Corbino-Castillo
    ! CONTEXT: This interface declares the assembly of the full dense gradient operator matrix in the
    !          formal library. The implementation in gradient_operator_1D_s constructs the matrix by
    !          applying the operator to each standard basis vector via do concurrent. While the
    !          operator is typically applied in matrix-free form for efficiency, the dense matrix is
    !          useful for verifying the summation-by-parts identity and for debugging.
    pure module function assemble_gradient(self) result(G)
      !! Result is the assembled 1D mimetic gradient operator matrix
       implicit none
       class(gradient_operator_1D_t), intent(in) :: self
       double precision, allocatable :: G(:,:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Assembles the full dense matrix representation of the mimetic divergence operator,
    !          producing an (m+2) x (m+1) matrix.
    ! KEYWORDS: mimetic, divergence, matrix-assembly, dense-matrix, operator-assembly, structured-grid,
    !           staggered-grid, Corbino-Castillo
    ! CONTEXT: This interface declares the assembly of the full dense divergence operator matrix in
    !          the formal library. The implementation in divergence_operator_1D_s constructs the
    !          matrix by applying the operator to each standard basis vector via do concurrent. While
    !          the operator is typically applied in matrix-free form for efficiency, the dense matrix
    !          is useful for verifying the summation-by-parts identity and for debugging.
    pure module function assemble_divergence(self) result(D)
      !! Result is the assembled 1D mimetic divergence operator matrix
       implicit none
       class(divergence_operator_1D_t), intent(in) :: self
       double precision, allocatable :: D(:,:)
     end function
    ! END CODE CHUNK

    ! PURPOSE: Computes the matrix-vector product of the mimetic divergence operator with an input
    !          vector, producing the cell-centered divergence values with zero boundary entries.
    ! KEYWORDS: mimetic, divergence, matrix-vector-multiply, operator-application, Corbino-Castillo,
    !           structured-grid, staggered-grid, do-concurrent
    ! CONTEXT: This interface declares the matrix-free application of the mimetic divergence operator
    !          to a vector in the formal library. The implementation in divergence_operator_1D_s
    !          exploits the block structure to apply the upper boundary block, repeated interior
    !          stencil, and lower boundary block separately, avoiding assembly of the full dense
    !          matrix. This procedure backs the generic .x. operator on divergence_operator_1D_t.
    pure module function divergence_matrix_multiply(self, vec) result(matvec_product)
      !! Result is mimetic divergence defined at cell centers
      implicit none
      class(divergence_operator_1D_t), intent(in) :: self
      double precision, intent(in) :: vec(:)
      double precision, allocatable :: matvec_product(:)
    end function
    ! END CODE CHUNK

    ! PURPOSE: Serializes the mimetic_matrix_1D_t block components to a file_t object for output
    !          and debugging.
    ! KEYWORDS: mimetic, serialization, file-output, debugging, sparse-matrix, block-structure
    ! CONTEXT: This interface declares the serialization procedure for the mimetic_matrix_1D_t base
    !          type in the formal library. The implementation converts the stored upper block A,
    !          interior stencil row M, and lower block A' into a file_t representation suitable for
    !          writing to disk or displaying during debugging. This procedure is bound to the
    !          mimetic_matrix_1D_t type and is inherited by both gradient_operator_1D_t and
    !          divergence_operator_1D_t.
     pure module function to_file_t(self) result(file)
       implicit none
       class(mimetic_matrix_1D_t), intent(in) :: self
       type(file_t) file
     end function
    ! END CODE CHUNK

  end interface

contains

#if HAVE_DO_CONCURRENT_TYPE_SPEC_SUPPORT && HAVE_LOCALITY_SPECIFIER_SUPPORT

  ! PURPOSE: Transforms the upper boundary block submatrix "A" of a mimetic operator into the
  !          corresponding lower boundary block "A'" by reversing elements within rows (with sign
  !          negation) and then reversing elements within columns, implementing the antisymmetric
  !          reflection required by the Corbino & Castillo (2020) mimetic operator structure.
  ! KEYWORDS: mimetic, boundary-block, matrix-transformation, antisymmetric, Corbino-Castillo,
  !           do-concurrent, utility
  ! CONTEXT: This module-level function is compiled for compilers that support do concurrent with
  !          type specifiers and locality specifiers. It provides the transformation from the upper
  !          block "A" to the lower block "A'" needed when constructing mimetic gradient and
  !          divergence operators in the formal library. The gfortran-specific duplicates of this
  !          function are defined in divergence_operator_1D_s and gradient_operator_1D_s. Both the
  !          gradient and divergence operator constructors call this function during assembly.
  pure function negate_and_flip(A) result(Ap)
    !! Transform a mimetic matrix upper block into a lower block
    double precision, intent(in) :: A(:,:)
    double precision, allocatable :: Ap(:,:)

    allocate(Ap, mold=A)

    reverse_elements_within_rows_and_flip_sign: &
    do concurrent(integer :: row = 1:size(Ap,1)) default(none) shared(Ap, A)
      Ap(row,:) = -A(row,size(A,2):1:-1)
    end do reverse_elements_within_rows_and_flip_sign

    reverse_elements_within_columns: &
    do concurrent(integer :: column = 1 : size(Ap,2)) default(none) shared(Ap)
      Ap(:,column) = Ap(size(Ap,1):1:-1,column)
    end do reverse_elements_within_columns

  end function
  ! END CODE CHUNK
 
#else

! see divergence_operator_1D_s and gradient_operator_1D_s 

#endif

end module mimetic_operators_1D_m
