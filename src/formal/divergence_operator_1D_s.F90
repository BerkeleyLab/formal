! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "formal-language-support.F90"
#include "julienne-assert-macros.h"

submodule(mimetic_operators_1D_m) divergence_operator_1D_s
  use julienne_m, only : call_julienne_assert_, string_t
#if ASSERTIONS
  use julienne_m, only : operator(.isAtLeast.), operator(.equalsExpected.)
#endif
  implicit none
contains

#ifdef __GFORTRAN__

  ! PURPOSE: Transforms the upper boundary block submatrix "A" of a mimetic divergence operator into
  !          the corresponding lower boundary block "A'" by reversing elements within rows (with sign
  !          negation) and then reversing elements within columns, implementing the antisymmetric
  !          reflection required by the Corbino & Castillo (2020) mimetic operator structure.
  ! KEYWORDS: mimetic, divergence, boundary-block, matrix-transformation, antisymmetric, Corbino-Castillo,
  !           gfortran, utility
  ! CONTEXT: This helper function is only compiled under gfortran and provides the transformation from
  !          the upper block "A" to the lower block "A'" needed when constructing the mimetic divergence
  !          operator matrix in the formal library. The mimetic divergence operator has a block structure
  !          consisting of an upper boundary block A, a repeated interior row M, and a lower boundary
  !          block A' that is derived from A by negating and flipping. Other compilers may access this
  !          functionality through a different code path. This function is called by
  !          construct_1D_divergence_operator during operator assembly.
  pure function negate_and_flip(A) result(Ap)
    !! Transform a mimetic matrix upper block into a lower block
    double precision, intent(in) :: A(:,:)
    double precision, allocatable :: Ap(:,:)
    integer row, column

    allocate(Ap, mold=A)

    reverse_elements_within_rows_and_flip_sign: &
    do concurrent(row = 1:size(Ap,1))
      Ap(row,:) = -A(row,size(A,2):1:-1)
    end do reverse_elements_within_rows_and_flip_sign

    reverse_elements_within_columns: &
    do concurrent(column = 1 : size(Ap,2))
      Ap(:,column) = Ap(size(Ap,1):1:-1,column)
    end do reverse_elements_within_columns

  end function
  ! END CODE CHUNK

#endif
 
  ! PURPOSE: Constructs a 1D mimetic divergence operator for a given order of accuracy k and cell
  !          width dx on a grid with the specified number of cells. The operator is assembled in the
  !          block structure of Corbino & Castillo (2020), consisting of an upper boundary block A,
  !          a repeated interior stencil row M, and a lower boundary block A' derived from A via
  !          antisymmetric reflection. Internal helper functions A_block and M compute the order-
  !          specific stencil coefficients for 2nd-order and 4th-order accuracy.
  ! KEYWORDS: mimetic, divergence, operator-construction, Corbino-Castillo, finite-difference,
  !           structured-grid, staggered-grid, 2nd-order, 4th-order, boundary-block, interior-stencil,
  !           block-matrix
  ! CONTEXT: This procedure constructs the divergence_operator_1D_t object in the formal library's
  !          mimetic finite-difference framework following the formulation of Corbino & Castillo (2020).
  !          The divergence operator maps from m+1 node-centered values to m+2 cell-centered values
  !          (with zero boundary rows), where m is the number of cells. The internal function A_block
  !          returns the upper boundary submatrix specific to the requested order: an empty matrix for
  !          2nd-order (no boundary correction needed) and a 1x5 row for 4th-order. The internal
  !          function M returns the interior stencil row: a 2-point stencil for 2nd-order and a
  !          4-point stencil for 4th-order. The lower block A' is computed from A via negate_and_flip.
  !          An assertion verifies the grid has enough cells to accommodate the boundary blocks. The
  !          constructed object stores the block components along with the order k, cell width dx, and
  !          cell count m for use by the matrix-vector multiply and assembly procedures.
  module procedure construct_1D_divergence_operator

    double precision, allocatable :: Ap(:,:)

    call_julienne_assert(cells .isAtLeast. 2*k+1)

    associate(A => A_block(k,dx))
      if (size(A) /= 0) then
        Ap = negate_and_flip(A)
      else
        allocate(Ap, mold = A)
      end if
      divergence_operator_1D%mimetic_matrix_1D_t = mimetic_matrix_1D_t(A, M(k, dx), Ap)
      divergence_operator_1D%k_  = k
      divergence_operator_1D%dx_ = dx
      divergence_operator_1D%m_  = cells
    end associate

  contains

    pure function A_block(k, dx) result(matrix_block)
      !! Compute the upper block submatrix "A" of the Corbino & Castillo (2020) mimetic divergence operator
      integer, intent(in) :: k
      double precision, intent(in) :: dx
      double precision, allocatable :: matrix_block(:,:)

      order_of_accuracy: &
      select case(k)
      case(2)
        matrix_block = reshape([ double precision :: &
        ], shape=[0,0])
      case(4)
        matrix_block = reshape([ &
          -11/12D0, 17/24D0, 3/8D0, -5/24D0,  1/24D0 &
        ], shape=[1,5], order=[2,1]) / dx
      case default
        associate(string_k => string_t(k))
          error stop "A (divergence_operator_1D_s): unsupported order of accuracy: " // string_k%string()
        end associate
      end select order_of_accuracy

    end function

    pure function M(k, dx) result(row)
      !! Compute the middle block submatrix "M" of the Corbino & Castillo (2020) mimetic divergence operator
      integer, intent(in) :: k
      double precision, intent(in) :: dx
      double precision, allocatable :: row(:)

      order_of_accuracy: &
      select case(k)
      case(2)
        row = [-1D0, 1D0]/ dx        
      case(4)
        row = [1D0/24D0, -9D0/8D0, 9D0/8D0, -1D0/24D0] / dx        
      case default
        associate(string_k => string_t(k))
          error stop "M (divergence_operator_1D_s): unsupported order of accuracy: " // string_k%string()
        end associate
      end select order_of_accuracy

    end function

  end procedure construct_1D_divergence_operator
  ! END CODE CHUNK

  ! PURPOSE: Returns the number of rows in the upper boundary block submatrix A of the mimetic
  !          divergence operator.
  ! KEYWORDS: mimetic, divergence, boundary-block, submatrix-rows, accessor, getter
  ! CONTEXT: This procedure is a simple accessor that returns the row count of the upper boundary
  !          block A stored in a divergence_operator_1D_t object. The number of rows in A determines
  !          the depth of the boundary region where the divergence stencil differs from the interior
  !          stencil. This information is used when partitioning the operator into boundary and
  !          interior regions for matrix-vector multiplication and convergence analysis.
  module procedure submatrix_A_rows
    call_julienne_assert(allocated(self%upper_))
    rows = size(self%upper_,1)
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Computes the matrix-vector product of the mimetic divergence operator with an input
  !          vector of m+1 node-centered values, producing an output vector of m+2 cell-centered
  !          values with zero boundary entries, by applying the upper block A, the repeated interior
  !          stencil M via do concurrent, and the lower block A' separately.
  ! KEYWORDS: mimetic, divergence, matrix-vector-multiply, operator-application, Corbino-Castillo,
  !           structured-grid, staggered-grid, do-concurrent, boundary-block, interior-stencil,
  !           block-matrix
  ! CONTEXT: This procedure implements the action of the mimetic divergence operator on a vector in
  !          the formal library's mimetic finite-difference framework. Rather than assembling and
  !          storing the full dense matrix, it exploits the block structure of the Corbino & Castillo
  !          (2020) operator: the upper boundary rows are computed via matmul with the stored upper
  !          block, the interior rows are computed via dot_product with the repeated interior stencil
  !          in a do concurrent loop, and the lower boundary rows are computed via matmul with the
  !          stored lower block. The first and last entries of the result are set to zero, consistent
  !          with the mimetic operator structure. Conditional compilation handles differences in
  !          do concurrent syntax support across compilers. An assertion verifies the input vector
  !          has size m+1 and the output has size m+2.
  module procedure divergence_matrix_multiply

    double precision, allocatable :: product_inner(:)

    associate( &
       upper_rows => size(self%upper_,1) &
      ,lower_rows => size(self%lower_,1) &
    )
      associate( &
         inner_rows    => self%m_ - (upper_rows + lower_rows) & ! rows(A) + rows(M) + rows(A') + 2 rows of zeros == m + 2 (Corbino & Castillo, 2020)
        ,inner_columns => size(self%inner_) &
      )
        call_julienne_assert((size(vec) .equalsExpected. self%m_ + 1))
        allocate(product_inner(inner_rows))

#if HAVE_DO_CONCURRENT_TYPE_SPEC_SUPPORT && HAVE_LOCALITY_SPECIFIER_SUPPORT
        do concurrent(integer :: row = 1 : inner_rows) default(none) shared(product_inner, self, vec, inner_columns)
          product_inner(row) = dot_product(self%inner_, vec(row : row + inner_columns  - 1))
        end do
#else
        block
          integer row
          do concurrent(row = 1 : inner_rows)
            product_inner(row) = dot_product(self%inner_, vec(row : row + inner_columns  - 1))
          end do
        end block
#endif

      end associate
    end associate

    associate( &
       upper_columns => size(self%upper_,2) &
      ,lower_columns => size(self%lower_,2) &
    )
      matvec_product = [ &
         0D0 &
        ,matmul(self%upper_, vec(1 : upper_columns )) &
        ,product_inner &
        ,matmul(self%lower_, vec(size(vec) - lower_columns + 1 : )) &
        ,0D0 &
      ]
      call_julienne_assert(size(matvec_product) .equalsExpected. self%m_ + 2)
    end associate

  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Assembles the full dense matrix representation of the mimetic divergence operator by
  !          applying the operator to each column of the identity matrix via the matrix-vector
  !          multiply procedure, producing an (m+2) x (m+1) matrix. An internal helper function e
  !          generates the unit basis vectors used as identity matrix columns.
  ! KEYWORDS: mimetic, divergence, matrix-assembly, dense-matrix, identity-matrix, operator-assembly,
  !           structured-grid, staggered-grid, do-concurrent, Corbino-Castillo
  ! CONTEXT: This procedure constructs the full dense matrix form of the mimetic divergence operator
  !          in the formal library. While the operator is typically applied in matrix-free form via
  !          divergence_matrix_multiply for efficiency, the dense matrix is useful for debugging,
  !          visualization, and verification purposes. The assembly works by applying the operator to
  !          each standard basis vector e_i of length m+1 using do concurrent, where each resulting
  !          column of the output matrix is the operator's response to that basis vector. Conditional
  !          compilation handles differences in do concurrent syntax support across compilers, with
  !          a gfortran workaround that calls divergence_matrix_multiply directly rather than using
  !          the overloaded .x. operator. The internal function e constructs the i-th unit vector of
  !          the specified length.
  module procedure assemble_divergence

    associate(rows => self%m_ + 2, cols => self%m_ + 1)

      allocate(D(rows, cols))

#if HAVE_DO_CONCURRENT_TYPE_SPEC_SUPPORT && HAVE_LOCALITY_SPECIFIER_SUPPORT
      do concurrent(integer :: col=1:cols) default(none) shared(D, self, cols)
        D(:,col) = self .x. e(dir=col, length=cols)
      end do
#else
      block
        integer col
        do concurrent(col=1:cols)
          D(:,col) = divergence_matrix_multiply(self, e(dir=col, length=cols)) ! work around gfortran 13-14 on Ubuntu
        end do
      end block
#endif
    end associate

  contains

    pure function e(dir, length) result(unit_vector)
      !! Result is the dir-th column of the len x len identity matrix
      integer, intent(in) :: dir, length
      double precision :: unit_vector(length)
      unit_vector(1:dir-1) = 0D0
      unit_vector(dir)     = 1D0
      unit_vector(dir+1:)  = 0D0
    end function

  end procedure
  ! END CODE CHUNK

end submodule divergence_operator_1D_s
