! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"
#include "formal-language-support.F90"

submodule(mimetic_operators_1D_m) gradient_operator_1D_s
  use julienne_m, only : call_julienne_assert_, string_t
#if ASSERTIONS
  use julienne_m, only : operator(.isAtLeast.)
#endif
  implicit none

contains

#ifdef __GFORTRAN__

  ! PURPOSE: Transforms the upper boundary block submatrix "A" of a mimetic gradient operator into
  !          the corresponding lower boundary block "A'" by reversing elements within rows (with sign
  !          negation) and then reversing elements within columns, implementing the antisymmetric
  !          reflection required by the Corbino & Castillo (2020) mimetic operator structure.
  ! KEYWORDS: mimetic, gradient, boundary-block, matrix-transformation, antisymmetric, Corbino-Castillo,
  !           gfortran, utility
  ! CONTEXT: This helper function is only compiled under gfortran and provides the transformation from
  !          the upper block "A" to the lower block "A'" needed when constructing the mimetic gradient
  !          operator matrix in the formal library. The mimetic gradient operator has a block structure
  !          consisting of an upper boundary block A, a repeated interior row M, and a lower boundary
  !          block A' that is derived from A by negating and flipping. Other compilers may access this
  !          functionality through a different code path. This function is called by
  !          construct_1D_gradient_operator during operator assembly.
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

  ! PURPOSE: Constructs a 1D mimetic gradient operator for a given order of accuracy k and cell width
  !          dx on a grid with the specified number of cells. The operator is assembled in the block
  !          structure of Corbino & Castillo (2020), consisting of an upper boundary block A, a
  !          repeated interior stencil row M, and a lower boundary block A' derived from A via
  !          antisymmetric reflection. Internal helper functions corbino_castillo_A and
  !          corbino_castillo_M compute the order-specific stencil coefficients for 2nd-order and
  !          4th-order accuracy.
  ! KEYWORDS: mimetic, gradient, operator-construction, Corbino-Castillo, finite-difference,
  !           structured-grid, staggered-grid, 2nd-order, 4th-order, boundary-block, interior-stencil,
  !           block-matrix
  ! CONTEXT: This procedure constructs the gradient_operator_1D_t object in the formal library's
  !          mimetic finite-difference framework following the formulation of Corbino & Castillo (2020).
  !          The gradient operator maps from m+2 cell-centered values (including boundary ghost values)
  !          to m+1 node-centered values, where m is the number of cells. The internal function
  !          corbino_castillo_A returns the upper boundary submatrix specific to the requested order:
  !          a 1x3 row for 2nd-order and a 2x5 block for 4th-order. The internal function
  !          corbino_castillo_M returns the interior stencil row: a 2-point stencil for 2nd-order and
  !          a 4-point stencil for 4th-order. The lower block A' is computed from A via
  !          negate_and_flip. An assertion verifies the grid has enough cells to accommodate the
  !          boundary blocks. The constructed object stores the block components along with the order
  !          k, cell width dx, and cell count m for use by the matrix-vector multiply and assembly
  !          procedures.
  module procedure construct_1D_gradient_operator

    call_julienne_assert(cells .isAtLeast. 2*k)

    associate(A => corbino_castillo_A(k, dx), M => corbino_castillo_M(k, dx))
      gradient_operator_1D%mimetic_matrix_1D_t = mimetic_matrix_1D_t(A, M, negate_and_flip(A))
      gradient_operator_1D%k_  = k
      gradient_operator_1D%dx_ = dx
      gradient_operator_1D%m_  = cells
    end associate

  contains

    pure function corbino_castillo_A(k, dx) result(matrix_block)
      integer, intent(in) :: k
      double precision, intent(in) :: dx
      double precision, allocatable :: matrix_block(:,:)

      order_of_accuracy: &
      select case(k)
      case(2)
        matrix_block = reshape([-8D0/3D0, 3D0, -1D0/3D0] , shape=[1,3]) / dx
      case(4)
        matrix_block = reshape([ &
           -352D0/105D0,  35D0/ 8D0, -35D0/24D0, 21D0/40D0, -5D0/ 56D0 &
          ,  16D0/105D0, -31D0/24D0,  29D0/24D0, -3D0/40D0,  1D0/168D0 &
        ], shape=[2,5], order=[2,1]) / dx
      case default
        associate(string_k => string_t(k))
          error stop "corbino_castillo_A: unsupported order of accuracy: " // string_k%string()
        end associate
      end select order_of_accuracy

    end function

    pure function corbino_castillo_M(k, dx) result(row)
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
          error stop "corbino_castillo_A: unsupported order of accuracy: " // string_k%string()
        end associate
      end select order_of_accuracy

    end function

  end procedure construct_1D_gradient_operator
  ! END CODE CHUNK

  ! PURPOSE: Computes the matrix-vector product of the mimetic gradient operator with an input vector
  !          of m+2 cell-centered values (including boundary ghost values), producing an output vector
  !          of m+1 node-centered values, by applying the upper block A, the repeated interior stencil
  !          M via do concurrent, and the lower block A' separately.
  ! KEYWORDS: mimetic, gradient, matrix-vector-multiply, operator-application, Corbino-Castillo,
  !           structured-grid, staggered-grid, do-concurrent, boundary-block, interior-stencil,
  !           block-matrix
  ! CONTEXT: This procedure implements the action of the mimetic gradient operator on a vector in the
  !          formal library's mimetic finite-difference framework. Rather than assembling and storing
  !          the full dense matrix, it exploits the block structure of the Corbino & Castillo (2020)
  !          operator: the upper boundary rows are computed via matmul with the stored upper block, the
  !          interior rows are computed via dot_product with the repeated interior stencil in a do
  !          concurrent loop, and the lower boundary rows are computed via matmul with the stored lower
  !          block. Unlike the divergence operator, which has zero-padded first and last rows, the
  !          gradient operator produces values at all m+1 nodes without zero padding. The interior
  !          stencil is applied starting from an offset of 1 to account for the node-centered output
  !          grid's relationship to the cell-centered input grid. Conditional compilation handles
  !          differences in do concurrent syntax support across compilers.
  module procedure gradient_matrix_multiply

    double precision, allocatable :: product_inner(:)

    associate( &
       upper_rows => size(self%upper_,1) &
      ,lower_rows => size(self%lower_,1) &
    )
      associate( &
         inner_rows    => size(vec) - (upper_rows + lower_rows + 1) &
        ,inner_columns => size(self%inner_) &
      )
        allocate(product_inner(inner_rows))

#if HAVE_DO_CONCURRENT_TYPE_SPEC_SUPPORT && HAVE_LOCALITY_SPECIFIER_SUPPORT
        do concurrent(integer :: row = 1 : inner_rows) default(none) shared(product_inner, self, vec, inner_columns)
          product_inner(row) = dot_product(self%inner_, vec(row + 1 : row + inner_columns))
        end do
#else
        block
          integer row
          do concurrent(row = 1 : inner_rows)
            product_inner(row) = dot_product(self%inner_, vec(row + 1 : row + inner_columns))
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
         matmul(self%upper_, vec(1 : upper_columns)) &
        ,product_inner &
        ,matmul(self%lower_, vec(size(vec) - lower_columns + 1 : )) &
      ]
    end associate
  end procedure
  ! END CODE CHUNK

  ! PURPOSE: Assembles the full dense matrix representation of the mimetic gradient operator by
  !          applying the operator to each column of the identity matrix via the matrix-vector
  !          multiply procedure, producing an (m+1) x (m+2) matrix. An internal helper function e
  !          generates the unit basis vectors used as identity matrix columns.
  ! KEYWORDS: mimetic, gradient, matrix-assembly, dense-matrix, identity-matrix, operator-assembly,
  !           structured-grid, staggered-grid, do-concurrent, Corbino-Castillo
  ! CONTEXT: This procedure constructs the full dense matrix form of the mimetic gradient operator in
  !          the formal library. While the operator is typically applied in matrix-free form via
  !          gradient_matrix_multiply for efficiency, the dense matrix is useful for debugging,
  !          visualization, and verification purposes. The assembly works by applying the operator to
  !          each standard basis vector e_i of length m+2 using do concurrent, where each resulting
  !          column of the output matrix is the operator's response to that basis vector. Conditional
  !          compilation handles differences in do concurrent syntax support across compilers, with a
  !          gfortran workaround that calls gradient_matrix_multiply directly rather than using the
  !          overloaded .x. operator. The internal function e constructs the i-th unit vector of the
  !          specified length.
  module procedure assemble_gradient

    associate(rows => self%m_ + 1, cols => self%m_ + 2)

      allocate(G(rows, cols), source = 0D0)

#if HAVE_DO_CONCURRENT_TYPE_SPEC_SUPPORT && HAVE_LOCALITY_SPECIFIER_SUPPORT
      do concurrent(integer :: col=1:cols) default(none) shared(G, self, cols)
        G(:,col) = self .x. e(dir=col, length=cols)
      end do
#else
      block
        integer col
        do concurrent(col=1:cols)
          G(:,col) = gradient_matrix_multiply(self, e(dir=col, length=cols)) !! work around gfortran 13-14 issue
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

end submodule gradient_operator_1D_s
