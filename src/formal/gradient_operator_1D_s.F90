! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"
#include "formal-language-support.F90"

submodule(differential_operators_1D_m) gradient_operator_1D_s
  use julienne_m, only : call_julienne_assert_, string_t
#if ASSERTIONS
  use julienne_m, only : operator(.isAtLeast.)
#endif
  implicit none

contains

#if !(HAVE_DO_CONCURRENT_TYPE_SPEC_SUPPORT && HAVE_LOCALITY_SPECIFIER_SUPPORT)

  pure function negate_and_flip(A) result(Ap)
    !! Transform a mimetic matrix upper block into a lower block
    real, intent(in) :: A(:,:)
    real, allocatable :: Ap(:,:)
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

#endif
 
  module procedure construct_1D_gradient_operator

    call_julienne_assert(cells .isAtLeast. 2*k)

    associate(A => corbino_castillo_A(k, dx), M => corbino_castillo_M(k, dx))
      gradient_operator_1D%differential_operator_matrix_1D_t = differential_operator_matrix_1D_t(A, M, negate_and_flip(A))
      gradient_operator_1D%k_  = k
      gradient_operator_1D%dx_ = dx
      gradient_operator_1D%m_  = cells
    end associate

  contains

    pure function corbino_castillo_A(k, dx) result(matrix_block)
      integer, intent(in) :: k
      real, intent(in) :: dx
      real, allocatable :: matrix_block(:,:)

      order_of_accuracy: &
      select case(k)
      case(2)
        matrix_block = reshape([-8E0/3E0, 3E0, -1E0/3E0] , shape=[1,3]) / dx
      case(4)
        matrix_block = reshape([ &
           -352E0/105E0,  35E0/ 8E0, -35E0/24E0, 21E0/40E0, -5E0/ 56E0 &
          ,  16E0/105E0, -31E0/24E0,  29E0/24E0, -3E0/40E0,  1E0/168E0 &
        ], shape=[2,5], order=[2,1]) / dx
      case default
        associate(string_k => string_t(k))
          error stop "corbino_castillo_A: unsupported order of accuracy: " // string_k%string()
        end associate
      end select order_of_accuracy

    end function

    pure function corbino_castillo_M(k, dx) result(row)
      integer, intent(in) :: k
      real, intent(in) :: dx
      real, allocatable :: row(:)

      order_of_accuracy: &
      select case(k)
      case(2)
        row = [-1E0, 1E0]/ dx        
      case(4)
        row = [1E0/24E0, -9E0/8E0, 9E0/8E0, -1E0/24E0] / dx        
      case default
        associate(string_k => string_t(k))
          error stop "corbino_castillo_A: unsupported order of accuracy: " // string_k%string()
        end associate
      end select order_of_accuracy

    end function

  end procedure construct_1D_gradient_operator

  module procedure gradient_matrix_multiply

    real, allocatable :: product_inner(:)

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

  module procedure assemble_gradient

    associate(rows => self%m_ + 1, cols => self%m_ + 2)

      allocate(G(rows, cols), source = 0E0)

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
      real :: unit_vector(length)
      unit_vector(1:dir-1) = 0E0
      unit_vector(dir)     = 1E0
      unit_vector(dir+1:)  = 0E0
    end function

  end procedure

end submodule gradient_operator_1D_s