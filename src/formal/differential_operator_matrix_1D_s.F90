! Copyright (c) 2026, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

submodule(differential_operators_1D_m) differential_operator_matrix_1D_s
  use julienne_m, only : string_t, operator(.csv.)
  implicit none

contains

  module procedure construct_matrix_operator
    differential_operator_matrix_1D%upper_ = upper
    differential_operator_matrix_1D%inner_ = inner
    differential_operator_matrix_1D%lower_ = lower
  end procedure

  module procedure to_file_t
    type(string_t), allocatable :: lines(:)
    integer, parameter :: inner_rows = 1
    integer row

    associate(upper_rows => size(self%upper_,1), lower_rows => size(self%lower_,1))
      allocate(lines(upper_rows + inner_rows + lower_rows))
      do row = 1, upper_rows
        lines(row) = .csv. string_t(self%upper_(row,:))
      end do
      lines(upper_rows + inner_rows) = .csv. string_t(self%inner_)
      do row = 1, lower_rows
        lines(upper_rows + inner_rows + row) = .csv. string_t(self%lower_(row,:))
      end do
    end associate

    file = file_t(lines)

  end procedure

end submodule differential_operator_matrix_1D_s