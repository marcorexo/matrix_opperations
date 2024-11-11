! matrix_calculator.f90
! A command-line matrix calculator that performs basic operations on 2x2 matrices

module matrix_operations
    implicit none
    private
    public :: print_matrix, input_matrix, add_matrices, multiply_matrices, determinant
    
contains
    subroutine print_matrix(matrix)
        real, intent(in) :: matrix(2,2)
        integer :: i
        
        do i = 1, 2
            print *, matrix(i,:)
        end do
    end subroutine print_matrix
    
    subroutine input_matrix(matrix)
        real, intent(out) :: matrix(2,2)
        integer :: i, j
        
        print *, "Enter matrix elements row by row (2x2):"
        do i = 1, 2
            do j = 1, 2
                read *, matrix(i,j)
            end do
        end do
    end subroutine input_matrix
    
    function add_matrices(matrix1, matrix2) result(result_matrix)
        real, intent(in) :: matrix1(2,2), matrix2(2,2)
        real :: result_matrix(2,2)
        
        result_matrix = matrix1 + matrix2
    end function add_matrices
    
    function multiply_matrices(matrix1, matrix2) result(result_matrix)
        real, intent(in) :: matrix1(2,2), matrix2(2,2)
        real :: result_matrix(2,2)
        integer :: i, j, k
        
        result_matrix = matmul(matrix1, matrix2)
    end function multiply_matrices
    
    function determinant(matrix) result(det)
        real, intent(in) :: matrix(2,2)
        real :: det
        
        det = matrix(1,1) * matrix(2,2) - matrix(1,2) * matrix(2,1)
    end function determinant
end module matrix_operations

program main
    use matrix_operations
    use, intrinsic :: iso_fortran_env, only: input_unit, output_unit, error_unit
    implicit none
    
    real :: matrix1(2,2), matrix2(2,2), result_matrix(2,2)
    real :: det
    character(1) :: operation
    
    write(output_unit,*) "2x2 Matrix Calculator"
    write(output_unit,*) "-------------------"
    
    ! Input first matrix
    write(output_unit,*) "Enter first matrix:"
    call input_matrix(matrix1)
    
    write(output_unit,*) "Choose operation:"
    write(output_unit,*) "'+' for addition"
    write(output_unit,*) "'*' for multiplication"
    write(output_unit,*) "'d' for determinant"
    read(input_unit,*) operation
    
    select case(operation)
        case('+')
            write(output_unit,*) "Enter second matrix:"
            call input_matrix(matrix2)
            result_matrix = add_matrices(matrix1, matrix2)
            write(output_unit,*) "Result:"
            call print_matrix(result_matrix)
            
        case('*')
            write(output_unit,*) "Enter second matrix:"
            call input_matrix(matrix2)
            result_matrix = multiply_matrices(matrix1, matrix2)
            write(output_unit,*) "Result:"
            call print_matrix(result_matrix)
            
        case('d')
            det = determinant(matrix1)
            write(output_unit,*) "Determinant:", det
            
        case default
            write(error_unit,*) "Invalid operation!"
    end select
    
end program main
