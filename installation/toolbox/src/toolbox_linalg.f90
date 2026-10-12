!##############################################################################
! MODULE toolbox_linalg
!
! Matrix operations, LU decomposition and solver, Cholesky decomposition.
!##############################################################################
module toolbox_linalg

    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64, &
        input_unit, output_unit, error_unit
    use toolbox_kinds
    use toolbox_core

    implicit none
    private

    public :: lu_solve, lu_invert, lu_dec
    public :: lu_decomp, lu_back, cholesky

contains

    !##############################################################################
    ! SUBROUTINE lu_solve
    !
    ! Solves a linear equation system by lu-decomposition.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine lu_solve(a, b, stat)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! matrix of the system
        real(dp), intent(in) :: a(:, :)
        
        ! right side of equation and solution of the system
        real(dp), intent(inout) :: b(:)
        
        ! status indicator
        integer(int32), intent(out), optional :: stat
        
        
        !##### OTHER VARIABLES ####################################################
        
        integer(int32) :: indx(size(b))
        real(dp) :: worka(size(a, 1), size(a, 2))
        real(dp) :: d
        integer(int32) :: n
        
        
        !##### ROUTINE CODE #######################################################
        
        if(present(stat)) stat = 0
        
        ! assert size equality
        n = assert_eq(size(a,1), size(a,2), size(b), 'lu_solve')
        n = n
        
        ! copy matrix to working matrix
        worka = a
        
        ! decompose matrix
        call lu_decomp(worka, indx, d, stat)
        if(present(stat))then
            if(stat /= 0) return
        endif
        
        ! solve system
        call lu_back(worka, indx, b)
    
    end subroutine lu_solve
    
    
    !##############################################################################
    ! FUNCTION lu_invert
    !
    ! Inverts a matrix by lu-decomposition.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    function lu_invert(a)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! matrix of the system
        real(dp), intent(in) :: a(:, :)
        
        ! the inverse of the matrix
        real(dp) :: lu_invert(size(a, 1), size(a, 2))
        
        
        !##### OTHER VARIABLES ####################################################
        
        integer(int32) :: j, n
        
        
        !##### ROUTINE CODE #######################################################
        
        ! assert size equality
        n = assert_eq(size(a,1), size(a,2), 'lu_invert')
        
        ! set up unity matrix
        lu_invert = 0.0_dp
        do j = 1, n
            lu_invert(j, j) = 1.0_dp
        enddo
        
        ! succesively solve the system with unity matrix
        do j = 1, n
            call lu_solve(a, lu_invert(:, j))
        enddo
    
    end function lu_invert
    
    
    !##############################################################################
    ! SUBROUTINE lu_decomp
    !
    ! Calculates lu-decomposition of matrices.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine lu_decomp(a, indx, d, stat)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! matrix that shall be decomposed
        real(dp), intent(inout) :: a(:, :)
        
        ! row permutation indicator due to pivoting
        integer(int32), intent(out) :: indx(:)
        
        ! indicates whether number of row permutations was even or odd
        real(dp), intent(out) :: d
        
        ! status indicator
        integer(int32), intent(out), optional :: stat
        
        
        !##### OTHER VARIABLES ####################################################
        
        real(dp) :: vv(size(a,1))
        real(dp), parameter :: tiny = 1.0e-20_dp
        integer(int32) :: j, n, imax
        
        
        !##### ROUTINE CODE #######################################################
        
        if(present(stat)) stat = 0
        
        ! check array sizes
        n = assert_eq(size(a,1), size(a,2), size(indx),'lu_decomp')
        
        ! initialize permutation indicator
        d = 1.0_dp
        
        ! get maximum value in every row
        vv = maxval(abs(a), dim=2)
        
        ! if there is a zero row then matrix is singular
        if (any(abs(vv) <= 1.0e-100_dp)) then
            if(present(stat)) then
                stat = 1
                return
            else
                call error('lu_decomp', 'matrix is singular')
            endif
        endif
        
        ! invert v
        vv = 1.0_dp/vv
        
        ! start lu-decomposition process
        do j = 1, n
        
            ! get index of pivot element
            imax = (j-1) + imaxloc(vv(j:n)*abs(a(j:n,j)))
            
            ! do pivoting if pivot element is not the first element
            if(j /= imax)then
                call swap(a(imax,:), a(j,:))
                d = -d
                vv(imax) = vv(j)
            endif
            
            ! indicate pivot element
            indx(j) = imax
            
            ! prevent division by 0
            if(abs(a(j,j))  <= 1.0e-100_dp) then
                if(present(stat)) then
                    stat = 1
                    return
                else
                    call error('lu_decomp', 'matrix is singular')
                endif
            endif
            
            ! calculate new elements
            a(j+1:n, j) = a(j+1:n,j)/a(j,j)
            a(j+1:n, j+1:n) = a(j+1:n,j+1:n)-outerprod(a(j+1:n,j), a(j,j+1:n))
        enddo
        
        
    !##### SUBROUTINES AND FUNCTIONS ##########################################
        
    contains
        
        
        pure function outerprod(a, b)
        
            real(dp), intent(in) :: a(:), b(:)
            real(dp) :: outerprod(size(a, 1),size(b, 1))
        
            outerprod = spread(a, dim=2, ncopies=size(b, 1)) * &
            spread(b, dim=1, ncopies=size(a, 1))
        
        end function outerprod
        
        
        pure subroutine swap(a, b)
        
            real(dp), intent(inout) :: a(:), b(:)
            real(dp) :: dum(size(a))
        
            dum = a
            a = b
            b = dum
        
        end subroutine swap
        
        
        pure function imaxloc(arr)
        
            real(dp), intent(in) :: arr(:)
            integer(int32) :: imaxloc
            integer(int32) :: imax(1)
        
            imax = maxloc(arr(:))
            imaxloc = imax(1)
        
        end function imaxloc
    
    end subroutine lu_decomp
    
    
    
    !##############################################################################
    ! SUBROUTINE lu_dec
    !
    ! Calculates lu-decomposition of matrices and returns L and U matrix.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine lu_dec(a, l, u, stat)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! matrix that shall be decomposed
        real(dp), intent(in) :: a(:, :)
        
        ! lower triangular matrix
        real(dp), intent(out) :: l(:, :)
        
        ! upper triangular martix
        real(dp), intent(out) :: u(:, :)
        
        ! status indicator
        integer(int32), intent(out), optional :: stat
        
        
        !##### OTHER VARIABLES ####################################################
        
        real(dp) :: Awork(size(a,1), size(a,2))
        integer(int32) :: indx(size(a,1))
        real(dp) :: d
        integer(int32) :: n, j
        
                
        !##### ROUTINE CODE #######################################################
        
        if(present(stat)) stat = 0
        
        ! check array sizes
        n = assert_eq((/size(a,1), size(a,2), size(l, 1), size(l, 2), &
        size(u, 1), size(u, 2)/), 'lu_dec')
        
        ! copy matrix
        Awork(:, :) = A(:, :)
        
        ! calculate decomposition
        call lu_decomp(Awork, indx, d, stat)
        if(present(stat))then
            if(stat /= 0) return
        endif
        
        ! initialize matrices
        L(:, :) = 0.0_dp
        U(:, :) = 0.0_dp
        
        ! set up new matrices
        do j = 1, n
        
            ! diagonal element of L
            L(j, j) = 1.0_dp
            
            ! other elements of L
            L(j, 1:j-1) = Awork(j, 1:j-1)
            
            ! elements of U
            U(j, j:n) = AWork(j, j:n)
        enddo
    
    end subroutine lu_dec
    
    
    !##############################################################################
    ! SUBROUTINE lu_back
    !
    ! Solves a lu decomposed linear equation system by backsubstitution.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine lu_back(a, indx, b)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! lu-decomposed matrix that defines system
        real(dp), intent(in) :: a(:, :)
        
        ! row permutation indicator due to pivoting
        integer(int32), intent(in) :: indx(:)
        
        ! right side of equation and solution of the system
        real(dp), intent(inout) :: b(:)
        
        
        !##### OTHER VARIABLES ####################################################
        
        real(dp) :: summ
        integer(int32) :: i, n, ii, ll
        
        
        !##### ROUTINE CODE #######################################################
        
        ! assert size equality
        n = assert_eq(size(a,1), size(a,2), size(indx), size(b), 'lu_back')
        
        ! start backward solving provess
        ii = 0
        do i = 1, n
        
            ll = indx(i)
            summ = b(ll)
            b(ll) = b(i)
            if(ii /= 0)then
                summ = summ-dot_product(a(i,ii:i-1), b(ii:i-1))
            elseif (abs(summ) >= 1.0e-100_dp) then
                ii = i
            endif
            b(i)=summ
        enddo
        
        do i=n, 1, -1
            b(i) = (b(i)-dot_product(a(i,i+1:n), b(i+1:n)))/a(i,i)
        enddo
    
    end subroutine lu_back
    
    
    
    !##############################################################################
    ! SUBROUTINE cholesky
    !
    ! Calculates cholesky factorization of a symmetric matrix.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine cholesky(a, l, stat)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! the matrix that should be decomposed
        real(dp), intent(in) :: a(:, :)
        
        ! the cholesky factor
        real(dp), intent(out) :: l(:, :)
        
        ! status indicator
        integer(int32), intent(out), optional :: stat
        
        
        !##### OTHER VARIABLES ####################################################
        
        integer(int32) :: i, n
        real(dp) :: summ, p(size(a,1))
        
        
        !##### ROUTINE CODE #######################################################
        
        if(present(stat)) stat = 0
        
        ! assert equalities
        n = assert_eq(size(a,1), size(a,2), size(l, 1), size(l, 2), &
            size(p), 'cholesky')
        
        ! copy matrix
        l = a
        
        ! decompose matrix
        do i = 1, n
            summ = l(i,i)-dot_product(l(i,1:i-1), l(i,1:i-1))
            if(summ <= 0.0_dp)then
                if(present(stat)) then
                    stat = 1
                    return
                else
                    call error('cholesky', 'Cholesky decomposition failed')
                endif
            endif
            p(i) = sqrt(summ)
            l(i+1:n,i) = (l(i,i+1:n)-matmul(l(i+1:n,1:i-1),l(i,1:i-1)))/p(i)
        enddo
        
        ! copy matrix
        do i = 1, n
            l(i, i) = p(i)
            l(i, i+1:n) = 0.0_dp
        enddo
    
    end subroutine cholesky
 
 
 
 
 









 
!############################################################################## 


end module toolbox_linalg
