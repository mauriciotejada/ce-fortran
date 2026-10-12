!##############################################################################
! PROGRAM lineqsys
!
! ## Solves a linear equation system by LU-decomposition
!
! This code is published under the GNU General Public License v3
!                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
!
! Authors: Hans Fehr and Fabian Kindermann
!          contact@ce-fortran.com
!
!##############################################################################
program lineqsys

    use ce_toolbox

    implicit none
    integer :: i, j
    real(dp) :: A(3, 3), b(3)
    real(dp) :: L(3, 3), U(3, 3)

    ! set up matrix and vector
    A(1, :) = (/ 2_dp,  0_dp,  1_dp/)
    A(2, :) = (/ 0_dp,  4_dp,  1_dp/)
    A(3, :) = (/ 1_dp,  1_dp,  2_dp/)
    b       = (/30_dp, 40_dp, 30_dp/)

    ! solve the system
    call lu_solve(A, b)

    ! decompose matrix
    call lu_dec(A, L, U)

    ! output
    write(*,'(a,3f7.2/)')'    x = ', (b(j),j=1,3)
    write(*,'(a,3f7.2/,2(5x,3f7.2/))') &
        ' L = ',((L(i,j),j=1,3),i=1,3)
    write(*,'(a,3f7.2/,2(5x,3f7.2/))') &
        ' U = ',((U(i,j),j=1,3),i=1,3)

end program
