!##############################################################################
! PROGRAM oligopoly2
!
! ## Multi-dimensional rootfinding using the Gauss-Seidel iteration approach
!
! This code is published under the GNU General Public License v3
!                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
!
! Authors: Hans Fehr and Fabian Kindermann
!          contact@ce-fortran.com
!
!##############################################################################
program oligopoly2

    implicit none
    integer, parameter :: dp = selected_real_kind(15, 307)
    real(dp) :: eta = 1.6_dp
    real(dp) :: c(2) = (/ 0.6_dp, 0.8_dp /)
    real(dp) :: qold(2), q(2), QQ, damp
    integer :: iter

    ! initialize q
    qold = 0.1_dp
    damp = 0.7_dp

    do iter = 1, 100

        QQ = sum(qold)
        q = 1.0_dp/c*(QQ**(-1.0_dp/eta)-1.0_dp/eta*QQ**(-1.0_dp/eta-1.0_dp)*qold)
        q = damp*q + (1.0_dp-damp)*qold

        ! write to screen
        write(*, '(a, i5,2f10.4)')'Iter: ', iter, q(1), q(2)

        ! check for convergence
        if( all(abs(q-qold) < 1.0e-6_dp))then
            write(*,'(/a, 2f10.4)')'   Output', q(1), q(2)
            stop
        endif

        ! update q
        qold = q
    enddo

end program


