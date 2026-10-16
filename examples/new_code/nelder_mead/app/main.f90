!##############################################################################
! PROGRAM rosenbrock_min
!
! ## Minimizing the Rosenbrock function with Nelder-Mead and Powell
!
! This code is published under the GNU General Public License v3
!                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
!
!##############################################################################
program rosenbrock_min

    use globals
    use ce_toolbox

    implicit none
    real(dp), parameter :: x0(2) = [-1.2_dp, 1.0_dp]
    real(dp) :: x(2), fret

    write(*,'(/a,2f8.3)')' Starting point: x0 =', x0

    ! Nelder-Mead: fminsearch without bounds
    x = x0
    n_eval = 0
    call fminsearch(x, fret, rosenbrock)
    call output('NELDER-MEAD (unbounded) :')

    ! Powell: fminsearch with bounds
    x = x0
    n_eval = 0
    call fminsearch(x, fret, [-5.0_dp, -5.0_dp], [5.0_dp, 5.0_dp], rosenbrock)
    call output('POWELL (on [-5, 5]^2) :')

    ! exact solution
    write(*,'(/a)')' EXACT :'
    write(*,'(a,2f14.10)')' x     =', 1.0_dp, 1.0_dp
    write(*,'(a,es14.4)')' f(x)  =', 0.0_dp

contains


    ! Writes the minimum, its function value and the number of evaluations.
    subroutine output(title)

        implicit none
        character(len=*), intent(in) :: title

        write(*,'(/a)')title
        write(*,'(a,2f14.10)')' x     =', x
        write(*,'(a,es14.4)')' f(x)  =', fret
        write(*,'(a,i14)')' evals =', n_eval

    end subroutine output

end program rosenbrock_min
