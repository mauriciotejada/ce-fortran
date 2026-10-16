!##############################################################################
! MODULE globals
!
! The Rosenbrock function and a counter of its evaluations.
!
! This code is published under the GNU General Public License v3
!                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
!
!##############################################################################
module globals

    use ce_toolbox, only: dp, int32

    implicit none
    private

    public :: n_eval, rosenbrock

    ! number of function evaluations
    integer(int32) :: n_eval = 0

contains


    ! The Rosenbrock function, minimum f = 0 at x = (1, 1)
    function rosenbrock(x)

        implicit none
        real(dp), intent(in) :: x(:)
        real(dp) :: rosenbrock

        n_eval = n_eval + 1
        rosenbrock = 100.0_dp*(x(2) - x(1)**2)**2 + (1.0_dp - x(1))**2

    end function rosenbrock

end module globals
