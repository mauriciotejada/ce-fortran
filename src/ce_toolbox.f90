!##############################################################################
!##############################################################################
! MODULE ce_toolbox
!
! Convenience alias module for package 'ce-toolbox' re-exporting the full
! 'toolbox' facade. Allows consuming programs to use either:
!     use toolbox
! or:
!     use ce_toolbox
!
! Published under the GNU General Public License v3
!                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
!
! Authors: Hans Fehr and Fabian Kindermann
!          contact@ce-fortran.com
!##############################################################################
!##############################################################################

module ce_toolbox

    use toolbox

    implicit none
    public

end module ce_toolbox
