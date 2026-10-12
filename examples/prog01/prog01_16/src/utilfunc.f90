!##############################################################################
! MODULE UtilFunc
!##############################################################################
module UtilFunc

    implicit none

contains


    ! a utility function
    function utility(c1, c2)

        use Globals

        implicit none
        real*8, intent(in) :: c1, c2
        real*8 :: utility

        utility = 1d0/(1d0-eta)*c1**(1d0-eta) + beta/(1d0-eta)*c2**(1d0-eta)

    end function

end module
