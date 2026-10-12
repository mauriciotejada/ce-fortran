!##############################################################################
! MODULE toolbox_quad
!
! Numerical quadrature and integration (Legendre, Gauss-Legendre, Gauss-Hermite).
!##############################################################################
module toolbox_quad

    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64, &
        input_unit, output_unit, error_unit
    use toolbox_kinds
    use toolbox_core

    implicit none
    private

    public :: legendre

contains

    ! SUBROUTINE legendre
    !
    ! Calculates Gauss-Legendre abscissas and weights on [x1, x2].
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine legendre(x1, x2, x, w)
    
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! left interval point on which integral should be calculated
        real(dp), intent(in) :: x1
     
        ! left interval point on which integral should be calculated
        real(dp), intent(in) :: x2
     
        ! abscissas of gaussian integration formula
        real(dp), intent(out) :: x(:)
     
        ! weights of gaussian integration formula
        real(dp), intent(out) :: w(:)
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp), parameter :: eps = 3.0e-14_dp
        integer(int32), parameter :: maxits = 10
        real(dp), parameter :: pi = 3.14159265358979_dp
        integer(int32) :: its, j, m, n
        real(dp) :: xl, xm
        real(dp), dimension((size(x)+1)/2) :: p1, p2, p3, pp, z, z1
        logical, dimension((size(x)+1)/2) :: unfinished
     
     
        !##### ROUTINE CODE #######################################################
     
        ! assert size equality
        n = assert_eq(size(x), size(w), 'legendre')
     
        ! calculate only up to (n+1)/2 due to symmetry
        m = (n+1)/2
     
        ! calculate interval midpoint
        xm = 0.5_dp*(x2+x1)
     
        ! calculate half of interval length
        xl = 0.5_dp*(x2-x1)
     
        ! set initial guess for the roots
        z = cos(pi*(arth(1,1,m)-0.25_dp)/(n+0.5_dp))
     
        ! initialized unfinished
        unfinished = .true.
     
        ! iterate Newton steps up to maximum iterations
        do its = 1, maxits
     
            ! calculate Legendre polynomial at z where root has not yet been found
     
            ! initialize p1 and p2
            where (unfinished)
                p1 = 1.0_dp
                p2 = 0.0_dp
            endwhere
     
            ! calculate polynomial value at z by recursive formula
            do j = 1, n
     
                ! only where root has not yet been found
                where (unfinished)
     
                    ! the polynomial of order n - 2
                    p3 = p2
     
                    ! the polynomial of order n - 1
                    p2 = p1
     
                    ! the legendre polynomial
                    p1 = ((2.0_dp*j-1.0_dp)*z*p2-(j-1.0_dp)*p3)/j
                endwhere
            enddo
     
            ! calculate derivative of polynomial p1 at z
            where (unfinished)
     
                ! derivative
                pp = n*(z*p1-p2)/(z*z-1.0_dp)
     
                ! store old z
                z1 = z
     
                ! perform the newton step
                z = z1-p1/pp
     
                ! check for difference between old and new guess being small enough
                unfinished=(abs(z-z1) > EPS)
            endwhere
     
            ! if all values have sufficiently converged, stop iteration
            if (.not. any(unfinished)) exit
        end do
     
        ! throw error message if not sufficiently convergerd
        if(its == maxits+1)call error('legendre', 'too many iterations')
     
        ! else calculate abscissas
        x(1:m) = xm-xl*z
     
        ! symmetry for abscissas
        x(n:n-m+1:-1) = xm+xl*z
     
        ! calculate weights
        w(1:m) = 2.0_dp*xl/((1.0_dp-z**2)*pp**2)
     
        ! symmetry for weights
        w(n:n-m+1:-1) = w(1:m)
    
    end subroutine legendre
    
    
    !##############################################################################
    ! FUNCTION arth
    !
    ! Calculates incremented array from first with n entries.
    !##############################################################################
    function arth(first, increment, n)
    
        integer(int32), intent(in) :: first, increment, n
        integer(int32), parameter :: npar_arth = 16
        integer(int32), parameter :: npar2_arth = 8
        integer(int32) :: arth(n)
        integer(int32) :: k, k2, temp
     
        ! initialize first element
        if(n > 0)arth(1) = first
     
        ! calculate by hand if n <= 16
        if(n <= npar_arth) then
            do k = 2, n
                arth(k) = arth(k-1) + increment
            enddo
     
        ! else set entries stepwise by 8 steps
        else
            do k = 2, npar2_arth
                arth(k) = arth(k-1) + increment
            enddo
            temp = increment*npar2_arth
            k = npar2_arth
            do
                if(k >= n)exit
                k2 = k+k
                arth(k+1:min(k2,n)) = temp+arth(1:min(k,n-k))
                temp = temp + temp
                k = k2
            enddo
        endif
    
    end function arth















!##############################################################################


end module toolbox_quad
