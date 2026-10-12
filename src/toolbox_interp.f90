!##############################################################################
! MODULE toolbox_interp
!
! Multidimensional linear interpolation, polynomial interpolation, and splines.
!##############################################################################
module toolbox_interp

    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64, &
        input_unit, output_unit, error_unit
    use toolbox_kinds
    use toolbox_core
    use toolbox_grids

    implicit none
    private

    public :: linint_Equi, linint_Cheb, linint_Grow, linint_Gen, linint_Gen_detail
    public :: linint_Equi_1, linint_Equi_m, linint_Cheb_1, linint_Cheb_m
    public :: linint_Grow_1, linint_Grow_m
    public :: poly_interpol, poly_interpol_1, poly_interpol_m
    public :: spline_interp, spline_eval, spline
    public :: spline_interp1, spline_interp2, spline_interp3, spline_interp4
    public :: spline_interp5, spline_interp6, spline_interp7
    public :: spline1, spline1_grid, spline2, spline2_grid
    public :: spline3, spline3_grid, spline4, spline4_grid
    public :: spline5, spline5_grid, spline6, spline6_grid, spline7, spline7_grid
    public :: spline1_complete, spline1_complete_m, spline2_complete, spline2_complete_m
    public :: spline3_complete, spline3_complete_m, spline4_complete, spline4_complete_m
    public :: spline5_complete, spline5_complete_m, spline6_complete, spline6_complete_m
    public :: spline7_complete, spline7_complete_m

!##############################################################################
! INTERFACE poly_interpol
!
! Interface for onedimensional polynomial interpolation.
!##############################################################################
interface poly_interpol

    module procedure poly_interpol_1, poly_interpol_m

end interface


!##############################################################################
! INTERFACE linint_Equi
!
! Interface for inverting gridpoints.
!##############################################################################
interface linint_Equi

    module procedure linint_Equi_1, linint_Equi_m

end interface


!##############################################################################
! INTERFACE linint_Cheb
!
! Interface for inverting gridpoints.
!##############################################################################
interface linint_Cheb

    module procedure linint_Cheb_1, linint_Cheb_m

end interface

  
!##############################################################################
! INTERFACE linint_Grow
!
! Interface for inverting gridpoints.
!##############################################################################
interface linint_Grow

    module procedure linint_Grow_1, linint_Grow_m

end interface


!##############################################################################
! INTERFACE spline_interp
!
! Interface for one- or multidimensional spline interpolation.
!##############################################################################
interface spline_interp

    module procedure spline_interp1, spline_interp2, spline_interp3, &
        spline_interp4, spline_interp5, spline_interp6, spline_interp7

end interface


!##############################################################################
! INTERFACE spline_eval
!
! Interface for evaluation of one- or multidimensional spline.
!##############################################################################
interface spline_eval

    module procedure &
        spline1, spline1_grid, &
        spline2, spline2_grid, &
        spline3, spline3_grid, &
        spline4, spline4_grid, &
        spline5, spline5_grid, &
        spline6, spline6_grid, &
        spline7, spline7_grid

end interface


!##############################################################################
! INTERFACE spline
!
! Interface for complete interpolation and evaluation of one- or
!     multidimensional spline.
!##############################################################################
interface spline

    module procedure &
        spline1_complete, spline1_complete_m , &
        spline2_complete, spline2_complete_m, &
        spline3_complete, spline3_complete_m , &
        spline4_complete, spline4_complete_m, &
        spline5_complete, spline5_complete_m, &
        spline6_complete, spline6_complete_m, &
        spline7_complete, spline7_complete_m

end interface




contains


    !##############################################################################
    ! subroutine linint_Equi_1
    !
    ! Calculates linear interpolant on an equidistant grid.
    !##############################################################################
    pure subroutine linint_Equi_1(x, left, right, n, il, ir, phi)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! left interpolation point
        integer(int32), intent(out) :: il
        
        ! right interpolation point
        integer(int32), intent(out) :: ir

        ! interpolation fraction
        real(dp), intent(out) :: phi
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: h, xinv, xl, xr
     
     
        !##### ROUTINE CODE #######################################################
             
        ! invert the grid to get point
        xinv = grid_Inv_Equi_1(min(max(x, left), right), left, right, n)
     
        ! get left and right gridpoint
        il = min(max(floor(xinv), 0), n-1)
        ir = il+1

        ! determine left and right gridpoint
        h  = (right-left)/n
        xl = h*real(il, dp)+left
        xr = h*real(ir, dp)+left

        ! get share on the left point
        phi = (xr-x)/(xr-xl)
    
    end subroutine linint_Equi_1


    !##############################################################################
    ! subroutine linint_Equi_m
    !
    ! Calculates linear interpolant on an equidistant grid.
    !##############################################################################
    pure subroutine linint_Equi_m(x, left, right, n, il, ir, phi)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x(:)
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! left interpolation point
        integer(int32), intent(out) :: il(:)
        
        ! right interpolation point
        integer(int32), intent(out) :: ir(:)

        ! interpolation fraction
        real(dp), intent(out) :: phi(:)
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: m
        real(dp) :: h, xinv(size(x, 1)), xl(size(x, 1)), xr(size(x, 1))
     
     
        !##### ROUTINE CODE #######################################################

        ! check for sizes
        m = assert_eq(size(x, 1), size(il, 1), size(ir, 1), size(phi, 1), 'linint_Equi')
        m = m
             
        ! invert the grid to get point
        xinv = grid_Inv_Equi_m(min(max(x, left), right), left, right, n)
     
        ! get left and right gridpoint
        il = min(max(floor(xinv), 0), n-1)
        ir = il+1

        ! determine left and right gridpoint
        h  = (right-left)/n
        xl = h*real(il, dp)+left
        xr = h*real(ir, dp)+left

        ! get share on the left point
        phi = (xr-x)/(xr-xl)
    
    end subroutine linint_Equi_m


    !##############################################################################
    ! subroutine linint_Cheb_1
    !
    ! Calculates linear interpolant on an equidistant grid.
    !##############################################################################
    pure subroutine linint_Cheb_1(x, left, right, n, il, ir, phi)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! left interpolation point
        integer(int32), intent(out) :: il
        
        ! right interpolation point
        integer(int32), intent(out) :: ir

        ! interpolation fraction
        real(dp), intent(out) :: phi
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: xinv, xl, xr
        real(dp), parameter :: pi = 3.1415926535897932_dp 
     
     
        !##### ROUTINE CODE #######################################################
     
        ! invert the grid to get point
        xinv = grid_Inv_Cheb_1(min(max(x, left), right), left, right, n)
     
        ! get left and right gridpoint
        il = min(max(floor(xinv), 0), n-1)
        ir = il+1

        ! determine left and right gridpoint        
        xl = (left+right)/2.0_dp + (right-left)/2.0_dp* &
            cos((real(n, dp)-real(il, dp)+0.5_dp)/real(n+1, dp)*pi)
        xr = (left+right)/2.0_dp + (right-left)/2.0_dp* &
            cos((real(n, dp)-real(ir, dp)+0.5_dp)/real(n+1, dp)*pi)

        ! get share on the left point
        phi = (xr-x)/(xr-xl)
    
    end subroutine linint_Cheb_1


    !##############################################################################
    ! subroutine linint_Cheb_m
    !
    ! Calculates linear interpolant on a Chebychev grid.
    !##############################################################################
    pure subroutine linint_Cheb_m(x, left, right, n, il, ir, phi)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x(:)
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! left interpolation point
        integer(int32), intent(out) :: il(:)
        
        ! right interpolation point
        integer(int32), intent(out) :: ir(:)

        ! interpolation fraction
        real(dp), intent(out) :: phi(:)
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: m
        real(dp) :: xinv(size(x, 1)), xl(size(x, 1)), xr(size(x, 1))
        real(dp), parameter :: pi = 3.1415926535897932_dp
     
     
        !##### ROUTINE CODE #######################################################

        ! check for sizes
        m = assert_eq(size(x, 1), size(il, 1), size(ir, 1), size(phi, 1), 'linint_Equi')
        m = m
     
        ! invert the grid to get point
        xinv = grid_Inv_Cheb_m(min(max(x, left), right), -1.0_dp, 1.0_dp, n)
     
        ! get left and right gridpoint
        il = min(max(floor(xinv), 0), n-1)
        ir = il+1

        ! determine left and right gridpoint
        xl = (left+right)/2.0_dp + (right-left)/2.0_dp* &
            cos((real(n, dp)-real(il, dp)+0.5_dp)/real(n+1, dp)*pi)
        xr = (left+right)/2.0_dp + (right-left)/2.0_dp* &
            cos((real(n, dp)-real(ir, dp)+0.5_dp)/real(n+1, dp)*pi)

        ! get share on the left point
        phi = (xr-x)/(xr-xl)
    
    end subroutine linint_Cheb_m

    
    !##############################################################################
    ! subroutine linint_Grow_1
    !
    ! Calculates linear interpolant on a growing grid.
    !##############################################################################
    pure subroutine linint_Grow_1(x, left, right, growth, n, il, ir, phi)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x
     
        ! left and right interval point
        real(dp), intent(in) :: left, right

        ! growth rate
        real(dp), intent(in) :: growth
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! left interpolation point
        integer(int32), intent(out) :: il
        
        ! right interpolation point
        integer(int32), intent(out) :: ir

        ! interpolation fraction
        real(dp), intent(out) :: phi
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: h, xinv, xl, xr
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: linint_Grow: left interval point greater than right point'
     
        ! invert the grid to get point
        xinv = grid_Inv_Grow_1(min(max(x, left), right), left, right, growth, n)
     
        ! get left and right gridpoint
        il = min(max(floor(xinv), 0), n-1)
        ir = il+1

        ! determine left and right gridpoint
        h = (right-left)/((1.0_dp+growth)**n-1.0_dp)     
        xl = h*((1.0_dp+growth)**real(il, dp)-1.0_dp)+left
        xr = h*((1.0_dp+growth)**real(ir, dp)-1.0_dp)+left

        ! get share on the left point
        phi = (xr-x)/(xr-xl)
    
    end subroutine linint_Grow_1


    !##############################################################################
    ! subroutine linint_Grow_m
    !
    ! Calculates linear interpolant on an equidistant grid.
    !##############################################################################
    pure subroutine linint_Grow_m(x, left, right, growth, n, il, ir, phi)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x(:)
     
        ! left and right interval point
        real(dp), intent(in) :: left, right

        ! growth rate
        real(dp), intent(in) :: growth
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! left interpolation point
        integer(int32), intent(out) :: il(:)
        
        ! right interpolation point
        integer(int32), intent(out) :: ir(:)

        ! interpolation fraction
        real(dp), intent(out) :: phi(:)
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: m
        real(dp) :: h, xinv(size(x, 1)), xl(size(x, 1)), xr(size(x, 1))
     
     
        !##### ROUTINE CODE #######################################################

        ! check for sizes
        m = assert_eq(size(x, 1), size(il, 1), size(ir, 1), size(phi, 1), 'linint_Equi')
        m = m
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: linint_Grow: left interval point greater than right point'
     
        ! invert the grid to get point
        xinv = grid_Inv_Grow_m(min(max(x, left), right), left, right, growth, n)
     
        ! get left and right gridpoint
        il = min(max(floor(xinv), 0), n-1)
        ir = il+1

        ! determine left and right gridpoint
        h = (right-left)/((1.0_dp+growth)**n-1.0_dp)     
        xl = h*((1.0_dp+growth)**real(il, dp)-1.0_dp)+left
        xr = h*((1.0_dp+growth)**real(ir, dp)-1.0_dp)+left

        ! get share on the left point
        phi = (xr-x)/(xr-xl)
    
    end subroutine linint_Grow_m


    !##############################################################################
    ! function linint_Gen
    !
    ! For linear interpolation on irregular grids.
    !##############################################################################
    pure function linint_Gen(x, xi, yi, istart_in)

        
        !##### INPUT/OUTPUT VARIABLES #############################################

        ! point where to evaluate
        real(dp), intent(in) :: x

        ! grid on which to evaluate
        real(dp), intent(in) :: xi(0:)

        ! data which to evaluate
        real(dp), intent(in) :: yi(0:)
        
        ! (optional) point where to start
        integer(int32), intent(in), optional :: istart_in

        ! output value
        real(dp) :: linint_Gen

        
        !##### OTHER VARIABLES ####################################################

        ! other variables
        integer(int32) :: ial, iar, n, istart
        real(dp) :: phi


        !##### ROUTINE CODE #######################################################

        n = assert_eq(size(xi,1), size(yi,1), 'linint_Gen')-1

        if(present(istart_in))then
            istart = min(max(istart_in, 0), n)
        else
            istart = n/2
        endif
        
        ! if grid value too large, search for first smaller point
        if(xi(istart) > x)then
            ial = istart
            do 
                ial = ial - 1
                if(ial <= 0)exit
                if(xi(ial) <= x)exit
            enddo
            ial = max(ial, 0)
            ial = min(ial, n-1)            
            iar = ial+1

        ! if grid value too small, search for first larger point
        else
            iar = istart
            do 
                iar = iar + 1
                if(iar >= n)exit
                if(xi(iar) >= x)exit
            enddo
            iar = max(iar, 1)
            iar = min(iar, n)            
            ial = iar-1
        endif

        ! linearly interpolate between the two points
        phi = 1.0_dp - (x-xi(ial))/(xi(iar)-xi(ial))        
        linint_Gen = phi*yi(ial) + (1.0_dp-phi)*yi(iar)

    end function linint_Gen


    !##############################################################################
    ! subroutine linint_Gen_detail
    !
    ! For linear interpolation on irregular grids (with detailed return values).
    !##############################################################################
    pure subroutine linint_Gen_detail(x, xi, ial, iar, phi, istart_in)

        !##### INPUT/OUTPUT VARIABLES #############################################

        ! point where to evaluate
        real(dp), intent(in) :: x

        ! grid on which to evaluate
        real(dp), intent(in) :: xi(0:)

        ! left interpolation point
        integer(int32), intent(out) :: ial

        ! right interpolation point
        integer(int32), intent(out) :: iar

        ! interpolation fraction
        real(dp), intent(out) :: phi

        ! (optional) point where to start
        integer(int32), intent(in), optional :: istart_in


        !##### OTHER VARIABLES ####################################################

        ! other variables
        integer(int32) :: istart, n


        !##### ROUTINE CODE #######################################################


        n = size(xi,1)-1

        if(present(istart_in))then
            istart = min(max(istart_in, 0), n)
        else
            istart = n/2
        endif
        
        ! if grid value too large, search for first smaller point
        if(xi(istart) > x)then
            ial = istart
            do 
                ial = ial - 1
                if(ial <= 0)exit
                if(xi(ial) <= x)exit
            enddo
            ial = max(ial, 0)
            ial = min(ial, n-1)            
            iar = ial+1

        ! if grid value too small, search for first larger point
        else
            iar = istart
            do 
                iar = iar + 1
                if(iar >= n)exit
                if(xi(iar) >= x)exit
            enddo
            iar = max(iar, 1)
            iar = min(iar, n)            
            ial = iar-1
        endif

        ! linearly interpolate between the two points
        phi = 1.0_dp - (x-xi(ial))/(xi(iar)-xi(ial))            

    end subroutine linint_Gen_detail




 
!############################################################################## 

    ! FUNCTION poly_interpol_1
    !
    ! Constructs interpolating polynomial given nodes xi and data yi.
    !##############################################################################
    pure function poly_interpol_1(x, xi, yi)
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate polynomial
        real(dp), intent(in) :: x
     
        ! nodes of interpolation
        real(dp), intent(in) :: xi(0:)
     
        ! data of interpolation
        real(dp), intent(in) :: yi(0:)
     
        ! return value
        real(dp) :: poly_interpol_1
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: j, n, i
        real(dp) :: lagrange_poly(0:size(xi, 1)-1)
     
        !##### ROUTINE CODE #######################################################
     
        ! get number of interpolation nodes
        n = assert_eq(size(xi, 1), size(yi, 1), 'poly_interpol') - 1
     
        ! initialize lagrange basis polynomials
        lagrange_poly(:) = 1.0_dp
     
        ! span polynomials
        do j = 0, n
            do i = 0, n
                if(j /= i)then
                    lagrange_poly(j) = lagrange_poly(j) * (x-xi(i))/(xi(j)-xi(i))
                endif
            enddo
        enddo
     
        poly_interpol_1 = sum(yi*lagrange_poly, 1)
     
    end function poly_interpol_1
     
     
    !##############################################################################
    ! FUNCTION poly_interpol_m
    !
    ! Constructs interpolating polynomial given nodes xi and data yi and several
    !     points x.
    !##############################################################################
    pure function poly_interpol_m(x, xi, yi)
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate polynomial
        real(dp), intent(in) :: x(1:)
     
        ! nodes of interpolation
        real(dp), intent(in) :: xi(0:)
     
        ! data of interpolation
        real(dp), intent(in) :: yi(0:)
     
        ! return value
        real(dp) :: poly_interpol_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: j, n, i
        real(dp) :: lagrange_poly(size(x, 1), 0:size(xi, 1))
     
     
        !##### ROUTINE CODE #######################################################
     
        ! get number of interpolation nodes
        n = assert_eq(size(xi, 1), size(yi, 1), 'poly_interpol') - 1
     
        ! initialize lagrange basis polynomials
        lagrange_poly(:, :) = 1.0_dp
     
        ! span polynomials
        do j = 0, n
            do i = 0, n
                if(j /= i)then
                    lagrange_poly(:, j) = lagrange_poly(:, j) * &
                        (x-xi(i))/(xi(j)-xi(i))
                endif
            enddo
        enddo
     
        do j = 1, size(x, 1)
            poly_interpol_m(j) = sum(yi*lagrange_poly(j, :), 1)
        enddo
     
    end function poly_interpol_m









 
 
 
 
 
 
!############################################################################## 

    ! SUBROUTINE spline_interp1
    !
    ! Subroutine for one-dimensional spline interpolation.
    !##############################################################################
    subroutine spline_interp1(yi, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! interpolation data
        real(dp), intent(in) :: yi(0:)
     
        ! coefficients for spline interpolation
        real(dp), intent(out) :: c(1:)
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j
        real(dp), allocatable :: r(:), d(:)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! assert sizes for the two arrays do fit
        n = assert_eq(size(yi,1)+2, size(c,1), 'spline_interp')
     
        ! deallocate help arrays
        if(allocated(r))deallocate(r)
        if(allocated(d))deallocate(d)
     
        ! allocate help arrays
        allocate(r(n))
        allocate(d(n))
     
        ! calculate real n
        n = n-3
     
        ! calculate numerical derivatives at end points
        r(1) = (2.0_dp*yi(0)-5.0_dp*yi(1)+4.0_dp*yi(2)-yi(3))/6.0_dp
        r(n+3) = (2.0_dp*yi(n)-5.0_dp*yi(n-1)+4.0_dp*yi(n-2)-yi(n-3))/6.0_dp
     
        ! set rest of right side of equation system
        r(2:n+2) = yi(0:n)
     
        ! solve the spline interpolation equation system
        c(2) = (yi(0)-r(1))/6.0_dp
        c(n+2) = (yi(n)-r(n+3))/6.0_dp
     
        d(3) = 4.0_dp
        r(3) = yi(1)-c(2)
        r(n+1) = yi(n-1)-c(n+2)
     
        do j = 4, n+1
            d(j) = 4.0_dp-1.0_dp/d(j-1)
            r(j) = r(j)-r(j-1)/d(j-1)
        enddo
     
        c(n+1) = r(n+1)/d(n+1)
     
        do j = n, 3, -1
            c(j) = (r(j)-c(j+1))/d(j)
        enddo
     
        c(1) = r(1)+2.0_dp*c(2)-c(3)
        c(n+3) = r(n+3)+2.0_dp*c(n+2)-c(n+1)
    
    end subroutine spline_interp1
    
    
    !##############################################################################
    ! SUBROUTINE spline_interp2
    !
    ! Subroutine for two-dimensional spline interpolation.
    !##############################################################################
    subroutine spline_interp2(yi, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! interpolation data
        real(dp), intent(in) :: yi(0:, 0:)
     
        ! coefficients for spline interpolation
        real(dp), intent(out) :: c(1:, 1:)
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n(2), j
        real(dp), allocatable :: tempc(:, :)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate array sizes
        do j = 1, 2
            n(j) = assert_eq(size(yi,j)+2, size(c,j), 'spline_interp')
        enddo
     
        ! calculate real n
        n = n-3
     
        ! deallocate tempc
        if(allocated(tempc))deallocate(tempc)
     
        ! allocate tempc
        allocate(tempc(n(1)+3, 0:n(2)))
     
        ! calculate temporary coefficients
        do j = 0, n(2)
            call spline_interp1(yi(:, j), tempc(:, j))
        enddo
     
        ! calculate actual coefficients
        do j = 1, n(1)+3
            call spline_interp1(tempc(j, :), c(j, :))
        enddo
    
    end subroutine spline_interp2
    
    
    !##############################################################################
    ! SUBROUTINE spline_interp3
    !
    ! Subroutine for three-dimensional spline interpolation.
    !##############################################################################
    subroutine spline_interp3(yi, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! interpolation data
        real(dp), intent(in) :: yi(0:, 0:, 0:)
     
        ! coefficients for spline interpolation
        real(dp), intent(out) :: c(1:, 1:, 1:)
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n(3), j, j2
        real(dp), allocatable :: tempc(:, :, :)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate array sizes
        do j = 1, 3
            n(j) = assert_eq(size(yi,j)+2, size(c,j), 'spline_interp')
        enddo
     
        ! calculate real n
        n = n-3
     
        ! deallocate tempc
        if(allocated(tempc))deallocate(tempc)
     
        ! allocate tempc
        allocate(tempc(n(1)+3, n(2)+3, 0:n(3)))
     
        ! calculate temporary coefficients
        do j = 0, n(3)
            call spline_interp2(yi(:, :, j), tempc(:, :, j))
        enddo
     
        ! calculate actual coefficients
        do j = 1, n(1)+3
            do j2 = 1, n(2)+3
                call spline_interp1(tempc(j, j2, :), c(j, j2, :))
            enddo
        enddo
    
    end subroutine spline_interp3
    
    
    !##############################################################################
    ! SUBROUTINE spline_interp4
    !
    ! Subroutine for four-dimensional spline interpolation.
    !##############################################################################
    subroutine spline_interp4(yi, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! interpolation data
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:)
     
        ! coefficients for spline interpolation
        real(dp), intent(out) :: c(1:, 1:, 1:, 1:)
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n(4), j, j2, j3
        real(dp), allocatable :: tempc(:, :, :, :)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate array sizes
        do j = 1, 4
            n(j) = assert_eq(size(yi,j)+2, size(c,j), 'spline_interp')
        enddo
     
        ! calculate real n
        n = n-3
     
        ! deallocate tempc
        if(allocated(tempc))deallocate(tempc)
     
        ! allocate tempc
        allocate(tempc(n(1)+3, n(2)+3, n(3)+3, 0:n(4)))
     
        ! calculate temporary coefficients
        do j = 0, n(4)
            call spline_interp3(yi(:, :, :, j), tempc(:, :, :, j))
        enddo
     
        ! calculate actual coefficients
        do j = 1, n(1)+3
            do j2 = 1, n(2)+3
                do j3 = 1, n(3)+3
                    call spline_interp1(tempc(j, j2, j3, :), c(j, j2, j3, :))
                enddo
            enddo
        enddo
    
    end subroutine spline_interp4
    
    
    !##############################################################################
    ! SUBROUTINE spline_interp5
    !
    ! Subroutine for five-dimensional spline interpolation.
    !##############################################################################
    subroutine spline_interp5(yi, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! interpolation data
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:, 0:)
     
        ! coefficients for spline interpolation
        real(dp), intent(out) :: c(1:, 1:, 1:, 1:, 1:)
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n(5), j, j2, j3, j4
        real(dp), allocatable :: tempc(:, :, :, :, :)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate array sizes
        do j = 1, 5
            n(j) = assert_eq(size(yi,j)+2, size(c,j), 'spline_interp')
        enddo
     
        ! calculate real n
        n = n-3
     
        ! deallocate tempc
        if(allocated(tempc))deallocate(tempc)
     
        ! allocate tempc
        allocate(tempc(n(1)+3, n(2)+3, n(3)+3, n(4)+3, 0:n(5)))
     
        ! calculate temporary coefficients
        do j = 0, n(5)
            call spline_interp4(yi(:, :, :, :, j), tempc(:, :, :, :, j))
        enddo
     
        ! calculate actual coefficients
        do j = 1, n(1)+3
            do j2 = 1, n(2)+3
                do j3 = 1, n(3)+3
                    do j4 = 1, n(4)+3
                        call spline_interp1(tempc(j, j2, j3, j4, :), &
                            c(j, j2, j3, j4, :))
                    enddo
                enddo
            enddo
        enddo
    
    end subroutine spline_interp5
    
    
    !##############################################################################
    ! SUBROUTINE spline_interp6
    !
    ! Subroutine for six-dimensional spline interpolation.
    !##############################################################################
    subroutine spline_interp6(yi, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! interpolation data
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:, 0:, 0:)
     
        ! coefficients for spline interpolation
        real(dp), intent(out) :: c(1:, 1:, 1:, 1:, 1:, 1:)
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n(6), j, j2, j3, j4, j5
        real(dp), allocatable :: tempc(:, :, :, :, :, :)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate array sizes
        do j = 1, 6
            n(j) = assert_eq(size(yi,j)+2, size(c,j), 'spline_interp')
        enddo
     
        ! calculate real n
        n = n-3
     
        ! deallocate tempc
        if(allocated(tempc))deallocate(tempc)
     
        ! allocate tempc
        allocate(tempc(n(1)+3, n(2)+3, n(3)+3, n(4)+3, n(5)+3, 0:n(6)))
     
        ! calculate temporary coefficients
        do j = 0, n(6)
            call spline_interp5(yi(:, :, :, :, :, j), tempc(:, :, :, :, :, j))
        enddo
     
        ! calculate actual coefficients
        do j = 1, n(1)+3
            do j2 = 1, n(2)+3
                do j3 = 1, n(3)+3
                    do j4 = 1, n(4)+3
                        do j5 = 1, n(5)+3
                            call spline_interp1(tempc(j, j2, j3, j4, j5, :), &
                                c(j, j2, j3, j4, j5, :))
                        enddo
                    enddo
                enddo
            enddo
        enddo
    
    end subroutine spline_interp6
    
    
    !##############################################################################
    ! SUBROUTINE spline_interp7
    !
    ! Subroutine for seven-dimensional spline interpolation.
    !##############################################################################
    subroutine spline_interp7(yi, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! interpolation data
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:, 0:, 0:, 0:)
     
        ! coefficients for spline interpolation
        real(dp), intent(out) :: c(1:, 1:, 1:, 1:, 1:, 1:, 1:)
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n(7), j, j2, j3, j4, j5, j6
        real(dp), allocatable :: tempc(:, :, :, :, :, :, :)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate array sizes
        do j = 1, 7
            n(j) = assert_eq(size(yi,j)+2, size(c,j), 'spline_interp')
        enddo
     
        ! calculate real n
        n = n-3
     
        ! deallocate tempc
        if(allocated(tempc))deallocate(tempc)
     
        ! allocate tempc
        allocate(tempc(n(1)+3, n(2)+3, n(3)+3, n(4)+3, n(5)+3, n(6)+3, 0:n(7)))
     
        ! calculate temporary coefficients
        do j = 0, n(7)
            call spline_interp6(yi(:, :, :, :, :, :, j), tempc(:, :, :, :, :, :, j))
        enddo
     
        ! calculate actual coefficients
        do j = 1, n(1)+3
            do j2 = 1, n(2)+3
                do j3 = 1, n(3)+3
                    do j4 = 1, n(4)+3
                        do j5 = 1, n(5)+3
                            do j6 = 1, n(6)+3
                                call spline_interp1(tempc(j, j2, j3, &
                                    j4, j5, j6, :),c(j, j2, j3, j4, j5, j6, :))
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo
    
    end subroutine spline_interp7
    
    
    !##############################################################################
    ! FUNCTION spline1
    !
    ! Function for evaluation of one-dimensional spline.
    !##############################################################################
    function spline1(x, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:)
     
        ! value of spline function
        real(dp) :: spline1
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n1, j1, p1, q1
        real(dp) :: phi1, xtemp1
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of points used
        n1 = size(c, 1)
     
        ! calculate left and right summation end point
        p1 = max(floor(x)+1, 1)
        q1 = min(p1+3, n1)
     
        spline1 = 0.0_dp
     
        do j1 = p1, q1
     
            ! calculate value where to evaluate basis function
            xtemp1 = abs(x-j1+2)
     
            ! calculate basis function
            if(xtemp1 <= 1.0_dp)then
                phi1 = 4.0_dp+xtemp1**2*(3.0_dp*xtemp1-6.0_dp)
            elseif(xtemp1 <= 2.0_dp)then
                phi1 = (2.0_dp-xtemp1)**3
            else
                phi1 = 0.0_dp
            endif
     
            ! calculate spline value
            spline1 = spline1+c(j1)*phi1
        enddo
    
    end function spline1
    
    
    !##############################################################################
    ! FUNCTION spline1_grid
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid.
    !##############################################################################
    function spline1_grid(x, c, left, right, growth)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left
     
        ! right interval endpoint
        real(dp), intent(in) :: right
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth
     
        ! value of spline function
        real(dp) :: spline1_grid
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n
        real(dp) :: xtemp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of grid-points
        n = size(c, 1)-3
     
        ! invert grid
        if(present(growth))then
            xtemp = grid_Inv_Grow(x, left, right, growth, n)
        else
            xtemp = grid_Inv_Equi(x, left, right, n)
        endif
     
        ! calculate spline value
        spline1_grid = spline1(xtemp, c)
    
    end function spline1_grid
    
    
    !##############################################################################
    ! FUNCTION spline1_complete
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for a single point.
    !##############################################################################
    function spline1_complete(x, yi, left, right, growth)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left
     
        ! right interval endpoint
        real(dp), intent(in) :: right
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth
     
        ! value of spline function
        real(dp) :: spline1_complete
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: spline_temp(1)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! invert grid for every evaluation point
        if(present(growth))then
            spline_temp = spline1_complete_m((/x/), yi, left, right, growth)
        else
            spline_temp = spline1_complete_m((/x/), yi, left, right)
        endif
     
        ! paste data
        spline1_complete = spline_temp(1)
    
    end function spline1_complete
    
    
    !##############################################################################
    ! FUNCTION spline1_complete_m
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for many points.
    !##############################################################################
    function spline1_complete_m(x, yi, left, right, growth)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(1:)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left
     
        ! right interval endpoint
        real(dp), intent(in) :: right
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth
     
        ! value of spline function
        real(dp) :: spline1_complete_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: c(1:size(yi, 1)+2)
        real(dp) :: xtemp(1:size(x, 1))
        integer(int32) :: n, m, j
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of grid-points
        n = size(yi, 1)-1
     
        ! calculate number of evaluation points
        m = size(x, 1)
     
        ! invert grid for every evaluation point
        if(present(growth))then
            xtemp(:) = grid_Inv_Grow(x(:), left, right, growth, n)
        else
            xtemp(:) = grid_Inv_Equi(x(:), left, right, n)
        endif
     
        ! interpolate data
        call spline_interp1(yi, c)
     
        ! calculate spline values at point
        do j = 1, m
            spline1_complete_m(j) = spline1(xtemp(j), c)
        enddo
    
    end function spline1_complete_m
    
    
    !##############################################################################
    ! FUNCTION spline2
    !
    ! Function for evaluation of two-dimensional spline.
    !##############################################################################
    function spline2(x, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(2)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:)
     
        ! value of spline function
        real(dp) :: spline2
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n(2), p(2), q(2)
        integer(int32) :: j1, j2
        real(dp) :: phi1, xtemp1, phi2, xtemp2
        real(dp) :: s2
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of points used
        n(1) = size(c, 1)
        n(2) = size(c, 2)
     
        ! calculate left and right summation end point
        p = max(floor(x)+1, 1)
        q = min(p+3, n)
     
        spline2 = 0.0_dp
     
        do j1 = p(1), q(1)
     
            ! calculate value where to evaluate basis function
            xtemp1 = abs(x(1)-j1+2)
     
            ! calculate basis function
            if(xtemp1 <= 1.0_dp)then
                phi1 = 4.0_dp+xtemp1**2*(3.0_dp*xtemp1-6.0_dp)
            elseif(xtemp1 <= 2.0_dp)then
                phi1 = (2.0_dp-xtemp1)**3
            else
                phi1 = 0.0_dp
            endif
     
     
            !#### calculate spline for second dimension ###########################
     
            s2 = 0.0_dp
     
            do j2 = p(2), q(2)
     
                ! calculate value where to evaluate basis function
                xtemp2 = abs(x(2)-j2+2)
     
                ! calculate basis function
                if(xtemp2 <= 1.0_dp)then
                    phi2 = 4.0_dp+xtemp2**2*(3.0_dp*xtemp2-6.0_dp)
                elseif(xtemp2 <= 2.0_dp)then
                    phi2 = (2.0_dp-xtemp2)**3
                else
                    phi2 = 0.0_dp
                endif
     
                ! calculate spline value
                s2 = s2+c(j1, j2)*phi2
            enddo
     
            ! calculate spline value
            spline2 = spline2+s2*phi1
        enddo
    
    end function spline2
    
    
    !##############################################################################
    ! FUNCTION spline2_grid
    !
    ! Function for evaluation of two-dimensional spline, includ inverting grid.
    !##############################################################################
    function spline2_grid(x, c, left, right, growth)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(2)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(2)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(2)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(2)
     
        ! value of spline function
        real(dp) :: spline2_grid
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j
        real(dp) :: xtemp(2)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of grid-points
        do j = 1, size(x, 1)
            n = size(c, j)-3
     
            ! invert grid
            if(present(growth))then
                xtemp(j) = grid_Inv_Grow(x(j), left(j), right(j), growth(j), n)
            else
                xtemp(j) = grid_Inv_Equi(x(j), left(j), right(j), n)
            endif
        enddo
     
        ! calculate spline value
        spline2_grid = spline2(xtemp, c)
    
    end function spline2_grid
    
    
    !##############################################################################
    ! FUNCTION spline2_complete
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for a single point.
    !##############################################################################
    function spline2_complete(x, yi, left, right, growth)
    
    
        integer(int32), parameter :: dim = 2
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(dim)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline2_complete
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: xtemp(1, dim)
        real(dp) :: spline_temp(1)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! set xtemp
        xtemp(1, :) = x
     
        ! invert grid for every evaluation point
        if(present(growth))then
            spline_temp = spline2_complete_m(xtemp, yi, left, right, growth)
        else
            spline_temp = spline2_complete_m(xtemp, yi, left, right)
        endif
     
        ! paste data
        spline2_complete = spline_temp(1)
    
    end function spline2_complete
    
    
    !##############################################################################
    ! FUNCTION spline2_complete_m
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for many points.
    !##############################################################################
    function spline2_complete_m(x, yi, left, right, growth)
    
        integer(int32), parameter :: dim = 2
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(1:, 1:)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline2_complete_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: c(1:size(yi, 1)+2, 1:size(yi, 2)+2)
        real(dp) :: xtemp(1:size(x, 1), dim)
        integer(int32) :: n, m, j, k
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of evaluation points
        m = size(x, 1)
     
        ! check whether x has the right dimension
        n = assert_eq(size(x, 2), dim, 'spline')
     
        ! invert grid for every evaluation point
        if(present(growth))then
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Grow(x(:, k), &
                        left(k), right(k), growth(k), n)
            enddo
        else
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Equi(x(:, k), left(k), right(k), n)
            enddo
        endif
     
        ! interpolate data
        call spline_interp2(yi, c)
     
        ! calculate spline values at point
        do j = 1, m
            spline2_complete_m(j) = spline2(xtemp(j, :), c)
        enddo
    
    end function spline2_complete_m
    
    
    !##############################################################################
    ! FUNCTION spline3
    !
    ! Function for evaluation of three-dimensional spline.
    !##############################################################################
    function spline3(x, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(3)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:, 1:)
     
        ! value of spline function
        real(dp) :: spline3
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n(3), p(3), q(3)
        integer(int32) :: j1, j2, j3
        real(dp) :: phi1, xtemp1, phi2, xtemp2, phi3, xtemp3
        real(dp) :: s2, s3
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of points used
        n(1) = size(c, 1)
        n(2) = size(c, 2)
        n(3) = size(c, 3)
     
        ! calculate left and right summation end point
        p = max(floor(x)+1, 1)
        q = min(p+3, n)
     
        spline3 = 0.0_dp
     
        do j1 = p(1), q(1)
     
            ! calculate value where to evaluate basis function
            xtemp1 = abs(x(1)-j1+2)
     
            ! calculate basis function
            if(xtemp1 <= 1.0_dp)then
                phi1 = 4.0_dp+xtemp1**2*(3.0_dp*xtemp1-6.0_dp)
            elseif(xtemp1 <= 2.0_dp)then
                phi1 = (2.0_dp-xtemp1)**3
            else
                phi1 = 0.0_dp
            endif
     
     
            !#### calculate spline for second dimension ###########################
     
            s2 = 0.0_dp
     
            do j2 = p(2), q(2)
     
                ! calculate value where to evaluate basis function
                xtemp2 = abs(x(2)-j2+2)
     
                ! calculate basis function
                if(xtemp2 <= 1.0_dp)then
                    phi2 = 4.0_dp+xtemp2**2*(3.0_dp*xtemp2-6.0_dp)
                elseif(xtemp2 <= 2.0_dp)then
                    phi2 = (2.0_dp-xtemp2)**3
                else
                    phi2 = 0.0_dp
                endif
     
     
                !#### calculate spline for second dimension #######################
     
                s3 = 0.0_dp
     
                do j3 = p(3), q(3)
     
                    ! calculate value where to evaluate basis function
                    xtemp3 = abs(x(3)-j3+2)
     
                    ! calculate basis function
                    if(xtemp3 <= 1.0_dp)then
                        phi3 = 4.0_dp+xtemp3**2*(3.0_dp*xtemp3-6.0_dp)
                    elseif(xtemp3 <= 2.0_dp)then
                        phi3 = (2.0_dp-xtemp3)**3
                    else
                        phi3 = 0.0_dp
                    endif
     
                    ! calculate spline value
                    s3 = s3+c(j1, j2, j3)*phi3
                enddo
     
                ! calculate spline value
                s2 = s2+s3*phi2
            enddo
     
            ! calculate spline value
            spline3 = spline3+s2*phi1
        enddo
    
    end function spline3
    
    
    !##############################################################################
    ! FUNCTION spline3_grid
    !
    ! Function for evaluation of three-dimensional spline, includes inverting grid.
    !##############################################################################
    function spline3_grid(x, c, left, right, growth)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(3)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:, 1:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(3)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(3)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(3)
     
        ! value of spline function
        real(dp) :: spline3_grid
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j
        real(dp) :: xtemp(3)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of grid-points
        do j = 1, size(x, 1)
            n = size(c, j)-3
     
            ! invert grid
            if(present(growth))then
                xtemp(j) = grid_Inv_Grow(x(j), left(j), right(j), growth(j), n)
            else
                xtemp(j) = grid_Inv_Equi(x(j), left(j), right(j), n)
            endif
        enddo
     
        ! calculate spline value
        spline3_grid = spline3(xtemp, c)
    
    end function spline3_grid
    
    
    !##############################################################################
    ! FUNCTION spline3_complete
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for a single point.
    !##############################################################################
    function spline3_complete(x, yi, left, right, growth)
    
    
        integer(int32), parameter :: dim = 3
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(dim)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline3_complete
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: xtemp(1, dim)
        real(dp) :: spline_temp(1)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! set xtemp
        xtemp(1, :) = x
     
        ! invert grid for every evaluation point
        if(present(growth))then
            spline_temp = spline3_complete_m(xtemp, yi, left, right, growth)
        else
            spline_temp = spline3_complete_m(xtemp, yi, left, right)
        endif
     
        ! paste data
        spline3_complete = spline_temp(1)
    
    end function spline3_complete
    
    
    !##############################################################################
    ! FUNCTION spline3_complete_m
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for many points.
    !##############################################################################
    function spline3_complete_m(x, yi, left, right, growth)
    
        integer(int32), parameter :: dim = 3
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(1:, 1:)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline3_complete_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: c(1:size(yi, 1)+2, 1:size(yi, 2)+2, 1:size(yi, 3)+2)
        real(dp) :: xtemp(1:size(x, 1), dim)
        integer(int32) :: n, m, j, k
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of evaluation points
        m = size(x, 1)
     
        ! check whether x has the right dimension
        n = assert_eq(size(x, 2), dim, 'spline')
     
        ! invert grid for every evaluation point
        if(present(growth))then
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Grow(x(:, k), &
                        left(k), right(k), growth(k), n)
            enddo
        else
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Equi(x(:, k), left(k), right(k), n)
            enddo
        endif
     
        ! interpolate data
        call spline_interp3(yi, c)
     
        ! calculate spline values at point
        do j = 1, m
            spline3_complete_m(j) = spline3(xtemp(j, :), c)
        enddo
    
    end function spline3_complete_m
    
    
    !##############################################################################
    ! FUNCTION spline4
    !
    ! Function for evaluation of four-dimensional spline.
    !##############################################################################
    function spline4(x, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(4)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:, 1:, 1:)
     
        ! value of spline function
        real(dp) :: spline4
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j, p, q
        real(dp) :: phi, xtemp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of points used
        n = size(c, 1)
     
        ! calculate left and right summation end point
        p = max(floor(x(1))+1, 1)
        q = min(p+3, n)
     
        spline4 = 0.0_dp
     
        do j = p, q
     
            ! calculate value where to evaluate basis function
            xtemp = abs(x(1)-j+2)
     
            ! calculate basis function
            if(xtemp <= 1.0_dp)then
                phi = 4.0_dp+xtemp**2*(3.0_dp*xtemp-6.0_dp)
            elseif(xtemp <= 2.0_dp)then
                phi = (2.0_dp-xtemp)**3
            else
                phi = 0.0_dp
            endif
     
            ! calculate spline value
            spline4 = spline4+spline3(x(2:4), c(j, :, :, :))*phi
        enddo
    
    end function spline4
    
    
    !##############################################################################
    ! FUNCTION spline4_grid
    !
    ! Function for evaluation of four-dimensional spline, includes inverting grid.
    !##############################################################################
    function spline4_grid(x, c, left, right, growth)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(4)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:, 1:, 1:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(4)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(4)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(4)
     
        ! value of spline function
        real(dp) :: spline4_grid
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j
        real(dp) :: xtemp(4)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of grid-points
        do j = 1, size(x, 1)
            n = size(c, j)-3
     
            ! invert grid
            if(present(growth))then
                xtemp(j) = grid_Inv_Grow(x(j), left(j), right(j), growth(j), n)
            else
                xtemp(j) = grid_Inv_Equi(x(j), left(j), right(j), n)
            endif
        enddo
     
        ! calculate spline value
        spline4_grid = spline4(xtemp, c)
    
    end function spline4_grid
    
    
    !##############################################################################
    ! FUNCTION spline4_complete
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for a single point.
    !##############################################################################
    function spline4_complete(x, yi, left, right, growth)
    
    
        integer(int32), parameter :: dim = 4
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(dim)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline4_complete
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: xtemp(1, dim)
        real(dp) :: spline_temp(1)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! set xtemp
        xtemp(1, :) = x
     
        ! invert grid for every evaluation point
        if(present(growth))then
            spline_temp = spline4_complete_m(xtemp, yi, left, right, growth)
        else
            spline_temp = spline4_complete_m(xtemp, yi, left, right)
        endif
     
        ! paste data
        spline4_complete = spline_temp(1)
    
    end function spline4_complete
    
    
    !##############################################################################
    ! FUNCTION spline4_complete_m
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for many points.
    !##############################################################################
    function spline4_complete_m(x, yi, left, right, growth)
    
        integer(int32), parameter :: dim = 4
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(1:, 1:)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline4_complete_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: c(1:size(yi, 1)+2, 1:size(yi, 2)+2, 1:size(yi, 3)+2, &
            1:size(yi, 4)+2)
        real(dp) :: xtemp(1:size(x, 1), dim)
        integer(int32) :: n, m, j, k
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of evaluation points
        m = size(x, 1)
     
        ! check whether x has the right dimension
        n = assert_eq(size(x, 2), dim, 'spline')
     
        ! invert grid for every evaluation point
        if(present(growth))then
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Grow(x(:, k), &
                        left(k), right(k), growth(k), n)
            enddo
        else
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Equi(x(:, k), left(k), right(k), n)
            enddo
        endif
     
        ! interpolate data
        call spline_interp4(yi, c)
     
        ! calculate spline values at point
        do j = 1, m
            spline4_complete_m(j) = spline4(xtemp(j, :), c)
        enddo
    
    end function spline4_complete_m
    
    
    !##############################################################################
    ! FUNCTION spline5
    !
    ! Function for evaluation of five-dimensional spline.
    !##############################################################################
    function spline5(x, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(5)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:, 1:, 1:, 1:)
     
        ! value of spline function
        real(dp) :: spline5
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j, p, q
        real(dp) :: phi, xtemp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of points used
        n = size(c, 1)
     
        ! calculate left and right summation end point
        p = max(floor(x(1))+1, 1)
        q = min(p+3, n)
     
        spline5 = 0.0_dp
     
        do j = p, q
     
            ! calculate value where to evaluate basis function
            xtemp = abs(x(1)-j+2)
     
            ! calculate basis function
            if(xtemp <= 1.0_dp)then
                phi = 4.0_dp+xtemp**2*(3.0_dp*xtemp-6.0_dp)
            elseif(xtemp <= 2.0_dp)then
                phi = (2.0_dp-xtemp)**3
            else
                phi = 0.0_dp
            endif
     
            ! calculate spline value
            spline5 = spline5+spline4(x(2:5), c(j, :, :, :, :))*phi
        enddo
    
    end function spline5
    
    
    !##############################################################################
    ! FUNCTION spline5_grid
    !
    ! Function for evaluation of five-dimensional spline, includes inverting grid.
    !##############################################################################
    function spline5_grid(x, c, left, right, growth)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(5)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:, 1:, 1:, 1:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(5)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(5)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(5)
     
        ! value of spline function
        real(dp) :: spline5_grid
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j
        real(dp) :: xtemp(5)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of grid-points
        do j = 1, size(x, 1)
            n = size(c, j)-3
     
            ! invert grid
            if(present(growth))then
                xtemp(j) = grid_Inv_Grow(x(j), left(j), right(j), growth(j), n)
            else
                xtemp(j) = grid_Inv_Equi(x(j), left(j), right(j), n)
            endif
        enddo
     
        ! calculate spline value
        spline5_grid = spline5(xtemp, c)
    
    end function spline5_grid
    
    
    !##############################################################################
    ! FUNCTION spline5_complete
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for a single point.
    !##############################################################################
    function spline5_complete(x, yi, left, right, growth)
    
    
        integer(int32), parameter :: dim = 5
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(dim)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline5_complete
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: xtemp(1, dim)
        real(dp) :: spline_temp(1)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! set xtemp
        xtemp(1, :) = x
     
        ! invert grid for every evaluation point
        if(present(growth))then
            spline_temp = spline5_complete_m(xtemp, yi, left, right, growth)
        else
            spline_temp = spline5_complete_m(xtemp, yi, left, right)
        endif
     
        ! paste data
        spline5_complete = spline_temp(1)
    
    end function spline5_complete
    
    
    !##############################################################################
    ! FUNCTION spline5_complete_m
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for many points.
    !##############################################################################
    function spline5_complete_m(x, yi, left, right, growth)
    
        integer(int32), parameter :: dim = 5
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(1:, 1:)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline5_complete_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: c(1:size(yi, 1)+2, 1:size(yi, 2)+2, 1:size(yi, 3)+2, &
            1:size(yi, 4)+2, 1:size(yi, 5)+2)
        real(dp) :: xtemp(1:size(x, 1), dim)
        integer(int32) :: n, m, j, k
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of evaluation points
        m = size(x, 1)
     
        ! check whether x has the right dimension
        n = assert_eq(size(x, 2), dim, 'spline')
     
        ! invert grid for every evaluation point
        if(present(growth))then
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Grow(x(:, k), &
                        left(k), right(k), growth(k), n)
            enddo
        else
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Equi(x(:, k), left(k), right(k), n)
            enddo
        endif
     
        ! interpolate data
        call spline_interp5(yi, c)
     
        ! calculate spline values at point
        do j = 1, m
            spline5_complete_m(j) = spline5(xtemp(j, :), c)
        enddo
    
    end function spline5_complete_m
    
    
    !##############################################################################
    ! FUNCTION spline6
    !
    ! Function for evaluation of six-dimensional spline.
    !##############################################################################
    function spline6(x, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(6)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:, 1:, 1:, 1:, 1:)
     
        ! value of spline function
        real(dp) :: spline6
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j, p, q
        real(dp) :: phi, xtemp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of points used
        n = size(c, 1)
     
        ! calculate left and right summation end point
        p = max(floor(x(1))+1, 1)
        q = min(p+3, n)
     
        spline6 = 0.0_dp
     
        do j = p, q
     
            ! calculate value where to evaluate basis function
            xtemp = abs(x(1)-j+2)
     
            ! calculate basis function
            if(xtemp <= 1.0_dp)then
                phi = 4.0_dp+xtemp**2*(3.0_dp*xtemp-6.0_dp)
            elseif(xtemp <= 2.0_dp)then
                phi = (2.0_dp-xtemp)**3
            else
                phi = 0.0_dp
            endif
     
            ! calculate spline value
            spline6 = spline6+spline5(x(2:6), c(j, :, :, :, :, :))*phi
        enddo
    
    end function spline6
    
    
    !##############################################################################
    ! FUNCTION spline6_grid
    !
    ! Function for evaluation of six-dimensional spline, includes inverting grid.
    !##############################################################################
    function spline6_grid(x, c, left, right, growth)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(6)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:, 1:, 1:, 1:, 1:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(6)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(6)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(6)
     
        ! value of spline function
        real(dp) :: spline6_grid
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j
        real(dp) :: xtemp(6)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of grid-points
        do j = 1, size(x, 1)
            n = size(c, j)-3
     
            ! invert grid
            if(present(growth))then
                xtemp(j) = grid_Inv_Grow(x(j), left(j), right(j), growth(j), n)
            else
                xtemp(j) = grid_Inv_Equi(x(j), left(j), right(j), n)
            endif
        enddo
     
        ! calculate spline value
        spline6_grid = spline6(xtemp, c)
    
    end function spline6_grid
    
    
    !##############################################################################
    ! FUNCTION spline6_complete
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for a single point.
    !##############################################################################
    function spline6_complete(x, yi, left, right, growth)
    
    
        integer(int32), parameter :: dim = 6
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(dim)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:, 0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline6_complete
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: xtemp(1, dim)
        real(dp) :: spline_temp(1)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! set xtemp
        xtemp(1, :) = x
     
        ! invert grid for every evaluation point
        if(present(growth))then
            spline_temp = spline6_complete_m(xtemp, yi, left, right, growth)
        else
            spline_temp = spline6_complete_m(xtemp, yi, left, right)
        endif
     
        ! paste data
        spline6_complete = spline_temp(1)
    
    end function spline6_complete
    
    
    !##############################################################################
    ! FUNCTION spline6_complete_m
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for many points.
    !##############################################################################
    function spline6_complete_m(x, yi, left, right, growth)
    
        integer(int32), parameter :: dim = 6
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(1:, 1:)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:, 0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline6_complete_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: c(1:size(yi, 1)+2, 1:size(yi, 2)+2, 1:size(yi, 3)+2, &
            1:size(yi, 4)+2, 1:size(yi, 5)+2, 1:size(yi, 6)+2)
        real(dp) :: xtemp(1:size(x, 1), dim)
        integer(int32) :: n, m, j, k
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of evaluation points
        m = size(x, 1)
     
        ! check whether x has the right dimension
        n = assert_eq(size(x, 2), dim, 'spline')
     
        ! invert grid for every evaluation point
        if(present(growth))then
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Grow(x(:, k), &
                        left(k), right(k), growth(k), n)
            enddo
        else
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Equi(x(:, k), left(k), right(k), n)
            enddo
        endif
     
        ! interpolate data
        call spline_interp6(yi, c)
     
        ! calculate spline values at point
        do j = 1, m
            spline6_complete_m(j) = spline6(xtemp(j, :), c)
        enddo
    
    end function spline6_complete_m
    
    
    !##############################################################################
    ! FUNCTION spline7
    !
    ! Function for evaluation of seven-dimensional spline.
    !##############################################################################
    function spline7(x, c)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(7)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:, 1:, 1:, 1:, 1:, 1:)
     
        ! value of spline function
        real(dp) :: spline7
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j, p, q
        real(dp) :: phi, xtemp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of points used
        n = size(c, 1)
     
        ! calculate left and right summation end point
        p = max(floor(x(1))+1, 1)
        q = min(p+3, n)
     
        spline7 = 0.0_dp
     
        do j = p, q
     
            ! calculate value where to evaluate basis function
            xtemp = abs(x(1)-j+2)
     
            ! calculate basis function
            if(xtemp <= 1.0_dp)then
                phi = 4.0_dp+xtemp**2*(3.0_dp*xtemp-6.0_dp)
            elseif(xtemp <= 2.0_dp)then
                phi = (2.0_dp-xtemp)**3
            else
                phi = 0.0_dp
            endif
     
            ! calculate spline value
            spline7 = spline7+spline6(x(2:7), c(j, :, :, :, :, :, :))*phi
        enddo
    
    end function spline7
    
    
    !##############################################################################
    ! FUNCTION spline7_grid
    !
    ! Function for evaluation of seven-dimensional spline, includes inverting grid.
    !##############################################################################
    function spline7_grid(x, c, left, right, growth)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(7)
     
        ! coefficients for spline interpolation
        real(dp), intent(in) :: c(1:, 1:, 1:, 1:, 1:, 1:, 1:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(7)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(7)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(7)
     
        ! value of spline function
        real(dp) :: spline7_grid
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n, j
        real(dp) :: xtemp(7)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of grid-points
        do j = 1, size(x, 1)
            n = size(c, j)-3
     
            ! invert grid
            if(present(growth))then
                xtemp(j) = grid_Inv_Grow(x(j), left(j), right(j), growth(j), n)
            else
                xtemp(j) = grid_Inv_Equi(x(j), left(j), right(j), n)
            endif
        enddo
     
        ! calculate spline value
        spline7_grid = spline7(xtemp, c)
    
    end function spline7_grid
    
    
    !##############################################################################
    ! FUNCTION spline7_complete
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for a single point.
    !##############################################################################
    function spline7_complete(x, yi, left, right, growth)
    
    
        integer(int32), parameter :: dim = 7
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(dim)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:, 0:, 0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline7_complete
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: xtemp(1, dim)
        real(dp) :: spline_temp(1)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! set xtemp
        xtemp(1, :) = x
     
        ! invert grid for every evaluation point
        if(present(growth))then
            spline_temp = spline7_complete_m(xtemp, yi, left, right, growth)
        else
            spline_temp = spline7_complete_m(xtemp, yi, left, right)
        endif
     
        ! paste data
        spline7_complete = spline_temp(1)
    
    end function spline7_complete
    
    
    !##############################################################################
    ! FUNCTION spline7_complete_m
    !
    ! Function for evaluation of one-dimensional spline, includes inverting grid
    !     and interpolation method for many points.
    !##############################################################################
    function spline7_complete_m(x, yi, left, right, growth)
    
        integer(int32), parameter :: dim = 7
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! value where to evaluate spline
        real(dp), intent(in) :: x(1:, 1:)
     
        ! data for spline interpolation
        real(dp), intent(in) :: yi(0:, 0:, 0:, 0:, 0:, 0:, 0:)
     
        ! left interval endpoint
        real(dp), intent(in) :: left(dim)
     
        ! right interval endpoint
        real(dp), intent(in) :: right(dim)
     
        ! growth rate of grid
        real(dp), intent(in), optional :: growth(dim)
     
        ! value of spline function
        real(dp) :: spline7_complete_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: c(1:size(yi, 1)+2, 1:size(yi, 2)+2, 1:size(yi, 3)+2, &
            1:size(yi, 4)+2, 1:size(yi, 5)+2, 1:size(yi, 6)+2, 1:size(yi, 7)+2)
        real(dp) :: xtemp(1:size(x, 1), dim)
        integer(int32) :: n, m, j, k
     
     
        !##### ROUTINE CODE #######################################################
     
        ! calculate number of evaluation points
        m = size(x, 1)
     
        ! check whether x has the right dimension
        n = assert_eq(size(x, 2), dim, 'spline')
     
        ! invert grid for every evaluation point
        if(present(growth))then
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Grow(x(:, k), &
                        left(k), right(k), growth(k), n)
            enddo
        else
            do k = 1, dim
                ! calculate number of grid-points
                n = size(yi, k)-1
     
                xtemp(:, k) = grid_Inv_Equi(x(:, k), left(k), right(k), n)
            enddo
        endif
     
        ! interpolate data
        call spline_interp7(yi, c)
     
        ! calculate spline values at point
        do j = 1, m
            spline7_complete_m(j) = spline7(xtemp(j, :), c)
        enddo
    
    end function spline7_complete_m















!##############################################################################


end module toolbox_interp
