!##############################################################################
! MODULE toolbox_grids
!
! Grid construction, evaluation, and coordinate inversion.
!##############################################################################
module toolbox_grids

    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64, &
        input_unit, output_unit, error_unit
    use toolbox_kinds
    use toolbox_core

    implicit none
    private

    public :: grid_Cons_Equi, grid_Val_Equi, grid_Inv_Equi
    public :: grid_Cons_Cheb, grid_Val_Cheb, grid_Inv_Cheb
    public :: grid_Cons_Grow, grid_Val_Grow, grid_Inv_Grow
    public :: grid_Inv_Equi_1, grid_Inv_Equi_m
    public :: grid_Inv_Cheb_1, grid_Inv_Cheb_m
    public :: grid_Inv_Grow_1, grid_Inv_Grow_m

!##############################################################################
! INTERFACE grid_Inv_Equi
!
! Interface for inverting gridpoints.
!##############################################################################
interface grid_Inv_Equi

    module procedure grid_Inv_Equi_1, grid_Inv_Equi_m

end interface


!##############################################################################
! INTERFACE grid_Inv_Cheb
!
! Interface for inverting gridpoints.
!##############################################################################
interface grid_Inv_Cheb

    module procedure grid_Inv_Cheb_1, grid_Inv_Cheb_m

end interface


!##############################################################################
! INTERFACE grid_Inv_Grow
!
! Interface for inverting gridpoints.
!##############################################################################
interface grid_Inv_Grow

    module procedure grid_Inv_Grow_1, grid_Inv_Grow_m

end interface



contains

    !##############################################################################
    ! SUBROUTINE grid_Cons_Equi
    !
    ! Constructs a whole equidistant grid on [left,right].
    !##############################################################################
    pure subroutine grid_Cons_Equi(a, left, right)
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! the array to fill
        real(dp), intent(out) :: a(1:)
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: h
        integer(int32) :: j, n
     
     
        !##### ROUTINE CODE #######################################################
        
        ! get number of grid points
        n = size(a, 1)-1
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Cons_Equi: left interval point greater than right point'
     
        ! calculate distance between grid points
        h = (right-left)/real(n, dp)
     
        ! calculate grid value
        a = h*(/(real(j, dp), j=0,n)/)+left
     
    end subroutine grid_Cons_Equi
     
     
    !##############################################################################
    ! SUBROUTINE grid_Cons_Cheb
    !
    ! Constructs a whole grid on [left,right] using chebychev nodes.
    !##############################################################################
    pure subroutine grid_Cons_Cheb(a, left, right)
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! the array to fill
        real(dp), intent(out) :: a(1:)
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: j, n
        real(dp), parameter :: pi = 3.1415926535897932_dp
     
     
        !##### ROUTINE CODE #######################################################
        
        ! get number of grid points
        n = size(a, 1)-1
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Cons_Cheb: left interval point greater than right point'
     
        ! calculate grid value
        a = (left+right)/2.0_dp + (right-left)/2.0_dp* &
            cos((real(n, dp)-(/(real(j, dp), j=0,n)/)+0.5_dp)/real(n+1, dp)*pi)
     
    end subroutine grid_Cons_Cheb


    !##############################################################################
    ! SUBROUTINE grid_Cons_Grow
    !
    ! Constructs a growing grid on [left, right].
    !##############################################################################
    pure subroutine grid_Cons_Grow(a, left, right, growth)
        
        implicit none

        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! the array to fill
        real(dp), intent(out) :: a(1:)
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
        ! the growth rate of the grid
        real(dp), intent(in) :: growth
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: h
        integer(int32) :: j, n
     
     
        !##### ROUTINE CODE #######################################################
        
        ! get number of grid points
        n = size(a, 1)-1

        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Cons_Grow: left interval point greater than right point'

        ! check for growth
        if(growth <= 0.0_dp) error stop 'ERROR: grid_Cons_Grow: growth rate must be greater than zero'

        ! calculate factor
        h = (right-left)/((1.0_dp+growth)**n-1.0_dp)
     
        ! calculate grid value
        a = h*((1.0_dp+growth)**(/(real(j, dp), j=0,n)/)-1.0_dp)+left

    end subroutine grid_Cons_Grow
    
    
    !##############################################################################
    ! FUNCTION grid_Val_Equi
    !
    ! Calculates single gridpoint of an equidistant grid.
    !##############################################################################
    elemental function grid_Val_Equi(x, left, right, n)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x
     
        ! left and right interval point
        real(dp), intent(in) :: left, right

        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! value at the grid point x \in [0,n]
        real(dp) :: grid_Val_Equi
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: h
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Val_Equi: left interval point greater than right point'
     
        ! calculate distance between grid points
        h = (right-left)/n
     
        ! calculate grid value
        grid_Val_Equi = h*x+left
    
    end function grid_Val_Equi


    !##############################################################################
    ! FUNCTION grid_Val_Cheb
    !
    ! Calculates single gridpoint of a Chebychev grid.
    !##############################################################################
    elemental function grid_Val_Cheb(x, left, right, n)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x
     
        ! left and right interval point
        real(dp), intent(in) :: left, right

        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! value at the grid point x \in [0,n]
        real(dp) :: grid_Val_Cheb
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp), parameter :: pi = 3.1415926535897932_dp       
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Val_Cheb: left interval point greater than right point'
     
        ! calculate grid value
        grid_Val_Cheb = (left+right)/2.0_dp + (right-left)/2.0_dp* &
            cos((real(n, dp)-x+0.5_dp)/real(n+1, dp)*pi)
    
    end function grid_Val_Cheb
    

    !##############################################################################
    ! FUNCTION grid_Val_Grow
    !
    ! Calculates single gridpoint of a growing grid.
    !##############################################################################
    elemental function grid_Val_Grow(x, left, right, growth, n)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x
     
        ! left and right interval point
        real(dp), intent(in) :: left, right     
     
        ! growth rate
        real(dp), intent(in) :: growth

        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! value at the grid point x \in [0,n]
        real(dp) :: grid_Val_Grow
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: h
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Val_Grow: left interval point greater than right point'

        ! check for growth
        if(growth <= 0.0_dp) error stop 'ERROR: grid_Val_Grow: growth rate must be greater than zero'
     
        ! calculate factor
        h = (right-left)/((1.0_dp+growth)**n-1.0_dp)
     
        ! calculate grid value
        grid_Val_Grow = h*((1.0_dp+growth)**x-1.0_dp)+left
    
    end function grid_Val_Grow


    !##############################################################################
    ! FUNCTION grid_Inv_Equi_1
    !
    ! Calculates inverse of gridpoints of an equidistant grid.
    !##############################################################################
    pure function grid_Inv_Equi_1(x, left, right, n)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! value of the inverse of the gridpoint x \in [left, right]
        real(dp) :: grid_Inv_Equi_1
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: h
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Inv_Equi: left interval point greater than right point'
     
        ! calculate distance between grid points
        h = (right-left)/n
     
        ! calculate grid value
        grid_Inv_Equi_1 = (x-left)/h
    
    end function grid_Inv_Equi_1


    !##############################################################################
    ! FUNCTION grid_Inv_Cheb_1
    !
    ! Calculates inverse of gridpoints of a Chebychev grid.
    !##############################################################################
    pure function grid_Inv_Cheb_1(x, left, right, n)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n     
     
        ! value of the inverse of the gridpoint x \in [left, right]
        real(dp) :: grid_Inv_Cheb_1  


        !##### OTHER VARIABLES ####################################################
     
        real(dp), parameter :: pi = 3.1415926535897932_dp 
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Inv_Cheb: left interval point greater than right point'
     
        ! calculate grid value
        grid_Inv_Cheb_1 = real(n, dp) + 0.5_dp - acos((2.0_dp*x-(left+right)) &
            /(right-left))*real(n+1, dp)/pi
    
    end function grid_Inv_Cheb_1
    
    
    !##############################################################################
    ! FUNCTION grid_Inv_Grow_1
    !
    ! Calculates inverse of gridpoints of a growing grid.
    !##############################################################################
    pure function grid_Inv_Grow_1(x, left, right, growth, n)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x
     
        ! left and right interval point
        real(dp), intent(in) :: left, right

        ! growth rate
        real(dp), intent(in) :: growth
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n             
     
        ! value of the inverse of the gridpoint x \in [left, right]
        real(dp) :: grid_Inv_Grow_1
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: h
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Inv_Grow: left interval point greater than right point'

        ! check for growth
        if(growth <= 0.0_dp) error stop 'ERROR: grid_Inv_Grow: growth rate must be greater than zero'
     
        ! calculate factor
        h = (right-left)/((1.0_dp+growth)**n-1.0_dp)
     
        ! calculate grid value
        grid_Inv_Grow_1 = log((x-left)/h+1.0_dp)/log(1.0_dp+growth)
    
    end function grid_Inv_Grow_1
    
    
    !##############################################################################
    ! FUNCTION grid_Inv_Equi_m
    !
    ! Calculates inverse of gridpoints of an equidistant grid.
    !##############################################################################
    pure function grid_Inv_Equi_m(x, left, right, n)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x(:)
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n     
     
        ! value of the inverse of the gridpoint x \in [left, right]
        real(dp) :: grid_Inv_Equi_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: h
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Inv_Equi: left interval point greater than right point'
     
        ! calculate distance between grid points
        h = (right-left)/n
 
        ! calculate grid value
        grid_Inv_Equi_m = (x-left)/h
    
    end function grid_Inv_Equi_m


    !##############################################################################
    ! FUNCTION grid_Inv_Cheb_m
    !
    ! Calculates inverse of gridpoints of a Chebychev grid.
    !##############################################################################
    pure function grid_Inv_Cheb_m(x, left, right, n)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x(:)
     
        ! left and right interval point
        real(dp), intent(in) :: left, right
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n     
     
        ! value of the inverse of the gridpoint x \in [left, right]
        real(dp) :: grid_Inv_Cheb_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp), parameter :: pi = 3.1415926535897932_dp 
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Inv_Cheb: left interval point greater than right point'
 
        ! calculate grid value
        grid_Inv_Cheb_m = real(n, dp) + 0.5_dp - acos((2.0_dp*x-(left+right)) &
            /(right-left))*real(n+1, dp)/pi
    
    end function grid_Inv_Cheb_m


    !##############################################################################
    ! FUNCTION grid_Inv_Grow_m
    !
    ! Calculates inverse of gridpoints of a growing grid.
    !##############################################################################
    pure function grid_Inv_Grow_m(x, left, right, growth, n)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point that shall be calculated
        real(dp), intent(in) :: x(:)
     
        ! left and right interval point
        real(dp), intent(in) :: left, right

        ! growth rate
        real(dp), intent(in) :: growth
     
        ! last grid point: 0,1,...,n
        integer(int32), intent(in) :: n
     
        ! value of the inverse of the gridpoint x \in [left, right]
        real(dp) :: grid_Inv_Grow_m(size(x, 1))
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: h
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for left <= right
        if(left >= right) error stop 'ERROR: grid_Inv_Grow: left interval point greater than right point'

        ! check for growth
        if(growth <= 0.0_dp) error stop 'ERROR: grid_Inv_Grow: growth rate must be greater than zero'
     
        ! calculate factor
        h = (right-left)/((1.0_dp+growth)**n-1.0_dp)

        ! calculate grid value
        grid_Inv_Grow_m = log((x-left)/h+1.0_dp)/log(1.0_dp+growth)
    
    end function grid_Inv_Grow_m



end module toolbox_grids
