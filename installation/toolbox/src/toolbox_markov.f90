!##############################################################################
! MODULE toolbox_markov
!
! Discretization and simulation of Markov chains and AR(1) processes.
!##############################################################################
module toolbox_markov

    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64, &
        input_unit, output_unit, error_unit
    use toolbox_kinds
    use toolbox_core
    use toolbox_dist

    implicit none
    private

    public :: discretize_AR, discretize_log_AR, simulate_AR

contains

    !##############################################################################
    ! SUBROUTINE discretize_AR
    !
    ! Discretizes an AR(1) process of the form z_j = \rho*z_{j-1} + eps using
    !     the Rouwenhorst method.
    !
    ! REFERENCE: Kopecky, K.A., Suen, R.M.H., Finite state Markov-chain 
    !            approximations to highly persistent processes, Review of Economic
    !            Dynamics, Vol. 13, No. 3, 2010, 701-714.
    !##############################################################################
    subroutine discretize_AR(rho, mu, sigma_eps, z, pi, w)
    
       implicit none
    
    
       !##### INPUT/OUTPUT VARIABLES #############################################
    
       ! autoregression parameter
       real(dp), intent(in) :: rho
    
       ! unconditional mean of the process
       real(dp), intent(in) :: mu
    
       ! variance of the shock
       real(dp), intent(in) :: sigma_eps
    
       ! discrete shock values
       real(dp), intent(out) :: z(:)
    
       ! transition matrix
       real(dp), intent(out) :: pi(:, :)
    
       ! the stationary distribution
       real(dp), intent(out), optional :: w(:)
    
    
       !##### OTHER VARIABLES ####################################################
    
       integer(int32) :: n, in
       real(dp) :: psi, sigma_eta
    
    
       !##### ROUTINE CODE #######################################################
    
       ! assert size equality and get approximation points
       n = assert_eq(size(z), size(pi,1), size(pi,2), 'discretize_AR')
    
       ! calculate variance of the overall process
       sigma_eta = sigma_eps/(1.0_dp-rho**2)
    
       ! determine the transition matrix
       call rouwenhorst_matrix(rho, pi)
    
       ! determine the nodes
       psi = sqrt(real(n-1, dp))*sqrt(sigma_eta)
       do in = 1, n
           z(in) = -psi + 2.0_dp*psi*real(in-1, dp)/real(n-1, dp)
       enddo
       z = z + mu
    
       if(present(w))then
           w = 1.0_dp/real(n, dp)
           do in = 1, 10000
               w = matmul(transpose(pi), w)
           enddo
       endif
    
       !##########################################################################
       ! Subroutines and functions
       !##########################################################################
    
       contains
    
    
       !##########################################################################
       ! subroutine rouwenhorst_matrix
       !
       ! Calculates value of function that should be integrated for pis.
       !##########################################################################
       recursive subroutine rouwenhorst_matrix(rho, pi_new)
    
           implicit none
           real(dp), intent(in) :: rho
           real(dp), intent(out) :: pi_new(:, :)
           integer(int32) :: n
           real(dp) :: p, pi_old(size(pi_new,1)-1, size(pi_new,1)-1)
    
           n = size(pi_new, 1)
           p = (1.0_dp + rho)/2.0_dp
    
           if(n == 2)then
               pi_new(1, :) = (/p, 1.0_dp-p/)
               pi_new(2, :) = (/1.0_dp-p, p/)
           else
               call rouwenhorst_matrix(rho, pi_old)
               pi_new = 0.0_dp
    
               pi_new(1:n-1, 1:n-1) = pi_new(1:n-1, 1:n-1) + p*pi_old
               pi_new(1:n-1, 2:n  ) = pi_new(1:n-1, 2:n  ) + (1.0_dp-p)*pi_old
               pi_new(2:n  , 1:n-1) = pi_new(2:n  , 1:n-1) + (1.0_dp-p)*pi_old
               pi_new(2:n  , 2:n  ) = pi_new(2:n  , 2:n  ) + p*pi_old
    
               pi_new(2:n-1, :) = pi_new(2:n-1, :)/2.0_dp
           endif
       end subroutine
    
    end subroutine discretize_AR


    !##############################################################################
    ! SUBROUTINE discretize_log_AR
    !
    ! Discretizes a log-AR(1) process using the Rouwenhorst method.
    !
    ! REFERENCE: Kopecky, K.A., Suen, R.M.H., Finite state Markov-chain 
    !            approximations to highly persistent processes, Review of Economic
    !            Dynamics, Vol. 13, No. 3, 2010, 701-714.
    !##############################################################################
    subroutine discretize_log_AR(rho, mu, sigma_eps, z, pi, w)
    
        implicit none
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! autoregression parameter
        real(dp), intent(in) :: rho
     
        ! unconditional mean of the process
        real(dp), intent(in) :: mu
     
        ! variance of the shock
        real(dp), intent(in) :: sigma_eps
     
        ! discrete shock values
        real(dp), intent(out) :: z(:)
     
        ! transition matrix
        real(dp), intent(out) :: pi(:, :)
     
        ! the stationary distribution
        real(dp), intent(out), optional :: w(:)
    
    
        !##### OTHER VARIABLES ####################################################
    
        real(dp) :: sigma_eta, mu_c, sigma_c
    
    
        !##### ROUTINE CODE #######################################################

        ! calculate variance of the overall process
        sigma_eta = sigma_eps/(1.0_dp-rho**2)
    
        ! get the transformed variance and expectation
        sigma_c = log(1.0_dp+sigma_eta/mu**2)
        mu_c  = log(mu)-0.5_dp*sigma_c
        sigma_c = sigma_c*(1.0_dp-rho**2)

        ! discretize the log distribution
        if(present(w))then
            call discretize_AR(rho, mu_c, sigma_c, z, pi, w)
        else
            call discretize_AR(rho, mu_c, sigma_c, z, pi)
        endif

        ! take exponentials
        z = exp(z)
    
    end subroutine discretize_log_AR
    
    
    !##############################################################################
    ! SUBROUTINE simulate_AR
    !
    ! Simulates a discrete AR(1) process.
    !##############################################################################
    subroutine simulate_AR(pi, shocks, fixed)
    
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! transition matrix
        real(dp), intent(in) :: pi(:, :)
     
        ! simulated schocks
        integer(int32), intent(out) :: shocks(:)
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: T, n, j
     
     
        !##### ROUTINE CODE #######################################################
     
        ! assert size equality and get number of simulated schocks
        n = assert_eq(size(pi,1), size(pi,2), 'tauchen')
        T = size(shocks)
     
        ! initialize the random seed
        if(tbox_seed)then
            if(present(fixed))then
                call init_random_seed(fixed)
            else
                call init_random_seed()
            endif
            tbox_seed = .false.
        endif
     
        ! get first entry
        shocks(1) = n/2+1
     
        ! now calculate other shocks
        do j = 2, T
            shocks(j) = get_tomorrow(pi(shocks(j-1), :))
        enddo
    
    
    !##########################################################################
    ! Subroutines and functions
    !##########################################################################
    
    contains
    
    
        !##########################################################################
        ! FUNCTION get_tomorrow
        !
        ! Calculates value of function that should be integrated for pis.
        !##########################################################################
        function get_tomorrow(pi)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #########################################
     
            ! transition probabilities
            real(dp), intent(in) :: pi(:)
     
            ! tomorrows shock
            integer(int32) :: get_tomorrow
     
     
            !##### OTHER VARIABLES ################################################
     
            real(dp) :: rand
            integer(int32) :: i1
     
     
            !##### ROUTINE CODE ###################################################
     
            ! get random number
            call random_number(rand)
     
            ! get tomorrows value
            do i1 = 1, size(pi, 1)-1
     
                if(rand <= sum(pi(1:i1), 1))then
                    get_tomorrow = i1
                    return
                endif
            enddo
     
            ! else choose last value
            get_tomorrow = i1
            return
     
        end function
    
    end subroutine simulate_AR












!##############################################################################


end module toolbox_markov
