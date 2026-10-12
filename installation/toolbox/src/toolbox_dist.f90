!##############################################################################
! MODULE toolbox_dist
!
! Continuous and discrete probability distributions, random seed init, simulation.
!##############################################################################
module toolbox_dist

    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64, &
        input_unit, output_unit, error_unit
    use toolbox_kinds
    use toolbox_core
    use toolbox_linalg

    implicit none
    private

    ! should the random tbox_seed be set
    logical, public :: tbox_seed = .true.
    !$omp threadprivate(tbox_seed)

    public :: uniformPDF, uniformCDF, uniformCDF_Inv
    public :: normalPDF, normalCDF, normalCDF_Inv
    public :: log_normalPDF, log_normalCDF, log_normalCDF_Inv
    public :: GammaPDF, GammaCDF
    public :: betaPDF, betaCDF
    public :: bernoulliPDF, bernoulliCDF
    public :: binomialPDF, binomialCDF
    public :: binomial_coefficient

    public :: init_random_seed
    public :: simulate_uniform, simulate_uniform_1, simulate_uniform_n
    public :: simulate_normal, simulate_normal_1, simulate_normal_n
    public :: simulate_log_normal, simulate_log_normal_1, simulate_log_normal_n
    public :: simulate_Gamma, simulate_Gamma_1, simulate_Gamma_n
    public :: simulate_beta, simulate_beta_1, simulate_beta_n
    public :: simulate_bernoulli, simulate_bernoulli_1, simulate_bernoulli_n
    public :: simulate_binomial, simulate_binomial_1, simulate_binomial_n
    public :: normal_discrete, normal_discrete_1, normal_discrete_2
    public :: log_normal_discrete, log_normal_discrete_1, log_normal_discrete_2

!##############################################################################
! INTERFACE normal_discrete
!
! Discretizes normal distribution.
!##############################################################################
interface normal_discrete

    module procedure normal_discrete_1, normal_discrete_2

end interface


!##############################################################################
! INTERFACE log_normal_discrete
!
! Discretizes log-normal distribution.
!##############################################################################
interface log_normal_discrete

    module procedure log_normal_discrete_1, log_normal_discrete_2

end interface


!##############################################################################
! INTERFACE simulate_uniform
!
! Simulates uniformly distributed random variables.
!##############################################################################
interface simulate_uniform

    module procedure simulate_uniform_1, simulate_uniform_n

end interface


!##############################################################################
! INTERFACE simulate_normal
!
! Simulates normallly distributed random variables.
!##############################################################################
interface simulate_normal

    module procedure simulate_normal_1, simulate_normal_n

end interface


!##############################################################################
! INTERFACE simulate_log-normal
!
! Simulates log-normallly distributed random variables.
!##############################################################################
interface simulate_log_normal

    module procedure simulate_log_normal_1, simulate_log_normal_n

end interface


!##############################################################################
! INTERFACE simulate_Gamma
!
! Simulates Gamma distributed random variables.
!##############################################################################
interface simulate_Gamma

    module procedure simulate_Gamma_1, simulate_Gamma_n

end interface


!##############################################################################
! INTERFACE simulate_beta
!
! Simulates beta distributed random variables.
!##############################################################################
interface simulate_beta

    module procedure simulate_beta_1, simulate_beta_n

end interface


!##############################################################################
! INTERFACE simulate_bernoulli
!
! Simulates bernoulli distributed random variables.
!##############################################################################
interface simulate_bernoulli

    module procedure simulate_bernoulli_1, simulate_bernoulli_n

end interface


!##############################################################################
! INTERFACE simulate_binomial
!
! Simulates binomial distributed random variables.
!##############################################################################
interface simulate_binomial

    module procedure simulate_binomial_1, simulate_binomial_n

end interface




contains

    ! FUNCTION uniformPDF
    !
    ! Calculates uniform density at point x.
    !##############################################################################
    pure function uniformPDF(x, a, b)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! left end of the distribution
        real(dp), intent(in), optional :: a
     
        ! right end of the distribution
        real(dp), intent(in), optional :: b
        
        ! value of the uniform density
        real(dp) :: uniformPDF
        
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: a_c, b_c
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        a_c = 0.0_dp
        if(present(a))a_c = a
        b_c = 1.0_dp
        if(present(b))b_c = b
        
        if(b_c < a_c) error stop 'ERROR: uniformPDF: b is smaller than a'
     
        if(x < a_c .or. x > b_c)then
            uniformPDF = 0.0_dp
        else
            uniformPDF = 1.0_dp/(b_c-a_c)
        endif
     
    end function uniformPDF
    
    
    !##############################################################################
    ! FUNCTION uniformCDF
    !
    ! Calculates cumulated uniform distribution at point x.
    !##############################################################################
    pure function uniformCDF(x, a, b)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! left end of the distribution
        real(dp), intent(in), optional :: a
     
        ! right end of the distribution
        real(dp), intent(in), optional :: b
        
        ! value of the uniform distribution function
        real(dp) :: uniformCDF
        
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: a_c, b_c
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        a_c = 0.0_dp
        if(present(a))a_c = a
        b_c = 1.0_dp
        if(present(b))b_c = b
        
        if(b_c < a_c) error stop 'ERROR: uniformCDF: b is smaller than a'
     
        if(x <= a_c)then
            uniformCDF = 0.0_dp
        elseif(x >= b_c)then
            uniformCDF = 1.0_dp
        else
            uniformCDF = (x-a_c)/(b_c-a_c)
        endif
     
    end function uniformCDF
    
    
    !##############################################################################
    ! FUNCTION uniformCDF_Inv
    !
    ! Calculates inverse cumulated uniform distribution at point x.
    !##############################################################################
    pure function uniformCDF_Inv(x, a, b)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! left end of the distribution
        real(dp), intent(in), optional :: a
     
        ! right end of the distribution
        real(dp), intent(in), optional :: b
        
        ! value of the uniform distribution function
        real(dp) :: uniformCDF_Inv
        
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: a_c, b_c
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        a_c = 0.0_dp
        if(present(a))a_c = a
        b_c = 1.0_dp
        if(present(b))b_c = b
        
        if(b_c < a_c) error stop 'ERROR: uniformCDF_Inv: b is smaller than a'
     
        if(x <= 0.0_dp)then
            uniformCDF_Inv = a_c
        elseif(x >= 1.0_dp)then
            uniformCDF_Inv = b_c
        else
            uniformCDF_Inv = a_c + x*(b_c-a_c)
        endif
     
    end function uniformCDF_Inv


    !##############################################################################
    ! FUNCTION normalPDF
    !
    ! Calculates normal density functions at point x.
    !##############################################################################
    pure function normalPDF(x, mu, sigma)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! expectation of distribution
        real(dp), intent(in), optional :: mu
     
        ! variance of distribution
        real(dp), intent(in), optional :: sigma
     
        ! value of normal density at p
        real(dp) :: normalPDF
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c, sigma_c
        real(dp), parameter :: pi = 3.1415926535897_dp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = 0.0_dp
        if(present(mu))mu_c = mu
        sigma_c = 1.0_dp
        if(present(sigma))sigma_c = sqrt(sigma)
        
        if(sigma_c <= 0.0_dp) error stop 'ERROR: normalPDF: sigma has zero or negative value'
     
        normalPDF = 1.0_dp/(sigma_c*sqrt(2.0_dp*pi))*exp(-((x-mu_c)/sigma_c)**2/2.0_dp)
     
    end function normalPDF
    
 
    !##############################################################################
    ! FUNCTION normalCDF
    !
    ! Calculates cumulated normal distribution at point x.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Fortran Code by John Burkardt available as Algorithm ASA066 from 
    !     https://people.sc.fsu.edu/~jburkardt/f_src/asa066/asa066.html
    !
    !     REFERENCE: Hill, D. (1973). Algorithm AS 66: The Normal Integral.
    !                Applied Statistics, 22(3), 424-427.
    !##############################################################################
    pure function normalCDF(x, mu, sigma)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! expectation of distribution
        real(dp), intent(in), optional :: mu
     
        ! variance of distribution
        real(dp), intent(in), optional :: sigma
     
        ! value of the normal distribution at x
        real(dp) :: normalCDF
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c, sigma_c, xtrans, xabs, y
        real(dp), parameter :: a0  = 0.5_dp
        real(dp), parameter :: a1  = 0.398942280444_dp
        real(dp), parameter :: a2  = 0.399903438504_dp
        real(dp), parameter :: a3  = 5.75885480458_dp
        real(dp), parameter :: a4  = 29.8213557808_dp
        real(dp), parameter :: a5  = 2.62433121679_dp
        real(dp), parameter :: a6  = 48.6959930692_dp
        real(dp), parameter :: a7  = 5.92885724438_dp
        real(dp), parameter :: b0  = 0.398942280385_dp
        real(dp), parameter :: b1  = 3.8052e-8_dp
        real(dp), parameter :: b2  = 1.00000615302_dp
        real(dp), parameter :: b3  = 3.98064794e-4_dp
        real(dp), parameter :: b4  = 1.98615381364_dp
        real(dp), parameter :: b5  = 0.151679116635_dp
        real(dp), parameter :: b6  = 5.29330324926_dp
        real(dp), parameter :: b7  = 4.8385912808_dp
        real(dp), parameter :: b8  = 15.1508972451_dp
        real(dp), parameter :: b9  = 0.742380924027_dp
        real(dp), parameter :: b10 = 30.789933034_dp
        real(dp), parameter :: b11 = 3.99019417011_dp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = 0.0_dp
        if(present(mu))mu_c = mu
        sigma_c = 1.0_dp
        if(present(sigma))then
            if(sigma_c > 0.0_dp)sigma_c = sqrt(sigma)
        endif
        
        if(sigma_c <= 0.0_dp) error stop 'ERROR: normalCDF: sigma has zero or negative value'
     
        ! standardize evaluation point
        xtrans = (x - mu_c)/sigma_c
        
        ! calculate absolute value and quadratic
        xabs = abs(xtrans)
        y = a0*xtrans**2
        
        ! choose the right interval for calculation
        if(xabs <= 1.28_dp)then
            normalCDF = a0-xabs*(a1-a2*y/(y+a3-a4/(y+a5+a6/(y+a7))))
        elseif(xabs <= 12.7_dp)then
            normalCDF = b0*exp(-y)/(xabs-b1+b2/(xabs+b3+b4/(xabs-b5+b6/(xabs+b7-b8/ &
                    (xabs+b9+b10/(xabs+b11))))))
        else
            normalCDF = 0.0_dp
        endif
        
        ! transform if other side of the bell
        if(xtrans > 0.0_dp)normalCDF = 1.0_dp-normalCDF
     
    end function normalCDF
     
     
    !##############################################################################
    ! FUNCTION normalCDF_Inv
    !
    ! Calculates inverse cumulated normal distribution at point x.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Fortran Code by John Burkardt available as Algorithm ASA241 from 
    !     https://people.sc.fsu.edu/~jburkardt/f_src/asa241/asa241.html
    !
    !     REFERENCE: Wichura, M. (1988). Algorithm AS 241: The Percentage Points 
    !                of the Normal Distribution. Applied Statistics, 37(3),
    !                477-484.
    !##############################################################################
    pure function normalCDF_Inv(x, mu, sigma)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! expectation of distribution
        real(dp), intent(in), optional :: mu
     
        ! variance of distribution
        real(dp), intent(in), optional :: sigma
     
        ! value of the inverse of the normal distribution at x   
        real(dp) :: normalCDF_Inv
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c, sigma_c, q, r
        real(dp), parameter :: a(8) = (/  3.3871328727963666080_dp, &
                                        1.3314166789178437745e2_dp, &
                                        1.9715909503065514427e3_dp, &
                                        1.3731693765509461125e4_dp, &
                                        4.5921953931549871457e4_dp, &
                                        6.7265770927008700853e4_dp, &
                                        3.3430575583588128105e4_dp, &
                                        2.5090809287301226727e3_dp /)
        real(dp), parameter :: b(8) = (/  1.0_dp, &
                                        4.2313330701600911252e1_dp, &
                                        6.8718700749205790830e2_dp, &
                                        5.3941960214247511077e3_dp, &
                                        2.1213794301586595867e4_dp, &
                                        3.9307895800092710610e4_dp, &
                                        2.8729085735721942674e4_dp, &
                                        5.2264952788528545610e3_dp /)
        real(dp), parameter :: c(8) = (/  1.42343711074968357734_dp, &
                                        4.63033784615654529590_dp, &
                                        5.76949722146069140550_dp, &
                                        3.64784832476320460504_dp, &
                                        1.27045825245236838258_dp, &
                                        2.41780725177450611770e-1_dp, &
                                        2.27238449892691845833e-2_dp, &
                                        7.74545014278341407640e-4_dp /)
        real(dp), parameter :: const1 = 0.180625_dp
        real(dp), parameter :: const2 = 1.6_dp
        real(dp), parameter :: d(8) = (/  1.0_dp, &
                                        2.05319162663775882187_dp, &
                                        1.67638483018380384940_dp, &
                                        6.89767334985100004550e-1_dp, &
                                        1.48103976427480074590e-1_dp, &
                                        1.51986665636164571966e-2_dp, &
                                        5.47593808499534494600e-4_dp, &
                                        1.05075007164441684324e-9_dp /)
        real(dp), parameter :: e(8) = (/  6.65790464350110377720_dp, &
                                        5.46378491116411436990_dp, &
                                        1.78482653991729133580_dp, &
                                        2.96560571828504891230e-1_dp, &
                                        2.65321895265761230930e-2_dp, &
                                        1.24266094738807843860e-3_dp, &
                                        2.71155556874348757815e-5_dp, &
                                        2.01033439929228813265e-7_dp /)
        real(dp), parameter :: f(8) = (/   1.0_dp, &
                                        5.99832206555887937690e-1_dp, &
                                        1.36929880922735805310e-1_dp, &
                                        1.48753612908506148525e-2_dp, &
                                        7.86869131145613259100e-4_dp, &
                                        1.84631831751005468180e-5_dp, &
                                        1.42151175831644588870e-7_dp, &
                                        2.04426310338993978564e-15_dp /)

        real(dp), parameter :: split1 = 0.425_dp
        real(dp), parameter :: split2 = 5.0_dp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = 0.0_dp
        if(present(mu))mu_c = mu
        sigma_c = 1.0_dp
        if(present(sigma))then
            if(sigma_c > 0.0_dp)sigma_c = sqrt(sigma)
        endif
        
        if(sigma_c <= 0.0_dp) error stop 'ERROR: normalCDF_Inv: sigma has zero or negative value'
     
        ! check for very small or large values
        if(x <= 0.0_dp)then
            normalcdf_Inv = - huge(x)
            return
        endif
        
        if (x >= 1.0_dp)then
            normalcdf_Inv = huge(x)
            return
        endif
        
        ! calculate for reasonable values
        q = x - 0.5_dp
        
        if(abs(q) <= split1)then
            r = const1 - q*q
            normalcdf_inv = q*r8poly_value(8, a, r)/r8poly_value(8, b, r)
        else
        
            if (q < 0.0_dp) then
                r = x
            else
                r = 1.0_dp - x
            endif
        
            r = sqrt(-log(r))
        
            if(r <= split2)then
                r = r - const2
                normalcdf_Inv = r8poly_value(8, c, r)/r8poly_value(8, d, r)
            else
                r = r - split2
                normalcdf_Inv = r8poly_value(8, e, r)/r8poly_value (8, f, r)
            endif
        
            if(q < 0.0_dp)then
                normalcdf_Inv = -normalcdf_inv
            endif
        endif   
     
        ! transfer to mu and sigma
        normalCDF_Inv = mu_c + sigma_c*normalCDF_Inv
     
     
    contains
        
        ! stable evaluation of a polynomial
        pure function r8poly_value(n, a, x)
        
            implicit none
            
            
            !##### INPUT/OUTPUT VARIABLES #############################################
    
            ! degree of polynomial
            integer(int32), intent(in) :: n
            
            ! polynomial coefficients
            real(dp), intent(in) :: a(n)
            
            ! point where to evaluate the polynomial
            real(dp), intent(in) :: x
            
            ! the value of the polynomial
            real(dp) :: r8poly_value
            
            
            !##### OTHER VARIABLES ####################################################
            
            integer(int32) :: i
            
            
            !##### ROUTINE CODE #######################################################
        
            r8poly_value = 0.0_dp
            do i = n, 1, -1
                r8poly_value = r8poly_value * x + a(i)
            enddo
        
        end function r8poly_value
     
    end function normalCDF_Inv
    
    
    !##############################################################################
    ! FUNCTION log_normalPDF
    !
    ! Calculates log-normal density functions at point x.
    !##############################################################################
    pure function log_normalPDF(x, mu, sigma)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! expectation of distribution
        real(dp), intent(in), optional :: mu
     
        ! variance of distribution
        real(dp), intent(in), optional :: sigma
     
        ! value of normal density at p
        real(dp) :: log_normalPDF
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c, sigma_c
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = exp(0.5_dp)
        if(present(mu))mu_c = mu
        sigma_c = exp(1.0_dp)*(exp(1.0_dp)-1.0_dp)
        if(present(sigma))sigma_c = sigma
        
        if(sigma_c <= 0.0_dp) error stop 'ERROR: log_normalPDF: sigma has zero or negative value'
        if(mu_c <= 0.0_dp) error stop 'ERROR: log_normalPDF: mu has zero or negative value'
        if(x < 0.0_dp) error stop 'ERROR: log_normalPDF: x has negative value'
     
        ! get expectation and variance
        sigma_c = log(1.0_dp+sigma_c/mu_c**2)
        mu_c  = log(mu_c)-0.5_dp*sigma_c
     
        ! simulate normal and convert to log_normal
        if(x > 0.0_dp)then
            log_normalPDF = normalPDF(log(x), mu_c, sigma_c)/x
        else
            log_normalPDF = 0.0_dp
        endif
     
    end function log_normalPDF
    
    
    !##############################################################################
    ! FUNCTION log_normalCDF
    !
    ! Calculates log-normal distribution at point x.
    !##############################################################################
    pure function log_normalCDF(x, mu, sigma)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! expectation of distribution
        real(dp), intent(in), optional :: mu
     
        ! variance of distribution
        real(dp), intent(in), optional :: sigma
     
        ! value of normal density at p
        real(dp) :: log_normalCDF
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c, sigma_c
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = exp(0.5_dp)
        if(present(mu))mu_c = mu
        sigma_c = exp(1.0_dp)*(exp(1.0_dp)-1.0_dp)
        if(present(sigma))sigma_c = sigma
        
        if(sigma_c <= 0.0_dp) error stop 'ERROR: log_normalCDF: sigma has zero or negative value'
        if(mu_c <= 0.0_dp) error stop 'ERROR: log_normalCDF: mu has zero or negative value'
        if(x < 0.0_dp) error stop 'ERROR: log_normalCDF: x has negative value'
     
        ! get expectation and variance
        sigma_c = log(1.0_dp+sigma_c/mu_c**2)
        mu_c  = log(mu_c)-0.5_dp*sigma_c
     
        ! simulate normal and convert to log_normal
        if(x > 0.0_dp)then
            log_normalCDF = normalCDF(log(x), mu_c, sigma_c)
        else
            log_normalCDF = 0.0_dp
        endif
     
    end function log_normalCDF
    
    
    !##############################################################################
    ! FUNCTION log_normalCDF_Inv
    !
    ! Calculates cumulated log-normal distribution at point x.
    !##############################################################################
    pure function log_normalCDF_Inv(x, mu, sigma)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! expectation of distribution
        real(dp), intent(in), optional :: mu
     
        ! variance of distribution
        real(dp), intent(in), optional :: sigma
     
        ! value of normal density at p
        real(dp) :: log_normalCDF_Inv
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c, sigma_c
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = exp(0.5_dp)
        if(present(mu))mu_c = mu
        sigma_c = exp(1.0_dp)*(exp(1.0_dp)-1.0_dp)
        if(present(sigma))sigma_c = sigma
        
        if(sigma_c <= 0.0_dp) error stop 'ERROR: log_normalCDF_Inv: sigma has zero or negative value'
        if(mu_c <= 0.0_dp) error stop 'ERROR: log_normalCDF_Inv: mu has zero or negative value'
        
        ! get expectation and variance
        sigma_c = log(1.0_dp+sigma_c/mu_c**2)
        mu_c  = log(mu_c)-0.5_dp*sigma_c
     
        ! simulate normal and convert to log_normal
        log_normalCDF_Inv = normalCDF_Inv(x, mu_c, sigma_c)
        log_normalCDF_Inv = exp(log_normalCDF_Inv)
        
    end function log_normalCDF_Inv
    
    
    !##############################################################################
    ! FUNCTION GammaPDF
    !
    ! Calculates Gamma density functions at point x.
    !##############################################################################
    function GammaPDF(x, alpha, beta)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! shape parameter of the distribution
        real(dp), optional :: alpha
     
        ! scale parameter of the distribution
        real(dp), optional :: beta
     
        ! value of Gamma density at p
        real(dp) :: gammaPDF
        
        
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: alpha_c, beta_c
     
          
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        alpha_c = 1.0_dp
        if(present(alpha))alpha_c = alpha
        beta_c = 1.0_dp
        if(present(beta))beta_c = beta
     
        ! check for validity of parameters
        if(alpha_c <= 0.0_dp)then
            call error('GammaPDF','alpha has a non-positive value')
        endif
        if(beta_c <= 0.0_dp)then
            call error('GammaPDF','beta has a non-positive value')
        endif
     
        if(x < 0.0_dp .or. alpha_c < 1.0_dp .and. x <= 0.0_dp)then
            GammaPDF = 0.0_dp
        else
            GammaPDF = beta_c**alpha_c/gamma_function(alpha_c)* &
                        x**(alpha_c-1.0_dp)*exp(-beta_c*x)
        endif
     
    end function GammaPDF
    
    
    !##############################################################################
    ! FUNCTION GammaCDF
    !
    ! Calculates cumulated Gamma distribution at point x.
    !##############################################################################
    function GammaCDF(x, alpha, beta)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! shape parameter of the distribution
        real(dp), optional :: alpha
     
        ! scale parameter of the distribution
        real(dp), optional :: beta
     
        ! value of cumulated Gamma distribution at p
        real(dp) :: gammaCDF
        
        
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: alpha_c, beta_c
     
          
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        alpha_c = 1.0_dp
        if(present(alpha))alpha_c = alpha
        beta_c = 1.0_dp
        if(present(beta))beta_c = beta
     
        ! check for validity of parameters
        if(alpha_c <= 0.0_dp)then
            call error('GammaCDF','alpha has a non-positive value')
        endif
        if(beta_c <= 0.0_dp)then
            call error('GammaCDF','beta has a non-positive value')
        endif
     
        if(x <= 0.0_dp)then
            GammaCDF = 0.0_dp
        else
            GammaCDF = incomplete_gamma(beta_c*x, alpha_c)
        endif
     
    end function gammaCDF
    
    
    
    !##############################################################################
    ! FUNCTION betaPDF
    !
    ! Calculates beta density functions at point x.
    !##############################################################################
    function betaPDF(x, p, q)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! parameter of the distribution
        real(dp), optional :: p
     
        ! parameter of the distribution
        real(dp), optional :: q
     
        ! value of Gamma density at p
        real(dp) :: betaPDF
        
        
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: p_c, q_c, betanorm
     
          
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        p_c = 1.0_dp
        if(present(p))p_c = p
        q_c = 1.0_dp
        if(present(q))q_c = q
     
        ! check for validity of parameters
        if(p_c <= 0.0_dp)then
            call error('betaPDF','p has a non-positive value')
        endif
        if(q_c <= 0.0_dp)then
            call error('betaPDF','q has a non-positive value')
        endif
     
        if(x < 0.0_dp .or. x > 1.0_dp)then
            betaPDF = 0.0_dp
        else
            betanorm = exp(my_log_gamma(p_c) + my_log_gamma(q_c) - my_log_gamma(p_c+q_c))
            betaPDF = x**(p_c-1.0_dp)*(1.0_dp-x)**(q_c-1.0_dp)/betanorm
        endif
     
    end function betaPDF
    
    
    !##############################################################################
    ! FUNCTION betaCDF
    !
    ! Calculates cumulated beta distribution at point x.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Fortran Code by John Burkardt available as Algorithm ASA063 from 
    !     https://people.sc.fsu.edu/~jburkardt/f_src/asa063/asa063.html
    !
    !     REFERENCE: Majumder, K.L. & Bhattacharjee, G.P. (1973). Algorithm AS 63:
    !                The incomplete Beta Integral, Applied Statistics, 22(3), 
    !                409-411.
    !##############################################################################
    function betaCDF(x, p_in, q_in)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! parameter of the distribution
        real(dp), optional :: p_in
     
        ! parameter of the distribution
        real(dp), optional :: q_in
     
        ! value of Gamma density at p
        real(dp) :: betaCDF
        
        
        !##### OTHER VARIABLES ####################################################
     
        real(dp), parameter :: acu = 0.1e-14_dp
        real(dp) :: p, q, ai, beta_log, cx, pp, psq, qq, rx, temp, term, xx
        integer(int32) :: ns
        logical :: indx
     
          
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        p = 1.0_dp
        if(present(p_in))p = p_in
        q = 1.0_dp
        if(present(q_in))q = q_in
     
        ! check for validity of parameters
        if(p <= 0.0_dp)then
            call error('betaPDF','p has a non-positive value')
        endif
        if(q <= 0.0_dp)then
            call error('betaPDF','q has a non-positive value')
        endif
     
        ! check outside of range
        if(x <= 0.0_dp)then
            betaCDF = 0.0_dp
            return
        endif
        if(x >= 1.0_dp)then
            betaCDF = 1.0_dp
            return
        endif
        
        ! calculate logarithm of the complete beta function
        beta_log = my_log_gamma(p) + my_log_gamma(q) - my_log_gamma(p + q)

        psq = p + q
        cx = 1.0_dp - x

        if(p < psq*x)then
            xx = cx
            cx = x
            pp = q
            qq = p
            indx = .true.
        else
            xx = x
            pp = p
            qq = q
            indx = .false.
        endif

        term = 1.0_dp
        ai = 1.0_dp
        betaCDF = 1.0_dp
        ns = int(qq + cx*psq)

        ! use Soper's reduction formula.
        rx = xx / cx
        temp = qq - ai
        if (ns == 0) then
            rx = xx
        endif

        do

            term = term*temp*rx / (pp + ai)
            betaCDF = betaCDF + term
            temp = abs (term)
        
            if(temp <= acu .and. temp <= acu*betaCDF) then
        
                betaCDF = betaCDF*exp (pp*log(xx) &
                    + (qq-1.0_dp)*log(cx)-beta_log)/pp
        
                ! change if other tail
                if(indx)then
                    betaCDF = 1.0_dp - betaCDF
                endif
                exit       
            endif
        
            ai = ai + 1.0_dp
            ns = ns - 1
        
            if(0 <= ns)then
                temp = qq - ai
                if(ns == 0)then
                    rx = xx
                endif
            else
                temp = psq
                psq = psq + 1.0_dp
            endif
        enddo
     
    end function betaCDF
    
    
    !##############################################################################
    ! FUNCTION incomplete_gamma
    !
    ! Calculates the incomplete gamma integral.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Fortran Code by John Burkardt available as Algorithm ASA239 from 
    !     https://people.sc.fsu.edu/~jburkardt/f_src/asa239/asa239.html
    !
    !     REFERENCE: Shea, B. (1988). Algorithm AS 239: Chi-squared and Incomplete 
    !                Gamma Integral, Applied Statistics, 37(3), 466-473.
    !##############################################################################
    function incomplete_gamma(x, p)

        implicit none
        
        
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! parameter of the gamma function
        real(dp), intent(in) :: p
     
        ! value of normal density at p
        real(dp) :: incomplete_gamma
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: a, an, arg, b, c, pn1, pn2, pn3, pn4, pn5, pn6, rn
        real(dp), parameter :: elimit = - 88.0_dp
        real(dp), parameter :: oflo = 1.0e37_dp
        real(dp), parameter :: plimit = 1000.0_dp
        real(dp), parameter :: tol = 1.0e-14_dp
        real(dp), parameter :: xbig = 1.0e8_dp
     
     
        !##### ROUTINE CODE #######################################################

        incomplete_gamma = 0.0_dp
        
        ! check validity of inputs solution
        if(x < 0.0_dp)then
            call error('incomeplete_gamma','x is smaller than zero')
        endif
        
        if(p <= 0.0_dp)then
            call error('incomeplete_gamma','p is not positive')
        endif
        
        ! set incomeplete_gamma = 0 if x is zero
        if(x <= 0.0_dp)then
            incomplete_gamma = 0.0_dp
            return
        endif

        ! normal approximation for large values of p
        if(p > plimit)then
            pn1 = 3.0_dp*sqrt(p)*((x/p)**(1.0_dp/3.0_dp) + 1.0_dp/(9.0_dp*p) - 1.0_dp)
            incomplete_gamma = normalCDF(pn1)
            return
        endif
        
        ! set incomeplete_gamma = 1 if x is large
        if(x > xbig)then
            incomplete_gamma = 1.0_dp
        endif
        
        ! Pearson series expansion
        if(x <= 1.0_dp .or. x < p)then
            arg = p*log(x) - x - my_log_gamma(p+1.0_dp)
            c = 1.0_dp
            incomplete_gamma = 1.0_dp
            a = p
        
            do
                a = a + 1.0_dp
                c = c*x/a
                incomplete_gamma = incomplete_gamma + c
        
                if(c <= tol) then
                    exit
                endif
        
            enddo
        
            arg = arg + log (incomplete_gamma)
        
            if ( elimit <= arg ) then
              incomplete_gamma = exp ( arg )
            else
              incomplete_gamma = 0.0_dp
            endif
            
        ! Use a continued fraction expansion.
        else
        
            arg = p*log (x) - x - my_log_gamma(p)
            a = 1.0_dp - p
            b = a + x + 1.0_dp
            c = 0.0_dp
            pn1 = 1.0_dp
            pn2 = x
            pn3 = x + 1.0_dp
            pn4 = x*b
            incomplete_gamma = pn3/pn4
        
            do
        
                a = a + 1.0_dp
                b = b + 2.0_dp
                c = c + 1.0_dp
                an = a*c
                pn5 = b*pn3 - an*pn1
                pn6 = b*pn4 - an*pn2
        
                if ( abs(pn6) > 0.0_dp ) then
        
                    rn = pn5 / pn6
        
                    if ( abs(incomplete_gamma - rn) <= min(tol, tol*rn)) then
                        exit
                    endif
                    incomplete_gamma = rn
                endif
        
                pn1 = pn3
                pn2 = pn4
                pn3 = pn5
                pn4 = pn6
                
                !  Re-scale terms in continued fraction if terms are large.
                if (oflo <= abs(pn5) ) then
                    pn1 = pn1/oflo
                    pn2 = pn2/oflo
                    pn3 = pn3/oflo
                    pn4 = pn4/oflo
                endif
        
            enddo
        
            arg = arg + log (incomplete_gamma)
        
            if (arg >= elimit) then
              incomplete_gamma = 1.0_dp - exp (arg)
            else
              incomplete_gamma = 1.0_dp
            endif
        endif
    
    end function incomplete_gamma
    
    
    !##############################################################################
    ! FUNCTION gamma_function
    !
    ! Calculates the gamma fuction.
    !##############################################################################
    function gamma_function(x)

        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x
     
        ! value of log of the gamma function
        real(dp) :: gamma_function
             
     
        !##### ROUTINE CODE #######################################################

        
        ! check validity of inputs solution
        if(x < 0.0_dp)then
            call error('gamma_function','x is smaller than zero')
        endif
        
        gamma_function = my_log_gamma(x)
        gamma_function = exp(gamma_function)
        
    end function gamma_function
    
    
    !##############################################################################
    ! FUNCTION my_log_gamma
    !
    ! Calculates log of the gamma fuction.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Fortran Code by John Burkardt available as Algorithm AS245 from 
    !     https://people.sc.fsu.edu/~jburkardt/f_src/asa245/asa245.html
    !
    !     REFERENCE: Macleod, A.J. (1989). Algorithm AS 245: A Robust and Reliable 
    !                Algorithm for the  Logarithm of the Gamma Function. Applied 
    !                Statistics, 38(2), 397-402.
    !##############################################################################
    function my_log_gamma(x_in)

        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point where to calculate function
        real(dp), intent(in) :: x_in
     
        ! value of log of the gamma function
        real(dp) :: my_log_gamma
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp), parameter :: alr2pi = 9.18938533204673e-1_dp
        real(dp), parameter :: xlge = 5.10e6_dp 
        real(dp), parameter :: r1(9) = (/ -2.66685511495_dp, &
                                        -2.44387534237e1_dp, &
                                        -2.19698958928e1_dp, & 
                                         1.11667541262e1_dp, &
                                         3.13060547623_dp, &
                                         6.07771387771e-1_dp, &
                                         1.19400905721e1_dp, &
                                         3.14690115749e1_dp, &
                                         1.52346874070e1_dp /)
        real(dp), parameter :: r2(9) = (/ -7.83359299449e1_dp, &
                                        -1.42046296688e2_dp, &
                                         1.37519416416e2_dp, & 
                                         7.86994924154e1_dp, &
                                         4.16438922228_dp, &
                                         4.70668766060e1_dp, &
                                         3.13399215894e2_dp, & 
                                         2.63505074721e2_dp, &
                                         4.33400022514e1_dp /)
        real(dp), parameter :: r3(9) = (/ -2.12159572323e5_dp, & 
                                         2.30661510616e5_dp, &
                                         2.74647644705e4_dp, &
                                        -4.02621119975e4_dp, &
                                        -2.29660729780e3_dp, &
                                        -1.16328495004e5_dp, &
                                        -1.46025937511e5_dp, &
                                        -2.42357409629e4_dp, &
                                        -5.70691009324e2_dp /)
        real(dp), parameter :: r4(5) = (/  2.79195317918525e-1_dp, &
                                         4.917317610505968e-1_dp, &
                                         6.92910599291889e-2_dp, &
                                         3.350343815022304_dp, &
                                         6.012459259764103_dp /)
        real(dp) :: x, x1, x2, y
     
     
        !##### ROUTINE CODE #######################################################


        my_log_gamma = 0.0_dp
        x = x_in
        
        ! check validity of inputs solution
        if(x < 0.0_dp)then
            call error('my_log_gamma','x is smaller than zero')
        endif
        
        ! get solution for 0 < X < 0.5 and 0.5 <= x < 1.5
        if(x < 1.5_dp)then
            if(x < 0.5_dp)then
                my_log_gamma = -log(x)
                y = x + 1.0_dp
                
                ! return if x is smaller than machine epsilon
                if(y <= 1.0_dp)return
            else
                my_log_gamma = 0.0_dp
                y = x
                x = (x-0.5_dp) - 0.5_dp
            endif
            my_log_gamma = my_log_gamma + x * ((((r1(5)*y + r1(4))*y + r1(3))*y &
                + r1(2))*y + r1(1)) / ((((y + r1(9))*y + r1(8))*y + r1(7))*y + r1(6))
        

        ! get solution for 1.5 <= x < 4.0
        elseif(x < 4.0_dp)then
            y = (x - 1.0_dp) - 1.0_dp
            my_log_gamma = y * ((((r2(5)*x + r2(4))*x + r2(3))*x + r2(2))*x &
                + r2(1)) / ((((x + r2(9))*x + r2(8))*x + r2(7))*x + R2(6))

        ! get solution for 4.0 <= x < 12
        elseif(x < 12.0_dp)then
            my_log_gamma = ((((r3(5)*x + r3(4))*x + r3(3))*x + r3(2))*x + r3(1)) / &
                ((((x + r3(9))*x + r3(8))*x + r3(7))*x + r3(6))
        else
            y = log(x)
            my_log_gamma = x * (y -1.0_dp) - 0.5_dp*y + alr2pi
            if(x <= XLGE)then
                x1 = 1.0_dp/x
                x2 = x1 * x1
                my_log_gamma = my_log_gamma + x1 * ((r4(3)*x2 + r4(2))*x2 + r4(1)) / &
                    ((x2 + r4(5))*x2 + r4(4))
            endif
        endif
        
    end function my_log_gamma
    
    
    !##############################################################################
    ! FUNCTION bernoulliPDF
    !
    ! Calculates probabilities of the bernoulli distribution.
    !##############################################################################
    elemental function bernoulliPDF(k, p)

        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! zero or 1 draw
        integer(int32), intent(in) :: k
     
        ! the probability of the positive draw
        real(dp), intent(in) :: p

        ! the probability of this to happen
        real(dp) :: bernoulliPDF
        
             
        !##### ROUTINE CODE #######################################################
        
        ! check for validity
        if(p < 0.0_dp .or. p > 1.0_dp) error stop 'ERROR: bernoulliPDF: Probability p is out of [0,1]'
        
        ! outside of range
        if(k < 0)then
            bernoulliPDF = 0.0_dp
            return
        endif
        
        if(k > 1)then
            bernoulliPDF = 1.0_dp
            return
        endif
        
        if(k == 0)then
            bernoulliPDF = 1.0_dp - p
        else
            bernoulliPDF = p
        endif

    end function bernoulliPDF
    
    
    !##############################################################################
    ! FUNCTION bernoulliCDF
    !
    ! Calculates cumulated probabilities of the bernoulli distribution.
    !##############################################################################
    elemental function bernoulliCDF(k, p)

        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! zero or 1 draw
        integer(int32), intent(in) :: k
     
        ! the probability of the positive draw
        real(dp), intent(in) :: p

        ! the cumulated probability of this to happen
        real(dp) :: bernoulliCDF
        
             
        !##### ROUTINE CODE #######################################################
        
        ! check for validity
        if(p < 0.0_dp .or. p > 1.0_dp) error stop 'ERROR: bernoulliCDF: Probability p is out of [0,1]'
        
        ! outside of range
        if(k < 0)then
            bernoulliCDF = 0.0_dp
            return
        endif
        
        if(k >= 1)then
            bernoulliCDF = 1.0_dp
            return
        endif
        
        bernoulliCDF = 1.0_dp - p

    end function bernoulliCDF
    
    
    !##############################################################################
    ! FUNCTION binomialPDF
    !
    ! Calculates probabilities of the binomial distribution.
    !##############################################################################
    elemental function binomialPDF(k, n, p)

        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! the number of positive draws
        integer(int32), intent(in) :: k
     
        ! the total number of draws
        integer(int32), intent(in) :: n
     
        ! the probability of a positive draw
        real(dp), intent(in) :: p

        ! the probability of this to happen
        real(dp) :: binomialPDF
     
     
        !##### ROUTINE CODE #######################################################
        

        ! check for validity
        if(p < 0.0_dp .or. p > 1.0_dp) error stop 'ERROR: binomialPDF: Probability p is out of [0,1]'
        
        ! outside of range
        if(k < 0 .or. k > n)then
            binomialPDF = 0.0_dp
            return
        endif

        binomialPDF = binomial_coefficient(n, k)*p**k*(1.0_dp-p)**(n-k)        

    end function binomialPDF
    
    
    !##############################################################################
    ! FUNCTION binomialCDF
    !
    ! Calculates cumulated probabilities of the binomial distribution.
    !##############################################################################
    elemental function binomialCDF(k, n, p)

        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! the number of positive draws
        integer(int32), intent(in) :: k
     
        ! the total number of draws
        integer(int32), intent(in) :: n
     
        ! the probability of a positive draw
        real(dp), intent(in) :: p

        ! the cumulated probability of this to happen
        real(dp) :: binomialCDF
        
        
        !##### OTHER VARIABLES ####################################################
        
        integer(int32) :: ii
     
     
        !##### ROUTINE CODE #######################################################
        
        ! check for validity
        if(p < 0.0_dp .or. p > 1.0_dp) error stop 'ERROR: binomialCDF: Probability p is out of [0,1]'
        
        ! outside of range
        if(k < 0)then
            binomialCDF = 0.0_dp
            return
        endif
        
        if(k > n)then
            binomialCDF = 1.0_dp
            return
        endif
        
        binomialCDF = 0.0_dp
        do ii = 0, k
            binomialCDF = binomialCDF +  binomial_coefficient(n, ii)*p**ii*(1.0_dp-p)**(n-ii)
        enddo

    end function binomialCDF
    
    
    !##############################################################################
    ! FUNCTION binomial_coefficient
    !
    ! Calculate binomial coefficients efficiently
    !##############################################################################
    elemental function binomial_coefficient(n, k)
    
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! out of number
        integer(int32), intent(in) :: n
     
        ! number
        integer(int32), intent(in) :: k
     
        ! the probability of this to happen
        real(dp) :: binomial_coefficient
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: n0, k0, ii
     
     
        !##### ROUTINE CODE #######################################################
        
        
        if(k == 0 .or. k == n)then
            binomial_coefficient = 1.0_dp
        endif
        
        ! copy values
        n0 = n
        k0 = k
        
        ! use symmetry of binomial coefficients
        if(k0 > n0 - k0)then
            k0 = n0 - k0
        endif
        
        ! use factorial formula for stability
        binomial_coefficient = 1.0_dp
        do ii = 1, k
            binomial_coefficient = binomial_coefficient*real(n+1-ii, dp)
            binomial_coefficient = binomial_coefficient/real(ii, dp)
        enddo
    
    end function binomial_coefficient
    
     
     
    !##############################################################################
    ! SUBROUTINE init_random_seed
    !
    ! To ensure that each random draw is a new sequence
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !    The GNU GCC Fortran compiler library documentation
    !    https://gcc.gnu.org/onlinedocs/gcc-4.8.5/gfortran/RANDOM_005fSEED.html
    !##############################################################################
    subroutine init_random_seed(fixed)
     
        !$ use omp_lib, only: omp_get_thread_num
        implicit none
        integer(int32), allocatable :: seed(:)
        integer(int32) :: i, n, dt(8)
        integer(kind=int64) :: t
        logical, optional :: fixed
        !$ integer(int32) :: tid
     
        call random_seed(size = n)
        allocate(seed(n))
     
        call system_clock(t)
        if (t == 0) then
            call date_and_time(values=dt)
            t = (dt(1) - 1970) * 365_int64 * 24 * 60 * 60 * 1000 &
                + dt(2) * 31_int64 * 24 * 60 * 60 * 1000 &
                + dt(3) * 24_int64 * 60 * 60 * 1000 &
                + dt(5) * 60 * 60 * 1000 &
                + dt(6) * 60 * 1000 + dt(7) * 1000 &
                + dt(8)
        endif
        
        if(present(fixed))then
            if(fixed)t = 0
        endif
        !$ if (.not. (present(fixed) .and. (fixed .eqv. .true.))) then
        !$     tid = omp_get_thread_num()
        !$     t = t + int(tid, int64) * 104729_int64
        !$ end if
        do i = 1, n
            seed(i) = lcg(t)
        enddo
        call random_seed(put=seed)
     
    contains
     
        ! This simple PRNG might not be good enough for real work, but is
        ! sufficient for seeding a better PRNG.
        function lcg(s)
     
            implicit none
            integer(int32) :: lcg
            integer(int64) :: s
     
            if (s == 0) then
                s = 104729
            else
                s = mod(s, 4294967296_int64)
            endif
            s = mod(s * 279470273_int64, 4294967291_int64)
            lcg = int(mod(s, int(huge(0), int64)), kind(0))
     
        end function lcg
     
    end subroutine init_random_seed
     
     
    !##############################################################################
    ! SUBROUTINE simulate_uniform_1
    !
    ! Simulates one draw from a uniform distribution.
    !##############################################################################
    subroutine simulate_uniform_1(x, a, b, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x
     
        ! left end of the distribution
        real(dp), optional :: a
     
        ! right end of the distribution
        real(dp), optional :: b
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: a_c, b_c
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        a_c = 0.0_dp
        if(present(a))a_c = a
        b_c = 1.0_dp
        if(present(b))b_c = b
     
        ! initialize the random seed
        if(tbox_seed)then
            if(present(fixed))then
                call init_random_seed(fixed)
            else
                call init_random_seed()
            endif
            tbox_seed = .false.
        endif
     
        ! draw the random number
        call random_number(x)
     
        x = a_c + (b_c-a_c)*x
     
    end subroutine simulate_uniform_1
     
     
    !##############################################################################
    ! SUBROUTINE simulate_uniform_n
    !
    ! Simulates a series draw from a uniform distribution.
    !##############################################################################
    subroutine simulate_uniform_n(x, a, b, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x(:)
     
        ! left end of the distribution
        real(dp), optional :: a
     
        ! right end of the distribution
        real(dp), optional :: b
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: a_c, b_c
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        a_c = 0.0_dp
        if(present(a))a_c = a
        b_c = 1.0_dp
        if(present(b))b_c = b
     
        ! initialize the random seed
        if(tbox_seed)then
            if(present(fixed))then
                call init_random_seed(fixed)
            else
                call init_random_seed()
            endif
            tbox_seed = .false.
        endif
     
        call random_number(x)
     
        x = a_c + (b_c-a_c)*x
     
    end subroutine simulate_uniform_n
     
     
    !##############################################################################
    ! Subroutine simulate_normal_1
    !
    ! Simulates one draw from a normal distribution using
    !     Box-Muller tranformation.
    !
    ! REFERENCE: Box, G.E.P., Muller, M.E. (1958). A Note on the Generation of  
    !            Random Normal Deviates, The Annals of Mathematical Statistics,  
    !            29(2), 610–611.
    !##############################################################################
    subroutine simulate_normal_1(x, mu, sigma, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x
     
        ! expectation of distribution
        real(dp), optional :: mu
     
        ! variance of distribution
        real(dp), optional :: sigma
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: uni1, uni2, mu_c, sigma_c
        real(dp) :: pi = 3.141592653589793_dp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = 0.0_dp
        if(present(mu))mu_c = mu
        sigma_c = 1.0_dp
        if(present(sigma))sigma_c = sigma
        
        if(sigma_c <= 0.0_dp)then
            call error('simulate_normal','sigma has zero or negative value')
        endif
     
        ! simulate a uniform variable draw
        if(present(fixed))then
            call simulate_uniform_1(uni1, fixed=fixed)
            call simulate_uniform_1(uni2, fixed=fixed)
        else
            call simulate_uniform_1(uni1)
            call simulate_uniform_1(uni2)
        endif
     
        ! transform by Box-Muller transformation
        x = sqrt(-2.0_dp*log(uni1))*cos(2.0_dp*pi*uni2)
     
        ! transform to mean and variance
        x = mu_c + sqrt(sigma_c)*x
     
    end subroutine simulate_normal_1
     
     
    !##############################################################################
    ! SUBROUTINE simulate_normal_n
    !
    ! Simulates draws from a normal distribution using
    !     Box-Muller tranformation.
    !
    ! REFERENCE: Box, G.E.P., Muller, M.E. (1958). A Note on the Generation of  
    !            Random Normal Deviates, The Annals of Mathematical Statistics,  
    !            29(2), 610–611.
    !##############################################################################
    subroutine simulate_normal_n(x, mu, sigma, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x(:)
     
        ! expectation of distribution
        real(dp), optional :: mu
     
        ! variance of distribution
        real(dp), optional :: sigma
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: uni1(size(x, 1)/2), uni2(size(x, 1)/2), mu_c, sigma_c
        integer(int32) :: n, in, n2
        real(dp) :: pi = 3.141592653589793_dp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = 0.0_dp
        if(present(mu))mu_c = mu
        sigma_c = 1.0_dp
        if(present(sigma))sigma_c = sigma
        
        if(sigma_c <= 0.0_dp)then
            call error('simulate_normal','sigma has zero or negative value')
        endif
     
        ! get size of x
        n = size(x, 1)
        n2 = n/2
     
        ! simulate a uniform variable draw
        if(present(fixed))then
            call simulate_uniform_n(uni1, fixed=fixed)
            call simulate_uniform_n(uni2, fixed=fixed)
        else
            call simulate_uniform_n(uni1)
            call simulate_uniform_n(uni2)
        endif
     
        ! transform by Box-Muller transformation
        do in = 1, n2
            x(2*(in-1)+1) = sqrt(-2.0_dp*log(uni1(in)))*cos(2.0_dp*pi*uni2(in))
            x(2*(in-1)+2) = sqrt(-2.0_dp*log(uni1(in)))*sin(2.0_dp*pi*uni2(in))
        enddo
     
        if(2*n2 /= n)then
            call simulate_normal_1(x(n), 0.0_dp, 1.0_dp)
        endif
     
        ! transform to mean and variance
        x = mu_c + sqrt(sigma_c)*x
     
    end subroutine simulate_normal_n
     
     
    !##############################################################################
    ! SUBROUTINE simulate_log_normal_1
    !
    ! Simulates one draw from a log normal distribution.
    !##############################################################################
    subroutine simulate_log_normal_1(x, mu, sigma, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x
     
        ! expectation of distribution
        real(dp), optional :: mu
     
        ! variance of distribution
        real(dp), optional :: sigma
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c, sigma_c
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = exp(0.5_dp)
        if(present(mu))mu_c = mu
        sigma_c = exp(1.0_dp)*(exp(1.0_dp)-1.0_dp)
        if(present(sigma))sigma_c = sigma
        
        if(sigma_c <= 0.0_dp)then
            call error('simulate_log_normal','sigma has zero or negative value')
        endif
        if(mu_c <= 0.0_dp)then
            call error('simulate_log_normal','mu has zero or negative value')
        endif
        
        ! get expectation and variance
        sigma_c = log(1.0_dp+sigma_c/mu_c**2)
        mu_c  = log(mu_c)-0.5_dp*sigma_c
             
        ! simulate normal and convert to log_normal
        if(present(fixed))then
            call simulate_normal_1(x, mu_c, sigma_c, fixed)
        else
            call simulate_normal_1(x, mu_c, sigma_c)
        endif
     
        ! transform to log normal
        x = exp(x)
     
    end subroutine simulate_log_normal_1
     
     
    !##############################################################################
    ! SUBROUTINE simulate_log_normal_n
    !
    ! Simulates draws from a log normal distribution.
    !##############################################################################
    subroutine simulate_log_normal_n(x, mu, sigma, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x(:)
     
        ! expectation of distribution
        real(dp), optional :: mu
     
        ! variance of distribution
        real(dp), optional :: sigma
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c, sigma_c
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = exp(0.5_dp)
        if(present(mu))mu_c = mu
        sigma_c = exp(1.0_dp)*(exp(1.0_dp)-1.0_dp)
        if(present(sigma))sigma_c = sigma
        
        if(sigma_c <= 0.0_dp)then
            call error('simulate_log_normal','sigma has zero or negative value')
        endif
        if(mu_c <= 0.0_dp)then
            call error('simulate_log_normal','mu has zero or negative value')
        endif
        
        ! get expectation and variance
        sigma_c = log(1.0_dp+sigma_c/mu_c**2)
        mu_c  = log(mu_c)-0.5_dp*sigma_c   
          
        ! simulate normal and convert to log_normal
        if(present(fixed))then
            call simulate_normal_n(x, mu_c, sigma_c, fixed)
        else
            call simulate_normal_n(x, mu_c, sigma_c)
        endif
     
        ! transform to log normal
        x = exp(x)
     
    end subroutine simulate_log_normal_n
     
     
    !##############################################################################
    ! SUBROUTINE simulate_Gamma_1
    !
    ! Simulates draws from a gamma distribution using the
    !     Marsaglia/Tsang method.
    !
    ! REFERENCE: Marsaglia, G., Tsang, W.W. (2000). A simple method for generating 
    !            gamma variables, ACM Transactions on Mathematical Software, 26(2),
    !            363-372.
    !##############################################################################
    subroutine simulate_Gamma_1(x, alpha, beta, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x
     
        ! shape parameter
        real(dp), optional :: alpha
     
        ! rate parameter
        real(dp), optional :: beta
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: alpha_c, beta_c, d, c, uni
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        alpha_c = 1.0_dp
        if(present(alpha))alpha_c = alpha
        beta_c = 1.0_dp
        if(present(beta))beta_c = beta
     
        ! check for validity of parameters
        if(alpha_c <= 0.0_dp)then
            call error('simulate_Gamma','alpha has a non-positive value')
        endif
        if(beta_c <= 0.0_dp)then
            call error('simulate_Gamma','beta has a non-positive value')
        endif
     
        ! check whether alpha >= 1
        if(alpha_c >= 1.0_dp)then
     
            ! calculate c and d
            d = alpha_c - 1.0_dp/3.0_dp
            c = 1.0_dp/sqrt(9.0_dp*d)
     
            ! simulate the gammas step by step
            if(present(fixed))then
                x = simulate_Gamma_plain(c, d, fixed)
            else
                x = simulate_Gamma_plain(c, d)
            endif
        else
     
            ! generate alpha+1 variables
            d = alpha_c + 2.0_dp/3.0_dp
            c = 1.0_dp/sqrt(9.0_dp*d)
     
            ! simulate the gammas step by step
            if(present(fixed))then
                x = simulate_Gamma_plain(c, d, fixed)
            else
                x = simulate_Gamma_plain(c, d)
            endif
     
            ! add uniform transformation
            if(present(fixed))then
                call simulate_uniform_1(uni, fixed=fixed)
            else
                call simulate_uniform_1(uni)
            endif
            x = x*uni**(1.0_dp/alpha_c)
     
        endif
     
        ! apply scaling parameter
        x = x/beta_c
     
     
    end subroutine simulate_Gamma_1
     
     
    !##############################################################################
    ! SUBROUTINE simulate_Gamma_n
    !
    ! Simulates draws from a gamma distribution using the
    !     Marsaglia/Tsang method.
    !
    ! REFERENCE: Marsaglia, G., Tsang, W.W. (2000). A simple method for generating 
    !            gamma variables, ACM Transactions on Mathematical Software, 26(2),
    !            363-372.
    !##############################################################################
    subroutine simulate_Gamma_n(x, alpha, beta, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x(:)
     
        ! shape parameter
        real(dp), optional :: alpha
     
        ! rate parameter
        real(dp), optional :: beta
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: alpha_c, beta_c, d, c, uni(size(x, 1))
        integer(int32) :: n, in
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        alpha_c = 1.0_dp
        if(present(alpha))alpha_c = alpha
        beta_c = 1.0_dp
        if(present(beta))beta_c = beta
     
        ! check for validity of beta
        if(beta_c <= 0.0_dp)then
            call error('simulate_Gamma','beta has a non-positive value')
        endif
     
        ! get size of x
        n = size(x, 1)
     
        ! check whether alpha >= 1
        if(alpha_c >= 1.0_dp)then
     
            ! calculate c and d
            d = alpha_c - 1.0_dp/3.0_dp
            c = 1.0_dp/sqrt(9.0_dp*d)
     
            ! simulate the gammas step by step
            do in = 1, n
                if(present(fixed))then
                    x(in) = simulate_Gamma_plain(c, d, fixed)
                else
                    x(in) = simulate_Gamma_plain(c, d)
                endif
            enddo
        else
     
            ! generate alpha+1 variables
            d = alpha_c + 2.0_dp/3.0_dp
            c = 1.0_dp/sqrt(9.0_dp*d)
     
            ! simulate the gammas step by step
            do in = 1, n
                if(present(fixed))then
                    x(in) = simulate_Gamma_plain(c, d, fixed)
                else
                    x(in) = simulate_Gamma_plain(c, d)
                endif
            enddo
     
            ! add uniform transformation
            if(present(fixed))then
                call simulate_uniform_n(uni, fixed=fixed)
            else
                call simulate_uniform_n(uni)
            endif
            x = x*uni**(1.0_dp/alpha_c)
     
        endif
     
        ! apply scaling parameter
        x = x/beta_c
     
     
    end subroutine simulate_Gamma_n
     
     
    !##############################################################################
    ! FUNCTION simulate_Gamma_plain
    !
    ! Simulates one draw from gamma distribution without any testing.
    !
    ! REFERENCE: Marsaglia, G., Tsang, W.W., A simple method for generating gamma 
    !            variables, ACM Transactions on Mathematical Software, Vol. 26,
    !            No. 2, 2000, 363-372.
    !##############################################################################
    function simulate_Gamma_plain(c, d, fixed) result(res)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! c parameter
        real(dp) :: c
     
        ! d parameter
        real(dp) :: d
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
        ! a gamma realization
        real(dp) :: res
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: v, x, u
     
     
        !##### ROUTINE CODE #######################################################
     
        do
            ! generate v
            if(present(fixed))then
                call simulate_normal_1(x, fixed=fixed)
            else
                call simulate_normal_1(x)
            endif
            v = (1.0_dp+c*x)
            v = v*v*v
     
            ! test v
            if(v <= 0.0_dp)cycle
     
            ! generate u
            if(present(fixed))then
                call simulate_uniform_1(u, fixed=fixed)
            else
                call simulate_uniform_1(u)
            endif
     
            ! check squeeze
            if(u < 1.0_dp - 0.0331_dp*(x*x)*(x*x))then
                res = d*v
                return
            endif
     
            ! check real condition
            if(log(u) < 0.5_dp*x*x + d*(1.0_dp - v + log(v)))then
                res = d*v
                return
            endif
        enddo
     
    end function simulate_Gamma_plain
     
     
    !##############################################################################
    ! SUBROUTINE simulate_beta_1
    !
    ! Simulates one draw from a beta distribution.
    !##############################################################################
    subroutine simulate_beta_1(x, p, q, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x
     
        ! shape parameter
        real(dp), optional :: p
     
        ! rate parameter
        real(dp), optional :: q
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: p_c, q_c, gamma1, gamma2, bern
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        p_c = 1.0_dp
        if(present(p))p_c = p
        q_c = 1.0_dp
        if(present(q))q_c = q
     
        ! check for validity of parameters
        if(p_c <= 0.0_dp)then
            call error('simulate_beta','p has a non-positive value')
        endif
        if(q_c <= 0.0_dp)then
            call error('simulate_beta','q has a non-positive value')
        endif
     
        ! get bernoulli parametzer for small p and q
        bern = p_c/(p_c+q_c)
     
        if(present(fixed))then
            call simulate_Gamma_1(gamma1, p_c, fixed=fixed)
            call simulate_Gamma_1(gamma2, q_c, fixed=fixed)
        else
            call simulate_Gamma_1(gamma1, p_c)
            call simulate_Gamma_1(gamma2, q_c)
        endif
     
        ! check whether gammas both greaters 0
        if(gamma1 > 0.0_dp .or. gamma2 > 0.0_dp)then
            x = gamma1/(gamma1+gamma2)
        else
            if(present(fixed))then
                call simulate_bernoulli_1(x, bern, fixed=fixed)
            else
                call simulate_bernoulli_1(x, bern)
            endif
        endif
     
    end subroutine simulate_beta_1
     
     
    !##############################################################################
    ! SUBROUTINE simulate_beta_n
    !
    ! Simulates draws from a beta distribution.
    !##############################################################################
    subroutine simulate_beta_n(x, p, q, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x(:)
     
        ! shape parameter
        real(dp), optional :: p
     
        ! rate parameter
        real(dp), optional :: q
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: p_c, q_c, gamma1(size(x,1)), gamma2(size(x,1)), bern
        integer(int32) :: n, in
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        p_c = 1.0_dp
        if(present(p))p_c = p
        q_c = 1.0_dp
        if(present(q))q_c = q
     
        ! check for validity of parameters
        if(p_c <= 0.0_dp)then
            call error('simulate_beta','p has a non-positive value')
        endif
        if(q_c <= 0.0_dp)then
            call error('simulate_beta','q has a non-positive value')
        endif
     
        ! get size of x
        n = size(x, 1)
     
        ! get bernoulli parametzer for small p and q
        bern = p_c/(p_c+q_c)
     
        if(present(fixed))then
            call simulate_Gamma_n(gamma1, p_c, fixed=fixed)
            call simulate_Gamma_n(gamma2, q_c, fixed=fixed)
        else
            call simulate_Gamma_n(gamma1, p_c)
            call simulate_Gamma_n(gamma2, q_c)
        endif
     
        do in = 1, n
     
            ! check whether gammas both greaters 0
            if(gamma1(in) > 0.0_dp .or. gamma2(in) > 0.0_dp)then
                x(in) = gamma1(in)/(gamma1(in)+gamma2(in))
            else
                if(present(fixed))then
                    call simulate_bernoulli_1(x(in), bern, fixed=fixed)
                else
                    call simulate_bernoulli_1(x(in), bern)
                endif
            endif
        enddo
     
    end subroutine simulate_beta_n
     
     
    !##############################################################################
    ! SUBROUTINE simulate_bernoulli_1
    !
    ! Simulates one draw from bernoulli distribution
    !##############################################################################
    subroutine simulate_bernoulli_1(x, p, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x

        ! probability to draw 1
        real(dp), intent(in) :: p
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### ROUTINE CODE #######################################################
     
        if(p < 0.0_dp)then
            call error('simulate_bernoulli', 'p has a negative value')
        endif
     
        if(present(fixed))then
            call simulate_uniform_1(x, fixed=fixed)
        else
            call simulate_uniform_1(x)
        endif
     
        if(x <= p)then
            x = 1.0_dp
        else
            x = 0.0_dp
        endif
     
    end subroutine simulate_bernoulli_1
    
    
    !##############################################################################
    ! SUBROUTINE simulate_bernoulli_n
    !
    ! Simulates draws from bernoulli distribution
    !##############################################################################
    subroutine simulate_bernoulli_n(x, p, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x(:)

        ! probability to draw 1
        real(dp), intent(in) :: p
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: ii
     
     
        !##### ROUTINE CODE #######################################################
     
        if(p < 0.0_dp)then
            call error('simulate_bernoulli', 'p has a negative value')
        endif
     
        if(present(fixed))then
            call simulate_uniform_n(x, fixed=fixed)
        else
            call simulate_uniform_n(x)
        endif
        
        do ii = 1, size(x, 1)
            if(x(ii) <= p)then
                x(ii) = 1.0_dp
            else
                x(ii) = 0.0_dp
            endif
        enddo
     
    end subroutine simulate_bernoulli_n
    
    
    !##############################################################################
    ! SUBROUTINE simulate_binomial_1
    !
    ! Simulates one draw from binomial distribution
    !##############################################################################
    subroutine simulate_binomial_1(x, n, p, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x
        
        ! total number of draws
        integer(int32), intent(in) :: n

        ! probability to draw 1
        real(dp), intent(in) :: p
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
        
        
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: ii
        real(dp) :: cumprob
     
     
        !##### ROUTINE CODE #######################################################
     
        if(p < 0.0_dp)then
            call error('simulate_binomial', 'p has a negative value')
        endif
        
        if(n < 0)then
            call error('simulate_binomial', 'n must be positive')
        endif
     
        if(present(fixed))then
            call simulate_uniform_1(x, fixed=fixed)
        else
            call simulate_uniform_1(x)
        endif
        
        ! derive bernoulli probability schedule
        cumprob = binomialPDF(0, n, p)
        if(x <= cumprob)then
            x = 0.0_dp
        else
            do ii = 1, n
                cumprob = cumprob + binomialPDF(ii, n, p)
                if(x <= cumprob)then
                    x = real(ii, dp)
                    exit
                endif
            enddo
        endif
     
    end subroutine simulate_binomial_1
    
    
    !##############################################################################
    ! SUBROUTINE simulate_binomial_n
    !
    ! Simulates draws from binomial distribution
    !##############################################################################
    subroutine simulate_binomial_n(x, n, p, fixed)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! point into which the draw should be saved
        real(dp), intent(out) :: x(:)
        
        ! total number of draws
        integer(int32), intent(in) :: n

        ! probability to draw 1
        real(dp), intent(in) :: p
        
        ! should the random seed be initialized at a fixed values
        logical, optional :: fixed
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: ii, it
        real(dp) :: cumprob(0:n)
     
     
        !##### ROUTINE CODE #######################################################
     
        if(p < 0.0_dp)then
            call error('simulate_binomial', 'p has a negative value')
        endif
        
        if(n < 0)then
            call error('simulate_binomial', 'n must be positive')
        endif
     
        ! simulate
        if(present(fixed))then
            call simulate_uniform_n(x, fixed=fixed)
        else
            call simulate_uniform_n(x)
        endif
        
        ! derive bernoulli probability schedule
        cumprob(0) = binomialPDF(0, n, p)
        do ii = 1, n
            cumprob(ii) = cumprob(ii-1) + binomialPDF(ii, n, p)
        enddo
        
        ! get draws
        do it = 1, size(x, 1)
            do ii = 0, n
                if(x(it) <= cumprob(ii))then
                    x(it) = real(ii, dp)
                    exit
                endif
            enddo
        enddo
             
    end subroutine simulate_binomial_n
     
     
    !##############################################################################
    ! SUBROUTINE normal_discrete_1
    !
    ! Creates n points and probabilities for a normal distribution.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     CompEcon toolbox of Mario Miranda and Paul Fackler available under
    !     http://www4.ncsu.edu/~pfackler/compecon/toolbox.html
    !
    ! REFERENCE: Miranda, M. & Fackler, P. (2002). Applied Computational Economics 
    !            and Finance. Cambridge: MIT Press.
    !##############################################################################
    subroutine normal_discrete_1(x, prob, mu, sigma)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! discrete points of normal distribution
        real(dp), intent(out) :: x(:)
     
        ! probability weights
        real(dp), intent(out) :: prob(:)
     
        ! expectation of distribution
        real(dp), optional :: mu
     
        ! variance of distribution
        real(dp), optional :: sigma
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c, sigma_c, pim4, z, z1, p1, p2, p3, pp
        integer(int32) :: n, m, i, j, its
        integer(int32), parameter :: maxit = 200
        real(dp), parameter :: pi = 3.1415926535897932_dp
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters        
        mu_c = 0.0_dp
        if(present(mu))mu_c = mu
        sigma_c = 1.0_dp
        if(present(sigma))sigma_c = sqrt(sigma)
        
        if(sigma_c < 0.0_dp)then
            call error('normal_discrete','sigma has negative value')
        endif
        
        z = 0.0_dp
     
        ! check for right array sizes
        n = assert_eq(size(x,1), size(prob,1), 'normal_discrete')
     
        ! calculate 1/pi^0.25
        pim4 = 1.0_dp/pi**0.25_dp
     
        ! get number of points
        m = (n+1)/2
     
        ! initialize x and prob
        x = 0.0_dp
        prob = 0.0_dp
     
        ! start iteration
        do i = 1, m
     
            ! set reasonable starting values
            if(i == 1)then
                z = sqrt(real(2*n+1, dp))-1.85575_dp*(real(2*n+1, dp)**(-1.0_dp/6.0_dp))
            elseif(i == 2)then
                z = z - 1.14_dp*(real(n, dp)**0.426_dp)/z
            elseif(i == 3)then
                z = 1.86_dp*z+0.86_dp*x(1)
            elseif(i == 4)then
                z = 1.91_dp*z+0.91_dp*x(2);
            else
                z = 2.0_dp*z+x(i-2);
            endif
     
            ! root finding iterations
            its = 0
            do while(its < maxit)
                its = its+1
                p1 = pim4
                p2 = 0.0_dp
                do j = 1, n
                    p3 = p2
                    p2 = p1
                    p1 = z*sqrt(2.0_dp/real(j, dp))*p2-sqrt(real(j-1, dp)/real(j, dp))*p3
                enddo
                pp = sqrt(2.0_dp*real(n, dp))*p2
                z1 = z
                z  = z1-p1/pp
                if(abs(z-z1) < 1.0e-14_dp)exit
            enddo
            if(its >= maxit)then
                call error('normal_discrete', &
                    'Could not discretize normal distribution')
            endif
            x(n+1-i) = z
            x(i) = -z
            prob(i) = 2.0_dp/pp**2
            prob(n+1-i) = prob(i)
        enddo
     
        ! set output data
        prob = prob/sqrt(pi)
        x = x*sqrt(2.0_dp)*sigma_c + mu_c
     
    end subroutine normal_discrete_1
     
     
    !##############################################################################
    ! SUBROUTINE normal_discrete_2
    !
    ! Creates n1*n2 points and probabilities for a two-dimensional normal
    !     distribution.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     CompEcon toolbox of Mario Miranda and Paul Fackler available under
    !     http://www4.ncsu.edu/~pfackler/compecon/toolbox.html
    !
    ! REFERENCE: Miranda, M. & Fackler, P. (2002). Applied Computational Economics 
    !            and Finance. Cambridge: MIT Press.
    !##############################################################################
    subroutine normal_discrete_2(n, x, prob, mu, sigma, rho)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! number of points in every direction
        integer(int32), intent(in) :: n(2)
     
        ! discrete points of normal distribution
        real(dp), intent(out) :: x(:, :)
     
        ! probability weights
        real(dp), intent(out) :: prob(:)
     
        ! expectation of distribution
        real(dp), optional :: mu(2)
     
        ! variance of distribution
        real(dp), optional :: sigma(2)
     
        ! correlation of distribution
        real(dp), optional :: rho
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c(2), sig_c(2), rho_c, sigma_c(2,2), l(2,2)
        real(dp) :: x1(n(1)), x2(n(2)), p1(n(1)), p2(n(2))
        integer(int32) :: m, j, k
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = 0.0_dp
        if(present(mu))mu_c = mu
        sig_c = 1.0_dp
        if(present(sigma))sig_c = sigma
        rho_c = 0.0_dp
        if(present(rho))rho_c = rho
     
        ! set up variance covariance matrix
        sigma_c(1, 1) = sig_c(1)
        sigma_c(2, 2) = sig_c(2)
        sigma_c(1, 2) = rho_c*sqrt(sig_c(1)*sig_c(2))
        sigma_c(2, 1) = sigma_c(1, 2)
     
        ! check for right array sizes
        m = assert_eq(size(x,1), size(prob,1), n(1)*n(2), 'normal_discrete')
        m = assert_eq(size(x,2), 2, 'normal_discrete')
     
        ! check whether sigma is symmetric
        if(any(abs(transpose(sigma_c) - sigma_c) > 1.0e-20_dp)) &
            call error('normal_discrete', &
            'Variance-Covariance matrix is not symmetric')
     
        ! get standard normal distributed random variables
        call normal_discrete(x1, p1, 0.0_dp, 1.0_dp)
        call normal_discrete(x2, p2, 0.0_dp, 1.0_dp)
     
        ! get joint distribution
        m = 1
        do k = 1, n(2)
            do j = 1, n(1)
                prob(m) = p1(j)*p2(k)
                x(m, :) = (/x1(j), x2(k)/)
                m = m+1
            enddo
        enddo
     
        ! decompose var-cov matrix
        if(.not.any(abs(sig_c)  <= 1.0e-100_dp))then
            call cholesky(sigma_c, l)
        else
            l = 0.0_dp
            l(1,1) = sqrt(sig_c(1))
            l(2,2) = sqrt(sig_c(2))
        endif
     
        ! calculate distribution
        x = matmul(x, transpose(l))
        x(:, 1) = x(:, 1) + mu_c(1)
        x(:, 2) = x(:, 2) + mu_c(2)
     
    end subroutine normal_discrete_2
     
     
    !##############################################################################
    ! SUBROUTINE log_normal_discrete_1
    !
    ! Creates n points and probabilities for a log-normal distribution.
    !
    ! REFERENCE: Miranda, M. & Fackler, P. (2002). Applied Computational Economics 
    !            and Finance. Cambridge: MIT Press.
    !##############################################################################
    subroutine log_normal_discrete_1(x, prob, mu, sigma)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! discrete points of normal distribution
        real(dp), intent(out) :: x(:)
     
        ! probability weights
        real(dp), intent(out) :: prob(:)
     
        ! expectation of distribution
        real(dp), optional :: mu
     
        ! variance of distribution
        real(dp), optional :: sigma
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c, sigma_c
        integer(int32) :: n
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = exp(0.5_dp)
        if(present(mu))mu_c = mu
        sigma_c = exp(1.0_dp)*(exp(1.0_dp)-1.0_dp)
        if(present(sigma))sigma_c = sigma
        
        if(sigma_c < 0.0_dp)then
            call error('log_normal_discrete','sigma has negative value')
        endif
        if(mu_c <= 0.0_dp)then
            call error('log_normal_discrete','mu has zero or negative value')
        endif
        
        ! get expectation and variance
        sigma_c = log(1.0_dp+sigma_c/mu_c**2)
        mu_c  = log(mu_c)-0.5_dp*sigma_c
     
        ! check for right array sizes
        n = assert_eq(size(x,1), size(prob,1), 'normal_discrete')
        n = n
     
        call normal_discrete(x, prob, mu_c, sigma_c)
     
        x = exp(x)
     
    end subroutine log_normal_discrete_1
     
     
    !##############################################################################
    ! SUBROUTINE log_normal_discrete_2
    !
    ! Creates n1*n2 points and probabilities for a two-dimensional log-normal
    !     distribution.
    !
    ! REFERENCE: Miranda, M. & Fackler, P. (2002). Applied Computational Economics 
    !            and Finance. Cambridge: MIT Press.
    !##############################################################################
    subroutine log_normal_discrete_2(n, x, prob, mu, sigma, rho)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! number of points in every direction
        integer(int32), intent(in) :: n(2)
     
        ! discrete points of normal distribution
        real(dp), intent(out) :: x(:, :)
     
        ! probability weights
        real(dp), intent(out) :: prob(:)
     
        ! expectation of distribution
        real(dp), optional :: mu(2)
     
        ! variance of distribution
        real(dp), optional :: sigma(2)
     
        ! correlation of distribution
        real(dp), optional :: rho
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp) :: mu_c(2), sig_c(2), rho_c, sigma_c(2,2), l(2,2)
        real(dp) :: x1(n(1)), x2(n(2)), p1(n(1)), p2(n(2))
        integer(int32) :: m, j, k
     
     
        !##### ROUTINE CODE #######################################################
     
        ! initialize parameters
        mu_c = exp(0.5_dp)
        if(present(mu))mu_c = mu
        sig_c = exp(1.0_dp)*(exp(1.0_dp)-1.0_dp)
        if(present(sigma))sig_c = sigma
        rho_c = 0.0_dp
        if(present(rho))rho_c = rho
        
        if(any(sig_c < 0.0_dp))then
            call error('log_normal_discrete','sigma has negative value')
        endif
        if(any(mu_c <= 0.0_dp))then
            call error('log_normal_discrete','mu has zero or negative value')
        endif
        if(rho_c < -1.0_dp .or. rho_c > 1.0_dp)then
            call error('log_normal_discrete','rho is outside -1 to 1')
        endif
        
        ! get expectation and variance
        sig_c = log(1.0_dp+sig_c/mu_c**2)
        mu_c  = log(mu_c)-0.5_dp*sig_c
        
        ! set up covariance matrix
        sigma_c(1, 1) = sig_c(1)
        sigma_c(2, 2) = sig_c(2)
        sigma_c(1, 2) = log(rho_c*sqrt(exp(sig_c(1))-1.0_dp)* &
            sqrt(exp(sig_c(2))-1.0_dp)+1.0_dp)
        sigma_c(2, 1) = sigma_c(1, 2)
     
        ! check for right array sizes
        m = assert_eq(size(x,1), size(prob,1), n(1)*n(2), 'normal_discrete')
        m = assert_eq(size(x,2), 2, 'normal_discrete')
     
        ! check whether sigma is symmetric
        if(any(abs(transpose(sigma_c) - sigma_c) > 1.0e-20_dp)) &
            call error('normal_discrete', &
            'Variance-Covariance matrix is not symmetric')
     
        ! get standard normal distributed random variables
        call normal_discrete(x1, p1, 0.0_dp, 1.0_dp)
        call normal_discrete(x2, p2, 0.0_dp, 1.0_dp)
     
        ! get joint distribution
        m = 1
        do k = 1, n(2)
            do j = 1, n(1)
                prob(m) = p1(j)*p2(k)
                x(m, :) = (/x1(j), x2(k)/)
                m = m+1
            enddo
        enddo
     
        ! decompose var-cov matrix
        if(.not.any(abs(sig_c) <= 1.0e-100_dp))then
            call cholesky(sigma_c, l)
        else
            l = 0.0_dp
            l(1,1) = sqrt(sig_c(1))
            l(2,2) = sqrt(sig_c(2))
        endif
     
        ! calculate distribution
        x = matmul(x, transpose(l))
        x(:, 1) = x(:, 1) + mu_c(1)
        x(:, 2) = x(:, 2) + mu_c(2)
        x = exp(x)
     
    end subroutine log_normal_discrete_2








 
 
 
 
 
 
!############################################################################## 


end module toolbox_dist
