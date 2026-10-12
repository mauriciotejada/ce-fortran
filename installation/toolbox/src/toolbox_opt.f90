!##############################################################################
! MODULE toolbox_opt
!
! Rootfinding (fzero), optimization (fminsearch), and linear programming (simplex).
!##############################################################################
module toolbox_opt

    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64, &
        input_unit, output_unit, error_unit
    use toolbox_kinds
    use toolbox_core

    implicit none
    private

    ! Level of tolerance for all routines
    real(dp),  private  :: tbox_gftol = 1.0e-8_dp

    ! Maximum number of iterations
    integer(int32), private  :: tbox_itermax_min = 200

    ! Maximum number of iterations for brent_pow
    integer(int32), parameter, private  :: tbox_tbox_itermax_pow_b = 150

    ! Level of tolerance for all routines
    real(dp),  private  :: tbox_gftol_root = 1.0e-8_dp

    ! Maximum number of iterations for broydn
    integer(int32), private  :: itermax_root = 200

    ! Thread-local storage for OpenMP parallel regions
    !$omp threadprivate(tbox_gftol, tbox_itermax_min, tbox_gftol_root, itermax_root)

    public :: settol_root, setiter_root, fzero
    public :: settol_min, setiter_min, fminsearch
    public :: solve_lin
    public :: brent, powell, newton_interpol, broydn

!##############################################################################
! INTERFACE fminsearch
!
! Finds minimum of a function.
!##############################################################################
interface fminsearch

    module procedure brent, powell

end interface


!##############################################################################
! INTERFACE fzero
!
! Finds root of a function.
!##############################################################################
interface fzero

    module procedure newton_interpol, broydn

end interface




contains

    ! SUBROUTINE settol_min
    !
    ! For setting global tolerance level.
    !##############################################################################
    subroutine settol_min(tol)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! tolerance level
        real(dp), intent(in) :: tol
     
     
        !##### ROUTINE CODE #######################################################
    
        ! check whether tolerance level is valid
        if(tol > 0.0_dp)then
            tbox_gftol = tol
        else
            call warning('settol_min', 'tolerance level is not valid')
        endif
     
    end subroutine settol_min
     
     
    !##############################################################################
    ! SUBROUTINE setiter_min
    !
    ! For setting maximum number of iterations.
    !##############################################################################
    subroutine setiter_min(iter)
     
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! tolerance level
        integer(int32), intent(in) :: iter
     
     
        !##### ROUTINE CODE #######################################################
    
        ! check whether tolerance level is valid
        if(iter > 0)then
            tbox_itermax_min = iter
        else
            call warning('setiter_min', 'number of iterations is not valid')
        endif
     
    end subroutine setiter_min
     
     
    !##############################################################################
    ! SUBROUTINE brent
    !
    ! Minimizes a one dimensional function.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine brent(xmin, fret, minimum, maximum, func, tol, itermax)
     
        implicit none
        
        
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! minimum value found
        real(dp), intent(inout) :: xmin
        
        ! function value at minimum
        real(dp), intent(out) :: fret
        
        ! left, middle and right interval points
        real(dp), intent(in) :: minimum, maximum
        
        ! optional tolerance level
        real(dp), intent(in), optional :: tol

        ! optional maximum number of iterations
        integer(int32), intent(in), optional :: itermax
        
        
        !##### OTHER VARIABLES ####################################################
        
        real(dp) :: tol_val
        real(dp), parameter :: cgold = 0.3819660_dp
        real(dp), parameter :: zeps = 1.0e-3_dp*epsilon(xmin)
        real(dp) :: a, b, d, e, etemp, fu, fv, fw, fx, p, q, r, tol1, tol2, &
            u, v, w, x, xm, ax, bx, cx
        integer(int32) :: iter, max_iter
        
        
        !##### INTERFACES #########################################################
        
        ! interface for the function
        interface
            function func(p)
                import :: dp
                implicit none
                real(dp), intent(in) :: p
                real(dp) :: func
            end function func        
        end interface
        
        
        !##### ROUTINE CODE #######################################################
        
        ! set tolerance level and max iterations
        if(present(tol))then
            tol_val = tol
        else
            tol_val = tbox_gftol
        endif
        if(present(itermax))then
            max_iter = itermax
        else
            max_iter = tbox_itermax_min
        endif
        
        ! define a, b, d, e to zero
        a = 0.0_dp
        b = 0.0_dp
        d = 0.0_dp
        e = 0.0_dp
        
        ! set ax, bx and cx
        ax = minimum
        cx = maximum

        a = min(ax, cx)
        b = max(ax, cx)
        
        if(abs(xmin-a) <= 1.0e-6_dp)then
            bx = a + 1.0e-6_dp
        elseif(abs(xmin-b) <= 1.0e-6_dp)then
            bx = b - 1.0e-6_dp
        elseif(xmin > a .and. xmin < b)then
            bx = xmin
        else
            bx = (ax+cx)/2.0_dp
        endif
        
        v = bx
        w = v
        x = v
        e = 0.0_dp
        fx = func(x)
        fv = fx
        fw = fx
        
        do iter = 1, max_iter
            xm = 0.5_dp*(a+b)
            tol1 = tol_val*abs(x)+zeps
            tol2 = 2.0_dp*tol1
        
            if(abs(x-xm) <= (tol2-0.5_dp*(b-a)))then
                xmin = x
                fret = fx
                return
            endif
        
            if(abs(e) > tol1)then
                r = (x-w)*(fx-fv)
                q = (x-v)*(fx-fw)
                p = (x-v)*q-(x-w)*r
                q = 2.0_dp*(q-r)
                if (q > 0.0_dp) p = -p
                q = abs(q)
                etemp = e
                e = d
                if(abs(p) >= abs(0.5_dp*q*etemp) .or. &
                        p <= q*(a-x) .or. p >= q*(b-x))then
                    e = merge(a-x, b-x, x >= xm )
                    d = CGOLD*e
                else
                    d = p/q
                    u = x+d
                    if(u-a < tol2 .or. b-u < tol2) d = sign(tol1, xm-x)
                endif
            
            else
                e = merge(a-x, b-x, x >= xm )
                d = CGOLD*e
            endif
            
            u = merge(x+d, x+sign(tol1, d), abs(d) >= tol1 )
            fu = func(u)
            if(fu <= fx)then
                if(u >= x)then
                    a = x
                else
                b = x
                endif
                call shft(v, w, x, u)
                call shft(fv, fw, fx, fu)
            else
                if(u < x)then
                    a = u
                else
                    b = u
                endif
                if(fu <= fw .or. abs(w-x)  <= 1.0e-100_dp)then
                    v = w
                    fv = fw
                    w = u
                    fw = fu
                elseif(fu <= fv .or. abs(v-x) <= 1.0e-100_dp .or. abs(v-w) <= 1.0e-100_dp)then
                    v = u
                    fv = fu
                endif
            endif
        enddo
        
        call warning('fminsearch', 'maximum iterations exceeded')
    
    
    !##### SUBROUTINES AND FUNCTIONS ##########################################
    
    contains
    
    
        !##########################################################################
        ! SUBROUTINE shft
        !
        ! Shifts b to a, c to b and d to c.
        !##########################################################################
        subroutine shft(a, b, c, d)
 
            implicit none
 
 
            !##### INPUT/OUTPUT VARIABLES #########################################
 
            real(dp), intent(out)   :: a
            real(dp), intent(inout) :: b, c
            real(dp), intent(in   ) :: d
 
 
            !##### ROUTINE CODE ###################################################
            a = b
            b = c
            c = d
        end subroutine shft
    
    end subroutine brent
    
    
    
    !##############################################################################
    ! SUBROUTINE powell
    !
    ! Powell is a multidimensional function minimizer.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    subroutine powell(p, fret, minimum, maximum, func, tol, itermax)
    
        implicit none
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
    
        ! starting and ending point
        real(dp), intent(inout) :: p(:)
    
        ! value of function in minimum
        real(dp), intent(out) :: fret
    
        ! minimum optimization interval point
        real(dp), intent(in) :: minimum(:)
    
        ! maximum optimization interval point
        real(dp), intent(in) :: maximum(:)

        ! optional tolerance level
        real(dp), intent(in), optional :: tol

        ! optional maximum number of iterations
        integer(int32), intent(in), optional :: itermax
    
    
        !##### OTHER VARIABLES ####################################################
    
        real(dp) :: xi(size(p, 1), size(p, 1))
        real(dp) :: ftol
        real(dp), parameter :: tiny = 1.0e-25_dp
        integer(int32) :: i, ibig, n, iter, max_iter
        real(dp) :: del, fp, fptt, t
        real(dp), dimension(size(p)) :: pt, ptt, xit
        real(dp) :: xicom(size(p,1))
    
        
        !##### INTERFACES #########################################################
    
        ! interface for the function
        interface
            function func(p)
                import :: dp
                implicit none
                real(dp), intent(in) :: p(:)
                real(dp) :: func
            end function func
        end interface
    
    
        !##### ROUTINE CODE #######################################################
    
        ! set tolerance level and max iterations
        if(present(tol))then
            ftol = tol
        else
            ftol = tbox_gftol
        endif
        if(present(itermax))then
            max_iter = itermax
        else
            max_iter = tbox_itermax_min
        endif
        
        ! Set number of points
        n = assert_eq(size(p), size(minimum, 1), size(maximum,1), 'fminsearch')
        
        ! initialize direction set
        xi = 0.0_dp
        do i = 1, n
            xi(i, i) = 1.0_dp
        enddo
        
        ! calculate function value
        fret = func(p)
        
        ! store old p
        pt(:) = p(:)
        
        ! start iteration
        iter = 0
        do
            ! step counter
            iter = iter+1
            
            ! save old function value
            fp = fret
            
            ! ibig will be direction of steepest decline
            ibig = 0
            del = 0.0_dp
            
            ! iterate over all dimensions
            do i = 1, n
            
                ! copy direction i and store old function value
                xit(:) = xi(:,i)
                fptt = fret
                
                ! minimize along this direction
                call linmin(p, xit, n, fret, func)
                
                ! store i into i big if i is the direction of steepest decline
                if (fptt-fret > del) then
                    del=fptt-fret
                    ibig=i
                endif
            enddo
            
            ! termination criterion
            if (2.0_dp*(fp - fret) <= ftol*(abs(fp) + abs(fret)) + tiny) return
            
            ! quit if maximum iterations reached
            if(iter == max_iter)then
                call warning('fminsearch', 'maximum iterations exceeded')
            endif
            
            ! construct extrapolated point
            ptt(:) = 2.0_dp*p(:) - pt(:)
            xit(:) = p(:) - pt(:)
            pt(:) = p(:)
            
            ! calculate function value at extrapolated point
            fptt = func(ptt)
            
            ! if function value greater than actual value
            ! -> no change of directions
            if (fptt >= fp) cycle
            
            ! calculate t
            t = 2.0_dp*(fp - 2.0_dp*fret + fptt) * (fp - fret - del)**2 &
                - del*(fp - fptt)**2
            
            ! if t > 0 -> no change of directions
            if (t >= 0.0_dp) cycle
            
            ! else minimize along new direction and start new iteration
            call linmin(p, xit, n, fret, func)
            xi(:, ibig) = xi(:, n)
            xi(:,n) = xit(:)
        enddo
    
    
    !##### SUBROUTINES AND FUNCTIONS ##########################################
    
    contains
    
    
        !##########################################################################
        ! SUBROUTINE linmin
        !
        ! Minimizes multidimensional function along a given direction.
        !##########################################################################
        subroutine linmin(p, xi, n, fret, func)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #########################################
     
            ! point where to start and minimum if minimization is done
            real(dp), intent(inout) :: p(n)
     
            ! direction in which to optimize
            real(dp), intent(inout) :: xi(n)
     
            ! number of dimensions
            integer(int32), intent(in) :: n
     
            ! value of function at minimum
            real(dp), intent(out)  :: fret
     
     
            !##### OTHER VARIABLES ################################################
     
            real(dp) :: tol
            real(dp)  :: ax, bx, cx, xmin
     
     
            !##### INTERFACES #####################################################
     
                ! interface for the function
            interface
                function func(p)
                    import :: dp
                    implicit none
                    real(dp), intent(in) :: p(:)
                    real(dp) :: func
                end function func
            end interface
     
     
            !##### ROUTINE CODE ###################################################
     
            ! set tolerance level
            tol = ftol
     
            xicom(:) = xi(:)
     
            ! get optimization interval
            call getintervall(ax, bx, cx)
     
            ! minimize function using one dimensional optimizer
            fret = brent_pow(ax, bx, cx, tol, xmin, func)
     
            ! calculate new direction and endpoint
            xi(:) = xmin*xi(:)
            p(:) = p(:) + xi(:)
     
        end subroutine linmin
     
     
        !##########################################################################
        ! SUBROUTINE getinterval
        !
        ! Calculates optimization interval along a given direction.
        !##########################################################################
        subroutine getintervall(ax, bx, cx)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #########################################
     
            ! left, middle and right interval point
            real(dp), intent(out) :: ax, bx, cx
     
     
            !##### OTHER VARIABLES ################################################
     
            integer(int32) :: i
            real(dp)  :: w(0:1,n)
     
     
            !##### ROUTINE CODE ###################################################
     
            ! calculate right interval point
            cx = -1.e20_dp
            do i = 1, n
                if(abs(xicom(i)) >= 1.0e-100_dp)then
                    w(0,i) = (extr(i, 0, xicom(i))-p(i))/xicom(i)
                else
                    w(0,i) = -1.e20_dp
                endif
                if(w(0,i) > cx)cx = w(0, i)
            enddo
     
            ! calculate left interval point
            ax = 1.e20_dp
            do i=1, n
                if(abs(xicom(i)) >= 1.0e-100_dp)then
                    w(1,i) = (extr(i, 1, xicom(i))-p(i))/xicom(i)
                else
                    w(1,i) = 1.e20_dp
                endif
                if(w(1,i) < ax)ax = w(1,i)
            enddo
     
            ! calculate point in between [ax, cx]
            bx = 0.0_dp
     
        end subroutine getintervall
     
     
        !##########################################################################
        ! FUNCTION extr
        !
        ! Calculates interval endpoint in a given dimension.
        !##########################################################################
        real(dp) function extr(dim, maxxx, richt)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #########################################
     
            ! dimension in which to search
            integer(int32), intent(in) :: dim
     
            ! do you want the maximum or minimum endpoint
            integer(int32), intent(in) :: maxxx
     
            ! what is the optimization direction
            real(dp) :: richt
     
     
            !##### ROUTINE CODE ###################################################
     
            if(richt > 0.0_dp .and. maxxx == 1 .or. richt < 0.0_dp .and. maxxx == 0)then
                extr = maximum(dim)
            else
                extr = minimum(dim)
            endif
     
        end function extr
     
     
        !##########################################################################
        ! FUNCTION f1dim
        !
        ! Maps multidimensional function and point/direction combo into
        !     one-dimensional function.
        !##########################################################################
        function f1dim(x, func)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #########################################
     
            ! point where to evaluate the multidimensional function
            real(dp), intent(in)  :: x
     
            ! function value
            real(dp) :: f1dim
     
     
            !##### OTHER VARIABLES ################################################
     
            real(dp) :: xt(n)
     
     
            !##### INTERFACES #####################################################
     
            ! interface for the function
            interface
                function func(p)
                    import :: dp
                    implicit none
                    real(dp), intent(in) :: p(:)
                    real(dp) :: func
                end function func
            end interface
     
     
            !##### ROUTINE CODE ###################################################
     
            ! create point where to evaluate func
            xt(:) = p(:)+x*xicom(:)
     
            ! evaluate func at this point
            f1dim = func(xt)
     
        end function f1dim
     
     
        !##########################################################################
        ! FUNCTION brent_pow
        !
        ! Minimizes a one dimensional function.
        !##########################################################################
        function brent_pow(ax, bx, cx, tol, xmin, func)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #########################################
     
            ! left, middle and right interval points
            real(dp), intent(in) :: ax, bx, cx
     
            ! level of tolerance
            real(dp), intent(in) :: tol
     
            ! minimum value found
            real(dp), intent(out) :: xmin
     
            ! function value at minimum
            real(dp) :: brent_pow
     
     
            !##### OTHER VARIABLES ################################################
     
            real(dp), parameter :: cgold = 0.3819660_dp
            real(dp), parameter :: zeps=1.0e-3_dp*epsilon(ax)
            integer(int32) :: iter
            real(dp) :: a, b, d, e, etemp
            real(dp) :: fu, fv, fw, fx, p, q, r, tol1, tol2, &
                u, v, w, x, xm
     
     
            !##### INTERFACES #####################################################
     
            ! interface for the function
            interface
                function func(p)
                    import :: dp
                    implicit none
                    real(dp), intent(in) :: p(:)
                    real(dp) :: func
                end function func
            end interface
     
     
            !##### ROUTINE CODE ###################################################

            ! set a, b, d, e, etemp to zero
            a = 0.0_dp
            b = 0.0_dp
            d = 0.0_dp
            e = 0.0_dp
            etemp = 0.0_dp
     
            ! start algorithm
            a = min(ax, cx)
            b = max(ax, cx)
            v = bx
            w = v
            x = v
            e = 0.0_dp
            fx = f1dim(x, func)
            fv = fx
            fw = fx
     
            do iter = 1,tbox_tbox_itermax_pow_b
            
                xm = 0.5_dp*(a+b)
                tol1 = tol*abs(x)+zeps
                tol2 = 2.0_dp*tol1

                if(abs(x-xm) <= (tol2-0.5_dp*(b-a)))then
                    xmin = x
                    brent_pow = fx
                    return
                endif

                if(abs(e) > tol1)then
                    r = (x-w)*(fx-fv)
                    q = (x-v)*(fx-fw)
                    p = (x-v)*q-(x-w)*r
                    q = 2.0_dp*(q-r)
                    if (q > 0.0_dp) p = -p
                    q = abs(q)
                    etemp = e
                    e = d
                    if(abs(p) >= abs(0.5_dp*q*etemp ) .or. &
                            p <= q*(a-x) .or. p >= q*(b-x))then
                        e = merge(a-x, b-x, x >= xm )
                        d = CGOLD*e
                    else
                        d = p/q
                        u = x+d
                        if(u-a < tol2 .or. b-u < tol2) d = sign(tol1, xm-x)
                    endif

                else
                    e = merge(a-x, b-x, x >= xm )
                    d = CGOLD*e
                endif
                u = merge(x+d, x+sign(tol1, d), abs(d) >= tol1 )
                fu = f1dim(u, func)
                if(fu <= fx)then
                    if(u >= x)then
                        a = x
                    else
                        b = x
                    endif
                    call shft(v, w, x, u)
                    call shft(fv, fw, fx, fu)
                else
                    if(u < x)then
                        a = u
                    else
                        b = u
                    endif
                    if(fu <= fw .or. abs(w-x) <= 1.0e-100_dp)then
                        v = w
                        fv = fw
                        w = u
                        fw = fu
                    elseif(fu <= fv .or. abs(v-x) <= 1.0e-100_dp .or. abs(v-w) <= 1.0e-100_dp)then
                        v = u
                        fv = fu
                    endif
               endif
            enddo
            
            xmin = x
            brent_pow = fx
            call warning('fminsearch', 'maximum iterations exceeded')
     
        end function brent_pow
     
     
        !##########################################################################
        ! SUBROUTINE shft
        !
        ! Shifts b to a, c to b and d to c.
        !##########################################################################
        subroutine shft(a, b, c, d)
 
            implicit none
 
 
            !##### INPUT/OUTPUT VARIABLES #########################################
 
            real(dp), intent(out)   :: a
            real(dp), intent(inout) :: b, c
            real(dp), intent(in   ) :: d
 
 
            !##### ROUTINE CODE ###################################################
            a = b
            b = c
            c = d
        end subroutine shft
    
    end subroutine powell















!##############################################################################

    !##############################################################################
    ! SUBROUTINE solve_lin
    !
    ! For solving a linear program in normal form by means of the simplex
    !   algorithm.
    !##############################################################################
    subroutine solve_lin(x, c, A, b, numle, numge, numeq)
    
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! solution of the linear program
        real(dp), intent(inout) :: x(:)
     
        ! coefficients in the function to minimize
        real(dp), intent(in) :: c(:)
     
        ! constraint matrix 
        real(dp), intent(in) :: A(:, :)
     
        ! target vectors of constraint
        real(dp), intent(in) :: b(:)
     
        ! number of lower equal constraints
        integer(int32), intent(in) :: numle
     
        ! number of greater equal constraints
        integer(int32), intent(in) :: numge
     
        ! number of equality constraints
        integer(int32), intent(in) :: numeq
     
     
        !##### OTHER VARIABLES ####################################################
     
        integer(int32) :: m, n, i1
        real(dp) :: A_h(size(A, 1), size(A, 2)+numle+numge)
        real(dp) :: c_h(size(c, 1)+numle+numge), b_h(size(b, 1))
        real(dp) :: x_h(size(x, 1)+numle+numge)
        real(dp) :: newA(size(A, 1), size(A, 2)+numle+numge)
     
     
        !##### ROUTINE CODE #######################################################
     
        ! check for sizes
        n = assert_eq(size(x, 1), size(c, 1), size(A, 2), 'solve_lin')
        m = assert_eq(size(A, 1), size(b, 1), 'solve_lin')
     
        ! check for correct inputs
        if(numle < 0)then
           call error('solve_lin', 'Number of lower equal constraints must '// &
               'not be negative')
        elseif(numge < 0)then
           call error('solve_lin', 'Number of greater equal constraints must '// &
               'not be negative')
        elseif(numeq < 0)then
           call error('solve_lin', 'Number of equality constraints must '// &
               'not be negative')
        elseif(numle+numge+numeq /= size(b,1))then
           call error('solve_lin', 'Number of equations does not match size of b')
        endif
    
        ! set up optimization problem
        A_h = 0.0_dp
        A_h(1:m, 1:n) = A(:, :)
        do i1 = 1, numle
            A_h(i1, n+i1) = 1.0_dp
        enddo
        do i1 = 1, numge
            A_h(numle+i1, n+numle+i1) = -1.0_dp
        enddo
     
        ! check for negative bs
        b_h = b
        do i1 = 1, m
            if(b(i1) < 0.0_dp)then
                A_h(i1, :) = -A_h(i1, :)
                b_h(i1) = -b_h(i1)
            endif
        enddo
     
        ! initialize c
        c_h = 0.0_dp
        c_h(1:n) = c(:)
     
        call get_starting_value(x_h, A_h, b_h, newA)
     
        call solve_simplex(x_h, c_h, newA)
     
        x = x_h(1:n)
    
    contains
    
    
        !##############################################################################
        ! SUBROUTINE get_starting_value
        !
        ! Calculates a starting value for the linear program.
        !##############################################################################
        subroutine get_starting_value(x0, A, b, newA)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #############################################
     
            ! starting value of the linear program
            real(dp), intent(out) :: x0(:)
     
            ! constraint matrix
            real(dp), intent(in) :: A(:, :)
     
            ! target vectors of constraint
            real(dp), intent(in) :: b(:)
     
            ! new matrix for simplex
            real(dp), intent(out) :: newA(:, :)
     
     
            !##### OTHER VARIABLES ####################################################
     
            integer(int32) :: m, n, j
            real(dp) :: Astart(size(A,1), size(A,1)+size(A,2))
            real(dp) :: cstart(size(A,1)+size(A,2)), xstart(size(A,1)+size(A,2))
     
            !##### ROUTINE CODE #######################################################
     
            ! get sizes
            n = size(A, 2)
            m = size(A, 1)
     
            ! set up help problem
            cstart(1:n) = 0.0_dp
            cstart(n+1:n+m) = 1.0_dp
     
            ! set up help matrix
            Astart(1:m, 1:n) = A
            Astart(1:m, n+1:n+m) = 0.0_dp
            do j = 1, m
                Astart(j,n+j) = 1.0_dp
            enddo
     
            ! get initial guess
            xstart(1:n) = 0.0_dp
            xstart(n+1:n+m) = b
     
            ! solve linear program
            call solve_simplex(xstart, cstart, Astart, newA)
     
            ! set starting value
            x0 = xstart(1:n)
     
        end subroutine get_starting_value
     
     
        !##############################################################################
        ! SUBROUTINE solve_simplex
        !
        ! Solves a linear program in canonic form.
        !##############################################################################
        subroutine solve_simplex(x, c, A, newA)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #############################################
     
            ! starting value of the linear program and result
            real(dp), intent(inout) :: x(:)
     
            ! coefficients of the program
            real(dp), intent(in) :: c(:)
     
            ! constraint matrix
            real(dp), intent(in) :: A(:, :)
     
            ! new tableau if starting value calculation
            real(dp), intent(out), optional :: newA(:, :)
     
     
            !##### OTHER VARIABLES ####################################################
     
            integer(int32) :: k, m, n, j, ibas, inot, piv(2), ihelp, i1, i2, n1, bhelp
            integer(int32) :: bas(size(A, 1)), nbas(size(A, 2)-size(A, 1))
            real(dp) :: alpha(size(A, 1), size(A,2)-size(A, 1))
            real(dp) :: gamma(size(A, 2)-size(A,1)), x0(size(A, 1))
            real(dp) :: pivcheck(size(A,1)), phelp, alpha_h(size(alpha, 2))
     
            !##### ROUTINE CODE #######################################################
     
            ! get sizes
            n = size(A, 2)
            m = size(A, 1)
            k = size(A, 2)-size(A,1)
     
            ! set up basis and non basis elements
            ibas = 1
            inot = 1
            do j = 1, n
                if(abs(x(j)) >= 1.0e-100_dp)then
                    bas(ibas) = j
                    ibas = ibas + 1
                else
                    nbas(inot) = j
                    inot = inot + 1
                endif
            enddo
     
            ! set up x0 and c'x
            do ibas = 1, m
                x0(ibas) = x(bas(ibas))
            enddo
     
            ! set up alphas
            do inot = 1, k
                alpha(:, inot) = -A(:, nbas(inot))
            enddo
     
            ! set up gammas
            do inot = 1, k
                gamma(inot) = 0.0_dp
                do ibas = 1, m
                    gamma(inot) = gamma(inot) + alpha(ibas, inot)*c(bas(ibas))
                enddo
                gamma(inot) = gamma(inot) + c(nbas(inot))
            enddo
     
            ! start algorithm
            do
     
                ! choose pivot column
                piv = 0
                do inot = 1, k
                    if(gamma(inot) < 0.0_dp)then
                        piv(2) = inot
                        exit
                    endif
                enddo
     
                ! algorithm ends of no gamma < 0
                if(piv(2) == 0)exit
     
                ! else choose pivot row
                do ibas = 1, m
                    if(abs(alpha(ibas, piv(2))) >= 1.0e-100_dp)then
                        pivcheck(ibas) = x0(ibas)/alpha(ibas, piv(2))
                    else
                        pivcheck(ibas) = -1.0e300_dp
                    endif
                enddo
                phelp = -1.0e300_dp
                do ibas = 1, m
                    if(alpha(ibas, piv(2)) < 0.0_dp .and. pivcheck(ibas) > phelp)then
                        phelp = pivcheck(ibas)
                        piv(1) = ibas
                    endif
                enddo
     
                ! no solution in piv(1) == 0
                if(piv(1) == 0)then
                    call error('solve_lin','Problem has no solution')
                endif
     
                ! Apply basis change
                Ihelp = nbas(piv(2))
                nbas(piv(2)) = bas(piv(1))
                bas(piv(1)) = Ihelp
     
                ! change pivot element
                alpha(piv(1), piv(2)) = 1.0_dp/alpha(piv(1), piv(2))
     
                ! change pivot column
                do ibas = 1, m
                    if(ibas /= piv(1))then
                        alpha(ibas, piv(2)) = alpha(ibas, piv(2))* &
                            alpha(piv(1), piv(2))
                    endif
                enddo
     
                ! change pivot row
                do inot = 1, k
                    if(inot /= piv(2))then
                        alpha(piv(1), inot) = -alpha(piv(1), inot)* &
                            alpha(piv(1), piv(2))
                    endif
                enddo
     
                ! change other elements of alpha
                do ibas = 1, m
                    do inot = 1, k
                        if(ibas /= piv(1) .and. inot /= piv(2))then
                            alpha(ibas, inot) = alpha(ibas, inot) + &
                                alpha(ibas, piv(2))*alpha(piv(1), inot)/ &
                                alpha(piv(1), piv(2))
                        endif
                    enddo
                enddo
     
                ! change x0
                x0(piv(1)) = -x0(piv(1))*alpha(piv(1), piv(2))
                do ibas = 1, m
                    if(ibas /= piv(1))then
                        x0(ibas) = x0(ibas) + alpha(ibas, piv(2))*x0(piv(1))/ &
                            alpha(piv(1), piv(2))
                    endif
                enddo
     
                ! change gammas
                gamma(piv(2)) = gamma(piv(2))*alpha(piv(1), piv(2))
                do inot = 1, k
                    if(inot /= piv(2))then
                        gamma(inot) = gamma(inot) + alpha(piv(1), inot)* &
                            gamma(piv(2))/alpha(piv(1), piv(2))
                    endif
                enddo
     
            enddo
     
            ! get solution
            x = 0.0_dp
            do ibas = 1, m
                x(bas(ibas)) = x0(ibas)
            enddo
     
            ! set up new tableau if needed
            if(present(newA))then
     
                ! check for existence of a solution
                n1 = 1
                do i1 = 1, n
                    if(c(i1) > 0.0_dp)then
                        n1 = i1
                        exit
                    endif
                enddo
                if(any(x(n1:n) > 0.0_dp))then
                    call error('solve_lin', &
                        'Linear Program does not have a solution')
                endif
     
                ! sort tableau in ascending order
                do i1 = m-1,1,-1
                    do i2 = 1, i1
                        if(bas(i2) > bas(i2+1))then
                            bhelp = bas(i2)
                            bas(i2) = bas(i2+1)
                            bas(i2+1) = bhelp
     
                            alpha_h = alpha(i2, :)
                            alpha(i2, :) = alpha(i2+1, :)
                            alpha(i2+1, :) = alpha_h
                        endif
                    enddo
                enddo
     
                ! get new matrix
                newA = 0.0_dp
                do i1 = 1, k
                    if(nbas(i1) <= n1-1)then
                        newA(:, nbas(i1)) = -alpha(:, i1)
                    endif
                enddo
            endif
     
        end subroutine solve_simplex
    
    end subroutine solve_lin















!##############################################################################

    !##############################################################################
    ! SUBROUTINE settol_root
    !
    ! For setting global tolerance level.
    !##############################################################################
    subroutine settol_root(tol)
    
        implicit none
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
    
        ! tolerance level
        real(dp), intent(in) :: tol
    
    
        !##### ROUTINE CODE #######################################################
 
        ! check whether tolerance level is valid
        if(tol > 0.0_dp)then
            tbox_gftol_root = tol
        else
            call warning('settol_root', 'tolerance level is not valid')
        endif
    
    end subroutine settol_root
    
    
    !##############################################################################
    ! SUBROUTINE setiter_root
    !
    ! For setting maximum number of iterations.
    !##############################################################################
    subroutine setiter_root(iter)
    
        implicit none
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
    
        ! number of iterations
        integer(int32), intent(in) :: iter
    
    
        !##### ROUTINE CODE #######################################################
 
        ! check whether number of iterations is valid
        if(iter > 0)then
            itermax_root = iter
        else
            call warning('setiter_root', 'number of iterations is not valid')
        endif
    
    end subroutine setiter_root
    
    
    !##############################################################################
    ! SUBROUTINE newton_interpol
    !
    ! Find root of one-dimensional function by interpolatory newton method.
    !##############################################################################
    subroutine newton_interpol(x, funcv, check_return, tol, itermax)
    
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! initial guess and root of the function
        real(dp), intent(inout) :: x
     
        ! check is true if newton_interpol converged to local minimum or can make no
        !     further progress
        logical, intent(out), optional :: check_return

        ! optional tolerance level
        real(dp), intent(in), optional :: tol

        ! optional maximum number of iterations
        integer(int32), intent(in), optional :: itermax
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp), parameter :: eps = epsilon(x)
        real(dp) :: tolf, tolmin
        real(dp), parameter :: tolx = eps
        real(dp), parameter :: stpmx = 100.0_dp
        real(dp) :: x1, x2, f1, f2, xnew, fnew, h
        integer(int32) :: its, max_iter
     
     
        !##### INTERFACES #########################################################
     
        ! interface for the function
        interface
            function funcv(p)
                import :: dp
                implicit none
                real(dp), intent(in) :: p
                real(dp) :: funcv
            end function funcv
        end interface
     
     
        !##### ROUTINE CODE #######################################################
     
        ! set tolerance levels and max iterations
        if(present(tol))then
            tolf = tol
            tolmin = tol
        else
            tolf = tbox_gftol_root
            tolmin = tbox_gftol_root
        endif
        if(present(itermax))then
            max_iter = itermax
        else
            max_iter = itermax_root
        endif
        if(present(check_return))check_return = .false.
     
        ! initialize values
        x1 = x
        h = 1.0e-6_dp*max(abs(x), 0.01_dp)
        x2 = x + h
     
        ! calculate function values at x1, x2
        f1 = funcv(x1)
        f2 = funcv(x2)
     
        ! check if already in zero
        if(abs(f1) < 0.01_dp*tolf)then
            x = x1
            if(present(check_return))check_return = .false.
            return
        endif
     
        if(abs(f2) < 0.01_dp*tolf)then
            x = x2
            if(present(check_return))check_return = .false.
            return
        endif
        
        if(abs((1.0_dp-f1/f2)/(x2-x1)) < tolmin .or. &
            abs((1.0_dp-f1/f2)) < epsilon(1.0_dp))then
            x = x1
            if(present(check_return))check_return = .true.
            return
        endif
     
        ! start iteration
        do its = 1, max_iter
     
            ! calculate new point xnew
            xnew = x2 - (x2-x1)/(1.0_dp-f1/f2)
     
            ! calculate new function value
            fnew = funcv(xnew)
     
            ! check wether function is small enough
            if(abs(fnew) < tolf)then
                x = xnew
                if(present(check_return))check_return = .false.
                return
            endif
     
            ! check whether you are in a minimum or cannot proceed further
            if(2.0_dp*abs(xnew-x2) < tolx*abs(xnew+x2))then
                x = x2
                if(present(check_return))check_return = .true.
                return
            endif
     
            ! else set new data and repeat step
            x1 = x2
            f1 = f2
            x2 = xnew
            f2 = fnew

            if(abs((1.0_dp-f1/f2)/(x2-x1)) < tolmin .or. &
               abs((1.0_dp-f1/f2)) < epsilon(1.0_dp))then
                x = x1
                if(present(check_return))check_return = .true.
                return
            endif
        enddo
     
        ! throw warning if newton didn't converge
        if(present(check_return))check_return = .true.
     
        x = xnew
    
    end subroutine newton_interpol
    
    
    !##############################################################################
    ! SUBROUTINE broydn
    !
    ! Find root of multidimensional function of.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press. 
    !##############################################################################
    subroutine broydn(x, funcv, check_return, tol, itermax)
    
        implicit none
     
     
        !##### INPUT/OUTPUT VARIABLES #############################################
     
        ! initial guess and root of the function
        real(dp), intent(inout) :: x(:)
     
        ! check is true if broydn converged to local minimum or can make no
        !     further progress
        logical, intent(out), optional :: check_return

        ! optional tolerance level
        real(dp), intent(in), optional :: tol

        ! optional maximum number of iterations
        integer(int32), intent(in), optional :: itermax
     
     
        !##### OTHER VARIABLES ####################################################
     
        real(dp), parameter :: eps = epsilon(x)
        real(dp) :: tolf, tolmin
        real(dp), parameter :: tolx = eps
        real(dp), parameter :: stpmx = 100.0_dp
        integer(int32) :: i, j, k, its, n, max_iter
        real(dp) :: f, fold, stpmax
        real(dp), dimension(size(x)) :: c, d, fvcold, g, p, s, t, w, xold
        real(dp), dimension(size(x),size(x)) :: qt, r
        logical :: restrt, sing, check
        real(dp) :: fvec(size(x,1))
     
     
        !##### INTERFACES #########################################################
     
        ! interface for the function
        interface
            function funcv(p)
                import :: dp
                implicit none
                real(dp), intent(in) :: p(:)
                real(dp) :: funcv(size(p, 1))
            end function funcv
        end interface
     
     
        !##### ROUTINE CODE #######################################################
     
        ! set tolerance levels and max iterations
        if(present(tol))then
            tolf = tol
            tolmin = tol
        else
            tolf = tbox_gftol_root
            tolmin = tbox_gftol_root
        endif
        if(present(itermax))then
            max_iter = itermax
        else
            max_iter = itermax_root
        endif
        if(present(check_return))check_return = .false.
     
        ! get size of x
        n = size(x)   
     
        ! calculate function euklidean norm at starting point
        f = fmin(x, funcv)
     
        ! check if root has been found
        if (maxval(abs(fvec(:))) < 0.01_dp*tolf) then
            if(present(check_return))check_return = .false.
            return
        endif
     
        stpmax = stpmx*max(sqrt(dot_product(x(:),x(:))),real(n, dp))
        restrt = .true.
     
        ! iterate broydn steps
        do its = 1, max_iter
     
            ! If restart then calculate jacobian of function
            if (restrt) then
     
                ! calculate jacobian of func at x
                call fdjac(x, fvec, r, funcv)
     
                ! make q-r-decomposition of jacobian
                call qrdcmp(r, c, d, sing)
     
                ! throw error if jacobian is singular
                if(sing)then
                    call warning('fzero', 'singular jacobian')
                    if(present(check_return))check_return = .true.
                    return
                endif
     
                ! create unity matrix
                qt(:,:) = 0.0_dp
                    do j = 1, n
                            qt(j, j) = 1.0_dp
                    enddo
     
                ! for Q^T explicitly
                do k = 1, n-1
                    if (abs(c(k)) >= 1.0e-100_dp) then
                        qt(k:n,:) = qt(k:n, :)-outerprod(r(k:n, k), &
                            matmul(r(k:n, k), qt(k:n, :)))/c(k)
                    endif
                enddo
                where(lower_triangle(n,n))r(:, :) = 0.0_dp
     
                ! puts diagonal elements of R matrix to r
                do j = 1, n
                    r(j, j) = d(j)
                enddo
     
            ! else do Broydn update step
            else
     
                ! set up s as delta x
                s(:) = x(:)-xold(:)
     
                ! t = R*delta x
                do i = 1, n
                    t(i) = dot_product(r(i,i:n), s(i:n))
                enddo
     
                ! w = delta f - B*s = delta f - R*s*Q^T
                w(:) = fvec(:)-fvcold(:)-matmul(t(:), qt(:,:))
     
                ! if w entries are small enough, set them to zero
                where(abs(w(:)) < EPS*(abs(fvec(:))+abs(fvcold(:)))) w(:) = 0.0_dp
     
                ! update for non-noisy components of w
                if(any(abs(w(:)) >= 1.0e-100_dp))then
     
                    ! update t and s
                    t(:) = matmul(qt(:,:),w(:))
                    s(:)=s(:)/dot_product(s,s)
     
                    ! update R and Q^T
                    call qrupdt(r,qt,t,s)
     
                    ! get diagonal of matrix r
                    do j = 1, size(r,1)
                        d(j) = r(j,j)
                    enddo
     
                    ! if any diagonal value of r is 0, then jacobian is singular
                    if(any(abs(d(:)) <= 1.0e-100_dp))then
                        call warning('fzero', 'singular jacobian')
                        if(present(check_return))check_return = .true.
                        return  
                    endif
                endif
            endif
     
            ! perform the newton step by inverting jacobian
            p(:) = -matmul(qt(:,:), fvec(:))
            do i = 1, n
                g(i) = -dot_product(r(1:i,i), p(1:i))
            enddo
     
            ! store old x, function value and function norm
            xold(:) = x(:)
            fvcold(:) = fvec(:)
            fold = f
     
            ! solve linear equation with upper triangular matrix r
            call rsolv(r, d, p)
     
            ! searches along the new gradient direction for new x and f
            call lnsrch(xold, fold, g, p, x, f, stpmax, check, funcv)
     
            ! check whether root was found
            if(maxval(abs(fvec(:))) < tolf)then
                if(present(check_return))check_return = .false.
                return
            endif
     
            ! if check is true
            if(check)then
     
                ! check if improvement can be made, if not, return
                if(restrt .or. maxval(abs(g(:))*max(abs(x(:)), &
                        1.0_dp)/max(f, 0.5_dp*n)) < tolmin)then
                    if(present(check_return))check_return = check
                    return
                endif
     
                ! else calculate new jacobian
                restrt=.true.
     
            ! if check is false
            else
     
                ! do broydn step
                restrt=.false.
     
                ! check for convergence
                if (maxval((abs(x(:)-xold(:)))/max(abs(x(:)), &
                    1.0_dp)) < tolx)then
                    if(present(check_return))check_return = check
                    return
                endif
            endif
        enddo
     
        ! throw warning if broydn didn't converge
        if(present(check_return))check_return = .true.
     
     
    !##### SUBROUTINES AND FUNCTIONS ##########################################
     
    contains
     
     
        !##########################################################################
        ! FUNCTION fdjac
        !
        ! Calculates finite difference jacobian.
        !##########################################################################
        subroutine fdjac(x, fvec, df, funcv)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #########################################
     
            ! value where to calculate finite difference jacobian
            real(dp), intent(inout) :: x(:)
     
            ! function value at x
            real(dp), intent(in) :: fvec(:)
     
            ! resulting finite difference jacobian
            real(dp), intent(out) :: df(:, :)
     
     
            !##### OTHER VARIABLES ################################################
     
            real(dp), parameter :: eps = 1.0e-6_dp
            integer(int32) :: j, n
            real(dp), dimension(size(x)) :: xsav, xph, h
     
     
            !##### INTERFACES #####################################################
     
            ! interface for the function
            interface
                function funcv(p)
                    import :: dp
                    implicit none
                    real(dp), intent(in) :: p(:)
                    real(dp) :: funcv(size(p, 1))
                end function funcv
            end interface
     
     
            !##### ROUTINE CODE ###################################################
     
            ! check equality of sizes
            n = assert_eq(size(x), size(fvec), size(df,1), size(df,2), 'fdjac')
     
            ! store old x
            xsav = x
     
            ! calculate difference
            h = eps*abs(xsav)
            where(abs(h) <= 1.0e-100_dp)h = EPS
     
            ! calculate x + h
            xph = xsav + h
            h = xph - xsav
     
            ! itertate over dimensions and calculate difference
            do j = 1, n
                x(j) = xph(j)
                df(:,j) = (funcv(x)-fvec(:))/h(j)
                x(j) = xsav(j)
            enddo
     
        end subroutine fdjac
     
     
        !##########################################################################
        ! FUNCTION lnsrch
        !
        ! Finds point along a line, given function value and gradient, where
        !     function has decreased sufficiently (for one dimensional function).
        !##########################################################################
        subroutine lnsrch(xold, fold, g, p, x, f, stpmax, check, funcv)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #########################################
     
            ! point where to start line search
            real(dp), intent(in) :: xold(:)
     
            ! the old function value
            real(dp), intent(in) :: fold
     
            ! gradient at this point
            real(dp), intent(in) :: g(:)
     
            ! a line search direction
            real(dp), intent(inout) :: p(:)
     
            ! new value along the search line
            real(dp), intent(out) :: x(:)
     
            ! function value at new x
            real(dp), intent(out) :: f
     
            ! maximum size of steps such that lnsrch does not search un undefined
            !     areas
            real(dp), intent(in) :: stpmax
     
            ! is true if x is too close at xold
            logical, intent(out) :: check
     
     
            !##### OTHER VARIABLES ################################################
     
            real(dp), parameter :: alf = 1.0e-4_dp
            real(dp), parameter :: tolx = epsilon(x)
            integer(int32) :: ndum
            real(dp) :: a, alam, alam2, alamin, b, disc, f2, pabs, rhs1, rhs2, &
                slope, tmplam
     
     
            !##### INTERFACES #####################################################
     
            ! interface for the function
            interface
                function funcv(p)
                    import :: dp
                    implicit none
                    real(dp), intent(in) :: p(:)
                    real(dp) :: funcv(size(p, 1))
                end function funcv
            end interface
     
     
            !##### ROUTINE CODE ###################################################
            
            ! set variables to zero
            alam2 = 0.0_dp
            f2 = 0.0_dp
            
     
            ! assert sizes or arrays
            ndum = assert_eq(size(g), size(p), size(x), size(xold), 'lnsrch')
            ndum = ndum
     
            ! set check's default value
            check=.false.
     
            ! calculate norm of p
            pabs = sqrt(dot_product(p, p))
     
            ! restrict p to maximum stepsize
            if(pabs > stpmax)p(:) = p(:)*stpmax/pabs
     
            ! calculate slope
            slope = dot_product(g, p)
     
            ! throw error if you would go uphill
            if(slope >= 0.0_dp)then
                call warning('lnsrch', 'roundoff problem, I cannot go uphill')
                return
            endif
     
            ! calculate newton stepsize
            alamin = tolx/maxval(abs(p(:))/max(abs(xold(:)),1.0_dp))
            alam = 1.0_dp
     
            ! start iteration
            do
                ! calculate calculate new x
                x(:) = xold(:)+alam*p(:)
     
                ! calculate new function value at x
                f = fmin(x, funcv)
     
                ! if new x is not away enough return with check=true
                if(alam < alamin)then
                    x(:) = xold(:)
                    check = .true.
                    return
     
                ! if optimal value found return with false
                elseif(f <= fold+alf*alam*slope)then
                    return
     
                ! else do backtracking
                else
                    if(abs(alam -1.0_dp) <= 1.0e-100_dp)then
                        tmplam = -slope/(2.0_dp*(f-fold-slope))
                    else
                        rhs1 = f-fold-alam*slope
                        rhs2 = f2-fold-alam2*slope
                        a = (rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
                        b = (-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
                        if(abs(a) <= 1.0e-100_dp)then
                            tmplam = -slope/(2.0_dp*b)
                        else
                            disc = b*b-3.0_dp*a*slope
                            if(disc < 0.0_dp)then
                                tmplam = 0.5_dp*alam
                            elseif(b <= 0.0_dp)then
                                tmplam = (-b+sqrt(disc))/(3.0_dp*a)
                            else
                                tmplam = -slope/(b+sqrt(disc))
                            endif
                        endif
                        if(tmplam > 0.5_dp*alam)tmplam = 0.5_dp*alam
                    endif
                endif
                alam2 = alam
                f2 = f
                alam = max(tmplam,0.1_dp*alam)
            enddo
     
        end subroutine lnsrch
     
     
        !##########################################################################
        ! FUNCTION fmin
        !
        ! Calculates vector norm of multidimensional function.
        !##########################################################################
        function fmin(x, funcv)
     
            implicit none
     
     
            !##### INPUT/OUTPUT VARIABLES #########################################
     
            ! value where to evaluate function
            real(dp), intent(in) :: x(:)
     
            ! euklidean square norm of function at x
            real(dp) :: fmin
     
     
            !##### INTERFACES #####################################################
     
            ! interface for the function
            interface
                function funcv(p)
                    import :: dp
                    implicit none
                    real(dp), intent(in) :: p(:)
                    real(dp) :: funcv(size(p, 1))
                end function funcv
            end interface
     
            ! calculate function value
            fvec = funcv(x)
     
            ! calculate squared norm
            fmin = 0.5_dp*dot_product(fvec, fvec)
     
        end function fmin
     
     
        !##########################################################################
        ! SUBROUTINE qrdcmp
        !
        ! Calculates QR decomposition of a matrix.
        !##########################################################################
        subroutine qrdcmp(a, c, d, sing)
     
            implicit none
     
            real(dp), intent(inout) :: a(:, :)
            real(dp), intent(out) :: c(:), d(:)
            logical, intent(out) :: sing
            integer(int32) :: k, n
            real(dp) :: scale, sigma
     
            n = assert_eq(size(a,1), size(a,2), size(c), size(d), 'qrdcmp')
            sing = .false.
            do k = 1, n-1
                scale = maxval(abs(a(k:n, k)))
                if(abs(scale) <= 1.0e-100_dp)then
                        sing = .true.
                        c(k) = 0.0_dp
                        d(k) = 0.0_dp
                else
                        a(k:n, k) = a(k:n, k)/scale
                        sigma = sign(sqrt(dot_product(a(k:n, k),a(k:n, k))),a(k, k))
                        a(k,k) = a(k, k)+sigma
                        c(k) = sigma*a(k, k)
                        d(k) = -scale*sigma
                        a(k:n, k+1:n) = a(k:n, k+1:n)-outerprod(a(k:n, k),&
                                matmul(a(k:n, k),a(k:n, k+1:n)))/c(k)
                endif
            enddo
            d(n) = a(n, n)
            if (abs(d(n)) <= 1.0e-100_dp) sing = .true.
     
        end subroutine qrdcmp
     
     
        !##########################################################################
        ! SUBROUTINE qrupdt
        !
        ! Updates qr-matrices.
        !##########################################################################
        subroutine qrupdt(r,qt,u,v)
     
            implicit none
     
            real(dp), intent(inout) :: r(:, :), qt(:, :)
            real(dp), intent(inout) :: u(:)
            real(dp), intent(in) :: v(:)
            integer(int32) :: i, k, n
     
            n = assert_eq((/ size(r,1), size(r,2), size(qt,1), size(qt,2), &
                size(u), size(v)/), 'qrupdt')
            k = n+1-ifirstloc(abs(u(n:1:-1)) >= 1.0e-100_dp)
            if(k < 1)k=1
            do i = k-1, 1, -1
                call rotate(r,qt,i,u(i),-u(i+1))
                u(i) = pythag(u(i),u(i+1))
            enddo
            r(1,:) = r(1,:)+u(1)*v
            do i = 1,k-1
                call rotate(r,qt,i,r(i,i),-r(i+1,i))
            enddo
        end subroutine qrupdt
     
        !##########################################################################
        ! SUBROUTINE rsolv
        !
        ! Solves upper diagonal system.
        !##########################################################################
        subroutine rsolv(a, d, b)
     
            implicit none
     
            real(dp), intent(in) :: a(:, :), d(:)
            real(dp), intent(inout) :: b(:)
            integer(int32) :: i, n
     
            n = assert_eq(size(a,1), size(a,2), size(b), size(d), 'rsolv')
            b(n) = b(n)/d(n)
            do i = n-1, 1, -1
                    b(i) =( b(i)-dot_product(a(i, i+1:n),b(i+1:n)))/d(i)
            enddo
     
        end subroutine rsolv
     
     
        subroutine rotate(r, qt, i, a, b)
     
            implicit none
     
            real(dp), intent(inout) :: r(:, :), qt(:, :)
            integer(int32), intent(in) :: i
            real(dp), intent(in) :: a, b
            integer(int32) :: n
            real(dp) :: c, fact, s, temp(size(r,1))
     
            n = assert_eq(size(r,1), size(r,2), size(qt,1), size(qt,2), 'rotate')
            if(abs(a) <= 1.0e-100_dp)then
                c = 0.0_dp
                s = sign(1.0_dp, b)
            elseif(abs(a) > abs(b))then
                fact = b/a
                c = sign(1.0_dp/sqrt(1.0_dp+fact**2), a)
                s = fact*c
            else
                fact = a/b
                s = sign(1.0_dp/sqrt(1.0_dp+fact**2), b)
                c=fact*s
            endif
            temp(i:n) = r(i, i:n)
            r(i, i:n) = c*temp(i:n)-s*r(i+1, i:n)
            r(i+1, i:n) = s*temp(i:n)+c*r(i+1, i:n)
            temp = qt(i, :)
            qt(i, :) = c*temp-s*qt(i+1, :)
            qt(i+1, :) = s*temp+c*qt(i+1, :)
     
        end subroutine rotate
     
     
        function pythag(a, b)
     
            implicit none
     
            real(dp), intent(in) :: a, b
            real(dp) :: pythag
            real(dp) :: absa, absb
     
            absa = abs(a)
            absb = abs(b)
            if(absa > absb)then
                pythag = absa*sqrt(1.0_dp+(absb/absa)**2)
            else
                if(abs(absb) <= 1.0e-100_dp)then
                    pythag = 0.0_dp
                else
                    pythag = absb*sqrt(1.0_dp+(absa/absb)**2)
                endif
            endif
     
        end function pythag
     
     
        function ifirstloc(mask)
     
            logical, intent(in) :: mask(:)
            integer(int32) :: ifirstloc, loca(1)
     
            loca = maxloc(merge(1, 0, mask))
            ifirstloc = loca(1)
            if(.not. mask(ifirstloc))ifirstloc = size(mask)+1
     
        end function ifirstloc
     
     
        function lower_triangle(j, k, extra)
     
            integer(int32), intent(in) :: j, k
            integer(int32), intent(in), optional :: extra
            logical :: lower_triangle(j, k)
            integer(int32) :: n
     
            n = 0
            if(present(extra))n = extra
     
            lower_triangle = (outerdiff(arth_i(1, 1, j), arth_i(1, 1, k)) > -n)
     
        end function lower_triangle
     
     
        function outerdiff(a, b)
     
            integer(int32), intent(in) :: a(:), b(:)
            integer(int32) :: outerdiff(size(a, 1),size(b, 1))
     
            outerdiff = spread(a, dim=2, ncopies=size(b, 1)) - &
                spread(b, dim=1, ncopies=size(a, 1))
     
        end function outerdiff
     
     
        function outerprod(a, b)
     
            real(dp), intent(in) :: a(:), b(:)
            real(dp) :: outerprod(size(a, 1),size(b, 1))
     
            outerprod = spread(a, dim=2, ncopies=size(b, 1)) * &
                spread(b, dim=1, ncopies=size(a, 1))
     
        end function outerprod
     
     
        function arth_i(first, increment, n)
     
            integer(int32), intent(in) :: first, increment, n
            integer(int32), parameter :: npar_arth = 16
            integer(int32), parameter :: npar2_arth = 8
            integer(int32) :: arth_i(n)
            integer(int32) :: k, k2, temp
     
            if(n > 0)arth_i(1) = first
            if(n <= npar_arth) then
                do k = 2, n
                    arth_i(k) = arth_i(k-1) + increment
                enddo
            else
                do k = 2, npar2_arth
                    arth_i(k) = arth_i(k-1) + increment
                enddo
                temp = increment*npar2_arth
                k = npar2_arth
                do
                    if(k >= n)exit
                    k2 = k+k
                    arth_i(k+1:min(k2,n)) = temp+arth_i(1:min(k,n-k))
                    temp = temp + temp
                    k = k2
                enddo
            endif
        end function arth_i
    
    end subroutine broydn















!##############################################################################


end module toolbox_opt
