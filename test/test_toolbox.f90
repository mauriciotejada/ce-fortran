program test_toolbox
    use ce_toolbox
    implicit none

    real(dp) :: grid_equi(5), grid_cheb(5)
    real(dp) :: A(3, 3), b(3), x_exact(3)
    real(dp) :: arr(5)
    real(dp) :: x_min(2), f_min
    real(dp) :: t_start, elapsed
    integer(int32) :: i

    print *, "=========================================="
    print *, "  Running ce-toolbox test suite (fpm)     "
    print *, "=========================================="

    ! 1. Timer routines (OpenMP)
    call tic(t_start)

    ! 2. Grid generation
    call grid_Cons_Equi(grid_equi, 0.0_dp, 1.0_dp)
    if (abs(grid_equi(1) - 0.0_dp) > 1.0e-12_dp .or. abs(grid_equi(5) - 1.0_dp) > 1.0e-12_dp) then
        error stop "Error in grid_Cons_Equi!"
    end if
    print *, "[PASS] Equidistant grid generated successfully."

    call grid_Cons_Cheb(grid_cheb, -1.0_dp, 1.0_dp)
    if (grid_cheb(1) < -1.0_dp .or. grid_cheb(5) > 1.0_dp) then
        error stop "Error in grid_Cons_Cheb!"
    end if
    print *, "[PASS] Chebyshev grid generated successfully."

    ! 3. Linear algebra (LU solve)
    A(1, :) = [2.0_dp, 0.0_dp, 1.0_dp]
    A(2, :) = [0.0_dp, 4.0_dp, 1.0_dp]
    A(3, :) = [1.0_dp, 1.0_dp, 2.0_dp]
    x_exact = [10.0_dp, 8.0_dp, 10.0_dp]
    b = matmul(A, x_exact)

    call lu_solve(A, b)
    if (maxval(abs(b - x_exact)) > 1.0e-10_dp) then
        error stop "Error in lu_solve!"
    end if
    print *, "[PASS] Linear algebra LU solver verified."

    ! 4. Sorting algorithm
    arr = [5.5_dp, 1.2_dp, 4.4_dp, 2.3_dp, 3.1_dp]
    call sort_r(arr)
    do i = 1, 4
        if (arr(i) > arr(i + 1)) then
            error stop "Error in sort_r!"
        end if
    end do
    print *, "[PASS] Real quicksort verified."

    ! 5. Nelder-Mead minimizer (unbounded fminsearch)
    x_min = [-1.2_dp, 1.0_dp]
    call fminsearch(x_min, f_min, rosenbrock)
    if (maxval(abs(x_min - 1.0_dp)) > 1.0e-5_dp) then
        error stop "Error in nelder_mead!"
    end if
    print *, "[PASS] Nelder-Mead minimizer verified."

    ! 6. Adaptive Nelder-Mead (dimension-dependent coefficients)
    x_min = [-1.2_dp, 1.0_dp]
    call fminsearch(x_min, f_min, rosenbrock, adaptive=.true.)
    if (maxval(abs(x_min - 1.0_dp)) > 1.0e-5_dp) then
        error stop "Error in adaptive nelder_mead!"
    end if
    print *, "[PASS] Adaptive Nelder-Mead minimizer verified."

    ! Stop timer
    call toc(t=t_start, elapsed=elapsed)
    print *, "=========================================="
    print *, "All tests passed successfully!"
    print *, "=========================================="

contains

    ! Rosenbrock function, minimum at (1, 1)
    function rosenbrock(p)
        real(dp), intent(in) :: p(:)
        real(dp) :: rosenbrock

        rosenbrock = 100.0_dp*(p(2) - p(1)**2)**2 + (1.0_dp - p(1))**2
    end function rosenbrock

end program test_toolbox
