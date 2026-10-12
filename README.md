# ce-toolbox: Computational Economics Fortran Toolbox

`ce-toolbox` is a Fortran library providing numerical and scientific computing routines tailored for computational economics. It packages the numerical toolbox from:

> [Fehr, H. & Kindermann, F. (2018). *Introduction to Computational Economics Using Fortran*. Oxford University Press.](https://global.oup.com/academic/product/introduction-to-computational-economics-using-fortran-9780198804406)

This branch (`ce-fortran-fpm`) is packaged as a standalone library ready to be used with the [Fortran Package Manager (`fpm`)](https://github.com/fortran-lang/fpm), modeled after standard Fortran ecosystem packages like [fortran-lang/stdlib](https://github.com/fortran-lang/stdlib).

---

## Features

- **Linear Algebra (`toolbox_linalg`)**: LU decomposition, linear solvers, matrix inversion, Cholesky decomposition.
- **Rootfinding & Optimization (`toolbox_opt`)**: Brent 1D rootfinding, Powell multi-D minimization, Broyden solver, Newton interpolation, simplex linear programming.
- **Interpolation (`toolbox_interp`)**: Multidimensional linear, polynomial, and cubic spline interpolation.
- **Quadrature (`toolbox_quad`)**: Gauss-Legendre numerical integration.
- **Grids (`toolbox_grids`)**: Equidistant, Chebyshev, and exponential/growth grids.
- **Distributions & Random Simulation (`toolbox_dist`)**: Discrete normal/log-normal approximations, uniform, normal, log-normal, gamma, beta, Bernoulli, binomial PRNG.
- **Markov Chains (`toolbox_markov`)**: Rouwenhorst discretization and Markov chain simulation for AR(1) processes.
- **Sorting (`toolbox_sort`)**: In-place Quicksort for real and integer arrays.
- **Timers & Utilities (`toolbox_core`, `toolbox_kinds`)**: CPU & wall-clock timers (`tic`, `toc`) with OpenMP support, standard real/integer kinds (`dp`, `int32`), assertion checks.
- **Visualization (`toolbox_gnuplot`)**: 2D and 3D plotting interface via Gnuplot.

---

## Using `ce-toolbox` as an FPM Dependency

In your project's `fpm.toml`, add `ce-toolbox` to your `[dependencies]`:

```toml
[dependencies]
ce-toolbox = { git = "https://github.com/mauriciotejada/ce-fortran.git", branch = "ce-fortran-fpm" }
```

In your Fortran program or module, import the toolbox using either:

```fortran
program main
    use ce_toolbox
    implicit none

    real(dp) :: grid(10)
    call grid_Cons_Equi(grid, 0.0_dp, 1.0_dp)
    print *, grid
end program main
```

or the classic module name:

```fortran
use toolbox
```

You can also import specialized submodules directly:

```fortran
use toolbox_linalg, only: lu_solve, chol_decomp
use toolbox_opt,    only: brent, powell
use toolbox_grids,  only: grid_Cons_Equi, grid_Cons_Cheb
```

---

## Building and Testing

To build the library with `fpm`:

```bash
fpm build
```

To run the built-in test suite:

```bash
fpm test
```

---

## License

This software is published under the [GNU General Public License v3 (GPL-3.0-or-later)](LICENSE).