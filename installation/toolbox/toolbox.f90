!##############################################################################
!##############################################################################
! MODULE toolbox
!
! Facade module aggregating specialized numerical submodules:
!   - toolbox_kinds:   Storage sizes and standard I/O units
!   - toolbox_core:    Assertions, error handling, wall-clock & CPU timers
!   - toolbox_grids:   Equidistant, Chebyshev, and growth grid generation
!   - toolbox_linalg:  LU decomposition, solver, inverter, Cholesky decomposition
!   - toolbox_dist:    Probability distributions, PRNG initialization & simulation
!   - toolbox_interp:  Multidimensional linear, polynomial, and spline interpolation
!   - toolbox_opt:     1D & multi-D optimization, rootfinding, linear programming
!   - toolbox_quad:    Gauss-Legendre numerical quadrature
!   - toolbox_markov:  Discretization & simulation of AR(1) processes (Rouwenhorst)
!   - toolbox_gnuplot: 2D & 3D Gnuplot visualization interface
!   - toolbox_sort:    Quicksort algorithms for real and integer arrays
!
! This code is published under the GNU General Public License v3
!                         (https://www.gnu.org/licenses/gpl-3.0.en.html)
!
! Authors: Hans Fehr and Fabian Kindermann
!          contact@ce-fortran.com
!
! It accompanies the textbook:
!     Fehr, H. & Kindermann, F. (2018). Introduction to Computational
!         Economics using Fortran. Oxford: Oxford University Press.
!
!##############################################################################
!##############################################################################

include 'src/toolbox_kinds.f90'
include 'src/toolbox_core.f90'
include 'src/toolbox_grids.f90'
include 'src/toolbox_linalg.f90'
include 'src/toolbox_dist.f90'
include 'src/toolbox_interp.f90'
include 'src/toolbox_opt.f90'
include 'src/toolbox_quad.f90'
include 'src/toolbox_markov.f90'
include 'src/toolbox_gnuplot.f90'
include 'src/toolbox_sort.f90'

module toolbox

    use toolbox_kinds
    use toolbox_core
    use toolbox_grids
    use toolbox_linalg
    use toolbox_dist
    use toolbox_interp
    use toolbox_opt
    use toolbox_quad
    use toolbox_markov
    use toolbox_gnuplot
    use toolbox_sort

    implicit none
    public

end module toolbox
