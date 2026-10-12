!##############################################################################
! MODULE toolbox_kinds
!
! Standard intrinsic module for storage sizes and standard I/O units
!##############################################################################
module toolbox_kinds

    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64, &
        input_unit, output_unit, error_unit

    implicit none
    private

    public :: sp, dp, qp
    public :: int8, int16, int32, int64
    public :: input_unit, output_unit, error_unit

end module toolbox_kinds
