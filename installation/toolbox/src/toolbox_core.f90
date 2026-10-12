!##############################################################################
! MODULE toolbox_core
!
! Error handling, dimension assertions, and CPU/OpenMP wall-clock timers.
!##############################################################################
module toolbox_core

    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64, &
        input_unit, output_unit, error_unit
    use toolbox_kinds

    implicit none
    private

    ! starting time for cpu timer
    real(dp), private :: starttime_cpu
    !$omp threadprivate(starttime_cpu)

    public :: error, warning
    public :: tic, toc, outTime
    public :: assert_eq
    public :: assert_eq2, assert_eq3, assert_eq4, assert_eq5, assert_eqn

!##############################################################################
! INTERFACE assert_eq
!
! Interface for equality assertions by assert_eqx functions.
!##############################################################################
interface assert_eq

    module procedure assert_eq2, assert_eq3, assert_eq4, assert_eq5, &
        assert_eqn

end interface



contains

!##############################################################################
!##############################################################################
! MODULE errwarn
!##############################################################################
!############################################################################## 
 

    !##############################################################################
    ! SUBROUTINE error
    !
    ! Throws error message and stops program.
    !##############################################################################
    subroutine error(routine, message)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! routine in which error occured
        character(len=*), intent(in) :: routine
        
        ! error message
        character(len=*), intent(in) :: message
        
        
        !##### ROUTINE CODE #######################################################
        
        ! write error message
        write(error_unit,'(/a,a,a,a/)')'ERROR in ',routine,': ',message
        
        ! stop program
        error stop 1
    
    end subroutine error
    
    
    !##############################################################################
    ! SUBROUTINE warning
    !
    ! Throws warning message
    !##############################################################################
    subroutine warning(routine, message)
        
        
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! routine in which warning occured
        character(len=*), intent(in) :: routine
        
        ! warning message
        character(len=*), intent(in) :: message
        
        
        !##### ROUTINE CODE #######################################################
        
        ! write warning message
        write(error_unit,'(/a,a,a,a/)')'WARNING in ',routine,': ',message
    
    end subroutine warning

 
 
 
 
 









!############################################################################## 

!##############################################################################
! MODULE assertions
!##############################################################################
!##############################################################################
 
 
    !##############################################################################
    ! FUNCTION assert_eq2
    !
    ! Checks equality for two integers.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    pure function assert_eq2(n1, n2, string)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! integers to compare for equality
        integer(int32), intent(in) :: n1, n2
        
        ! routine from which error should be thrown
        character(len=*), intent(in) :: string
        
        ! return value
        integer(int32) :: assert_eq2
        
        
        !##### ROUTINE CODE #######################################################
        
        ! if equality, set return value to n1
        if (n1 == n2)then
            assert_eq2 = n1
        
        ! else throw error message
        else
            error stop 'ERROR: an assertion failed in assert_eq2 (' // trim(string) // ')'
        end if
    
    end function assert_eq2
    
    
    !##############################################################################
    ! FUNCTION assert_eq3
    !
    ! Checks equality for three integers.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    pure function assert_eq3(n1, n2, n3, string)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! integers to compare for equality
        integer(int32), intent(in) :: n1, n2, n3
        
        ! routine from which error should be thrown
        character(len=*), intent(in) :: string
        
        ! return value
        integer(int32) :: assert_eq3
        
        
        !##### ROUTINE CODE #######################################################
        
        ! if equality, set return value to n1
        if (n1 == n2 .and. n2 == n3)then
            assert_eq3 = n1
        
        ! else throw error message
        else
            error stop 'ERROR: an assertion failed in assert_eq3 (' // trim(string) // ')'
        end if
    
    end function assert_eq3
    
    
    !##############################################################################
    ! FUNCTION assert_eq4
    !
    ! Checks equality for four integers.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    pure function assert_eq4(n1, n2, n3, n4, string)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! integers to compare for equality
        integer(int32), intent(in) :: n1, n2, n3, n4
        
        ! routine from which error should be thrown
        character(len=*), intent(in) :: string
        
        ! return value
        integer(int32) :: assert_eq4
        
        
        !##### ROUTINE CODE #######################################################
        
        ! if equality, set return value to n1
        if (n1 == n2 .and. n2 == n3 .and. n3 == n4)then
            assert_eq4 = n1
        
        ! else throw error message
        else
            error stop 'ERROR: an assertion failed in assert_eq4 (' // trim(string) // ')'
        end if
    
    end function assert_eq4
    
    
    !##############################################################################
    ! FUNCTION assert_eq5
    !
    ! Checks equality for five integers.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    pure function assert_eq5(n1, n2, n3, n4, n5, string)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! integers to compare for equality
        integer(int32), intent(in) :: n1, n2, n3, n4, n5
        
        ! routine from which error should be thrown
        character(len=*), intent(in) :: string
        
        ! return value
        integer(int32) :: assert_eq5
        
        
        !##### ROUTINE CODE #######################################################
        
        ! if equality, set return value to n1
        if (n1 == n2 .and. n2 == n3 .and. n3 == n4 .and. n4 == n5)then
            assert_eq5 = n1
        
        ! else throw error message
        else
            error stop 'ERROR: an assertion failed in assert_eq5 (' // trim(string) // ')'
        end if
    
    end function assert_eq5
    
    
    !##############################################################################
    ! FUNCTION assert_eqn
    !
    ! Checks equality for n integers.
    !
    ! PARTS OF THIS PROCEDURE WERE COPIED AND ADAPTED FROM:
    !     Press, W.H., Teukolsky, S.A., Vetterling, W.T. & Flannery, B.P. (1992). 
    !     Numerical Recipes in Fortran 90: The Art of Parallel Scientific
    !     Computing, 2nd edition. Cambridge: Cambridge University Press.
    !##############################################################################
    pure function assert_eqn(nn, string)
    
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! integers to compare for equality
        integer(int32), intent(in) :: nn(:)
        
        ! routine from which error should be thrown
        character(len=*), intent(in) :: string
        
        ! return value
        integer(int32) :: assert_eqn
        
        
        !##### ROUTINE CODE #######################################################
        
        ! if equality, set return value to n1
        if (all(nn(2:) == nn(1)))then
            assert_eqn = nn(1)
        
        ! else throw error message
        else
            error stop 'ERROR: an assertion failed in assert_eqn (' // trim(string) // ')'
        end if
    
    end function assert_eqn
 
 
 
 
 









 
!############################################################################## 

!##############################################################################
! MODULE clock
!############################################################################## 
!##############################################################################
 

    !##############################################################################
    ! SUBROUTINE tic
    !
    ! Starts cpu timer. Optionally returns current timestamp.
    !##############################################################################
    subroutine tic(t)
    
        use omp_lib

        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! optional starting time output
        real(dp), intent(out), optional :: t
        real(dp) :: now
    

        !##### ROUTINE CODE #######################################################
        
        ! get cpu time
        now = omp_get_wtime()
        starttime_cpu = now
        if(present(t)) t = now
    
    end subroutine tic
    
    
    !##############################################################################
    ! SUBROUTINE toc
    !
    ! Stops cpu timer. Optionally takes explicit start time and returns elapsed.
    !##############################################################################
    subroutine toc(file, t, elapsed)
    
        use omp_lib

    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! optional file identifier
        integer(int32), intent(in), optional :: file

        ! optional starting time input
        real(dp), intent(in), optional :: t

        ! optional elapsed seconds output
        real(dp), intent(out), optional :: elapsed
        
        
        !##### OTHER VARIABLES ####################################################
        
        real(dp) :: time
        integer(int32) :: outfile
        real(dp) :: times(4)
        
        
        !##### ROUTINE CODE #######################################################
        
        ! get output file identifier
        if(present(file))then
            outfile = file
        else
            outfile = 0
        endif
        
        ! get cpu time
        time = omp_get_wtime()
        
        ! calculate time difference
        if(present(t))then
            time = time - t
        else
            time = time - starttime_cpu
        endif

        ! record elapsed seconds if requested
        if(present(elapsed)) elapsed = time
        
        ! get number of days
        times(1) = floor(time/(24.0_dp*60.0_dp*60.0_dp))
        time = time - times(1)*24.0_dp*60.0_dp*60.0_dp
        
        ! get number of hours
        times(2) = floor(time/(60.0_dp*60.0_dp))
        time = time - times(2)*60.0_dp*60.0_dp
        
        ! get number of minutes
        times(3) = floor(time/60.0_dp)
        time = time - times(3)*60.0_dp
        
        ! get number of seconds
        times(4) = time
        
        call outTime(times, outfile)
    
    end subroutine toc
 
    
    !##############################################################################
    ! SUBROUTINE outTime
    !
    ! Writes time to file.
    !##############################################################################
    subroutine outTime(times, file)
    
        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! time as integer array
        real(dp), intent(in) :: times(4)
        
        ! the output file identifier
        integer(int32), intent(in) :: file
        
        !##### OTHER VARIABLES ####################################################
        
        character(len=200) :: output1, output2
        
        
        !##### ROUTINE CODE #######################################################
        
        ! set up output
        write(output1, '(a)')'Time elapsed: '
        output2 = output1
        
        ! write time values
        if(times(1) > 0.0_dp)then
            write(output1, '(a,1x,i3,a)') trim(output2), int(times(1)), ' d  '
            output2 = output1
        endif
        if(times(2) > 0.0_dp)then
            write(output1, '(a,1x,i3,a)') trim(output2), int(times(2)), ' h  '
            output2 = output1
        endif
        if(times(3) > 0.0_dp) then
            write(output1, '(a,1x,i3,a)')trim(output2), int(times(3)), ' min  '
            output2 = output1
        endif
        
        write(output1, '(a,1x,f7.3,a)')trim(output2), times(4), ' s  '
        
        if(file > 0) then
            write(file, '(/a/)')trim(output1)
        else
            write(*, '(/a/)')trim(output1)
        endif
    
    end subroutine outTime














!############################################################################## 


end module toolbox_core
