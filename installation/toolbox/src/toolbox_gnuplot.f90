!##############################################################################
! MODULE toolbox_gnuplot
!
! 2D and 3D plotting interface for Gnuplot.
!##############################################################################
module toolbox_gnuplot

    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64, &
        input_unit, output_unit, error_unit
    use toolbox_kinds
    use toolbox_core
    use toolbox_grids

    implicit none
    private

    ! control variables for gnuplot
    logical, private :: gnu_addtoplot = .false.
    logical, private :: gnu_dolegend = .false.
    logical, private :: gnu_histogram = .false.
    integer(int32), private :: gnu_nmax, gnu_mmax
    integer(int32), private :: gnu_plotnumber = 0

    ! plot data for gnuplot
    real(dp), allocatable, private :: gnu_x(:, :), gnu_y(:, :)
    real(dp), allocatable, private :: gnu_x_temp(:, :), gnu_y_temp(:, :)

    ! style data for gnuplot
    character(LEN = 2000), dimension(1000), private :: gnu_definitions 

    public :: plot, plot3d, plot_hist, execplot
    public :: plot3d_grid, plot3d_line, plot_hist_old, plot_hist_new

!##############################################################################
! INTERFACE plot_hist
!
! Creates histogram plot
!##############################################################################
interface plot_hist

    module procedure &
        plot_hist_old, plot_hist_new

end interface



!##############################################################################
! INTERFACE plot3d
!
! Creates a two-dimensional plot.
!##############################################################################
interface plot3d

    module procedure &
        plot3d_grid, plot3d_line

end interface




contains


    !##############################################################################
    ! SUBROUTINE execplot
    !
    ! Actually creates the plot files.
    !##############################################################################
    subroutine execplot(xlim, xticks, xtrange, xlabel, ylim, yticks, ytrange, ylabel, title, &
        legend, ratio, nogrid, nodisplay, filename, filetype, output)

        implicit none


        !##### INPUT/OUTPUT VARIABLES #############################################

        ! x axis definitiion
        real(dp), optional :: xlim(2)

        ! x axis tick definitions
        real(dp), optional :: xticks
        
        ! x axis range for ticks
        real(dp), optional :: xtrange(2)

        ! y axis definition
        real(dp), optional :: ylim(2)

        ! y axis tick definitions
        real(dp), optional :: yticks
        
        ! y axis range for ticks
        real(dp), optional :: ytrange(2)

        ! output file name
        character(LEN=*), optional :: title

        ! output file name
        character(LEN=*), optional :: xlabel

        ! output file name
        character(LEN=*), optional :: ylabel

        ! legend position
        character(LEN=2), optional :: legend
        
        ! y-to-x axis ratio
        real(dp), optional :: ratio
        
        ! print no grid
        logical, optional :: nogrid
        
        ! should plot windows be shown or not
        logical, optional :: nodisplay

        ! output file name
        character(LEN=*), optional :: filename

        ! file type
        character(LEN=*), optional :: filetype

        ! output file name
        character(LEN=*), optional :: output
        
        !##### OTHER VARIABLES ####################################################
        integer(int32) :: u_data, u_cmd, ios
 
        integer(int32) :: i1, i2
        logical :: nw_copy
        character(LEN=3) :: ft, plotnumber
        character(LEN=150) :: cfile, dfile, pfile, term, location
     
     
        !##### ROUTINE CODE #######################################################

        ! check for whether to display graph
        nw_copy = .false.
        if(present(nodisplay))then
            if(nodisplay)nw_copy = .true.
        endif
        
        if(access('/usr/local/include/toolbox_visual.txt', ' ') == 0)then
            nw_copy = .true.
        endif

        ! create output files
        if(present(output))then
            cfile = trim(output)//'_c.dat'
            dfile = trim(output)//'_d.dat'
        else
            ! increment gnu_plotnumber by 1 (restart counting after 999 plots)
            gnu_plotnumber = gnu_plotnumber + 1
            if(gnu_plotnumber >= 1000) gnu_plotnumber = 1

            if(gnu_plotnumber < 10)then
                write(plotnumber,'(i1)')gnu_plotnumber
            elseif(gnu_plotnumber < 100)then
                write(plotnumber,'(i2)')gnu_plotnumber
            else
                write(plotnumber,'(i3)')gnu_plotnumber
            endif
            cfile = 'gnufigure_'//trim(plotnumber)//'_c.dat'
            dfile = 'gnufigure_'//trim(plotnumber)//'_d.dat'
        endif

        ! create plot file name
        if(present(filename))then
            pfile = trim(filename)
        else
            pfile = 'gnufigure_'//trim(plotnumber)
        endif

        ! write the output data file
        open(newunit=u_data, file=trim(dfile), status='replace', action='write', iostat=ios)
        if(ios /= 0) call error('execplot', 'Failed to open data file: '//trim(dfile))
        do i1 = 1, gnu_nmax
            write(u_data, '(2000e21.10e4)')(gnu_x(i1, i2), gnu_y(i1, i2), i2=1, gnu_mmax)
        enddo
        close(u_data)

        ! write the command file
        open(newunit=u_cmd, file=trim(cfile), status='replace', action='write', iostat=ios)
        if(ios /= 0) call error('execplot', 'Failed to open command file: '//trim(cfile))

        ! set file or terminal
        if(nw_copy)then
        
            ! write graph to file            
            ft = "pdf"
            term = "pdfcairo"
            if(present(filetype))then
                if(filetype(1:3) == "png")then
                    ft=  "png"
                    term = "pngcairo"
                endif
                if(filetype(1:3) == "eps")then
                    ft = "eps"
                    term = "epscairo"
                endif
            endif
            
            if(present(ratio) .and. ft /= "png")then
                write(term, '(a,f4.2)')trim(term)//" size 5,", 5.0_dp*ratio
            endif
            
            write(u_cmd, *)
            write(u_cmd, '(a)')'set terminal '//trim(term)//'  color enhanced fontscale 0.7'
            write(u_cmd, '(a)')'set output "'//trim(pfile)//'.'//ft//'"'

            write(*,'(/a/)')'CREATING PLOT '//trim(pfile)//'.'//ft
        else
        
            ! print graph to terminal

            write(u_cmd, '(a)')'if (strstrt(GPVAL_TERMINALS, "wxt") > 0) {'
            write(u_cmd, '(a)')'    set terminal wxt title "Gnuplot" enhanced'
            write(u_cmd, '(a)')'} else {'
            write(u_cmd, '(a)')'    if (strstrt(GPVAL_TERMINALS, "x11") > 0) {'
            write(u_cmd, '(a)')'        set terminal x11 enhanced'
            write(u_cmd, '(a)')'    } else {'
            write(u_cmd, '(a)')'        if (strstrt(GPVAL_TERMINALS, "qt") > 0) {'
            write(u_cmd, '(a)')'            set terminal qt enhanced'
            write(u_cmd, '(a)')'        } else {'
            write(u_cmd, '(a)')'            print ""'
            write(u_cmd, '(a)')'            print "ATTENTION: There seems to be NO VALID TERMINAL installed in "'
            write(u_cmd, '(a)')'            print "           your version of gnuplot. It might be a good idea to "'
            write(u_cmd, '(a)')'            print "           completely uninstall your Fortran/gnuplot/geany installation"'
            write(u_cmd, '(a)')'            print "           using the uninstallation files on www.ce-fortran.com. "'
            write(u_cmd, '(a)')'            print "           Afterwards you should reinstall the Fortran system again."'
            write(u_cmd, '(a)')'            print "           If this does not help, please refer to the forum."'
            write(u_cmd, '(a)')'            print ""'
            write(u_cmd, '(a)')'            q'
            write(u_cmd, '(a)')'        }'
            write(u_cmd, '(a)')'    }'
            write(u_cmd, '(a)')'}'
        endif


        ! set the grid definition
        if(present(nogrid))then
            if(nogrid)then
                write(u_cmd, '(a)')'unset grid'
            else
                write(u_cmd, '(a)')'set grid'
            endif
        else
            write(u_cmd, '(a)')'set grid'
        endif
        
        ! set histogram definition
        if(gnu_histogram)then
            write(u_cmd, '(a)')'set style data histograms'
            write(u_cmd, '(a)')'set style fill solid border -1'
        endif
          
        ! set x axis
        if(present(xlim))write(u_cmd, '(a,e13.5,a,e13.5,a)')'set xrange [',minval(xlim),':',maxval(xlim),']'
        if(present(xticks))then
            if(present(xtrange))then
                write(u_cmd, '(a,e13.5,a,e13.5,a,e13.5)')'set xtics ',xtrange(1),',', xticks,',', xtrange(2)
            else
                write(u_cmd, '(a,e13.5)')'set xtics ',xticks
            endif
        endif
        if(present(xlabel))write(u_cmd, '(a)')'set xlabel "'//xlabel//'"font ",14"'
        
        ! set y axis
        if(present(ylim) .and. .not. gnu_histogram) &
            write(u_cmd, '(a,e13.5,a,e13.5,a)')'set yrange [',minval(ylim),':',maxval(ylim),']'
        if(gnu_histogram)then
            if(present(ylim))then
                write(u_cmd, '(a,e13.5,a,e13.5,a)') &
                    'set yrange [',min(minval(ylim),0.0_dp),':',maxval(ylim),']'
            else
                write(u_cmd, '(a)') 'set yrange [0:]'
            endif
        endif
                    
        if(present(yticks))then
            if(present(ytrange))then
                write(u_cmd, '(a,e13.5,a,e13.5,a,e13.5)')'set ytics ',ytrange(1),',', yticks,',', ytrange(2)
            else
                write(u_cmd, '(a,e13.5)')'set ytics ',yticks
            endif
        endif
        if(present(ylabel))write(u_cmd, '(a)')'set ylabel "'//ylabel//'"font ",14"'

        ! set title
        if(present(title))write(u_cmd, '(a)')'set title "'//title//'" font ",16"'
        
        ! legend statement
        if(gnu_dolegend)then
            if(present(legend))then            
                select case (legend(1:2))
                    case("ln")
                        location = 'inside left top'
                    case("ls")
                        location = 'inside left bottom'
                    case("lo")
                        location = 'outside vert center'
                    case("lb")
                        location = 'outside left below'
                    case("rn")
                        location = 'inside right top'
                    case("rs")
                        location = 'inside right bottom'
                    case("ro")
                        location = 'outside vert center right'
                    case("rb")
                        location = 'key outside right below'
                    case("cn")
                        location = 'inside center top'
                    case("cs")
                        location = 'inside center bottom'
                    case("co")
                        location = 'outside vert top center'
                    case("cb")
                        location = 'outside center below'
                    case default
                        location = 'outside vert bottom center' 
                end select
            else
                location = 'center below'
            endif   
            
            write(u_cmd, '(a)')'set key '//trim(location)//' box Left reverse spacing 1.2'
        else
            write(u_cmd, '(a)')'unset key'
        endif

        ! write plot lines
        if(gnu_mmax == 1)then
            write(u_cmd, '(a)')'plot "'//trim(dfile)//'" using 1:2 '//trim(gnu_definitions(1))
        else
            write(u_cmd, '(a)')'plot "'//trim(dfile)//'" using 1:2 '//trim(gnu_definitions(1))//',\' 
            do i1 = 2, gnu_mmax-1
                write(u_cmd, '(a,i4,a,i4,a)')'     "'//trim(dfile)//'" using '&
                    ,2*(i1-1)+1,':',2*(i1-1)+2,' '//trim(gnu_definitions(i1))//',\' 
            enddo
            write(u_cmd, '(a,i4,a,i4,a)')'     "'//trim(dfile)//'" using ',  &
                2*(gnu_mmax-1)+1,':',2*(gnu_mmax-1)+2,' '//trim(gnu_definitions(i1))
        endif
                        
        if(.not. nw_copy)write(u_cmd, '(a)')'pause mouse close'

        ! write graph to file
        if(.not. nw_copy)then
            if(present(filename))then                
                ft = "pdf"
                term = "pdfcairo"
                if(present(filetype))then
                    if(filetype(1:3) == "png")then
                        ft=  "png"
                        term = "pngcairo"
                    endif
                    if(filetype(1:3) == "eps")then
                        ft = "eps"
                        term = "epscairo"
                    endif
                endif
                
                if(present(ratio) .and. ft /= "png")then
                    write(term, '(a,f4.2)')trim(term)//" size 5,", 5.0_dp*ratio
                endif
                
                write(u_cmd, *)
                write(u_cmd, '(a)')'set terminal '//trim(term)//'  color enhanced fontscale 0.7'
                write(u_cmd, '(a)')'set output "'//trim(pfile)//'.'//ft//'"'
                write(u_cmd, '(a)')'replot'
            endif
        endif
        
        write(u_cmd, '(a)')'q'
        close(u_cmd)
        
        call system('gnuplot "'//trim(cfile)//'"') 
        if(.not.present(output))then
            open(newunit=u_data, file=trim(dfile), iostat=ios)
            if(ios == 0) close(u_data, status='delete')
            open(newunit=u_cmd, file=trim(cfile), iostat=ios)
            if(ios == 0) close(u_cmd, status='delete')
        endif

        gnu_addtoplot = .false.
        gnu_dolegend = .false.
        gnu_histogram = .false.

    end subroutine

    
    !##############################################################################
    ! SUBROUTINE plot
    !
    ! Plots a x-y-data column to the output file.
    !##############################################################################
    !##############################################################################
    ! SUBROUTINE plot
    !
    ! Plots a x-y-data column to the output file.
    !##############################################################################
    subroutine plot(xin, yin, color, linewidth, dashtype, marker, markersize, noline, legend)

        implicit none


        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! input variables
        real(dp), intent(in) :: xin(:), yin(:)

        ! line color
        character(LEN=*), optional :: color

        ! width of plotting line
        real(dp), optional :: linewidth

        ! dashtype of the plotting line
        character(LEN=*), optional :: dashtype

        ! marker definition
        integer(int32), optional :: marker

        ! size of the marker
        real(dp), optional :: markersize

        ! if no line should be plotted
        logical, optional :: noline

        ! legend description
        character(LEN=*), optional :: legend


        !##### OTHER VARIABLES ####################################################
 
        integer(int32) :: n, i1
        logical :: lines, points
     
     
        !##### ROUTINE CODE #######################################################

        n = assert_eq(size(xin, 1), size(yin, 1), 'plot')

        ! generate the respective plot data
        if(.not.gnu_addtoplot)then

            ! allocate new x array
            if(allocated(gnu_x))deallocate(gnu_x)
            allocate(gnu_x(n, 1))
            gnu_x(:, 1) = xin  

            ! allocate new y array
            if(allocated(gnu_y))deallocate(gnu_y)
            allocate(gnu_y(n, 1))
            gnu_y(:, 1) = yin  

            ! allocate new x_temp array
            if(allocated(gnu_x_temp))deallocate(gnu_x_temp)
            allocate(gnu_x_temp(n, 1))
            gnu_x_temp(:, 1) = xin  

            ! allocate new y_temp array
            if(allocated(gnu_y_temp))deallocate(gnu_y_temp)
            allocate(gnu_y_temp(n, 1))
            gnu_y_temp(:, 1) = yin

            gnu_addtoplot = .true.
            gnu_nmax = n
            gnu_mmax = 1

        else
    
            ! get new number of lines
            gnu_mmax = gnu_mmax+1
            if(gnu_mmax > 1000)then
                write(*,'(/a/)')'SORRY: I CANNOT PLOT MORE THAN 1000 LINES'
                return
            endif

            ! deallocate arrays
            deallocate(gnu_x)
            deallocate(gnu_y)
            
            ! if the new array is the longer one
            if(n > gnu_nmax)then
                allocate(gnu_x(n, gnu_mmax))
                allocate(gnu_y(n, gnu_mmax))

                gnu_x(1:gnu_nmax, 1:gnu_mmax-1) = gnu_x_temp                
                gnu_y(1:gnu_nmax, 1:gnu_mmax-1) = gnu_y_temp

                ! fill up with the same values
                do i1 = gnu_nmax+1, n
                    gnu_x(i1, 1:gnu_mmax-1) = gnu_x(gnu_nmax, 1:gnu_mmax-1)
                    gnu_y(i1, 1:gnu_mmax-1) = gnu_y(gnu_nmax, 1:gnu_mmax-1)
                enddo

                gnu_x(:, gnu_mmax) = xin
                gnu_y(:, gnu_mmax) = yin

                ! set new nmax
                gnu_nmax = n

            ! if the old array is the longer one
            else

                allocate(gnu_x(gnu_nmax, gnu_mmax))
                allocate(gnu_y(gnu_nmax, gnu_mmax))

                gnu_x(:, 1:gnu_mmax-1) = gnu_x_temp                
                gnu_y(:, 1:gnu_mmax-1) = gnu_y_temp                

                gnu_x(1:n, gnu_mmax) = xin
                gnu_y(1:n, gnu_mmax) = yin

                ! fill up with same values
                do i1 = n+1, gnu_nmax
                    gnu_x(i1, gnu_mmax) = gnu_x(n, gnu_mmax)
                    gnu_y(i1, gnu_mmax) = gnu_y(n, gnu_mmax)
                enddo                
            endif

            deallocate(gnu_x_temp)
            allocate(gnu_x_temp(size(gnu_x,1), size(gnu_x,2)))
            gnu_x_temp = gnu_x

            deallocate(gnu_y_temp)
            allocate(gnu_y_temp(size(gnu_y,1), size(gnu_y,2)))
            gnu_y_temp = gnu_y
                
        endif

        ! check for lines and points
        lines = .true.
        if(present(noline))then
            if(noline)lines = .false.
        endif

        if(.not.lines)then
            points = .true.
        else
            points = .false.
        endif
        if(present(marker))points = .true.

        ! set up definitions
        gnu_definitions(gnu_mmax) = 'with'

        ! get lines and points
        if(lines .and. points)then
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' linespoints'
        elseif(lines)then
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' lines'
        else
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' points'
        endif

        ! get the line color
        if(present(color))then
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' lc rgb "'//adjustl(trim(color))//'"'
        else
            if(gnu_mmax == 1)then
                gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' lc rgb "blue"'
            else
                write(gnu_definitions(gnu_mmax), '(a,i4)')trim(gnu_definitions(gnu_mmax))//' lc ', gnu_mmax-1
            endif
        endif        

        ! get line width
        if(lines)then
            if(present(linewidth)) then
                write(gnu_definitions(gnu_mmax), '(a,f8.2)')trim(gnu_definitions(gnu_mmax))//' lw ', max(linewidth, 0.0_dp)
            else
                write(gnu_definitions(gnu_mmax), '(a,f8.2)')trim(gnu_definitions(gnu_mmax))//' lw ', 2.0_dp
            endif
            
            if(present(dashtype)) then
                write(gnu_definitions(gnu_mmax), '(a,f8.2)')trim(gnu_definitions(gnu_mmax))//' dashtype "'//adjustl(trim(dashtype))//'"'
            endif
        endif

        ! get marker definition
        if(points)then
          
            if(present(marker)) then
                write(gnu_definitions(gnu_mmax), '(a,i2)')trim(gnu_definitions(gnu_mmax))//' pt ', min(marker, 13)
            else
                write(gnu_definitions(gnu_mmax), '(a,i2)')trim(gnu_definitions(gnu_mmax))//' pt ', 1
            endif

            if(present(markersize))then
                write(gnu_definitions(gnu_mmax), '(a,f8.2)')trim(gnu_definitions(gnu_mmax))//' ps ', max(markersize, 0.0_dp)
            else
                write(gnu_definitions(gnu_mmax), '(a,f8.2)')trim(gnu_definitions(gnu_mmax))//' ps ', 1.0_dp
            endif
        endif               

        ! set the legend
        if(present(legend))then
            gnu_dolegend = .true.
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' title "'//adjustl(trim(legend))//'"'
        else
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' notitle '
        endif

    end subroutine


    !##############################################################################
    ! SUBROUTINE plot_hist_old
    !
    ! Plots a histogram.
    !##############################################################################
    subroutine plot_hist_old(xin, yin, color, legend)

        implicit none


        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! input variables
        real(dp), intent(in) :: xin(:), yin(:)
        
        ! histogram color
        character(LEN=*), optional :: color

        ! legend description
        character(LEN=*), optional :: legend


        !##### OTHER VARIABLES ####################################################
 
        integer(int32) :: n, i1
     
     
        !##### ROUTINE CODE #######################################################

        n = assert_eq(size(xin, 1), size(yin, 1), 'plot')

        ! generate the respective plot data
        if(.not.gnu_addtoplot)then

            ! allocate new x array
            if(allocated(gnu_x))deallocate(gnu_x)
            allocate(gnu_x(n, 1))
            gnu_x(:, 1) = xin  

            ! allocate new y array
            if(allocated(gnu_y))deallocate(gnu_y)
            allocate(gnu_y(n, 1))
            gnu_y(:, 1) = yin  

            ! allocate new x_temp array
            if(allocated(gnu_x_temp))deallocate(gnu_x_temp)
            allocate(gnu_x_temp(n, 1))
            gnu_x_temp(:, 1) = xin  

            ! allocate new y_temp array
            if(allocated(gnu_y_temp))deallocate(gnu_y_temp)
            allocate(gnu_y_temp(n, 1))
            gnu_y_temp(:, 1) = yin

            gnu_addtoplot = .true.
            gnu_nmax = n
            gnu_mmax = 1

        else
    
            ! get new number of lines
            gnu_mmax = gnu_mmax+1
            if(gnu_mmax > 1000)then
                write(*,'(/a/)')'SORRY: I CANNOT PLOT MORE THAN 1000 LINES'
                return
            endif

            ! deallocate arrays
            deallocate(gnu_x)
            deallocate(gnu_y)
            
            ! if the new array is the longer one
            if(n > gnu_nmax)then
                allocate(gnu_x(n, gnu_mmax))
                allocate(gnu_y(n, gnu_mmax))

                gnu_x(1:gnu_nmax, 1:gnu_mmax-1) = gnu_x_temp                
                gnu_y(1:gnu_nmax, 1:gnu_mmax-1) = gnu_y_temp

                ! fill up with the same values
                do i1 = gnu_nmax+1, n
                    gnu_x(i1, 1:gnu_mmax-1) = gnu_x(gnu_nmax, 1:gnu_mmax-1)
                    gnu_y(i1, 1:gnu_mmax-1) = gnu_y(gnu_nmax, 1:gnu_mmax-1)
                enddo

                gnu_x(:, gnu_mmax) = xin
                gnu_y(:, gnu_mmax) = yin

                ! set new nmax
                gnu_nmax = n

            ! if the old array is the longer one
            else

                allocate(gnu_x(gnu_nmax, gnu_mmax))
                allocate(gnu_y(gnu_nmax, gnu_mmax))

                gnu_x(:, 1:gnu_mmax-1) = gnu_x_temp                
                gnu_y(:, 1:gnu_mmax-1) = gnu_y_temp                

                gnu_x(1:n, gnu_mmax) = xin
                gnu_y(1:n, gnu_mmax) = yin

                ! fill up with same values
                do i1 = n+1, gnu_nmax
                    gnu_x(i1, gnu_mmax) = gnu_x(n, gnu_mmax)
                    gnu_y(i1, gnu_mmax) = gnu_y(n, gnu_mmax)
                enddo                
            endif

            deallocate(gnu_x_temp)
            allocate(gnu_x_temp(size(gnu_x,1), size(gnu_x,2)))
            gnu_x_temp = gnu_x

            deallocate(gnu_y_temp)
            allocate(gnu_y_temp(size(gnu_y,1), size(gnu_y,2)))
            gnu_y_temp = gnu_y
                
        endif        

        ! set up definitions
        gnu_definitions(gnu_mmax) = 'with boxes'

        ! get the line color
        if(present(color))then
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' lc rgb "'//adjustl(trim(color))//'"'
        else
            write(gnu_definitions(gnu_mmax), '(a,i4)')trim(gnu_definitions(gnu_mmax))//' lc ', gnu_mmax
        endif             

        ! set the legend
        if(present(legend))then
            gnu_dolegend = .true.
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' title "'//adjustl(trim(legend))//'"'
        else
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' notitle '
        endif

        gnu_histogram = .true.

    end subroutine
    
    
    !##############################################################################
    ! SUBROUTINE plot_hist_new
    !
    ! Plots a histogram.
    !##############################################################################
    subroutine plot_hist_new(xvalues, nbins, left, right, incl_outside, absolute, color, legend)

        implicit none


        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! input variables
        real(dp), intent(in) :: xvalues(1:)

        ! number of bins
        integer(int32), intent(in) :: nbins

        ! left end of the bins
        real(dp), optional :: left

        ! right end of the bins
        real(dp), optional :: right

        ! should observations lying outside of the bins be included?
        logical, optional :: incl_outside

        ! absolute of relative values on y-axis
        logical, optional :: absolute

        ! histogram color
        character(LEN=*), optional :: color

        ! legend description
        character(LEN=*), optional :: legend


        !##### OTHER VARIABLES ####################################################
 
        integer(int32) :: n, i1
        real(dp) :: lower, upper, total
        real(dp) :: thresholds(0:nbins), freq(0:nbins)
        real(dp) :: xin(nbins), yin(nbins)
        
     
     
        !##### ROUTINE CODE #######################################################

        n = nbins
        
        ! get lower and upper endpoint as well as width
        lower = minval(xvalues)
        if(present(left))lower = left
        
        upper = maxval(xvalues)
        if(present(right))upper = right
        
        ! get thresholds
        call grid_Cons_Equi(thresholds, lower, upper)
        
        ! get frequency
        freq(0) = real(count(xvalues < lower), dp)
        do i1 = 1, nbins
            freq(i1) = real(count(xvalues <= thresholds(i1)), dp)
        enddo
        total = freq(nbins) - freq(0)
        
        if(present(incl_outside))then
            if(incl_outside)then
                freq(0) = 0.0_dp
                freq(nbins) = freq(nbins) + real(count(xvalues >= upper), dp)
                total = freq(nbins)
            endif
        endif
        
        ! get histogram data
        do i1 = 1, nbins
            xin(i1) = (thresholds(i1-1) + thresholds(i1))/2.0_dp
            yin(i1) = (freq(i1) - freq(i1-1))
        enddo
        
        if(present(absolute))then
            if(.not. absolute)then
                yin = yin/total
            endif
        else
            yin = yin/total
        endif

        ! generate the respective plot data
        if(.not.gnu_addtoplot)then

            ! allocate new x array
            if(allocated(gnu_x))deallocate(gnu_x)
            allocate(gnu_x(n, 1))
            gnu_x(:, 1) = xin  

            ! allocate new y array
            if(allocated(gnu_y))deallocate(gnu_y)
            allocate(gnu_y(n, 1))
            gnu_y(:, 1) = yin  

            ! allocate new x_temp array
            if(allocated(gnu_x_temp))deallocate(gnu_x_temp)
            allocate(gnu_x_temp(n, 1))
            gnu_x_temp(:, 1) = xin  

            ! allocate new y_temp array
            if(allocated(gnu_y_temp))deallocate(gnu_y_temp)
            allocate(gnu_y_temp(n, 1))
            gnu_y_temp(:, 1) = yin

            gnu_addtoplot = .true.
            gnu_nmax = n
            gnu_mmax = 1

        else
    
            ! get new number of lines
            gnu_mmax = gnu_mmax+1
            if(gnu_mmax > 1000)then
                write(*,'(/a/)')'SORRY: I CANNOT PLOT MORE THAN 1000 LINES'
                return
            endif

            ! deallocate arrays
            deallocate(gnu_x)
            deallocate(gnu_y)
            
            ! if the new array is the longer one
            if(n > gnu_nmax)then
                allocate(gnu_x(n, gnu_mmax))
                allocate(gnu_y(n, gnu_mmax))

                gnu_x(1:gnu_nmax, 1:gnu_mmax-1) = gnu_x_temp                
                gnu_y(1:gnu_nmax, 1:gnu_mmax-1) = gnu_y_temp

                ! fill up with the same values
                do i1 = gnu_nmax+1, n
                    gnu_x(i1, 1:gnu_mmax-1) = gnu_x(gnu_nmax, 1:gnu_mmax-1)
                    gnu_y(i1, 1:gnu_mmax-1) = gnu_y(gnu_nmax, 1:gnu_mmax-1)
                enddo

                gnu_x(:, gnu_mmax) = xin
                gnu_y(:, gnu_mmax) = yin

                ! set new nmax
                gnu_nmax = n

            ! if the old array is the longer one
            else

                allocate(gnu_x(gnu_nmax, gnu_mmax))
                allocate(gnu_y(gnu_nmax, gnu_mmax))

                gnu_x(:, 1:gnu_mmax-1) = gnu_x_temp                
                gnu_y(:, 1:gnu_mmax-1) = gnu_y_temp                

                gnu_x(1:n, gnu_mmax) = xin
                gnu_y(1:n, gnu_mmax) = yin

                ! fill up with same values
                do i1 = n+1, gnu_nmax
                    gnu_x(i1, gnu_mmax) = gnu_x(n, gnu_mmax)
                    gnu_y(i1, gnu_mmax) = gnu_y(n, gnu_mmax)
                enddo                
            endif

            deallocate(gnu_x_temp)
            allocate(gnu_x_temp(size(gnu_x,1), size(gnu_x,2)))
            gnu_x_temp = gnu_x

            deallocate(gnu_y_temp)
            allocate(gnu_y_temp(size(gnu_y,1), size(gnu_y,2)))
            gnu_y_temp = gnu_y
                
        endif        

        ! set up definitions
        gnu_definitions(gnu_mmax) = 'with boxes'

        ! get the line color
        if(present(color))then
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' lc rgb "'//adjustl(trim(color))//'"'
        else
            write(gnu_definitions(gnu_mmax), '(a,i4)')trim(gnu_definitions(gnu_mmax))//' lc ', gnu_mmax
        endif             

        ! set the legend
        if(present(legend))then
            gnu_dolegend = .true.
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' title "'//adjustl(trim(legend))//'"'
        else
            gnu_definitions(gnu_mmax) = trim(gnu_definitions(gnu_mmax))//' notitle '
        endif

        gnu_histogram = .true.

    end subroutine


    !##############################################################################
    ! SUBROUTINE plot3d_grid
    !
    ! Plots a x-y-z-data column on a grid. Plot will be executed immediately.
    !
    ! Credits to Patrick Wiesmann for providing a first version of a 3d plotting 
    !     subroutine during his research assistantship.
    !##############################################################################
    subroutine plot3d_grid(xin, yin, zin, color, linewidth, dashtype, marker, markersize, noline, &
                xlim, xticks, xtrange, xlabel, ylim, yticks, ylabel, ytrange, zlim, zticks, ztrange, zlevel, zlabel, & 
                surf, surf_color, transparent, view, title, ratio, nogrid, nodisplay, filename, filetype, output)

        implicit none


        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! input variables
        real(dp), intent(in) :: xin(:), yin(:), zin(:,:)

        ! line color
        character(LEN=*), optional :: color

        ! width of the plotting line
        real(dp), optional :: linewidth

        ! dashtype of the plotting line
        character(LEN=*), optional :: dashtype

        ! marker definition
        integer(int32), optional :: marker

        ! size of the marker
        real(dp), optional :: markersize

        ! if no line should be plotted
        logical, optional :: noline

        ! x axis definition
        real(dp), optional :: xlim(2)

        ! x axis tick definitions
        real(dp), optional :: xticks

        ! x axis range for ticks
        real(dp), optional :: xtrange(2)

        ! y axis definition
        real(dp), optional :: ylim(2)

        ! y axis tick definitions
        real(dp), optional :: yticks

        ! y axis range for ticks
        real(dp), optional :: ytrange(2)

           ! z axis definition
        real(dp), optional :: zlim(2)

        ! z axis tick definitions
        real(dp), optional :: zticks

        ! x axis range for ticks
        real(dp), optional :: ztrange(2)

        ! point where the z axis is placed
        real(dp), optional :: zlevel

        ! rotates the graph
        real(dp), optional :: view(2)

        ! colored surface or not
        logical, optional :: surf

        ! color of the surface
        integer(int32), optional :: surf_color

        ! see through surface or not
        logical, optional :: transparent

        ! output file name
        character(LEN=*), optional :: xlabel

        ! output file name
        character(LEN=*), optional :: ylabel

        ! output file name
        character(LEN=*), optional :: zlabel
        
        ! output file name
        character(LEN=*), optional :: title

        ! y-to-x axis ratio
        real(dp), optional :: ratio

        ! print no grid
        logical, optional :: nogrid
        
        ! should plot windows be shown or not
        logical, optional :: nodisplay

        ! output file name
        character(LEN=*), optional :: filename

        ! file type
        character(LEN=*), optional :: filetype

        ! output file name
        character(LEN=*), optional :: output


        !##### OTHER VARIABLES ####################################################
        integer(int32) :: u_data, u_cmd, ios
 
        logical :: lines, points, nw_copy
        character(LEN = 2000) :: definition 
        integer(int32) :: i1, i2, n1, n2
        character(LEN=3) :: ft, plotnumber
        character(LEN=150) :: cfile, dfile, pfile, term
     
     
        !##### ROUTINE CODE #######################################################

        ! assert that the sizes are coorect
        n1 = assert_eq(size(xin, 1), size(zin, 1), 'plot3d')
        n2 = assert_eq(size(yin, 1), size(zin, 2), 'plot3d')

        ! check for lines and points
        lines = .true.
        if(present(noline))then
            if(noline)lines = .false.
        endif

        if(.not.lines)then
            points = .true.
        else
            points = .false.
        endif
        if(present(marker))points = .true.

        ! check for whether to display graph
        nw_copy = .false.
        if(present(nodisplay))then
            if(nodisplay)nw_copy = .true.
        endif
        
        if(access('/usr/local/include/toolbox_visual.txt', ' ') == 0)then
            nw_copy = .true.
        endif

        ! set up definitions
        definition = 'with'

        ! get lines and points
        if(lines .and. points)then
            definition = trim(definition)//' linespoints'
        elseif(lines)then
            definition = trim(definition)//' lines'
        else
            definition = trim(definition)//' points'
        endif

        ! get the line color
        if(present(color))then
            definition = trim(definition)//' lc rgb "'//adjustl(trim(color))//'"'
        else
            definition = trim(definition)//' lc rgb "blue"'
        endif        

        ! get line width
        if(lines)then
            if(present(linewidth)) then
                write(definition, '(a,f8.2)')trim(definition)//' lw ', max(linewidth, 0.0_dp)
            else
                if(present(surf))then
                    write(definition, '(a,f8.2)')trim(definition)//' lw ', 0.3_dp
                else
                    write(definition, '(a,f8.2)')trim(definition)//' lw ', 1.0_dp
                endif
            endif

            if(present(dashtype)) then
                write(definition, '(a,f8.2)')trim(definition)//' dashtype "'//adjustl(trim(dashtype))//'"'
            endif
        endif

        ! get marker definition
        if(points)then
          
            if(present(marker)) then
                write(definition, '(a,i2)')trim(definition)//' pt ', min(marker, 13)
            else
                write(definition, '(a,i2)')trim(definition)//' pt ', 1
            endif

            if(present(markersize))then
                write(definition, '(a,f8.2)')trim(definition)//' ps ', max(markersize, 0.0_dp)
            else
                write(definition, '(a,f8.2)')trim(definition)//' ps ', 1.0_dp
            endif
        endif               

        ! set the legend
        definition = trim(definition)//' notitle '

        ! NOW DIRECTLY CREATE PLOT

        ! create output files
        if(present(output))then
            cfile = trim(output)//'_c.dat'
            dfile = trim(output)//'_d.dat'
        else
            ! increment gnu_plotnumber by 1 (restart counting after 999 plots)
            gnu_plotnumber = gnu_plotnumber + 1
            if(gnu_plotnumber >= 1000) gnu_plotnumber = 1

            if(gnu_plotnumber < 10)then
                write(plotnumber,'(i1)')gnu_plotnumber
            elseif(gnu_plotnumber < 100)then
                write(plotnumber,'(i2)')gnu_plotnumber
            else
                write(plotnumber,'(i3)')gnu_plotnumber
            endif
            cfile = 'gnufigure_'//trim(plotnumber)//'_c.dat'
            dfile = 'gnufigure_'//trim(plotnumber)//'_d.dat'
        endif

        ! create plot file name
        if(present(filename))then
            pfile = trim(filename)
        else
            pfile = 'gnufigure_'//trim(plotnumber)
        endif

        ! write the output data file
        open(newunit=u_data, file=trim(dfile), status='replace', action='write', iostat=ios)
        if(ios /= 0) call error('plot3d_grid', 'Failed to open data file: '//trim(dfile))
        do i1 = 1, n1
            do i2 = 1, n2
                write(u_data, '(3e21.10e4)')xin(i1), yin(i2), zin(i1, i2)
             enddo
            write(u_data,*)            
        enddo
        close(u_data)

        ! write the command file
        open(newunit=u_cmd, file=trim(cfile), status='replace', action='write', iostat=ios)
        if(ios /= 0) call error('plot3d_grid', 'Failed to open command file: '//trim(cfile))

        ! set file or terminal
        if(nw_copy)then
        
            ! write graph to file            
            ft = "pdf"
            term = "pdfcairo"
            if(present(filetype))then
                if(filetype(1:3) == "png")then
                    ft=  "png"
                    term = "pngcairo"
                endif
                if(filetype(1:3) == "eps")then
                    ft = "eps"
                    term = "epscairo"
                endif
            endif
            
            if(present(ratio) .and. ft /= "png")then
                write(term, '(a,f4.2)')trim(term)//" size 5,", 5.0_dp*ratio
            endif
            
            write(u_cmd, *)
            write(u_cmd, '(a)')'set terminal '//trim(term)//'  color enhanced fontscale 0.7'
            write(u_cmd, '(a)')'set output "'//trim(pfile)//'.'//ft//'"'

            write(*,'(/a/)')'CREATING PLOT '//trim(pfile)//'.'//ft
        else
        
            ! print graph to terminal

            write(u_cmd, '(a)')'if (strstrt(GPVAL_TERMINALS, "wxt") > 0) {'
            write(u_cmd, '(a)')'    set terminal wxt title "Gnuplot" enhanced'
            write(u_cmd, '(a)')'} else {'
            write(u_cmd, '(a)')'    if (strstrt(GPVAL_TERMINALS, "x11") > 0) {'
            write(u_cmd, '(a)')'        set terminal x11 enhanced'
            write(u_cmd, '(a)')'    } else {'
            write(u_cmd, '(a)')'        if (strstrt(GPVAL_TERMINALS, "qt") > 0) {'
            write(u_cmd, '(a)')'            set terminal qt enhanced'
            write(u_cmd, '(a)')'        } else {'
            write(u_cmd, '(a)')'            print ""'
            write(u_cmd, '(a)')'            print "ATTENTION: There seems to be NO VALID TERMINAL installed in "'
            write(u_cmd, '(a)')'            print "           your version of gnuplot. It might be a good idea to "'
            write(u_cmd, '(a)')'            print "           completely uninstall your Fortran/gnuplot/geany installation"'
            write(u_cmd, '(a)')'            print "           using the uninstallation files on www.ce-fortran.com. "'
            write(u_cmd, '(a)')'            print "           Afterwards you should reinstall the Fortran system again."'
            write(u_cmd, '(a)')'            print "           If this does not help, please refer to the forum."'
            write(u_cmd, '(a)')'            print ""'
            write(u_cmd, '(a)')'            q'
            write(u_cmd, '(a)')'        }'
            write(u_cmd, '(a)')'    }'
            write(u_cmd, '(a)')'}'
        endif
        
        ! set the grid definition
        if(present(nogrid))then
            if(nogrid)then
                write(u_cmd, '(a)')'unset grid'
            else
                write(u_cmd, '(a)')'set grid'
            endif
        else
            write(u_cmd, '(a)')'set grid'
        endif
        
        if(present(zlevel)) then
            write(u_cmd, '(a,e13.5)')'set ticslevel ', -min(max(zlevel, 0.0_dp), 1.0_dp)
        else 
            write(u_cmd, '(a,e13.5)')'set ticslevel 0'
        endif
        
        if(present(surf)) then
            if(surf) then
                write(u_cmd, '(a)')'set pm3d'
                if(present(surf_color)) then
                    select case (surf_color)
                        case(1)
                             write(u_cmd,'(a)')'set palette gray'
                        case(2)
                            write(u_cmd,'(a)')'set palette rgbformulae 33,13,10'
                        case(3)
                            write(u_cmd,'(a)')'set palette rgbformulae 3,11,6'
                        case(4)
                            write(u_cmd,'(a)')'set palette rgbformulae 23,28,3'
                        case(5)
                            write(u_cmd, '(a)')'set palette rgbformulae 21,22,23'
                        case default
                            write(u_cmd, '(a)')'set palette rgbformulae 7,5,15'
                        end select
                endif
            endif
        endif

        if(present(transparent)) then
            if(.not. transparent) then
                write(u_cmd, '(a)')'set hidden3d'
            endif
        endif
        
        if(present(view)) then
            write(u_cmd, '(a,e13.5,a,e13.5)')'set view ', min(max(view(1), 0.0_dp), 360.0_dp), &
                ',',min(max(view(2), 0.0_dp), 360.0_dp)
        endif
          
        ! set x axis
        if(present(xlim))write(u_cmd, '(a,e13.5,a,e13.5,a)')'set xrange [',minval(xlim),':',maxval(xlim),']'
        if(present(xticks))then
            if(present(xtrange))then
                write(u_cmd, '(a,e13.5,a,e13.5,a,e13.5)')'set xtics ',xtrange(1),',', xticks,',', xtrange(2)
            else
                write(u_cmd, '(a,e13.5)')'set xtics ',xticks
            endif
        endif
        if(present(xlabel))write(u_cmd, '(a)')'set xlabel "'//xlabel//'"font ",12"'
        
        ! set y axis
        if(present(ylim))write(u_cmd, '(a,e13.5,a,e13.5,a)')'set yrange [',minval(ylim),':',maxval(ylim),']'
        if(present(yticks))then
            if(present(ytrange))then
                write(u_cmd, '(a,e13.5,a,e13.5,a,e13.5)')'set ytics ',ytrange(1),',', yticks,',', ytrange(2)
            else
                write(u_cmd, '(a,e13.5)')'set ytics ',yticks
            endif
        endif
        if(present(ylabel))write(u_cmd, '(a)')'set ylabel "'//ylabel//'"font ",12"'

        ! set z axis
        if(present(zlim))write(u_cmd, '(a,e13.5,a,e13.5,a)')'set zrange [',minval(zlim),':',maxval(zlim),']'
        if(present(zticks))then
            if(present(ztrange))then
                write(u_cmd, '(a,e13.5,a,e13.5,a,e13.5)')'set ztics ',ztrange(1),',', zticks,',', ztrange(2)
            else
                write(u_cmd, '(a,e13.5)')'set ztics ',zticks
            endif
        endif       
        if(present(zlabel))write(u_cmd, '(a)')'set zlabel "'//zlabel//'"font ",12"'

        ! set title
        if(present(title))write(u_cmd, '(a)')'set title "'//title//'" font ",16"'
        
        ! legend statement
        write(u_cmd, '(a)')'unset key'

        ! write plot lines for 3D-data
        write(u_cmd, '(a)')'splot "'//trim(dfile)//'" using 1:2:3 '//trim(definition)
                        
        if(.not. nw_copy)write(u_cmd, '(a)')'pause mouse close'

        ! write graph to file
        if(.not. nw_copy)then
            if(present(filename))then                
                ft = "pdf"
                term = "pdfcairo"
                if(present(filetype))then
                    if(filetype(1:3) == "png")then
                        ft=  "png"
                        term = "pngcairo"
                    endif
                    if(filetype(1:3) == "eps")then
                        ft = "eps"
                        term = "epscairo"
                    endif
                endif
                
                if(present(ratio) .and. ft /= "png")then
                    write(term, '(a,f4.2)')trim(term)//" size 5,", 5.0_dp*ratio
                endif
                
                write(u_cmd, *)
                write(u_cmd, '(a)')'set terminal '//trim(term)//'  color enhanced fontscale 0.7'
                write(u_cmd, '(a)')'set output "'//trim(pfile)//'.'//ft//'"'
                write(u_cmd, '(a)')'replot'
            endif
        endif
        
        write(u_cmd, '(a)')'q'
        close(u_cmd)
        
        call system('gnuplot "'//trim(cfile)//'"') 
        if(.not.present(output))then
            open(newunit=u_data, file=trim(dfile), iostat=ios)
            if(ios == 0) close(u_data, status='delete')
            open(newunit=u_cmd, file=trim(cfile), iostat=ios)
            if(ios == 0) close(u_cmd, status='delete')
        endif
        
    end subroutine


    !##############################################################################
    ! SUBROUTINE plot3d_line
    !
    ! Plots a x-y-z-data column with random points. Plot will be executed immediately.
    !
    ! Credits to Patrick Wiesmann for providing a first version of a 3d plotting 
    !     subroutine during his research assistantship.
    !##############################################################################
    subroutine plot3d_line(xin, yin, zin, color, linewidth, dashtype, marker, markersize, noline, &
                xlim, xticks, xtrange, xlabel, ylim, yticks, ylabel, ytrange, zlim, zticks, ztrange, zlevel, zlabel, & 
                view, title, ratio, nogrid, nodisplay, filename, filetype, output)

        implicit none


        !##### INPUT/OUTPUT VARIABLES #############################################
        
        ! input variables
        real(dp), intent(in) :: xin(:), yin(:), zin(:)

        ! line and marker color
        character(LEN=*), optional :: color

        ! width of the plotting line
        real(dp), optional :: linewidth

        ! dashtype of the plotting line
        character(LEN=*), optional :: dashtype

        ! marker definition
        integer(int32), optional :: marker

        ! size of the marker
        real(dp), optional :: markersize

        ! if no line should be plotted
        logical, optional :: noline

        ! x axis definition
        real(dp), optional :: xlim(2)

        ! x axis tick definitions
        real(dp), optional :: xticks

        ! x axis range for ticks
        real(dp), optional :: xtrange(2)

        ! y axis definition
        real(dp), optional :: ylim(2)

        ! y axis tick definitions
        real(dp), optional :: yticks

        ! y axis range for ticks
        real(dp), optional :: ytrange(2)

           ! z axis definition
        real(dp), optional :: zlim(2)

        ! z axis tick definitions
        real(dp), optional :: zticks

        ! x axis range for ticks
        real(dp), optional :: ztrange(2)

        ! point where the z axis is placed
        real(dp), optional :: zlevel

        ! rotates the graph
        real(dp), optional :: view(2)

        ! output file name
        character(LEN=*), optional :: xlabel

        ! output file name
        character(LEN=*), optional :: ylabel

        ! output file name
        character(LEN=*), optional :: zlabel
        
        ! output file name
        character(LEN=*), optional :: title

        ! y-to-x axis ratio
        real(dp), optional :: ratio

        ! print no grid
        logical, optional :: nogrid
        
        ! should plot windows be shown or not
        logical, optional :: nodisplay

        ! output file name
        character(LEN=*), optional :: filename

        ! file type
        character(LEN=*), optional :: filetype

        ! output file name
        character(LEN=*), optional :: output


        !##### OTHER VARIABLES ####################################################
        integer(int32) :: u_data, u_cmd, ios
 
        logical :: lines, points, nw_copy
        character(LEN = 2000) :: definition 
        integer(int32) :: i1, n
        character(LEN=3) :: ft, plotnumber
        character(LEN=150) :: cfile, dfile, pfile, term
     
     
        !##### ROUTINE CODE #######################################################

        ! assert that the sizes are coorect
        n = assert_eq(size(xin, 1), size(yin, 1), size(zin, 1), 'plot3d')

        ! check for lines and points
        lines = .true.
        if(present(noline))then
            if(noline)lines = .false.
        endif

        if(.not.lines)then
            points = .true.
        else
            points = .false.
        endif
        if(present(marker))points = .true.

        ! check for whether to display graph
        nw_copy = .false.
        if(present(nodisplay))then
            if(nodisplay)nw_copy = .true.
        endif
        
        if(access('/usr/local/include/toolbox_visual.txt', ' ') == 0)then
            nw_copy = .true.
        endif

        ! set up definitions
        definition = 'with'

        ! get lines and points
        if(lines .and. points)then
            definition = trim(definition)//' linespoints'
        elseif(lines)then
            definition = trim(definition)//' lines'
        else
            definition = trim(definition)//' points'
        endif

        ! get the line color
        if(present(color))then
            definition = trim(definition)//' lc rgb "'//adjustl(trim(color))//'"'
        else
            definition = trim(definition)//' lc rgb "blue"'
        endif        

        ! get line width
        if(lines)then
            if(present(linewidth)) then
                write(definition, '(a,f8.2)')trim(definition)//' lw ', max(linewidth, 0.0_dp)
            else
                write(definition, '(a,f8.2)')trim(definition)//' lw ', 1.0_dp
            endif

            if(present(dashtype)) then
                write(definition, '(a,f8.2)')trim(definition)//' dashtype "'//adjustl(trim(dashtype))//'"'
            endif
        endif

        ! get marker definition
        if(points)then
          
            if(present(marker)) then
                write(definition, '(a,i2)')trim(definition)//' pt ', min(marker, 13)
            else
                write(definition, '(a,i2)')trim(definition)//' pt ', 1
            endif

            if(present(markersize))then
                write(definition, '(a,f8.2)')trim(definition)//' ps ', max(markersize, 0.0_dp)
            else
                write(definition, '(a,f8.2)')trim(definition)//' ps ', 1.0_dp
            endif
        endif               

        ! set the legend
        definition = trim(definition)//' notitle '

        ! NOW DIRECTLY CREATE PLOT

        ! create output files
        if(present(output))then
            cfile = trim(output)//'_c.dat'
            dfile = trim(output)//'_d.dat'
        else
            ! increment gnu_plotnumber by 1 (restart counting after 999 plots)
            gnu_plotnumber = gnu_plotnumber + 1
            if(gnu_plotnumber >= 1000) gnu_plotnumber = 1

            if(gnu_plotnumber < 10)then
                write(plotnumber,'(i1)')gnu_plotnumber
            elseif(gnu_plotnumber < 100)then
                write(plotnumber,'(i2)')gnu_plotnumber
            else
                write(plotnumber,'(i3)')gnu_plotnumber
            endif
            cfile = 'gnufigure_'//trim(plotnumber)//'_c.dat'
            dfile = 'gnufigure_'//trim(plotnumber)//'_d.dat'
        endif

        ! create plot file name
        if(present(filename))then
            pfile = trim(filename)
        else
            pfile = 'gnufigure_'//trim(plotnumber)
        endif

        ! write the output data file
        open(newunit=u_data, file=trim(dfile), status='replace', action='write', iostat=ios)
        if(ios /= 0) call error('plot3d_line', 'Failed to open data file: '//trim(dfile))
        do i1 = 1, n
            write(u_data, '(3e21.10e4)')xin(i1), yin(i1), zin(i1)
        enddo
        close(u_data)

        ! write the command file
        open(newunit=u_cmd, file=trim(cfile), status='replace', action='write', iostat=ios)
        if(ios /= 0) call error('plot3d_line', 'Failed to open command file: '//trim(cfile))

        ! set file or terminal
        if(nw_copy)then
        
            ! write graph to file            
            ft = "pdf"
            term = "pdfcairo"
            if(present(filetype))then
                if(filetype(1:3) == "png")then
                    ft=  "png"
                    term = "pngcairo"
                endif
                if(filetype(1:3) == "eps")then
                    ft = "eps"
                    term = "epscairo"
                endif
            endif
            
            if(present(ratio) .and. ft /= "png")then
                write(term, '(a,f4.2)')trim(term)//" size 5,", 5.0_dp*ratio
            endif
            
            write(u_cmd, *)
            write(u_cmd, '(a)')'set terminal '//trim(term)//'  color enhanced fontscale 0.7'
            write(u_cmd, '(a)')'set output "'//trim(pfile)//'.'//ft//'"'

            write(*,'(/a/)')'CREATING PLOT '//trim(pfile)//'.'//ft
        else
        
            ! print graph to terminal

            write(u_cmd, '(a)')'if (strstrt(GPVAL_TERMINALS, "wxt") > 0) {'
            write(u_cmd, '(a)')'    set terminal wxt title "Gnuplot" enhanced'
            write(u_cmd, '(a)')'} else {'
            write(u_cmd, '(a)')'    if (strstrt(GPVAL_TERMINALS, "x11") > 0) {'
            write(u_cmd, '(a)')'        set terminal x11 enhanced'
            write(u_cmd, '(a)')'    } else {'
            write(u_cmd, '(a)')'        if (strstrt(GPVAL_TERMINALS, "qt") > 0) {'
            write(u_cmd, '(a)')'            set terminal qt enhanced'
            write(u_cmd, '(a)')'        } else {'
            write(u_cmd, '(a)')'            print ""'
            write(u_cmd, '(a)')'            print "ATTENTION: There seems to be NO VALID TERMINAL installed in "'
            write(u_cmd, '(a)')'            print "           your version of gnuplot. It might be a good idea to "'
            write(u_cmd, '(a)')'            print "           completely uninstall your Fortran/gnuplot/geany installation"'
            write(u_cmd, '(a)')'            print "           using the uninstallation files on www.ce-fortran.com. "'
            write(u_cmd, '(a)')'            print "           Afterwards you should reinstall the Fortran system again."'
            write(u_cmd, '(a)')'            print "           If this does not help, please refer to the forum."'
            write(u_cmd, '(a)')'            print ""'
            write(u_cmd, '(a)')'            q'
            write(u_cmd, '(a)')'        }'
            write(u_cmd, '(a)')'    }'
            write(u_cmd, '(a)')'}'
        endif
        
        ! set the grid definition
        if(present(nogrid))then
            if(nogrid)then
                write(u_cmd, '(a)')'unset grid'
            else
                write(u_cmd, '(a)')'set grid'
            endif
        else
            write(u_cmd, '(a)')'set grid'
        endif
        
        if(present(zlevel)) then
            write(u_cmd, '(a,e13.5)')'set ticslevel ', -min(max(zlevel, 0.0_dp), 1.0_dp)
        else 
            write(u_cmd, '(a,e13.5)')'set ticslevel 0'
        endif
               
        if(present(view)) then
            write(u_cmd, '(a,e13.5,a,e13.5)')'set view ', min(max(view(1), 0.0_dp), 360.0_dp), &
                ',',min(max(view(2), 0.0_dp), 360.0_dp)
        endif
          
        ! set x axis
        if(present(xlim))write(u_cmd, '(a,e13.5,a,e13.5,a)')'set xrange [',minval(xlim),':',maxval(xlim),']'
        if(present(xticks))then
            if(present(xtrange))then
                write(u_cmd, '(a,e13.5,a,e13.5,a,e13.5)')'set xtics ',xtrange(1),',', xticks,',', xtrange(2)
            else
                write(u_cmd, '(a,e13.5)')'set xtics ',xticks
            endif
        endif
        if(present(xlabel))write(u_cmd, '(a)')'set xlabel "'//xlabel//'"font ",12"'
        
        ! set y axis
        if(present(ylim))write(u_cmd, '(a,e13.5,a,e13.5,a)')'set yrange [',minval(ylim),':',maxval(ylim),']'
        if(present(yticks))then
            if(present(ytrange))then
                write(u_cmd, '(a,e13.5,a,e13.5,a,e13.5)')'set ytics ',ytrange(1),',', yticks,',', ytrange(2)
            else
                write(u_cmd, '(a,e13.5)')'set ytics ',yticks
            endif
        endif
        if(present(ylabel))write(u_cmd, '(a)')'set ylabel "'//ylabel//'"font ",12"'

        ! set z axis
        if(present(zlim))write(u_cmd, '(a,e13.5,a,e13.5,a)')'set zrange [',minval(zlim),':',maxval(zlim),']'
        if(present(zticks))then
            if(present(ztrange))then
                write(u_cmd, '(a,e13.5,a,e13.5,a,e13.5)')'set ztics ',ztrange(1),',', zticks,',', ztrange(2)
            else
                write(u_cmd, '(a,e13.5)')'set ztics ',zticks
            endif
        endif       
        if(present(zlabel))write(u_cmd, '(a)')'set zlabel "'//zlabel//'"font ",12"'

        ! set title
        if(present(title))write(u_cmd, '(a)')'set title "'//title//'" font ",16"'
        
        ! legend statement
        write(u_cmd, '(a)')'unset key'

        ! write plot lines for 3D-data
        write(u_cmd, '(a)')'splot "'//trim(dfile)//'" using 1:2:3 '//trim(definition)
                        
        if(.not. nw_copy)write(u_cmd, '(a)')'pause mouse close'

        ! write graph to file
        if(.not. nw_copy)then
            if(present(filename))then                
                ft = "pdf"
                term = "pdfcairo"
                if(present(filetype))then
                    if(filetype(1:3) == "png")then
                        ft=  "png"
                        term = "pngcairo"
                    endif
                    if(filetype(1:3) == "eps")then
                        ft = "eps"
                        term = "epscairo"
                    endif
                endif
                
                if(present(ratio) .and. ft /= "png")then
                    write(term, '(a,f4.2)')trim(term)//" size 5,", 5.0_dp*ratio
                endif
                
                write(u_cmd, *)
                write(u_cmd, '(a)')'set terminal '//trim(term)//'  color enhanced fontscale 0.7'
                write(u_cmd, '(a)')'set output "'//trim(pfile)//'.'//ft//'"'
                write(u_cmd, '(a)')'replot'
            endif
        endif
        
        write(u_cmd, '(a)')'q'
        close(u_cmd)
        
        call system('gnuplot "'//trim(cfile)//'"') 
        if(.not.present(output))then
            open(newunit=u_data, file=trim(dfile), iostat=ios)
            if(ios == 0) close(u_data, status='delete')
            open(newunit=u_cmd, file=trim(cfile), iostat=ios)
            if(ios == 0) close(u_cmd, status='delete')
        endif
        
    end subroutine











!##############################################################################


end module toolbox_gnuplot
