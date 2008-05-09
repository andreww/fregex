module fexp_debug

     implicit none

     logical, save :: ldebug 
     integer, save :: un
     integer :: calldepth


     contains 

     subroutine debug_set(debug, filename, unitnumber)

         logical :: debug
         character(len=*), optional :: filename 
         integer, optional :: unitnumber

         integer :: ioern

         if (debug) then
            
            if (present(filename).and.present(unitnumber)) then
                 open (unit=unitnumber, iostat=ioern, file=filename, &
                       & action='readwrite', position='append', &
                       & status='unknown')
             else
                 stop 'API error in debug_set: filename and unitnumber needed'
             endif

             if (ioern.ne.0) stop 'Error opening debug file for append'
             un = unitnumber
             ldebug = .true.
             calldepth = 0
             write(un, '(a)') "Debug session starts..."
         else
             ldebug = .false.
         endif

     end subroutine debug_set

     subroutine debug_stop

         if (ldebug) then
             write(un, '(a)') "Debug session ends..."
             ldebug = .false.
             close(unit=un)
         endif

     end subroutine debug_stop


     subroutine proc_start(procname, procarg1, procarg2, &
                           & procarg3, procarg4, procarg5)

         character(len=*), intent(in) :: procname
         character(len=*), optional, intent(in) :: procarg1, procarg2, & 
                          & procarg3, procarg4, procarg5

         if (ldebug) then
             calldepth = calldepth + 1
             write(un, '(a)'), repeat('-',calldepth) // "> Start of " // procname
             if (present(procarg1)) then
                 write(un, '(a)'), repeat('-',calldepth) // "> arg1 " // procarg1
             endif
             if (present(procarg2)) then
                 write(un, '(a)'), repeat('-',calldepth) // "> arg2 " // procarg2
             endif
             if (present(procarg3)) then
                 write(un, '(a)'), repeat('-',calldepth) // "> arg3 " // procarg3
             endif
             if (present(procarg4)) then
                 write(un, '(a)'), repeat('-',calldepth) // "> arg4 " // procarg4
             endif
             if (present(procarg5)) then
                 write(un, '(a)'), repeat('-',calldepth) // "> arg5 " // procarg5
             endif
         endif

      end subroutine proc_start

      subroutine proc_end (procname, returnval)

         character(len=*), intent(in) :: procname
         character(len=*), optional, intent(in)  :: returnval

         if (ldebug) then
            if (present(returnval)) then
                 write(un, '(a)'), repeat('-',calldepth) // "> rv " // returnval
            endif
            write(un, '(a)'), repeat('-',calldepth) // "> End of " // procname
            calldepth = calldepth - 1
         endif
     end subroutine proc_end

end module fexp_debug
