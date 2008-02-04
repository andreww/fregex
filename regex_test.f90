program regex_test

    use fexp

    implicit none

    character(len=1000) :: regex
    character(len=10000) :: text

    if (command_argument_count().eq.2) then
        call get_command_argument(1, regex)
        call get_command_argument(2, text)
        print*, match(trim(regex), trim(text))
        print*, "Match started at ", matchstart, " and was ",  & 
               & matchlength, " long"
    else
        stop("regex_test error: number of arguments should be exactly 2!")
    endif

end program regex_test
