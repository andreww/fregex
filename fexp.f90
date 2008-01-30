module fexp

 implicit none

contains

 logical function match(regexp, text)

    character(len=*), intent(in) :: regexp
    character(len=*), intent(in) :: text

    integer :: pos

    if (regexp(1:1).eq."^") then
        match = matchhere(regexp(2:len(regexp)), text)
    else

        do pos = 1, len(text)
            if ( matchhere(regexp, text(pos:len(text))) ) then
                match = .true.
                exit
            endif
            match = .false.
        enddo
    endif

  end function match

  logical recursive function matchhere(regexp, text) result(res)

    character(len=*), intent(in) :: regexp
    character(len=*), intent(in) :: text
    
    integer :: classend 

    if (len(regexp).eq.1) then 
        if (text(1:1).eq.regexp(1:1)) then
                res = .true.
        elseif (regexp(1:1).eq.".") then
                res = .true.
        elseif ((regexp(1:1).eq."$") &
                & .and.(len(text).eq.0)) then
                res = .true.
        else 
                res = .false.
        endif
    elseif (regexp(1:1).eq.'[') then
        classend = index(regexp(2:len(regexp)), ']')
        if (classend.eq.0) stop("No terminating char class")
        res = matchclass(regexp(2:classend), regexp(classend+2:len(regexp)), text)
    elseif (regexp(2:2).eq."*") then
        res = matchstar(regexp(1:1), & 
                     & regexp(3:len(regexp)), text)
    elseif (regexp(2:2).eq."+") then
        res = matchhere(regexp(1:1)//regexp(1:1)// & 
                     & '*'//regexp(3:len(regexp)), text)
    elseif (text(1:1).eq.regexp(1:1)) then
        res =  matchhere( regexp(2:len(regexp)), &
                     & text(2:len(text)) )
    elseif (regexp(1:1).eq.".") then
        res =  matchhere( regexp(2:len(regexp)), & 
                     &  text(2:len(text)) )
    else 
        res = .false.
    endif

  end function matchhere

  logical recursive function matchclass(class, regexp, text)

      character(len=*), intent(in) :: class
      character(len=*), intent(in) :: regexp
      character(len=*), intent(in) :: text

      if (scan(text,class).eq.1) then
          matchclass = matchhere(regexp, text)
      else
          matchclass = .false.
      endif

  end function matchclass

  logical recursive function matchstar(starchar, regexp, text) 

      character(len=1), intent(in) :: starchar
      character(len=*), intent(in) :: regexp
      character(len=*), intent(in) :: text

      integer :: pos
      
      pos = 1
      do 
          if (matchhere(regexp, text)) then
              matchstar = .true.
              exit
          elseif ((pos.le.len(text)) &
                  & .and. ( &
                  &    (text(pos:pos).eq.starchar) & 
                  &    .or.(starchar.eq.".") & 
                  & )) then
              pos = pos+1
          else
              matchstar = .false.
              exit
          endif
      enddo


  end function matchstar

end module fexp
