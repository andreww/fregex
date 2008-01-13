module regex

  implicit none

  private

  public :: new_re, match_re, sub_re


  integer, parameter :: DEBUG_CHANNEL = 25
  logical, save :: debug = .false.

contains

  function new_re (regex_str)

    character (len=*) :: regex_str
    character (len=250) :: new_re
    
  end function new_re

  function match_re (regex, str)

    character (len=250) :: regex
    character (len=*) :: str
    logical :: match_re

  end function match_re

  function sub_re (regex, str)

    character (len=250) :: regex
    character (len=*) :: str
    logical :: sub_re

  end function sub_re

end module regex
