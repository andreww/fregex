program fox_time

    use fexp

    implicit none

    character(len=30) :: qname = 'avalid:qname12345678abcdefghi'
    character(len=52) :: xmlstartletter = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=12) :: xmlothers = '-_1234567890' ! NB - should include '.'
    
    integer :: i
    logical :: l


    !do i = 1, 10000000
    do i = 1, 1
          print*, match('^['//xmlstartletter//']['//xmlothers//xmlstartletter//']*:[' &
                & //xmlstartletter//']['//xmlothers//xmlstartletter//']*$' ,'avalid:qname'), &
                & matchstart, matchlength
          print*,  match('^['//xmlstartletter//']['//xmlothers//xmlstartletter//']*:[' &
                & //xmlstartletter//']['//xmlothers//xmlstartletter//']*' ,'avalid:qname'), &
                & matchstart, matchlength
          print*,  match('['//xmlstartletter//']['//xmlothers//xmlstartletter//']*:[' &
                & //xmlstartletter//']['//xmlothers//xmlstartletter//']*' ,'avalid:qname'), &
                & matchstart, matchlength
    enddo


end program fox_time
