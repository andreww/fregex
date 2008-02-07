program fox_time

    use m_common_namecheck
    use m_common_struct

    implicit none

    type(xml_doc_state) :: xds
    character(len=30) :: qname = 'avalid:qname12345678abcdefghi'
    integer :: i
    logical :: l

    call init_xml_doc_state(xds)

    do i = 1, 10000000
          l = checkQName('avalid:qname', xds)
    enddo

    call destroy_xml_doc_state(xds)

end program fox_time
