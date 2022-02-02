!
! Fortran FastCGI stack
! Based on Fortran FastCGI by Ricolindo.Carino@gmail.com and arjen.markus895@gmail.com
!
! Requires:
!    - the FLIBS modules cgi_protocol and fcgi_protocol
!    - the FastCGI library
!

program app

    use fcgi_protocol

    implicit none

    type(DICT_STRUCT), pointer  :: dict => null() ! Initialisation is important!
    logical                     :: stopped = .false. ! set to true in respond() to terminate program
    integer                     :: unitNo ! unit number  for a scratch file
    character(len=20)           :: mime = 'text/plain' ! mime type

    ! open scratch file
    open(newunit=unitNo, status='scratch')

    ! wait for environment variables from webserver
    do while (fcgip_accept_environment_variables() >= 0)

        ! build dictionary from GET or POST data, environment variables
        call fcgip_make_dictionary( dict, unitNo )

        call respond(dict, unitNo, stopped, mime)

        call fcgip_put_file( unitNo, mime )

        ! terminate?
        if (stopped) exit

    end do !  while (fcgip_accept_environment_variables() >= 0)

    close(unitNo)

    ! webserver will return an error since this process will now terminate
    unitNo = fcgip_accept_environment_variables()


contains
    subroutine respond ( dict, unitNo, stopped, mime )

        type(DICT_STRUCT), pointer        :: dict
        integer, intent(in)               :: unitNo
        logical, intent(out)              :: stopped
        character(len=20), intent(out)    :: mime

        ! the script name
        character(len=80)  :: scriptName, query

        ! retrieve script name (key=DOCUMENT_URI) from dictionary
        call cgi_get( dict, "DOCUMENT_URI", scriptName )

        select case (trim(scriptName))
            case ('/json')
                mime = 'application/json'
                write(unitNo, AFORMAT) '{"message": "Hello, World!"}'
            case ('/plaintext')
                mime = 'text/plain'
                write(unitNo, AFORMAT) 'Hello, World!'
            case DEFAULT
        end select

        return

    end subroutine respond

end program app
