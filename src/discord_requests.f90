module discord_requests

    use json_module, only: json_file
    use http, only: request, response_type, HTTP_GET, HTTP_POST, pair_type

    implicit none

    private 
    public :: discord_post, discord_get

contains

    subroutine discord_post(url, token, payload, success)
        character(len=:), allocatable, intent(in) :: url
        character(len=:), allocatable, intent(in) :: token
        character(len=:), allocatable, intent(in) :: payload
        logical, optional, intent(out) :: success

        type(pair_type), allocatable :: req_header(:)
        type(response_type) :: response

        if (present(success)) success = .false.

        req_header = [ &
            pair_type('Content-Type', 'application/json'), &
            pair_type('Authorization', 'Bot '//token) &
        ]

        response = request(url=url, method=HTTP_POST, header=req_header, data=payload)

        if (.not. response%ok) then
            ! request failed
            print *, "Error: ", response%err_msg, " [", response%status_code, "]"
        else
            if (present(success)) success = .true.
        end if
    end subroutine

    function discord_get(url, token, success) result(res)
        character(len=:), allocatable, intent(in) :: url
        character(len=:), allocatable, intent(in) :: token
        logical, intent(out) :: success

        type(json_file) :: res

        type(pair_type), allocatable :: req_header(:)
        type(response_type) :: response

        success = .false.

        req_header = [ &
            pair_type('Content-Type', 'application/json'), &
            pair_type('Authorization', 'Bot '//token) &
        ]

        response = request(url=url, method=HTTP_GET, header=req_header)

        if (.not. response%ok) then
            ! request failed
            print *, "Error: ", response%err_msg, " [", response%status_code, "]"
            return
        end if

        call res%initialize()
        call res%deserialize(response%content)

        success = .true.
    end function
end module discord_requests
