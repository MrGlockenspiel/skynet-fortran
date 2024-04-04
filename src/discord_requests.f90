module discord_requests

    use json_module, only: json_file
    use http, only: request, response_type, HTTP_GET, HTTP_POST, pair_type

    implicit none

    private 
    public :: discord_post, discord_get

contains

     subroutine discord_post(url, token, payload)
        character(len=:), allocatable, intent(in) :: url 
        character(len=:), allocatable, intent(in) :: token
        character(len=:), allocatable, intent(in) :: payload
    
        type(pair_type), allocatable :: req_header(:)
        type(response_type) :: response 

        req_header = [ &
            pair_type('Content-Type', 'application/json'), &
            pair_type('Authorization', 'Bot '//token) &
        ]

        response = request(url=url, method=HTTP_POST, header=req_header, data=payload)
        
        if (.not. response%ok) then
            ! request failed
            print *, "Error: ", response%err_msg, " [", response%status_code, "]"
        end if 
    end subroutine discord_post

    function discord_get(url, token) result(res)
        character(len=:), allocatable, intent(in) :: url
        character(len=:), allocatable, intent(in) :: token

        type(pair_type), allocatable :: req_header(:)
        type(response_type) :: response 

        type(json_file) :: res

        req_header = [ &
            pair_type('Content-Type', 'application/json'), &
            pair_type('Authorization', 'Bot '//token) &
        ]

        response = request(url=url, method=HTTP_GET, header=req_header)
        if (.not. response%ok) then
            ! request failed
            print *, "Error: ", response%err_msg, " [", response%status_code, "]"
        end if 

        call res%initialize()
        call res%deserialize(response%content)
    end function discord_get
end module discord_requests
