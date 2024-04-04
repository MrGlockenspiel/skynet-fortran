module util
    use json_module, only: json_file
    
    implicit none

    private
    public :: get_token, i32_to_str, i64_to_str

contains

    function get_token() result(token)
        character(len=:), allocatable :: token
        logical :: status
        type(json_file) :: config

        call config%initialize()
        call config%load("config.json")
        call config%get('token', token, status)
        
        if (.not. status) then 
            print *, "Error loading token from config.json"
            stop 1
        end if 

        print *, "Got token from config.json"
        token = trim(token)
    end function get_token

    ! https://stackoverflow.com/a/16437988
    function i64_to_str(int) result(str)
        integer(8), intent(in) :: int
        character(len=32) :: str 

        write(str, *) int
        str = adjustl(str)
    end function i64_to_str

    function i32_to_str(int) result(str)
        integer, intent(in) :: int 
        character(len=16) :: str 

        write(str, *) int 
        str = adjustl(str)
    end function i32_to_str

end module util
