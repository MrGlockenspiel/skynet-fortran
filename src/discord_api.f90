module discord_api
    ! minimal discord API impl (v9)

    use json_module, only: json_file, json_value
    use discord_requests, only: discord_get, discord_post
    use types, only: user_t, message_t
    use util, only: i32_to_str, i64_to_str

    implicit none

    private :: token
    public :: init_discord_api, send_message, get_last_channel_message, get_channel_messages

    character(len=:), allocatable :: token

contains

    ! test function
    subroutine send_message(channel_id, message)
        character(len=*), intent(in) :: channel_id
        character(len=*), intent(in) :: message

        character(len=:), allocatable :: api_url
        character(len=:), allocatable :: payload

        payload = '{"content":"'//json_escape(message)//'"}'

        api_url = "https://discord.com/api/v9/channels/"//channel_id//"/messages"

        call discord_post(api_url, token, payload)
    end subroutine

    function get_channel_messages(channel_id, limit) result(messages)
        character(len=*), intent(in) :: channel_id
        integer, intent(in) :: limit

        type(message_t), allocatable :: messages(:)
        
        type(json_file) :: response

        character(len=:), allocatable :: api_url
        character(len=:), allocatable :: query

        integer :: idx
        logical :: status
        real :: tmp

        query = "?limit="//trim(i32_to_str(limit))

        api_url = "https://discord.com/api/v9/channels/"//channel_id//"/messages"//query

        allocate(messages(limit))

        response = discord_get(api_url, token)
        
        do idx = 1, limit
            call response%get("["//trim(i32_to_str(idx))//"].id", messages(idx)%id, status)
            call response%get("["//trim(i32_to_str(idx))//"].channel_id", messages(idx)%channel_id, status)
            call response%get("["//trim(i32_to_str(idx))//"].content", messages(idx)%content, status)
        end do
        
        call response%destroy
    end function

    function get_last_channel_message(channel_id) result(message)
        character(len=*), intent(in) :: channel_id

        type(message_t), allocatable :: message
        
        type(json_file) :: response
        logical :: status
        !real :: tmp

        character(len=:), allocatable :: api_url

        api_url = "https://discord.com/api/v9/channels/" &
            //channel_id//"/messages?limit=1"

        response = discord_get(api_url, token)

        allocate(message)
        
        message%id = ''
        message%channel_id = ''
        message%content = ''
        
        call response%get("[1].id", message%id, status)
        
        if (status) then
            call response%get("[1].channel_id", message%channel_id, status)
            call response%get("[1].content", message%content, status)
        end if
        
        !call response%print
        call response%destroy
    end function

    subroutine init_discord_api(bot_token)
        character(len=*), intent(in) :: bot_token

        token = bot_token
    end subroutine

    function json_escape(value) result(escaped)
        character(len=*), intent(in) :: value
        
        character(len=:), allocatable :: escaped
        
        character(len=2) :: hex
        integer :: i, code

        escaped = ''
        do i = 1, len(value)
            code = iachar(value(i:i))
            select case (code)
            case (34)
                escaped = escaped // achar(92) // achar(34)
            case (92)
                escaped = escaped // achar(92) // achar(92)
            case (8)
                escaped = escaped // achar(92) // 'b'
            case (9)
                escaped = escaped // achar(92) // 't'
            case (10)
                escaped = escaped // achar(92) // 'n'
            case (12)
                escaped = escaped // achar(92) // 'f'
            case (13)
                escaped = escaped // achar(92) // 'r'
            case (0:7, 11, 14:31)
                write(hex, '(Z2.2)') code
                escaped = escaped // achar(92) // 'u00' // hex
            case default
                escaped = escaped // value(i:i)
            end select
        end do
    end function json_escape
end module
