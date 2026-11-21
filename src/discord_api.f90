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

        payload = '{"content":"'//message//'"}'

        api_url = "https://discord.com/api/v9/channels/"//channel_id//"/messages"

        call discord_post(api_url, token, payload)
    end subroutine

    function get_channel_messages(channel_id, limit, success) result(messages)
        character(len=*), intent(in) :: channel_id
        integer, intent(in) :: limit
        logical, intent(out), optional :: success

        type(message_t), allocatable :: messages(:)
        type(json_file) :: response

        character(len=:), allocatable :: api_url
        character(len=:), allocatable :: query

        integer :: idx, nread
        logical :: status, request_success

        if (present(success)) success = .false.

        allocate(messages(0))

        if (limit <= 0) return

        query = "?limit="//trim(i32_to_str(limit))

        api_url = "https://discord.com/api/v9/channels/"//channel_id//"/messages"//query

        response = discord_get(api_url, token, request_success)

        if (.not. request_success) then
            call response%destroy
            return
        end if

        deallocate(messages)
        allocate(messages(limit))

        nread = 0

        do idx = 1, limit
            call response%get("["//trim(i32_to_str(idx))//"].id", messages(idx)%id, status)
            if (.not. status) exit

            call response%get("["//trim(i32_to_str(idx))//"].channel_id", messages(idx)%channel_id, status)
            if (.not. status) exit

            call response%get("["//trim(i32_to_str(idx))//"].content", messages(idx)%content, status)
            if (.not. status) exit

            nread = nread + 1
        end do

        call response%destroy()

        if (nread /= limit) then
            block
                type(message_t), allocatable :: tmp(:)

                allocate(tmp(nread))
                if (nread > 0) tmp = messages(1:nread)
                call move_alloc(tmp, messages)
            end block
        end if

        if (present(success)) success = (nread .eq. limit)
    end function

    function get_last_channel_message(channel_id, success) result(message)
        character(len=*), intent(in) :: channel_id
        logical, intent(out), optional :: success

        type(message_t), allocatable :: message

        type(json_file) :: response
        logical :: status, request_success

        character(len=:), allocatable :: api_url

        if (present(success)) success = .false.

        allocate(message)

        message%id = ''
        message%channel_id = ''
        message%content = ''

        api_url = "https://discord.com/api/v9/channels/" &
            //channel_id//"/messages?limit=1"

        response = discord_get(api_url, token, request_success)

        if (.not. request_success) then
            call response%destroy
            return
        end if

        call response%get("[1].id", message%id, status)
        if (.not. status) then
            call response%destroy
            return
        end if

        call response%get("[1].channel_id", message%channel_id, status)
        if (.not. status) then
            call response%destroy
            return
        end if

        call response%get("[1].content", message%content, status)
        if (.not. status) then
            call response%destroy
            return
        end if

        call response%destroy()

        if (present(success)) success = .true.
    end function

    subroutine init_discord_api(bot_token)
        character(len=*), intent(in) :: bot_token

        token = bot_token
    end subroutine
end module
