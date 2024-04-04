module listener
    use discord_api, only: send_message, get_last_channel_message
    use types, only: message_t
    use markov, only: push_string, gen_string, reset_data
    use json_module, only: json_file
    use util, only: i32_to_str

    implicit none
    
    private
    public :: listen, omp_listen, load_channels, print_channels, parse_command

    type channel_t
        character(len=:), allocatable :: channel_id
        character(len=:), allocatable :: last_msg_id
        integer :: msg_count
    end type

    type(channel_t), allocatable :: channels(:)
    
contains
    ! this is by far the worst thing i have ever written in any language
    ! basically, every time this is called it will query all channels in the listener list
    ! if the message ID matches the last ID checked, ignore
    ! if it doesnt, try to parse a command out of it 
    ! if it doesnt start with a prefix, push the message content to the markov chain 
    ! if 10 messages have been counted, generate a string of 10 words and send it where the 10th message was sent
    subroutine listen
        integer :: idx
        type(message_t) :: msg

        do idx = 1, size(channels)
            msg = get_last_channel_message(channels(idx)%channel_id)
            
            if (msg%id /= channels(idx)%last_msg_id) then
                channels(idx)%last_msg_id = msg%id
                channels(idx)%msg_count = channels(idx)%msg_count + 1

                call parse_command(msg)

                if (channels(idx)%msg_count > 9) then
                    call send_message(channels(idx)%channel_id, gen_string(16))
                end if
            end if
        end do 
    end subroutine listen

    subroutine omp_listen
        integer :: idx
        type(message_t) :: msg

        !$omp parallel do 
        do idx = 1, size(channels)
            msg = get_last_channel_message(channels(idx)%channel_id)
            
            if (msg%id /= channels(idx)%last_msg_id) then
                channels(idx)%last_msg_id = msg%id
                channels(idx)%msg_count = channels(idx)%msg_count + 1

                call parse_command(msg)

                if (channels(idx)%msg_count > 9) then
                    call send_message(channels(idx)%channel_id, gen_string(16))
                end if
            end if
        end do 
        !$omp end parallel do
    end subroutine omp_listen

    subroutine parse_command(msg)
        type(message_t), intent(in) :: msg

        character(len=7) :: cmd

        if (msg%content(1:6) == "SKYNET") then
            cmd = msg%content(8:13)
            print *, "CMD RECEIVED: ", cmd
            
            select case(cmd)
            case("HELPME")
                print *, msg%channel_id, "    ", msg%content
                call send_message(msg%channel_id, "```\nAVAILABLE CMDS: \n\tHELPME\n\tRSTDAT\n\tMARKOV\n```")
            case("RSTDAT")
                call reset_data
                call send_message(msg%channel_id, "OK")
            case("MARKOV")
                call send_message(msg%channel_id, gen_string(64))
            end select
        else 
            call push_string(msg%content)
        endif
    end subroutine
    
    subroutine load_channels
        logical :: status
        type(json_file) :: config
        integer :: channel_count, idx

        call config%initialize
        call config%load("config.json")
        channel_count = count_elements_in_channel_array()
        
        allocate(channels(channel_count))
        do idx = 1, channel_count
            call config%get("channels("//trim(i32_to_str(idx))//")", channels(idx)%channel_id, status)
            channels(idx)%last_msg_id = ""
            channels(idx)%msg_count = 0
        end do

        if (.not. status) then 
            print *, "Error loading channels from config.json"
            stop 1
        end if 

        print *, "Got channels from config.json"

    end subroutine load_channels

    function count_elements_in_channel_array() result(n)
        integer :: n

        logical :: status = .true.
        type(json_file) :: json
        character(len=:), allocatable :: tmp

        call json%initialize
        call json%load("config.json")

        n = 0
        do while (status)
            n = n + 1
            call json%get("channels("//trim(i32_to_str(n + 1))//")", tmp, status)
        end do

        call json%destroy
    end function count_elements_in_channel_array
    
    subroutine print_channels
        integer :: idx

        do idx = 1, size(channels)
            print *, "Channel #", trim(i32_to_str(idx)), " = ", channels(idx)%channel_id 
        end do
    end subroutine print_channels
end module listener
