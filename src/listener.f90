module listener
    use discord_api, only: send_message, get_last_channel_message
    use types, only: message_t
    use markov, only: markov_generator_t
    use json_module, only: json_file
    use util, only: i32_to_str

    implicit none

    private
    public :: listen, load_channels, print_channels, parse_command

    type channel_t
        character(len=:), allocatable :: channel_id
        character(len=:), allocatable :: last_msg_id
        integer :: msg_count
    end type

    type(channel_t), allocatable :: channels(:)

contains
    subroutine listen(markov)
        type(markov_generator_t), intent(inout) :: markov
        integer :: idx
        type(message_t) :: msg

        do idx = 1, size(channels)
            msg = get_last_channel_message(channels(idx)%channel_id)

            if (.not. allocated(msg%id)) cycle
            if (len(msg%id) == 0) cycle

            if (msg%id /= channels(idx)%last_msg_id) then
                channels(idx)%last_msg_id = msg%id
                channels(idx)%msg_count = channels(idx)%msg_count + 1

                call parse_command(msg, markov)

                if (channels(idx)%msg_count > 9) then
                    call send_message(channels(idx)%channel_id, markov%gen_string(16))
                    channels(idx)%msg_count = 0
                end if
            end if
        end do
    end subroutine listen

    subroutine parse_command(msg, markov)
        type(message_t), intent(in) :: msg
        type(markov_generator_t), intent(inout) :: markov

        character(len=7) :: cmd

        if (len(msg%content) >= 6) then
            if (msg%content(1:6) == "SKYNET") then
                if (len(msg%content) < 13) return

                cmd = msg%content(8:13)
                print *, "CMD RECEIVED: ", cmd

                select case(cmd)
                case("HELPME")
                    print *, msg%channel_id, "    ", msg%content
                    call send_message(msg%channel_id, "```\nAVAILABLE CMDS: \n\tHELPME\n\tRSTDAT\n\tMARKOV\n\tSAVDAT\n\tLOADAT\n```")
                case("RSTDAT")
                    call markov%init
                    call send_message(msg%channel_id, "OK")
                case("MARKOV")
                    call send_message(msg%channel_id, markov%gen_string(16))
                case("SAVDAT")
                    call markov%save_csv("saved.csv")
                    call send_message(msg%channel_id, "OK")
                case("LOADAT")
                    block
                        logical :: loaded

                        call markov%load_csv("saved.csv", loaded)
                        if (loaded) then
                            call send_message(msg%channel_id, "OK")
                        else
                            call send_message(msg%channel_id, "SUPER ERROR!!!")
                        end if
                    end block
                end select
                return
            end if
        end if

        call markov%push_string(msg%content)
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

        call config%destroy

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
