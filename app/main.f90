program main
    use discord_api, only: init_discord_api
    use listener, only: print_channels, load_channels, omp_listen
    use markov, only: reset_data, push_string
    use util, only: get_token

    implicit none

    ! vars
    integer :: idx
    logical :: exit = .false.


    call init_discord_api(get_token())
    call load_channels
    call print_channels

    call reset_data

    do while (exit .neqv. .true.)
        call sleep(1)
        call omp_listen
    end do
end program main
