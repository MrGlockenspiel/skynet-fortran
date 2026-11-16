program main
    use discord_api, only: init_discord_api
    use listener, only: print_channels, load_channels, listen
    use markov, only: markov_generator_t
    use util, only: get_token

    implicit none

    ! vars
    integer :: idx
    logical :: exit = .false.
    type(markov_generator_t) :: superai

    call init_discord_api(get_token())
    call load_channels
    call print_channels

    call superai%init()
    
    ! suckless blazing fast configuration in main.f90

    ! optional load brain from raw text corpus
    ! call superai%load_data_from_file("corpus.txt")

    ! optional load brain from csv
    ! call superai%load_csv("saved.csv")
    
    print *, "loaded data"

    ! lmao why is this even a condition
    do while (exit .neqv. .true.)
        call sleep(1)
        call listen(superai)
    end do
end program main
