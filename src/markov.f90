module markov
    implicit none
    private

    public :: push_word, push_string, gen_word, gen_string, load_data_from_file, reset_data

    type word_t
        character(len=:), allocatable :: word
    end type

    type(word_t), allocatable :: data(:)
    type(word_t), allocatable :: tmp(:)
    integer(8) :: markov_data_len = 0

contains
    subroutine push_word(word)
        character(len=*), intent(in) :: word
        integer :: count

        if (.not. allocated(data)) then
            print *, "Allocating markov data(1)..."
            allocate(data(32))
        end if
        
        count = size(data)
        markov_data_len = markov_data_len + 1
        !print *, "markov data word count: ", markov_data_len

        if (markov_data_len >= count) then
            call resize(data)
            print *, "Markov data (size: ", count, ") overflow, resizing"
        end if
        
        ! allocate memory for the word and assign it
        allocate(data(markov_data_len)%word, source=word)
    end subroutine push_word

    subroutine push_string(input)
        character(len=*), intent(in) :: input
        character(len=:), allocatable :: word
        integer :: i, start_idx, end_idx, len_input

        len_input = len(input)

        start_idx = 1

        do i = 1, len_input
            ! check if the current char is a space or if its the end
            if (input(i:i) == ' ' .or. i == len_input) then
                ! calculate the end index of the word
                if (i == len_input) then
                    end_idx = i
                else
                    end_idx = i - 1
                end if

                ! extract the word
                word = input(start_idx:end_idx)

                ! call push_word subroutine with the extracted word
                call push_word(trim(word))

                ! update start index for the next word
                start_idx = i + 1
            end if
        end do
    end subroutine push_string

    function gen_string(len) result(output)
        integer, intent(in) :: len
        character(len=:), allocatable :: output
        integer :: i 

        output = gen_word()

        do i = 1, len - 1 
            output = output//" "//gen_word()
        end do
    end function gen_string

    function gen_word() result(output)
        character(len=:), allocatable :: output
        integer :: idx 

        idx = rand_int(markov_data_len)
        output = data(idx)%word
    end function

    subroutine resize(var, n)
        type(word_t), allocatable, intent(inout) :: var(:)
        integer, intent(in), optional :: n
        integer :: this_size, new_size
        integer, parameter :: initial_size = 16

        if (allocated(var)) then
            this_size = size(var, 1)
            call move_alloc(var, tmp)
        else
            this_size = initial_size
        end if

        if (present(n)) then
            new_size = n
        else
            new_size = this_size + this_size/2 + 1
        end if

        allocate(var(new_size))

        if (allocated(tmp)) then
            this_size = min(size(tmp, 1), size(var, 1))
            var(:this_size) = tmp(:this_size)
        end if
    end subroutine resize

    function rand_int(maxint) result(output)
        integer(8), intent(in) :: maxint
        integer :: output
        real :: num

        call random_number(num)

        output = FLOOR(num * maxint) + 1
        
    end function rand_int

    subroutine load_data_from_file(filename)
        character(len=:), intent(in), allocatable :: filename
        character(len=:), allocatable :: content
        integer :: io, file_size
        
        inquire(file=filename, size=file_size)
        print *, file_size

        allocate(character(len=file_size) :: content)

        open(newunit=io, file=filename, status="old", action="read")
        rewind(io)
        read(io, '(A)') content
        close(io)
        
        !print *, content
        call push_string(trim(content))
    end subroutine load_data_from_file

    subroutine reset_data
        if (allocated(data)) then
            deallocate(data)
        end if 

        allocate(data(1))
        markov_data_len = 1
        call push_string("hello everybody my name is skynet")
    end subroutine reset_data
end module markov
