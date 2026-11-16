module markov
    use, intrinsic :: iso_fortran_env, only: iostat_end
    implicit none
    private

    character(len=*), parameter :: end_token = '<END>'

    public :: markov_generator_t

    type :: word_entry
        character(len=:), allocatable :: value
    end type word_entry

    type :: chain_entry
        integer, allocatable :: next_indices(:)
        integer :: next_len = 0
        integer :: next_cap = 0
    end type chain_entry

    type :: markov_generator_t
        private
        type(word_entry), allocatable :: words(:)
        type(chain_entry), allocatable :: chain(:)
        integer :: data_len = 0
        integer :: data_cap = 0
    contains
        procedure :: init => markov_init
        procedure :: reset => markov_reset
        procedure :: push_string => markov_push_string
        procedure :: gen_string => markov_gen_string
        procedure :: load_data_from_file => markov_load_data_from_file
        procedure :: save_csv => markov_save_csv
        procedure :: load_csv => markov_load_csv
        procedure :: word_count => markov_word_count
        procedure :: transition_count => markov_transition_count
        procedure, private :: ensure_capacity => markov_ensure_capacity
        procedure, private :: add_word_get_index => markov_add_word_get_index
        procedure, private :: add_transition => markov_add_transition
        procedure, private :: gen_word => markov_gen_word
    end type markov_generator_t

contains

    subroutine markov_init(this, initial_text)
        class(markov_generator_t), intent(inout) :: this
        character(len=*), intent(in), optional :: initial_text

        call this%reset()
        if (present(initial_text)) then
            call this%push_string(initial_text)
        else
            call this%push_string('hello everybody my name is skynet.')
        end if
    end subroutine

    subroutine markov_reset(this)
        class(markov_generator_t), intent(inout) :: this

        if (allocated(this%words)) deallocate(this%words)
        if (allocated(this%chain)) deallocate(this%chain)

        this%data_len = 0
        this%data_cap = 0
    end subroutine

    subroutine markov_ensure_capacity(this, mincap)
        class(markov_generator_t), intent(inout) :: this
        integer, intent(in) :: mincap

        integer :: newcap, i
        type(word_entry), allocatable :: new_words(:)
        type(chain_entry), allocatable :: new_chain(:)

        if (mincap <= this%data_cap) return

        newcap = max(64, this%data_cap)

        do while (newcap < mincap)
            if (newcap > huge(newcap) / 2) then
                newcap = mincap
                exit
            end if

            newcap = newcap * 2
        end do

        allocate(new_words(newcap))
        allocate(new_chain(newcap))

        do i = 1, this%data_len
            new_words(i) = this%words(i)
            new_chain(i) = this%chain(i)
        end do

        call move_alloc(new_words, this%words)
        call move_alloc(new_chain, this%chain)

        this%data_cap = newcap
    end subroutine

    subroutine markov_add_word_get_index(this, word, idx)
        class(markov_generator_t), intent(inout) :: this
        character(len=*), intent(in) :: word
        integer, intent(out) :: idx

        integer :: i, n

        n = len_trim(word)
        do i = 1, this%data_len
            if (allocated(this%words(i)%value)) then
                if (this%words(i)%value == word(:n)) then
                    idx = i
                    return
                end if
            end if
        end do

        call this%ensure_capacity(this%data_len + 1)
        this%data_len = this%data_len + 1
        allocate(character(len=n) :: this%words(this%data_len)%value)
        this%words(this%data_len)%value = word(:n)
        idx = this%data_len
    end subroutine

    subroutine markov_add_transition(this, prev_idx, next_idx)
        class(markov_generator_t), intent(inout) :: this
        integer, intent(in) :: prev_idx, next_idx

        integer, allocatable :: tmp(:)
        integer :: newcap

        if (prev_idx < 1 .or. prev_idx > this%data_len) return
        if (next_idx < 1 .or. next_idx > this%data_len) return

        if (this%chain(prev_idx)%next_len == this%chain(prev_idx)%next_cap) then
            newcap = max(4, this%chain(prev_idx)%next_cap * 2)
            allocate(tmp(newcap))
            if (this%chain(prev_idx)%next_len > 0) then
                tmp(1:this%chain(prev_idx)%next_len) = &
                    this%chain(prev_idx)%next_indices(1:this%chain(prev_idx)%next_len)
            end if
            call move_alloc(tmp, this%chain(prev_idx)%next_indices)
            this%chain(prev_idx)%next_cap = newcap
        end if

        this%chain(prev_idx)%next_len = this%chain(prev_idx)%next_len + 1
        this%chain(prev_idx)%next_indices(this%chain(prev_idx)%next_len) = next_idx
    end subroutine

    subroutine markov_push_string(this, input)
        class(markov_generator_t), intent(inout) :: this
        character(len=*), intent(in) :: input

        character(len=:), allocatable :: token
        integer :: i, first, n, current_idx, previous_idx

        n = len_trim(input)
        if (n == 0) return

        first = 1
        previous_idx = 0

        do i = 1, n + 1
            if (i <= n) then
                if (.not. is_delimiter(input(i:i))) cycle
            end if

            if (i > first) then
                token = input(first:i-1)
                call this%add_word_get_index(token, current_idx)

                if (previous_idx /= 0) call this%add_transition(previous_idx, current_idx)
                previous_idx = current_idx

                if (is_sentence_terminator(token(len(token):len(token)))) then
                    call this%add_word_get_index(end_token, current_idx)
                    call this%add_transition(previous_idx, current_idx)
                    previous_idx = 0
                end if
            end if

            if (i <= n) first = i + 1
        end do

        if (previous_idx /= 0) then
            call this%add_word_get_index(end_token, current_idx)
            call this%add_transition(previous_idx, current_idx)
        end if
    end subroutine

    pure function is_delimiter(ch) result(res)
        character(len=1), intent(in) :: ch

        logical :: res

        res = ch == ' ' .or. ch == achar(9) .or. ch == achar(10) .or. ch == achar(13)
    end function

    pure function is_sentence_terminator(ch) result(res)
        character(len=1), intent(in) :: ch

        logical :: res

        res = (ch == '.' .or. ch == '!' .or. ch == '?')
    end function

    function rand_int(max_value) result(value)
        integer, intent(in) :: max_value

        integer :: value

        real :: sample

        if (max_value <= 0) then
            value = 0
            return
        end if

        call random_number(sample)
        value = min(max_value, int(sample * real(max_value)) + 1)
    end function

    function markov_gen_word(this, prev_idx) result(out_idx)
        class(markov_generator_t), intent(inout) :: this
        integer, intent(in), optional :: prev_idx

        integer :: out_idx

        integer :: p, r, candidate_count

        out_idx = 0

        if (this%data_len == 0) return

        if (present(prev_idx)) then
            p = prev_idx

            if (p < 1 .or. p > this%data_len) return
            if (this%chain(p)%next_len <= 0) return

            r = rand_int(this%chain(p)%next_len)
            out_idx = this%chain(p)%next_indices(r)

        else
            ! select a word but not <END>
            candidate_count = 0

            do p = 1, this%data_len
                if (this%words(p)%value /= end_token) then
                    candidate_count = candidate_count + 1
                end if
            end do

            if (candidate_count == 0) return

            r = rand_int(candidate_count)

            do p = 1, this%data_len
                if (this%words(p)%value == end_token) cycle

                r = r - 1

                if (r == 0) then
                    out_idx = p
                    exit
                end if
            end do
        end if
    end function

    function markov_gen_string(this, length) result(output)
        class(markov_generator_t), intent(inout) :: this
        integer, intent(in) :: length

        character(len=:), allocatable :: output

        character(len=:), allocatable :: token
        integer :: idx, i

        output = ''
        if (length <= 0 .or. this%data_len == 0) return

        idx = this%gen_word()
        if (idx <= 0) return

        do i = 1, length
            if (idx < 1 .or. idx > this%data_len) exit
            token = this%words(idx)%value
            if (token == end_token) exit

            if (len(output) > 0) output = output // ' '
            output = output // token

            idx = this%gen_word(idx)
            if (idx == 0) exit
        end do
    end function

    function markov_word_count(this) result(count)
        class(markov_generator_t), intent(in) :: this

        integer :: count

        count = this%data_len
    end function

    function markov_transition_count(this) result(count)
        class(markov_generator_t), intent(in) :: this

        integer :: count

        integer :: i

        count = 0
        do i = 1, this%data_len
            count = count + this%chain(i)%next_len
        end do
    end function

    subroutine markov_load_data_from_file(this, filename)
        class(markov_generator_t), intent(inout) :: this
        character(len=*), intent(in) :: filename

        character(len=4096) :: line
        integer :: unit, ios

        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) return

        do
            read(unit, '(A)', iostat=ios) line
            if (ios == iostat_end) exit
            if (ios /= 0) exit
            if (len_trim(line) > 0) call this%push_string(trim(line))
        end do
        close(unit)
    end subroutine

    subroutine markov_save_csv(this, filename)
        class(markov_generator_t), intent(in) :: this
        character(len=*), intent(in) :: filename

        integer :: unit, ios, i, j
        character(len=32) :: number
        character(len=:), allocatable :: transitions

        open(newunit=unit, file=filename, status='replace', action='write', iostat=ios)

        if (ios /= 0) return

        write(unit, '(A)', iostat=ios) 'index,word,transitions'

        if (ios /= 0) then
            close(unit)
            return
        end if

        do i = 1, this%data_len
            transitions = ''

            do j = 1, this%chain(i)%next_len
                write(number, '(I0)') this%chain(i)%next_indices(j)

                if (j > 1) transitions = transitions // ' '

                transitions = transitions // trim(number)
            end do

            write(unit, '(I0,A,A,A,A,A)', iostat=ios) &
                i, ',"', csv_escape(this%words(i)%value), &
                '","', transitions, '"'

            if (ios /= 0) exit
        end do

        close(unit)
    end subroutine

    subroutine markov_load_csv(this, filename, success)
        class(markov_generator_t), intent(inout) :: this
        character(len=*), intent(in) :: filename
        logical, intent(out), optional :: success

        character(len=8192) :: line
        character(len=:), allocatable :: fields(:)
        character(len=:), allocatable :: record
        character(len=:), allocatable :: token
        integer :: unit, ios, idx, max_idx, i, pos, value, parse_ios
        integer, allocatable :: indices(:)
        type(markov_generator_t) :: loaded

        if (present(success)) success = .false.
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) return

        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) then
            close(unit)
            return
        end if

        ! determine the largest serialized index
        max_idx = 0
        do
            call read_csv_record(unit, record, ios)
            if (ios /= 0) exit
            if (len_trim(record) == 0) cycle
            pos = index(record, ',')
            if (pos <= 1) cycle
            read(record(:pos-1), *, iostat=parse_ios) idx
            if (parse_ios == 0) then
                if (idx > max_idx) max_idx = idx
            end if
        end do

        if (ios /= iostat_end) then
            close(unit)
            return
        end if
        if (max_idx <= 0) then
            close(unit)
            return
        end if

        call loaded%ensure_capacity(max_idx)
        loaded%data_len = max_idx
        rewind(unit)
        read(unit, '(A)', iostat=ios) line  ! header
        if (ios /= 0) then
            close(unit)
            return
        end if

        do
            call read_csv_record(unit, record, ios)
            if (ios /= 0) exit
            if (len_trim(record) == 0) cycle

            call parse_csv_record(record, fields, ios)
            if (ios /= 0) cycle
            if (size(fields) < 3) cycle

            read(fields(1), *, iostat=parse_ios) idx
            if (parse_ios /= 0) cycle
            if (idx < 1 .or. idx > max_idx) cycle

            if (allocated(loaded%words(idx)%value)) deallocate(loaded%words(idx)%value)
            allocate(character(len=len(fields(2))) :: loaded%words(idx)%value)
            loaded%words(idx)%value = trim(fields(2))

            if (len_trim(fields(3)) == 0) cycle
            if (allocated(indices)) deallocate(indices)
            pos = 1
            do i = 1, len_trim(fields(3)) + 1
                if (i <= len_trim(fields(3))) then
                    if (fields(3)(i:i) /= ' ') cycle
                end if
                if (i > pos) then
                    token = fields(3)(pos:i-1)
                    read(token, *, iostat=parse_ios) value
                    if (parse_ios == 0) then
                        if (value >= 1 .and. value <= max_idx) then
                            if (.not. allocated(indices)) then
                                allocate(indices(1))
                                indices(1) = value
                            else
                                indices = [indices, value]
                            end if
                        end if
                    end if
                end if
                pos = i + 1
            end do

            if (allocated(indices)) then
                do i = 1, size(indices)
                    call loaded%add_transition(idx, indices(i))
                end do
                deallocate(indices)
            end if
        end do

        close(unit)
        if (ios /= iostat_end) return
        do i = 1, max_idx
            if (.not. allocated(loaded%words(i)%value)) then
                loaded%words(i)%value = end_token
            end if
        end do

        this%words = loaded%words
        this%chain = loaded%chain
        this%data_len = loaded%data_len
        this%data_cap = loaded%data_cap

        if (present(success)) success = .true.
    end subroutine

    function csv_escape(value) result(escaped)
        character(len=*), intent(in) :: value

        character(len=:), allocatable :: escaped

        integer :: i

        escaped = ''
        do i = 1, len(value)
            escaped = escaped // value(i:i)
            if (value(i:i) == '"') escaped = escaped // '"'
        end do
    end function

    subroutine read_csv_record(unit, record, status)
        integer, intent(in) :: unit
        character(len=:), allocatable, intent(out) :: record
        integer, intent(out) :: status

        character(len=8192) :: physical_line
        logical :: quoted
        integer :: i, n

        record = ''
        quoted = .false.
        do
            read(unit, '(A)', iostat=status) physical_line
            if (status /= 0) return

            n = len_trim(physical_line)
            if (quoted .and. yup_looks_like_a_csv_to_me_man(physical_line, n)) then
                backspace(unit)
                status = 0
                return
            end if

            if (len(record) > 0) record = record // achar(10)
            record = record // trim(physical_line)
            i = 1
            do while (i <= n)
                if (physical_line(i:i) == '"') then
                    if (quoted .and. i < n) then
                        if (physical_line(i+1:i+1) == '"') then
                            if (i + 2 <= n) then
                                if (physical_line(i+2:i+2) == ',') then
                                    quoted = .false.
                                    i = i + 2
                                    cycle
                                end if
                            end if
                            i = i + 2
                            cycle
                        end if
                    end if
                    quoted = .not. quoted
                end if
                i = i + 1
            end do

            if (.not. quoted) return
        end do
    end subroutine

    pure function yup_looks_like_a_csv_to_me_man(line, n) result(res)
        character(len=*), intent(in) :: line
        integer, intent(in) :: n

        logical :: res

        integer :: i

        res = .false.
        i = 1
        do while (i <= n)
            if (line(i:i) < '0' .or. line(i:i) > '9') exit
            i = i + 1
        end do
        if (i > 1) then
            if (i <= n) res = line(i:i) == ','
        end if
    end function

    subroutine parse_csv_record(line, fields, status)
        character(len=*), intent(in) :: line
        character(len=:), allocatable, intent(out) :: fields(:)
        integer, intent(out) :: status

        character(len=:), allocatable :: values(:), current
        integer :: i, n, count, capacity
        logical :: quoted

        n = len_trim(line)
        capacity = 4
        allocate(character(len=max(1, n)) :: values(capacity))
        values = ''
        current = ''
        count = 0
        quoted = .false.
        i = 1
        status = 0

        do while (i <= n)
            if (line(i:i) == '"') then
                if (quoted .and. i < n) then
                    if (line(i+1:i+1) == '"') then
                        if (i + 2 <= n) then
                            if (line(i+2:i+2) == ',') then
                                quoted = .false.
                                i = i + 2
                                cycle
                            end if
                        end if
                        current = current // '"'
                        i = i + 2
                        cycle
                    end if
                end if
                quoted = .not. quoted
            else if (line(i:i) == ',' .and. .not. quoted) then
                count = count + 1
                if (count > capacity) then
                    status = 1
                    return
                end if
                values(count) = current
                current = ''
            else
                current = current // line(i:i)
            end if
            i = i + 1
        end do

        if (quoted) then
            status = 1
            return
        end if
        count = count + 1
        if (count > capacity) then
            status = 1
            return
        end if
        values(count) = current
        allocate(character(len=max(1, n)) :: fields(count))
        fields = values(1:count)
    end subroutine
end module
