module m
    use iso_fortran_env

    contains
!___________________________________________________________________________
! these 2 fuctions' role is to identify whether the input is a number or an alphabet
    logical function isalpha(c)
        implicit none

        character, intent(in) :: c

        isalpha = ('a'.le.c .and. c.le.'z') .or. ('A'.le.c .and. c.le.'Z')
    end function isalpha
!___________________________________________________________________________

    logical function isnum(c)
        implicit none

        character, intent(in) :: c

        isnum = ('0'.le.c .and. c.le.'9')

    end function isnum
!___________________________________________________________________________
    integer function readint(s,is)
        implicit none

        character(len=*) :: s
        integer :: is, is0

        do while (.not. isnum(s(is:is)) .and. is.le.len_trim(s))
            is=is+1
        end do

        is0=is
        do while (isnum(s(is:is)) .and. is.le.len_trim(s))
            is=is+1
        end do

        read(s(is0:is-1),*) readint

    end function readint
!___________________________________________________________________________
    subroutine reading
        implicit none
        character(len=25) :: s
        integer :: io,isum

        isum=0

        open(unit=20, file='instructions.dat', status='old')
        open(unit=30, file='puzzle.dat', status='old')

        do
            read(30,'(25a)',iostat=io) s
            if (io==iostat_end) exit
            print*, trim(s)
        end do

        print*, "part a = ", isum

        close (20)
        close (30)

    end subroutine
!___________________________________________________________________________
end module
!___________________________________________________________________________
program hello
    use m
    implicit none

    print *, "start"

    call reading ()

    print*, "end"

end program

