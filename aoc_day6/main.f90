module m
    use iso_fortran_env

    contains
!========================================================================================================================
    subroutine my_answer()
        implicit none

        character(len=5000) :: line
        integer :: io,n,i

    !    k=0

        open(unit=20, file="characters.dat", status="old")
        read(20,'(a)',iostat=io) line
        print*, trim(line)

        n=len_trim(line)
        print*, n

        do i=1,n
            if ((line(i:i).eq.line(i+1:i+1)).and.(line(i:i).eq.line(i+2:i+2)).and.(line(i:i).eq.line(i+3:i+3))) then
                print*, i+4
                exit
            end if
        end do

        close(20)
    end subroutine
!========================================================================================================================

    logical function unique (s)
    ! are all chracters in c unique
        implicit none

        character(len=*) :: s
        integer :: i,j
      !  logical :: uniqu

        unique=.true.

        do i=1, len_trim(s)
            do j=1,i-1
                if (s(i:i)==s(j:j)) then
                    unique=.false.
                    return
                end if
            end do
        end do

    end function
!_________________________________________________________________
    subroutine correct_answer ()
        implicit none

        character(len=4100) :: s
        character(len=4) :: c
        integer :: i,ans,n

        ans=0

        open(unit=30, file="characters.dat", status="old")
        read(30,*)s

     !   print*, trim(s)

        n=4

        do i=1, len_trim(s)-n+1
            c=s(i:i+3)
            print*, c

            if (unique(c)) then
                ans=i+n-1
                exit
            end if
        end do

        print*, ans

    end subroutine
end module
program hello
    use m

    print*, "start"

  !  call my_answer()
    call correct_answer ()

    print*, "end"

end program

