module m

    use iso_fortran_env

	character(len = :), allocatable :: finput

    contains
!____________________________________
    subroutine idk_what_to_call_yet
        implicit none

        integer :: a,b,c,d
        integer :: io,i,j

        i=0; j=0

        open(unit=20, file='ranges.dat', status='old')

        do
            read(20,100,iostat=io) a,b,c,d
            write(*,100) a,b,c,d
            if(io==iostat_end) exit

            call idk_two (a,b,c,d,i,j)


        end do

            print*, i+j

        close(20)

        100 format (i1,1x,i1,1x,i1,1x,i1)

    end subroutine
!____________________________________

    subroutine idk_two (a,b,c,d,i,j)
        implicit none
        integer :: i,j
        integer :: a,b,c,d

     !   i=0; j=0

              if ((a.le.c).and.(b.ge.d)) then
                i=i+1
              !  print*, i
                return
            else if ((c.le.a).and.(d.ge.b)) then
                j=j+1
              !  print*, j
                return
            end if

    end subroutine
!____________________________________

end module
!____________________________________

program hello
    use m

    print*, "start"

    call idk_what_to_call_yet ()

    print*, "end"

end program

