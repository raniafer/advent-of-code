module m

	use iso_fortran_env

    contains
!___________________________________________________________________________________________

    subroutine count_calories()
        implicit none
        integer :: calories,total
        integer :: io,j
        integer, dimension(1000,2) :: tab
        integer :: i,k

        j=1
        total=0

        open(unit=20, file='calories.dat', status='old')
        open(unit=30, file='result.dat', status='old')

        do !i=1,100
            read(20,'(i5)',iostat=io) calories
            if (io==iostat_end) exit

        !    print*, "calories = ", calories

            total=calories+total

            call actual_counting(calories,j,total)

            write(30,*) j,total

       !     do i=1,1000
        !        read (30,*) tab
         !   end do

          !  print*, maxval(tab)

        !    print*, "total = ",total

        end do


        close(20)
        close(30)

    end subroutine
!___________________________________________________________________________________________
    subroutine actual_counting (calories,j,total)
        implicit none

        integer :: calories, j,total




        if (calories == 0)then
            print*, "elf number ",j," is carrying ",total," calories"
       !     tab(i,1)=j
        !    tab(i,2)=total
            j=j+1
            total=0
            return
        end if


    end subroutine
!___________________________________________________________________________________________
    subroutine j_and_total (j,total,tab)
        implicit none

        integer :: j, total
        integer :: i, k
        integer, dimension(j,2) :: tab

        do i=1,j
      !      do k=1,2
                tab(i,1)=j
                tab(i,2)=total
       !     end do
        end do

    end subroutine

end module
!___________________________________________________________________________________________

program hello
        use m
    implicit none

    print*, "start"

    call count_calories()

    print*, "end"
end program

