module m

    use iso_fortran_env
    implicit none

    contains
!_________________________________________________________________________
    subroutine rock_paper_scissors
        implicit none

        integer :: io
        character(len=15) :: opponent,you
        integer :: score

        score=0

        open(unit=20, file="input.dat", status="old")

        do
            read(20,*,iostat=io) opponent,you
            if (io==iostat_end) exit

       !     print*, "opponent= ", opponent,"you = ",you

            if (opponent=='A') then
                if (you=='X') then
                    score=score+1+3

                else if(you=='Y') then
                    score=score+2+6

                else if (you=='Z') then
                    score=score+3+0

                end if

            else if (opponent=='B') then
                if (you=='X') then
                    score=score+1+0

                else if(you=='Y') then
                    score=score+2+3

                else if (you=='Z') then
                    score=score+3+6

                end if

            else if (opponent=='C') then
                if (you=='X') then
                    score=score+1+6

                else if(you=='Y') then
                    score=score+2+0

                else if (you=='Z') then
                    score=score+3+3

                end if

            end if

        end do

        print*, 'score = ',score

    end subroutine
!_________________________________________________________________________

end module
!_________________________________________________________________________

program hello
    use m
    implicit none

    print *, "start"

    call rock_paper_scissors()

    print*, "end"

end program

