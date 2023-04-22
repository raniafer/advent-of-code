program hello
    implicit none

    print*, "start"
    call sub1()
    print*, "end"

    contains
!______________________________________________________
    subroutine sub1()

        implicit none

     !   integer :: k
        character(len=100) :: s,c1,c2,d
       ! c1 and c2 can be declared as allocatable
      !  common /components/ c1,c2
        integer :: io,i,isum
        integer :: n1, n2

        open(unit=20, file="components.dat", status="old")
        open(unit=21, file="supplies.dat", status = "old")

        isum=0

        do i=1,300
            read(21,'(a100)',iostat=io)s
      !     if (io==iostat_end) exit

            n1=len_trim(s)
            n2=n1/2
            c1=s(1:n2)
            c2=s(n2+1:n1)
      !      print*,"s= ",trim(s)
       !     print*, "n= ",n1
        !    print*,"c1= ",trim(c1)
         !   print*,"c2= ",trim(c2)

        call common_item(c1,c2,d)

        isum=isum+prio(d)
  !      print*, "d= ",d
   !     print*, prio(d)


        end do

        print*, "part 1 = ", isum

        close(20)
        close(21)


    end subroutine
!__________________________________________________
    subroutine common_item (c1,c2,d)
        implicit none
        character(len= *) :: c1,c2
        character :: d
        integer :: n2, i, j

        print*,"a|b= ",trim(c1),"|",trim(c2)

        n2=len_trim(c1)
        do i=1,n2
            do j=1,n2
              !  print*, c1(i:i), c2(i:i)
                if(c1(i:i)==c2(j:j)) then
                    d=c1(i:i)
                    return
                end if
            end do
        end do!


    end subroutine
!__________________________________________________
    integer function prio(d)
        implicit none
        character :: d
        integer :: p

        if ((iachar('a').le.iachar(d)).and.iachar(d).le.iachar('z')) then
            !lowercase
            p=iachar(d)-iachar('a')+1
            prio=p
       !     print*,prio
            return
        else
         !uppercase
            p=iachar(d)-iachar('A')+27
            prio=p
        !    print*, prio
            return
        end if

    end function
!__________________________________________________
end program

