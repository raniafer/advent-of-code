
program aire_et_volume
    implicit none

!declaration
    real :: r,a,v
    real :: pi=acos(-1.0)
   integer :: p

!input
    print*, "entrer rayon"
    read*, r

!condition
 do while (r<0)
 print*, "entrer rayon positif"
 read*, r
 end do

!calcul
  maboucle : do a=pi*r**2

    v=(4/3)*pi*r**3 end do maboucle

!affichage
    print*, "l'aire du cercle est",a,"et  &
    &  le volume de la sphere corr est",v

!deuxieme question
print*, "continuer ou non"
read*, p

do while (p==0)
    exit
end do

end program aire_et_volume

