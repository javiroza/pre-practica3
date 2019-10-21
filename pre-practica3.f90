!---------------------------- Pràctica 3 ----------------------------------!
! Autor: Javier Rozalén Sarmiento
! Grup: B1B
! Data: 22/10/2019
!
! Funcionalitat: es desenvolupen els algorismes de la bisecció i 
! de Newton-Raphson i comprova la seva eficàcia mitjançant exemples.

program pre_practica3
implicit none
double precision A,B,Eo(9),eps,xarrel,vect1(34),vect2(420),fu,dfu,dfu1(34),dfu2(420)
double precision imatges1(34),derivades1(34),imatges2(420),derivades2(420),der(34),der2(420)
integer niter,i,ndat1,ndat2
external fun

!---------------- Arrels d'F(E) en [0,2pi] --> Bisecció ------------------!
A = 1.d0
B = 1.5d0
eps = 1.d-12
call Bisection(A,B,eps,fun,niter,xarrel)
print*,""
A = 2.d0
B = 3.d0
call Bisection(A,B,eps,fun,niter,xarrel)
print*,""

!------------- Arrels d'F(E) en [0,2pi] --> Newton-Raphson ---------------!
Eo = (/ 0.1,0.2,0.67,0.7,1.,2.5,2.6,4.,5.4 /)
open(12,file="P3-1920-res.dat")
do i=1,9
    call NewtonRap(Eo(i),eps,fun,niter,xarrel)
    Eo = (/ 0.1,0.2,0.67,0.7,1.,2.5,2.6,4.,5.4 /)
    write(12,*) Eo(i),niter
enddo
close(12)

!---------------- Derivades numèriques d'F(E) en [0,2pi] -----------------!
! Generació dels vectors de 34 punts
ndat1=34
do i=1,ndat1
    vect1(i)=i*((2.d0*acos(-1.d0))/34.d0) ! Vector amb 34 valors x
    call fun(vect1(i),fu,dfu) 
    imatges1(i)=fu ! Vector amb les imatges de vect1
    derivades1(i)=dfu ! Vector amb les derivades de imatges1(n) al punt vect1(n)
enddo

! Generació dels vectors de 420 punts
ndat2=420
do i=1,ndat2
    vect2(i)=i*((2.d0*acos(-1.d0))/420.d0) ! Vector amb 420 valors x
    call fun(vect2(i),fu,dfu)
    imatges2(i)=fu ! Vector amb les imatges de vect2
    derivades2(i)=dfu ! Vector amb les derivades de imatges2(n) al punt vect2(n)
enddo

! Escriptura en el fitxer 1 (34 valors)
open(13,file="P3-1920-res3-n34.dat")
call derfun(ndat1,vect1(ndat1),imatges1(ndat1),dfu1(ndat1)) ! L'output és dfu1(ndat1)
do i=1,ndat1
    write(13,*) vect1(i),imatges1(i),der(i),derivades1(i)
enddo
close(13)

! Escriptura en el fitxer 2 (420 valors)
open(14,file="P3-1920-res3-n420.dat")
call derfun(ndat2,vect2(ndat1),imatges2(ndat1),dfu2(ndat2)) ! L'output és dfu2(ndat2)
do i=1,ndat2
    write(13,*) vect2(i),imatges2(i),der(i),derivades2(i)
enddo
close(14)

end program pre_practica3







! Subrutina NewtonRap --> retorna la posició d'un zero d'una funció donada
subroutine NewtonRap(x0,eps,function,niter,xarrel)
    implicit none
    double precision x0,x1,eps,xarrel,fu,dfu
    integer niter
    niter = 1
12  call function(x0,fu,dfu)
    x1 = x0-(fu/dfu)
    if (abs(x1-x0).le.eps) then
        xarrel = x1
    else
        x0 = x1
        niter = niter+1
        goto 12
    endif
    print*,"Nombre d'iteracions:",niter
    print*,"Arrel:",xarrel
    print*,""
    return
end subroutine NewtonRap

! Subrutina Bisection --> retorna la posició d'un zero d'una funció donada
subroutine Bisection(A,B,eps,function,niter,xarrel)
    implicit none
    double precision A,B,eps,C,fu,dfu,fa,fb,fc,xarrel
    integer niter,maxiter,i
    maxiter = nint(log((B-A)/eps)/log(2.d0)) ! Nombre màxim d'iteracions
    ! Algoritme de la bisecció
    do i=1,maxiter-1    
        call function(A,fu,dfu)
        fa = fu
        call function(B,fu,dfu)
        fb = fu
! Hi ha canvi de signe? Si és així, seguim
        if ((fa*fb).lt.0.d0) then
            C = (A+B)/2.d0
! L'interval és tant petit com es volia? Si és així, parem
            if ((B-A).lt.eps) then
                niter = i
                xarrel = C
                exit
            else
                call function(C,fu,dfu)
                fc = fu
! Hem trobat la solució exacta? Si és així, parem         
                if (fc.eq.0.d0) then 
                    niter = i
                    xarrel = C
                    exit
                else
! En cas contrari, continuem iterant                    
                    if ((fa*fc).lt.0.d0) then
                        B = C
                    else if ((fc*fb).lt.0.d0) then
                        A = C
                    else
                        print*,"Algo ha anat molt malament noi, fes-t'ho mirar."
                    endif
                endif
            endif
        else
            print*,"No hi ha canvi de signe en l'interval donat."
            exit
        endif
    enddo
    xarrel = C
    niter = i
    print*, "Bisecció"
    print*, "Nombre màxim d'iteracions:",maxiter
    print*, "Nombre d'iteracions: ",niter
    print*, "Arrel: ",xarrel
    return
end subroutine Bisection

! Subrutina fun --> retorna els valors numèrics de f(x) i f'(x)
subroutine fun(x,fu,dfu)
    implicit none
    double precision x,fu,dfu
! P(E) = 35.d0/16.d0 + 0.5d0*x - (61.d0/20.d0)*x**2 + x**3
! P'(E) = 0.5d0 - 2*(61.d0/20.d0)*x + 3*x**2
    fu = sinh(x)*(35.d0/16.d0+0.5d0*x-(61.d0/20.d0)*x**2+x**3)
    dfu = cosh(x)*(0.5d0-2*(61.d0/20.d0)*x+3*x**2)+sinh(x)*(0.5d0-2*(61.d0/20.d0)*x+3*x**2)
    return 
end subroutine fun

! Subrutina derfun --> retorna la derivada d'una funció en un interval de punts
subroutine derfun(ndat,x,fu,der)
    implicit none
    double precision x(ndat),fu(ndat),der(ndat),h
    integer ndat,i
    h=(2.d0*acos(-1.d0))/ndat
    do i=1,ndat
        if (i.eq.1) then
            der(i)=(fu(i+1)-fu(i))/h
        else if (i.eq.ndat) then
            der(i)=(fu(i)-fu(i-1))/h
        else
            der(i)=(fu(i+1)-fu(i-1))/(2*h)
        endif
    enddo
    return 
end subroutine derfun


