PROGRAM Metodo_del_Punto_Medio									!SOLO MODIFICAR FUNCION HASTA EL FINAL!

IMPLICIT NONE
!Declarar variables!!!!!!!!!!!!!!!
DOUBLE PRECISION :: yi, y !valor inicial variable dependiente
DOUBLE PRECISION :: xi !valor inicial variable independiente
DOUBLE PRECISION :: xf !valor final variable independiente
DOUBLE PRECISION :: dx !calculo del tamaño de paso
DOUBLE PRECISION :: xout !intervalo de salida
DOUBLE PRECISION :: x, xend, h
DOUBLE PRECISION, DIMENSION(1000) :: xp, yp	!ESPACIO DE 1000 RESULTADOS EN CASO DE SER NECESARIO 
INTEGER :: m, trabajar=1, i
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


WRITE(*,*)'Bienvenido al programa para Resolver EDO por el Metodo del Punto Medio'

!Asigna valores para
WRITE (*,*)
WRITE (*,*)'Ingrese el valor inicial ''xi'' de la variable independiente'
READ (*,*) xi
WRITE (*,*)'Ingrese el valor final ''xf'' variable independiente'
READ (*,*) xf
WRITE (*,*)'Ingrese el valor inicial ''yi'' de la variable dependiente'
READ (*,*) yi

DO WHILE (trabajar == 1)
  
WRITE (*,*)'Ingrese el tamanio de paso dx'
READ (*,*) dx
WRITE (*,*)'Ingrese el valor de los intervalos en los que desea imprimir los resultados'
WRITE (*,*)'(puede usar el tamanio de paso)'
READ (*,*) xout

y=yi
x=xi
m=1
xp(m)=x
yp(m)=y

Do
  xend=x+xout
  IF(xend>xf) THEN
    xend=xf
  END IF
  h=dx
  CALL Integrator(x,y,h,xend)
  m=m+1
  xp(m)=x
  yp(m)=y
  IF(x>=xf) EXIT
END DO

!!!!!!!!!!!!!!!!!AQUI IMPRIMIMOS LOS RESULTADOS 
WRITE (*,*)
write(*,*)'          x                          Function value'
DO i=1, m
  WRITE(*,*) xp(i),yp(i)
END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
WRITE (*,*)
WRITE(*,*) 'Ingrese 1 para trabajar de nuevo, Ingrese otro Numero para SALIR'
READ (*,*) trabajar

END DO
STOP
END PROGRAM


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE Integrator(x,y,h,xend)
IMPLICIT NONE

!Declaramos Parametros de llamado
DOUBLE PRECISION :: h
DOUBLE PRECISION :: xend
DOUBLE PRECISION :: x
DOUBLE PRECISION :: y
DOUBLE PRECISION :: ynew

DO
  IF ((xend-x)<h) THEN
    h=xend-x
  END IF
  CALL Midpoint(x,y,h,ynew)
  y=ynew
  IF (x>=xend) EXIT
END DO

RETURN
END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE Midpoint(x,y,h,ynew)
IMPLICIT NONE

!Declaramos Parametros de llamado
DOUBLE PRECISION :: x
DOUBLE PRECISION :: y
DOUBLE PRECISION :: ym
DOUBLE PRECISION :: h
DOUBLE PRECISION :: ynew
DOUBLE PRECISION :: dydx
DOUBLE PRECISION :: dymdx

CALL Derivs(x,y,dydx)
ym=y+dydx*(h/2)
CALL Derivs(x+(h/2),ym,dymdx)
ynew=y+dymdx*h
x=x+h

RETURN
END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE Derivs(x,y,dydx)
IMPLICIT NONE

!Declaramos Parametros de llamado
DOUBLE PRECISION :: x
DOUBLE PRECISION :: y
DOUBLE PRECISION :: dydx
DOUBLE PRECISION :: F

dydx=F(x,y)
RETURN
END SUBROUTINE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DOUBLE PRECISION FUNCTION F(x,y)
IMPLICIT NONE
!Declaramos arumentos llamados
DOUBLE PRECISION, INTENT(IN) :: x 
DOUBLE PRECISION, INTENT(IN) :: y

!Evaluar Expresion
F=y				!!!!!!!!!!!!!!!!!!!!!!!!!!!!ESTO ES LO QUE SE DEBE CAMBIAR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END FUNCTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
