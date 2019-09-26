PROGRAM rr 
!Rung- Kutta Method to solve a d.e. 
integer count 
real k1,k2 
count=0 
write(*,10) 
10 format(1x,'To solve a differential equation using second ',\) 
write(*,20) 
20 format('order Runge Kutta method.') 
write(*,*)'Enter the initial values:' 
read(*,*)x1,y1 
write(*,*)'Enter value at which function value is to be found:' 
read(*,*)a 
write(*,*)'Enter the number of subintervals:'
read(*,*)n 
h=(a-x1)/n 
write(*,*)'Values of x and corresponding function values are: ' 
write(*,*) 
write(*,30) 
30 format(8x,' x ',5x,' f(x) ') 
write(*,40)x1,y1 
40 format(1x,F10.4,F10.4) 
50 k1=h*f(x1,y1) 
k2=h*f(x1+h,y1+k1) 
y2=y1+(k1+k2)/2 
x2=x1+h 
write(*,40)x2,y2 
count=count+1 
IF(count.LT.n) THEN 
x1=x2 
y1=y2 
GOTO 50 
ENDIF 
write(*,*) 
write(*,60)x2,y2 
60 format(1x,'The value of the function at x=',F10.4,' is:',F10.4) 
STOP 
END 
function f(x,y) 
f=-2*(x**3)+12*(x**2)-20*x+8.5
return 
END 
