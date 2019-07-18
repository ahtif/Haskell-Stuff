x:=0;
y:=1;
n:=5;
while n>0 do
  z:= x+y;
  x:=y;
  y:=z;
  n:=n-1;
endwhile;