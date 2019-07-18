function fact(n)
 if n = 1 then 
   ret:=1;
 else 
    ret:= n * fact(n-1);
  endif;
  return ret;

  a:=5;
  res:=fact(a);