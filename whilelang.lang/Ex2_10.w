begin
  x := 2;
  y := 4;
  x := 1;
  (if y > x
    then z := y
    else x := y*y);
  x := z
end