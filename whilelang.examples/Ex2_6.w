#labeled // Reaching Definitions example
[x := 5]1;
[y := 1]2;
while [x > 1]3
do (
  [y := x * y]4;
  [x := x - 1]5
)