#labeled // Very Busy Expressions example
if [a > b]1
then (
  [x := b - a]2;
  [y := a - b]3
) else (
  [y := b - a]4;
  [x := a - b]5
)