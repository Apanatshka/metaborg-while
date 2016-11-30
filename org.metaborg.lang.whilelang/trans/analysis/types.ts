module types

imports

signatures/-
analysis/types-sig

type rules

Num(_) : Int()
True() : Bool()
False() : Bool()

  And(l, r)
+ Or(l, r) : Bool()
where l : ty1 and ty1 == Bool()
  else error $[Expected boolean, but got [ty1]] on l
and r : ty2 and ty2 == Bool()
  else error $[Expected boolean, but got [ty2]] on r

Not(e) : Bool()
where e : ty and ty == Bool()
  else error $[Expected boolean, but got [ty]] on e

Ref(r) : ty
where definition of r : ty

  Add(l, r)
+ Sub(l, r)
+ Mul(l, r)
+ Div(l, r) : Int()
where l : ty1 and ty1 == Int()
  else error $[Expected integer, but got [ty1]] on l
and r : ty2 and ty2 == Int()
  else error $[Expected integer, but got [ty2]] on r

  Gt(l, r)
+ Gte(l, r)
+ Lt(l, r)
+ Lte(l, r) : Bool()
where l : ty1 and ty1 == Int()
  else error $[Expected integer, but got [ty1]] on l
and r : ty2 and ty2 == Int()
  else error $[Expected integer, but got [ty2]] on r

e@Eq(l, r) : Bool()
where l : ty1 and r : ty2
  and ty1 == ty2
  else error $[Expected equal types but got [ty1] and [ty2]] on e 
