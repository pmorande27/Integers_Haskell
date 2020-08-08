import Prelude hiding (max, min)
data Nat = Zero | Suc Nat | Prev Nat deriving(Eq)
toString:: Nat -> String
toString Zero = "Zero"
toString (Prev x) = "Prev " ++ toString x
toString (Suc x) = "Suc " ++ toString x

simpli :: Nat -> Nat
simpli Zero = Zero
simpli (Suc(Prev x)) = simpli x 
simpli (Prev(Suc(x))) = simpli x
simpli (Suc Zero) = Suc Zero
simpli (Prev Zero) = Prev Zero
simpli (Suc x) =  (Suc (simpli x))
simpli (Prev x) =  (Prev (simpli x))
simplify :: Nat -> Nat
simplify x
    |simpli x == simpli (simpli x) = simpli x
    |otherwise = simplify (simpli x)
a :: Nat 
a = simplify (Prev(Prev(Suc(Suc(Suc(Prev(Prev(Zero))))))))
myEq :: Nat -> Nat -> Bool
myEq x y = simplify x == simplify y
b = (Suc(Suc(Prev(Prev(Prev(Zero))))))
neg :: Nat -> Nat
neg Zero = Zero
neg (Prev x) = Suc (neg x)
neg (Suc x) = Prev (neg x)
main = putStrLn (  toString (neg(simplify b)))
adding :: Nat -> Nat -> Nat
adding Zero x = x
adding (Suc x) y = Suc (adding x y)
adding (Prev x) y = Prev (adding x y)
add :: Nat -> Nat -> Nat 
(add) x y = simplify (adding x y)
subs :: Nat -> Nat -> Nat
subs x y = add x (neg y)
max :: Nat -> Nat -> Nat
max x y = helper x y (simplify x) (simplify y)
    where
    helper x y Zero (Prev(yy)) = x
    helper x y (Prev(xx)) Zero = y
    helper x y Zero yy = y
    helper x y xx Zero = x
    helper x y Zero yy = y
    helper x y xx Zero = x
  
    helper x y (Prev(xx)) (Prev(yy)) = helper x y xx yy
    helper x y (Suc(xx)) (Suc(yy)) = helper x y xx yy
    helper x y xx (Prev(yy)) = x
    helper x y xx (Prev(yy)) = y

min :: Nat -> Nat -> Nat
min x y 
    | max x y == x = y
    | otherwise = x


mult :: Nat -> Nat -> Nat
mult x y = helper (simplify x) (simplify y)
    where
    helper x Zero = Zero
    helper Zero y = Zero
    helper x (Suc Zero) = x
    helper (Suc Zero) x = x
    helper  (Prev Zero) x = neg x
    helper x (Prev Zero) = neg x
    helper (Suc x) (Suc y) = add (Suc x) (helper (Suc x) y) 
    helper (Suc x) (Prev y) = neg (helper (Suc x) (Suc (neg y))) 
    helper (Prev x) (Suc y) = neg (helper (Suc (neg x)) (Suc y) )
    helper (Prev x) (Prev y) = helper (Suc (neg x)) (Suc (neg y))


