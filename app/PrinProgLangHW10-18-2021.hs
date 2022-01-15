
data Poly a = P [a] deriving (Show, Eq)

degree :: Poly a -> Int

degree (P []) = 0
degree (P xs) = length xs - 1


removezerotrail xs = foldr (\b bs -> if null bs && (b == 0) then [] else b : bs) [] xs


scale :: (Num a, Eq a) => a -> Poly a -> Poly a
scale n (P xs) = (P (removezerotrail (map (\x -> n * x) xs)))


($$) :: (Num a, Eq a) => Poly a -> a -> a
($$) (P xs) v = polyevalhelper xs v 0

polyevalhelper (x:xs) v n = v^n * x + polyevalhelper xs v (n + 1)
polyevalhelper _ _ _ = 0


addPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a

addPoly (P xs) (P ys) = (P (removezerotrail ans))
                        where (P ans) = (addPolyHelper (P xs) (P ys))

addPolyHelper (P (x:xs)) (P (y:ys)) = (P (x+y:ans))
                                where (P ans) = addPolyHelper (P xs) (P ys)
addPolyHelper (P xs) (P []) = (P xs)
addPolyHelper (P []) (P ys) = (P ys)


multPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
multPoly (P []) _ = (P [])
multPoly (P (x : xs)) (P ys) = (addPoly (P (map (*x) ys)) (P (0 : next)))
                               where (P next) = (multPoly (P xs) (P ys))


instance (Num a, Eq a) => Num (Poly a) where
    (+) = addPoly
    negate = scale (-1)
    (*) = multPoly
    fromInteger 0 = P []
    fromInteger n = P [fromInteger n]
    abs = error "No abs for Poly"
    signum = error "No signum for Poly"

x :: (Num a) => Poly a
x = P [0,1]

y :: (Num a) => Poly (Poly a)
y = P [P [0,1]]

