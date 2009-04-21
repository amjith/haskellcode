doubleMe x = 2*x
boom_bang xl = [if x<10 then "Boom" else "Bang"|x<-xl, if (mod x 2 == 1) then True else False]
length' xs = sum[1|_<-xs]
{-factorial :: Integer->Integer-}
{-factorial n = product[1..n]-}
len :: String->Int
len n = sum[1|_<-n]
circ :: Double->Double
circ r = 2*r*pi
--triangle = [(a,b,c)|a<-[1..10],b<-[1..10],c<-[1..10], a+b+c=24, a^2+b^2=c^2]
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n =  n * factorial (n-1)
addVectors :: (Num a ) => (a,a,a) -> (a,a,a) -> (a,a,a)
addVectors (a,b,c) (d,e,f) = (a+d, b+e, c+f )
{-head' :: [a]-> a
head' [] = error "Are you kidding me? You just supplied an empty list!"
head' (a:_) = a-}
len' :: [a] -> Int
len' [] = 0
len' (_:a) = 1+len' a
sum' ::(Num a) => [a] -> a
sum' [] = 0
sum' (a:x) = a+sum' x
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
 | bmi <= 18.5 = "You're underweight, you emo, you!"  
 | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
 | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
 | otherwise   = "You're a whale, congratulations!"  
 
bmiStr :: (RealFloat a) => a -> a -> String
bmiStr ht wt
 | wt/ht^2 <= 20 = "Bmi is less than or equal to 20"
 | wt/ht^2 <= 30 = "Bmi is less than or equal to 30"
 | wt/ht^2 <= 40 = "Bmi is less than or equal to 40"
 | otherwise = "Bmi is more than 40"

chk :: (Num a) => a -> String
chk a =  if a == 0 then "Equal" else "Not Equal"
{-chk a =  if a < 1 then "Less than one" else "Gt than one"-}
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname  
initials' :: String -> String -> String
initials' (f:fn) (s:sn) = "Initials "++[f]++[s]

calcBMI :: (RealFloat a) => [(a,a)] -> [a]
calcBMI xs = [bmi w h| (w, h) <-xs]
	where bmi wt ht = wt/ht^2
chk1 ::  Int -> String
chk1 a = case a of 1 -> "a is less than 2"

replicate' :: (Ord a, Num a) => a->i->[i]
replicate' num elem
	| num <=0 = []
    | otherwise = elem:replicate' (num-1) elem

take' :: (Ord n, Num n) => n->[e]->[e]
take' n _ 
	| n <= 0 = []
take' _ [] = []
take' n (e:es) = e:take' (n-1) es

{-qsort :: (Ord a)=>[a] -> [a]-}
{-qsort [] = []-}
{-qsort (x:xs) = smallSort ++ [x] ++ bigSort-}
	{-where smallSort = qsort [a | a <-xs, a<=x]-}
		  {-bigSort   = qsort [a | a <-xs, a>x]-}

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

data BookInfo = Book Int String [String]
                deriving (Show)

fibs :: [Integer]
fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)]

fibonacci :: Integer -> Integer
fibonacci n
	| n == 0 = 0
	| n == 1 = 1
	| n >1  = fibonacci (n-1) + fibonacci (n-2)

{-fib :: Integer -> Integer-}
{-fib n = fibs !! n-}
  {-where-}
    {-fibs = 0 : 1 : zipWith (+) fibs (tail fibs)-}

prime :: Integer -> Bool
prime a
	| a==1 = True
	| (a `mod` 2) == 0 = False
	| a>1 = (0==sum[1|x<-[3,5..(a `div` 2)], (0 == (a `mod` x))])

primes :: [Integer]
primes = sieve (2:[3,5..])
	where sieve (p:xs) = p : sieve [x|x<-xs, x `mod` p /= 0]

undivisible :: [Integer]
undivisible = chklst [20,19..1]
	where 
	 chklst (p:xs) = p : chklst [x|x<-xs, p `mod` x /=0 ]
	 chklst [] = [] 

tst :: [Integer]
tst = tst1 [1..10]
	where
	  {-tst1 [] = []-}
	  tst1 xs = [if (x `mod` 2) == 0 then (x `div` 2) else x |x<-xs]

applyTwice :: (a->a) -> a ->a
applyTwice f x = f(f(x))

zipWith' :: (a->b->c) ->[a]->[b]->[c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y :zipWith' f xs ys

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| even n = n:chain(n `div` 2)
	| odd n =  n:chain(n*3+1)

numLongChains' :: Int
numLongChains' = length([x|x<-[1..100], length (chain x) > 15] )

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  

-- lambda
numLongChains'' :: Int
numLongChains'' = length (filter (\x->length(x)>15) (map chain [1..100]))

flip' :: (a->b->c) -> (b->a->c)
flip' f a b =  f b a

sum'' :: (Num a) => [a] ->a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a)=> a -> [a] -> Bool
elem' y xs = foldl (\acc x -> if x == y then True else acc) False xs

map' :: (a->b)->[a]->[b]
map' f xs = [f x |x<-xs]

map'' :: (a->b)->[a]->[b]
map'' f xs = foldr (\x acc -> f x:acc) [] xs

reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) [] 

product' :: (Num a)=>[a]->a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
 
head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  

last' :: [a] -> a  
last' = foldl1 (\_ x -> x)  


