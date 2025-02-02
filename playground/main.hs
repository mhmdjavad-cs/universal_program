import Data.Function ((&))


sq x y = x^2 + y^2
addOne x = x+1


--main = 2 & addOne & print
main = 2 & (\x -> sq x 3) & print

