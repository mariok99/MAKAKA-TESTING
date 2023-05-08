-- 3 -- 
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red us  =  longitud (amigosDe(red) us)


-- 4 -- 
-- 3 -- 
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red us  =  longitud (amigosDe (red) us)
-- 4 --
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = buscarUsuarioMax red (x:xs) m 
  where
    (x:xs) = usuarios red 
    m = maximo (listaCantDeAmigos red (x:xs))

buscarUsuarioMax :: RedSocial -> [Usuario] -> Int -> Usuario    
buscarUsuarioMax red (x:xs) m | cantidadDeAmigos red x == m = x     
                              | otherwise = buscarUsuarioMax red xs m

listaCantDeAmigos :: RedSocial -> [Usuario] -> [Int]
listaCantDeAmigos red [] = []  
listaCantDeAmigos red (x:xs) = cantidadDeAmigos red x : listaCantDeAmigos red xs 

maximo :: [Int] -> Int
maximo (x:[]) = x 
maximo (x:xs) | x > head xs = mayor x (maximo xs)
              | otherwise = mayor (head xs) (maximo xs)

mayor :: Int -> Int -> Int 
mayor x y | x >= y = x 
          | otherwise = y 