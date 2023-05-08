-- 2 -- 
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red us = aux_amigosDe (relaciones red) us

aux_amigosDe :: [Relacion] -> Usuario -> [Usuario]
aux_amigosDe [] _ = []
aux_amigosDe (rel:rels) us | (primero rel) == us = (segundo rel) : (aux_amigosDe rels us)
                           | (segundo rel) == us = (primero rel) : (aux_amigosDe rels us)  -- si ninguno se cumple, es que no era relacion del us -- 
                           | otherwise =  aux_amigosDe rels us 

primero :: (t,t) -> t
primero (x, y) = x

segundo :: (t,t) -> t
segundo (x, y) = y

-- ejercicio 6 -- 

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us = aux_publicacionesDe (publicaciones red) us  

aux_publicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
aux_publicacionesDe [] _ = []
aux_publicacionesDe (pub:pubs) us | us == usuarioDePublicacion pub = pub : aux_publicacionesDe pubs us 
                                  | otherwise = aux_publicacionesDe pubs us

-- ejercicio 9 -- 
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red us = aux_tieneSegFiel (publicacionesDe red us) 

aux_tieneSegFiel :: [Publicacion] -> Bool -- pubs es la lista de publicaciones de us
aux_tieneSegFiel [] = False
aux_tieneSegFiel pubs = aparicionesDeLikeador (eliminarRepetidos (likesDePublicacion (head pubs))) (concatenarLikesDePublicaciones pubs) (longitud pubs) 

aparicionesDeLikeador :: [Usuario] -> [Usuario] -> Int -> Bool -- usl1 son los usuarios que le dieron like a la primera publicacion, usl2 son los usuario de la concatenarLikesDePublicaciones. -- 
aparicionesDeLikeador [] _ _ = False -- si nadie dio like en la primera publicacion, entonces False -- 
aparicionesDeLikeador _ [] _ = False -- si concateno todos los likes y es vacío, es que nadie dio like = False
aparicionesDeLikeador  _ _ [] = False -- si no tenia ninguna publicacion en la red = Falso -- 
aparicionesDeLikeador (usl1:ussl1) likesTotales pubsTotales | (cantidadDeApariciones usl1 likesTotales) == pubsTotales = True -- likesTotales es la lista concatenarLikesDePublicaciones -- 
                                                              | otherwise = aparicionesDeLikeador ussl1 likesTotales pubsTotales 

cantidadDeApariciones :: Usuario -> [Usuario] -> Int -- usX es un usuario genérico -- 
cantidadDeApariciones _ [] = 0
cantidadDeApariciones usX (us:users) | usX == us = 1 + (cantidadDeApariciones usX users) 
                                     | otherwise = cantidadDeApariciones usX users 

concatenarLikesDePublicaciones :: [Publicacion] -> [Usuario] -- agarro los likes de la primera publicación y los concateno, luego hago recursión sobre la lista sin la primera publicación, y así... --
concatenarLikesDePublicaciones [] = []
concatenarLikesDePublicaciones (pub:pubs) = eliminarRepetidos (likesDePublicacion pub) ++ concatenarLikesDePublicaciones pubs -- agregué eliminar repetidos por si alguien dio like DOS VECES -- 

-- Auxiliares universales -- 

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece t [] = False 
pertenece t x = t == head x || (pertenece t (tail x))
               
longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | not (pertenece x xs) = x : eliminarRepetidos xs
                         | otherwise = x : eliminarRepetidos (quitarTodos x xs)

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos t [] = []
quitarTodos t (x:xs) | not (pertenece t (x:xs)) = (x:xs)
                     | t == x = quitarTodos t xs
                     | otherwise = x : quitarTodos t xs
