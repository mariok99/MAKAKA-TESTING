{-# OPTIONS_GHC -Wno-overlapping-patterns #-}


type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])



-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- ejercicio 6 -- 
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us = aux_publicacionesDe (publicaciones red) us  

aux_publicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
aux_publicacionesDe [] us = []
aux_publicacionesDe (pub:pubs) us | us == usuarioDePublicacion pub = pub:aux_publicacionesDe pubs us 
                                  | us /= usuarioDePublicacion pub = aux_publicacionesDe pubs us

primeraPublicacion :: [Publicacion] -> Publicacion 
primeraPublicacion (x:xs) = x  


-- ejercicio 9 -- 
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red us = aux1_tieneUnSeguidorFiel (publicacionesDe red us) us 
 



aux1_tieneUnSeguidorFiel :: [Publicacion] -> Usuario -> Bool --(pub:pubs) es la lista de publicaciones de us
aux1_tieneUnSeguidorFiel [] _ = False
aux1_tieneUnSeguidorFiel (pub:pubs) us = aux2_tieneUnSeguidorFiel (likesDePublicacion pub) (concatenarLikesDePublicaciones (pub:pubs) us) (pub:pubs) 

--cantidadDeApariciones (usuarioQueDioLike (likesDePrimeraPublicacionDeUsuarioX (pub:pubs) us)) (concatenarLikesDePublicaciones (pub:pubs) us) == cantidadDePublicacionesDe (pub:pubs) us = True

aux2_tieneUnSeguidorFiel :: [Usuario] -> [Usuario] -> [Publicacion] -> Bool -- usl1 son los usuarios que le dieron like a la primera publicacion, usl2 son los usuario de la concatenarLikesDePublicaciones. -- 
aux2_tieneUnSeguidorFiel [] _ _ = False -- si nadie dio like en la primera publicacion, entonces False -- 
aux2_tieneUnSeguidorFiel _ [] _ = False -- si concateno todos los likes y es vacío, es que nadie dio like = False
aux2_tieneUnSeguidorFiel  _ _ [] = False -- si no tenia ninguna publicacion en la red = Falso -- 
aux2_tieneUnSeguidorFiel (usl1:ussl1) likesTotales pubsDeUser | cantidadDeApariciones (usl1) likesTotales ==  -- likesTotales es la lista concatenarLikesDePublicaciones -- 
                                                                longitud pubsDeUser = True
                                                              | otherwise = aux2_tieneUnSeguidorFiel ussl1 likesTotales pubsDeUser 

cantidadDeApariciones :: Usuario -> [Usuario] -> Integer -- usX es un usuario genérico -- 
cantidadDeApariciones usX [] = 0
cantidadDeApariciones usX (us:uss) | usX == us = 1 + cantidadDeApariciones usX uss 
                                   | usX /= us = 0 + cantidadDeApariciones usX uss 

cantidadDePublicacionesDe :: [Publicacion] -> Usuario -> Integer
cantidadDePublicacionesDe (pub:pubs) us = longitud (aux_publicacionesDe (pub:pubs) us) 

concatenarLikesDePublicaciones :: [Publicacion] -> Usuario -> [Usuario] -- agarro los likes de la primera publicación y los concateno, luego hago recursión sobre la lista sin la primera publicación, y así... --
concatenarLikesDePublicaciones [] us = []
concatenarLikesDePublicaciones (pub:pubs) us = eliminarRepetidos (likesDePublicacion pub) ++ 
                                               concatenarLikesDePublicaciones pubs us -- agregué eliminar repetidos por si alguien dio like DOS VECES -- 


pertenece :: (Eq t) => t -> [t] -> Bool
pertenece t [] = False 
pertenece t x = t == head x || (pertenece t (tail x))

ultimo :: [t] -> t
ultimo x | (longitud x) == 1 = (head x)
         | otherwise = ultimo (tail x) 

principio :: [a] -> [a]
principio (x:xs) | longitud (x:xs) == 1 = []
                 | otherwise = x:(principio xs)
                 
longitud :: [t] -> Integer 
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | head (sacarSiPrimerEsRepetido (x:xs)) /= x = x:eliminarRepetidos xs
                         | otherwise = head (sacarSiPrimerEsRepetido (x:xs)): eliminarRepetidos ((quitarTodos (head (sacarSiPrimerEsRepetido (x:xs)))) (x:xs))

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos t [] = []
quitarTodos t (x:xs) | t == x = quitarTodos t xs
                     | otherwise = (x:quitarTodos t xs)

sacarSiPrimerEsRepetido :: (Eq t) => [t] -> [t]
sacarSiPrimerEsRepetido (x:xs) | not (pertenece x xs) = (x:xs) -- en realidad si no hay ningún repetido, deja la secuencia tal como estaba -- 
                        | pertenece x xs = [x] -- si el primero es repetido, lo separa en una lista con el repetido como único enlemento -- 
                        | otherwise = sacarSiPrimerEsRepetido xs               

-- 2 -- 
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red us = aux_amigosDe (relaciones red) us

aux_amigosDe ::[Relacion] -> Usuario -> [Usuario]
aux_amigosDe [] _ = []
aux_amigosDe (rel:rels) us | (primerElemDeRelacion rel) == us = (segundoElemDeRelacion rel):aux_amigosDe rels us
                           | segundoElemDeRelacion rel == us = (primerElemDeRelacion rel):aux_amigosDe rels us  -- si ninguno se cumple, es que no era relacion del us -- 
                           | otherwise =  aux_amigosDe rels us 

primerElemListaDeRelaciones :: [Relacion] -> Relacion
primerElemListaDeRelaciones (rel:rels) = rel 

primerElemDeRelacion :: Relacion -> Usuario
primerElemDeRelacion (usuarioX, usuarioY) = usuarioX

segundoElemDeRelacion :: Relacion -> Usuario
segundoElemDeRelacion (usuarioX, usuarioY) = usuarioY


 