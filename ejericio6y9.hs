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

cantidadDeApariciones :: Usuario -> [Usuario] -> Int -- usX es un usuario genérico -- 
cantidadDeApariciones usX [] = 0
cantidadDeApariciones usX (us:uss) | usX == us = 1 + cantidadDeApariciones usX uss 
                                   | usX /= us = 0 + cantidadDeApariciones usX uss 

cantidadDePublicacionesDe :: [Publicacion] -> Usuario -> Int
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
                 
longitud :: [t] -> Int
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
-- Ejemplos

usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)
 


 