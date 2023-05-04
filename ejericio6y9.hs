{-# OPTIONS_GHC -Wno-overlapping-patterns #-}


type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

usuario1 :: (Integer, String)
usuario1 = (1, "Juan")
usuario2 :: (Integer, String)
usuario2 = (2, "Natalia")
usuario3 :: (Integer, String)
usuario3 = (3, "Pedro")
usuario4 :: (Integer, String)
usuario4 = (4, "Mariela")
usuario5 :: (Integer, String)
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 :: ((Integer, String), (Integer, String))
relacion1_3 = (usuario1, usuario3)
relacion1_4 :: ((Integer, String), (Integer, String))
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 :: ((Integer, String), (Integer, String))
relacion2_3 = (usuario3, usuario2)
relacion2_4 :: ((Integer, String), (Integer, String))
relacion2_4 = (usuario2, usuario4)
relacion3_4 :: ((Integer, String), (Integer, String))
relacion3_4 = (usuario4, usuario3)

publicacion1_0 = (usuario1, "quiero muchos likes", [usuario2, usuario3, usuario4, usuario5])
publicacion1_1 :: ((Integer, String), String, [(Integer, String)])
publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 :: ((Integer, String), String, [(Integer, String)])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 :: ((Integer, String), String, [(Integer, String)])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 :: ((Integer, String), String, [a])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 :: ((Integer, String), String, [(Integer, String)])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])
publicacion1_6 = (usuario1, "MI SESTO POS", [usuario5, usuario3])

publicacion2_1 :: ((Integer, String), String, [(Integer, String)])
publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 :: ((Integer, String), String, [(Integer, String)])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 :: ((Integer, String), String, [a])
publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 :: ((Integer, String), String, [(Integer, String)])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 :: ((Integer, String), String, [(Integer, String)])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 :: ((Integer, String), String, [(Integer, String)])
publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 :: ((Integer, String), String, [a])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 :: ((Integer, String), String, [(Integer, String)])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])
publicacion4_4 = (usuario4, "Que funcione plis", [])
publicacion4_5 = (usuario4, "SIUIUIUIU", [])
publicacion4_6 = (usuario4, "otro ejemplo sin likes", [])

usuariosA :: [(Integer, String)]
usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA :: [((Integer, String), (Integer, String))]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA :: [((Integer, String), String, [(Integer, String)])]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA :: ([(Integer, String)], [((Integer, String), (Integer, String))],
 [((Integer, String), String, [(Integer, String)])])
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

redW = (usuariosA, relacionesA, publicacionesW)
publicacionesW = [publicacion1_0, publicacion1_3, publicacion1_5, publicacion1_6, publicacion4_1, publicacion4_3]

publicacionesC :: [((Integer, String), String, [(Integer, String)])]
publicacionesC = [publicacion1_1]

publicacionesD = [publicacion2_1]


redX = (usuariosA, relacionesA, publicacionesX)
publicacionesX = [publicacion3_2, publicacion3_3,publicacion4_4, publicacion4_5, publicacion4_6]

redY= (usuariosA, relacionesA, publicacionesX)
publicacionesY = [publicacion3_2, publicacion3_3,publicacion4_3, publicacion4_4, publicacion4_5, publicacion4_6]

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
aux_publicacionesDe (pub:pubs) us | us == usuarioDePublicacion (primeraPublicacion (pub:pubs)) = (primeraPublicacion (pub:pubs)):aux_publicacionesDe pubs us 
                                  | us /= usuarioDePublicacion (primeraPublicacion (pub:pubs)) = aux_publicacionesDe pubs us

primeraPublicacion :: [Publicacion] -> Publicacion 
primeraPublicacion (x:xs) = x  


-- ejercicio 9 -- 
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red us = aux1_tieneUnSeguidorFiel (publicaciones red) us 




aux1_tieneUnSeguidorFiel :: [Publicacion] -> Usuario -> Bool
aux1_tieneUnSeguidorFiel (pub:pubs) us = aux2_tieneUnSeguidorFiel (likesDePrimeraPublicacionDeUsuarioX (pub:pubs) us) (concatenarLikesDePublicaciones (pub:pubs) us) (aux_publicacionesDe (pub:pubs) us) 

--cantidadDeApariciones (usuarioQueDioLike (likesDePrimeraPublicacionDeUsuarioX (pub:pubs) us)) (concatenarLikesDePublicaciones (pub:pubs) us) == cantidadDePublicacionesDe (pub:pubs) us = True

aux2_tieneUnSeguidorFiel :: [Usuario] -> [Usuario] -> [Publicacion] -> Bool -- usl1 son los usuarios que le dieron like a la primera publicacion, usl2 son los que le dieron like al resto de publicaciones, incluyendo la primera.
aux2_tieneUnSeguidorFiel [] _ _ = False -- si nadie dio like en la primera publicacion, entonces False -- 
aux2_tieneUnSeguidorFiel _ [] _ = False -- si concateno todos los likes y es vacío, es que nadie dio like = False
aux2_tieneUnSeguidorFiel  _ _ [] = False -- si no tenia ninguna publicacion en la red = Falso -- 
aux2_tieneUnSeguidorFiel (usl1:ussl1) (usl2:ussl2) (pub:pubs) | cantidadDeApariciones (usuarioQueDioLike (usl1:ussl1)) (usl2:ussl2) == longitud (pub:pubs) = True
                                                              | otherwise = aux2_tieneUnSeguidorFiel ussl1 (usl2:ussl2) (pub:pubs) 

cantidadDeApariciones :: Usuario -> [Usuario] -> Integer
cantidadDeApariciones usX [] = 0
cantidadDeApariciones usX (us:uss) | usX == us = 1 + cantidadDeApariciones usX uss 
                                   | usX /= us = 0 + cantidadDeApariciones usX uss 

cantidadDePublicacionesDe :: [Publicacion] -> Usuario -> Integer
cantidadDePublicacionesDe (pub:pubs) us = longitud (aux_publicacionesDe (pub:pubs) us) 

concatenarLikesDePublicaciones :: [Publicacion] -> Usuario -> [Usuario] -- agarro los likes de la primera publicación y los concateno, luego hago recursión sobre la lista sin la primera publicación, y así... --
concatenarLikesDePublicaciones [] us = []
concatenarLikesDePublicaciones (pub:pubs) us = (eliminarRepetidos (likesDePrimeraPublicacionDeUsuarioX (pub:pubs) us)) ++ concatenarLikesDePublicaciones (tail(aux_publicacionesDe (pub:pubs) us)) us -- agregué eliminar repetidos por si alguien dio like DOS VECES -- 

likesDePrimeraPublicacionDeUsuarioX :: [Publicacion] -> Usuario -> [Usuario] -- devuelve la lista de usuarios que dieron like a la primera publicacion de un usuario determinado (us)-- 
likesDePrimeraPublicacionDeUsuarioX (pub:pubs) us | aux_publicacionesDe (pub:pubs) us == [] = [] -- si no tiene ninguna publicacion, no va a tener likes -- 
                                                  | aux_publicacionesDe (pub:pubs) us /= [] = likesDePublicacion (primeraPublicacion (aux_publicacionesDe (pub:pubs) us)) -- probada con publicacionesA usuario1, exito. publicacionesA usuario4, exito -- 

usuarioQueDioLike :: [Usuario] -> Usuario -- devuelve al primer usuario que dio like -- 
usuarioQueDioLike (us:uss) = us 

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


{-aux1_tieneUnSeguidorFiel :: [Publicacion] -> Usuario -> Bool
aux1_tieneUnSeguidorFiel (pub:pubs) us = (longitud (aux_publicacionesDe (pub:pubs) us) == 1) && (longitud (likesDePrimeraPublicacionDeUsuarioX (pub:pubs) us) >= 1) -- si us solo ha hecho una publicacion, y esa publicacion tiene 1 o más likes = true -- 
aux1_tieneUnSeguidorFiel (pub:pubs) us | pertenece (usuarioQueDioLike (likesDePrimeraPublicacionDeUsuarioX (pub:pubs) us)) (likesDePublicacion (ultimo (aux_publicacionesDe (pub:pubs) us))) && aux1_tieneUnSeguidorFiel (principio (pub:pubs)) us = True -- hago recursión sobre (likesDePublicacion (ultimo (aux_publicacionesDe (pub:pubs) us))) -- 
                                      | otherwise = aux1_tieneUnSeguidorFiel pubs us
                                      -- | not (pertenece (usuarioQueDioLike (likesDePrimeraPublicacionDeUsuarioX (pub:pubs) us)) (likesDePublicacion (ultimo (aux_publicacionesDe (pub:pubs) us)))) = aux1_tieneUnSeguidorFiel (principio (pub:pubs)) us --  
-}                                  
--aux1_tieneUnSeguidorFiel :: [Publicacion] -> Usuario -> Bool
--aux1_tieneUnSeguidorFiel (pub:pubs) us | cantidadDeApariciones

-- 2 -- 
{-amigosDe :: Red Social -> Usuario -> [Usuario]
amigosDe red us |primerElemDeRelacion (primerElemListaDeRelaciones (relaciones (red))) == us || segundoElemDeRelacion (primerElemListaDeRelaciones (relaciones (red)))
-}
primerElemListaDeRelaciones :: [Relacion] -> Relacion
primerElemListaDeRelaciones (rel:rels) = rel 


primerElemDeRelacion :: Relacion -> Usuario
primerElemDeRelacion (usuarioX, usuarioY) = usuarioX

segundoElemDeRelacion :: Relacion -> Usuario
segundoElemDeRelacion (usuarioX, usuarioY) = usuarioY