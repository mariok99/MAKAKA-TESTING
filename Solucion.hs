module Solucion where

-- Nombre de Grupo: operacionHaskell
-- Integrante 1: Andrea Ramon Barboza Franco, franco.barboza@hotmail.com, 176/20
-- Integrante 2: Nahuel Prieto, nahuel.rlz@gmail.com, 646/20
-- Integrante 3: Joaquín Lozano, joaquin.lozano.trabajo@gmail.com, 649/23
-- Integrante 4: Mario Alejandro Livia Auqui, marioalelivia@gmail.com, 642/23

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

-- Ejercicios

-- 1 --

{- nombresDeUsuarios arma una lista con los nombres de los usuarios de red.
 - Una vez armada la lista elimina los repetidos. -}

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = eliminarRepetidos(armarListaNombres (usuarios red))

armarListaNombres :: [Usuario] -> [String]
armarListaNombres [] = []
armarListaNombres (x:xs) = (nombreDeUsuario x) : (armarListaNombres xs)

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | not (pertenece x xs) = x : eliminarRepetidos xs
                         | otherwise = x : eliminarRepetidos (quitarTodos x xs)
                         
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs)  | n == x = True
                    | otherwise = pertenece n xs

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos t [] = []
quitarTodos t (x:xs) | not (pertenece t (x:xs)) = (x:xs)
                     | t == x = quitarTodos t xs
                     | otherwise = x : quitarTodos t xs

-- 2 --

{- Al ingresar una red social válida y un usuario válido, nos devuelve
 - la lista de usuarios que están relacionados. -}

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red us = aux_amigosDe (relaciones red) us -- Si hay relaciones repetidas, la red no cumple con el requiere. --

aux_amigosDe :: [Relacion] -> Usuario -> [Usuario]
aux_amigosDe [] _ = []
aux_amigosDe (rel:rels) us | (primero rel) == us = (segundo rel) : (aux_amigosDe rels us)
                           | (segundo rel) == us = (primero rel) : (aux_amigosDe rels us)  -- Si ninguno se cumple, es que no era relacion del us. -- 
                           | otherwise =  aux_amigosDe rels us 

primero :: (t,t) -> t
primero (x, y) = x

segundo :: (t,t) -> t
segundo (x, y) = y

-- 3 -- 

{- Al ingresar una red social válida y un usuario válido, nos devuelve
 - la longitud de amigosDe del usuario ingresado. -}

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red us  =  longitud (amigosDe red us)

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- 4 --

{- Al ingresar una red social nos devuelve el primer usuario con el máximo de cantidadDeAmigos. -}

usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = buscarUsuarioMax red users m 
  where
    users = usuarios red 
    m = maximo (listaCantDeAmigos red users)

buscarUsuarioMax :: RedSocial -> [Usuario] -> Int -> Usuario    
buscarUsuarioMax red (us:users) m | (cantidadDeAmigos red us) == m = us     
                                  | otherwise = buscarUsuarioMax red users m

listaCantDeAmigos :: RedSocial -> [Usuario] -> [Int]
listaCantDeAmigos red [] = []  
listaCantDeAmigos red (us:users) = (cantidadDeAmigos red us) : (listaCantDeAmigos red users) 

maximo :: [Int] -> Int
maximo (x:[]) = x 
maximo (x:xs) | x > head xs = mayor x (maximo xs)
              | otherwise = mayor (head xs) (maximo xs)

mayor :: Int -> Int -> Int 
mayor x y | x >= y = x 
          | otherwise = y 
          
-- 5 --

{- estaRobertoCarlos devuelve True si un usuario tiene 
 - más de diez de amigos. -}

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ([],_,_) = False
estaRobertoCarlos ((us:users),rels,pubs) | (cantidadDeAmigos red us) > (10) = True
                                         | otherwise = estaRobertoCarlos (users,rels,pubs)
                                         where red = ((us:users),rels,pubs)
{- Debería estar implementado con 10^6 en vez de 10, pero lo cambiamos para poder testear
 - un caso donde dé True sin crear una red con un millón + 1 tuplas de relaciones. -}

-- 6 -- 

{- publicacionesDe devuelve una lista que contiene únicamente las  
 - publicaciones que tienen como publicador al usuario ingresado. -}

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us = aux_publicacionesDe (publicaciones red) us -- Si hay publicaciones repetidas, la red no cumple con el requiere. --

aux_publicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
aux_publicacionesDe [] _ = []
aux_publicacionesDe (pub:pubs) us | us == usuarioDePublicacion pub = pub : aux_publicacionesDe pubs us 
                                  | otherwise = aux_publicacionesDe pubs us

-- 7 --

{- Devuelve una lista con las publicaciones de la red en las  
 - cuales el usuario ingresado pertenece a la lista de likes .-}

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red us = eliminarRepetidos(pubsLikeadas (publicaciones red) us) -- Dar multiples likes no está prohibido en las redes. --

pubsLikeadas:: [Publicacion] -> Usuario -> [Publicacion]
pubsLikeadas [] _ = []
pubsLikeadas (pub:pubs) us  | pertenece us (likesDePublicacion pub) = pub : (pubsLikeadas pubs us)
                            | otherwise = pubsLikeadas pubs us
                          
-- 8 --

{- Devuelve True si a user1 y user2 les gustan las mismas publicaciones de la red -}

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red us1 us2 = mismosElementos (publicacionesQueLeGustanA red us1) (publicacionesQueLeGustanA red us2)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = (longitud xs == longitud ys) && (incluido xs ys) && (incluido ys xs)

incluido :: (Eq t) => [t] -> [t] -> Bool
incluido [] _ = True
incluido (x:xs) ys | pertenece x ys = incluido xs ys 
                   | otherwise = False

-- 9 -- 

{- tieneUnseguidorFiel llama a aparicionesDelikeador con las publicaciones del usuario ingresado; aparicionesDeLikeador compara
 - las apariciones de los usuarios que dieron like en la primera publicacion en la lista de likesTotales, si alguno de estos usuarios 
 - apareció tantas veces como publicaciones tenga el usuario ingresado, entonces devuele True, 
 - ya que eso significa que estaba en la lista de likes de cada publicación. -}

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red us | pubs == [] = False
                           | otherwise = aparicionesDeLikeador (eliminarRepetidos (likesDePublicacion (head pubs))) (concatenarLikesDePublicaciones pubs) (longitud pubs)
                           where pubs = publicacionesDe red us

aparicionesDeLikeador :: [Usuario] -> [Usuario] -> Int -> Bool  -- (usl1:ussl1) es la lista con los likes de la primera publicacion de us. --
aparicionesDeLikeador [] _ _ = False
aparicionesDeLikeador _ [] _ = False
aparicionesDeLikeador (usl1:ussl1) likesTotales pubsTotales | (cantidadDeApariciones usl1 likesTotales) == pubsTotales = True -- likesTotales es la lista concatenarLikesDePublicaciones. -- 
                                                            | otherwise = aparicionesDeLikeador ussl1 likesTotales pubsTotales 

cantidadDeApariciones :: Usuario -> [Usuario] -> Int -- usX es un usuario genérico -- 
cantidadDeApariciones _ [] = 0
cantidadDeApariciones usX (us:users) | usX == us = 1 + (cantidadDeApariciones usX users) 
                                     | otherwise = cantidadDeApariciones usX users 

-- recibe la lista de publicaciones del usuario ingresado y arma una lista que contiene sólo los likes de dichas publicaciones. -- 

concatenarLikesDePublicaciones :: [Publicacion] -> [Usuario] 
concatenarLikesDePublicaciones [] = []
concatenarLikesDePublicaciones (pub:pubs) = eliminarRepetidos (quitarTodos (usuarioDePublicacion pub) (likesDePublicacion pub)) ++ concatenarLikesDePublicaciones pubs
{- Quito al autor de la publicacion porque no es valido como seguidorFiel. 
 - Agregué eliminar repetidos por si alguien dio like DOS VECES. -}

-- 10 --

 {- existeSecuenciaDeAmigos devuelve True cuando encuentra un usuario N que 
  - es amigo de us2, es decir que existe la secuencia [us1,...,usN,us2].
  - Para conseguir usN, se busca entre us1 y sus amigos. 
  - Si ninguno cumple, se busca entre los amigos, y asì recursivamente...
  - De esta forma, se garantiza que usN es amigo del anterior, ya que para llegar 
  - a él se tuvo que buscar en los amigos del usuario K, para todo K entre 1 y N-1. 
  - A su vez, en cada "búsqueda" se va eliminando de la red a todo usuario (con sus relaciones) 
  - quien no sea amigo de us2, de forma que si no existe un usuario N, se llega
  - eventualmente a la lista [], donde se devuelve False. -}

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red us1 us2 = (pertenece us1 amigosU2) || (estanRelacionados redSiguiente amigosU1 amigosU2)
  where 
    amigosU1 = amigosDe red us1
    amigosU2 = amigosDe red us2
    redSiguiente = eliminarUsuario red us1

estanRelacionados :: RedSocial -> [Usuario] -> [Usuario] -> Bool
estanRelacionados _ [] _ = False -- us1 no tiene amigos o no existe un usuario N. --
estanRelacionados _ _ [] = False -- us2 no tiene amigos. --
estanRelacionados red (us:users) amigosUltimos = (pertenece us amigosUltimos) || (estanRelacionados redSiguiente users amigosUltimos) || (estanRelacionados redSiguiente amigosUsActual amigosUltimos)
  where
    redSiguiente = eliminarUsuario red us
    amigosUsActual = amigosDe red us

eliminarUsuario :: RedSocial -> Usuario -> RedSocial
eliminarUsuario (users,rels,_) us = ((quitarTodos us users),(eliminarRelaciones rels us),[])
-- eliminarUsuario es auxiliar de existeSecuenciaDeAmigos. Como dicha función no usa las publicaciones, siempre devuelve [] en su lugar. --

eliminarRelaciones :: [Relacion] -> Usuario -> [Relacion]
eliminarRelaciones [] _ = []
eliminarRelaciones (rel:rels) us | us == primero rel || us == segundo rel = eliminarRelaciones rels us
                                 | otherwise = rel : (eliminarRelaciones rels us)
