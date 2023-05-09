-- Completar con los datos del grupo
--
-- Nombre de Grupo: operacionHaskell
-- Integrante 1: Nombre Apellido, email, LU
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Joaquín Lozano, joaquin.lozano.trabajo@gmail.com, 649/23
-- Integrante 4: Nombre Apellido, email, LU

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
                         
pertenece:: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs)  | n == x = True
                    | otherwise = pertenece n xs

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos t [] = []
quitarTodos t (x:xs) | not (pertenece t (x:xs)) = (x:xs)
                     | t == x = quitarTodos t xs
                     | otherwise = x : quitarTodos t xs

-- 2 --

{- Describir qué hace la función -}

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

-- 3 -- 

{- Describir qué hace la función -}

cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red us  =  longitud (amigosDe (red) us)

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- 4 --

{- Describir qué hace la función -}

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
 - un caso donde dé True sin crear una red con un millón + 1 tuplas. -}

-- 7 --

{- Describir qué hace la función -}

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red us = pubsLikeadas (publicaciones red) us

pubsLikeadas:: [Publicacion] -> Usuario -> [Publicacion]
pubsLikeadas [] _ = []
pubsLikeadas (pub:pubs) us  | pertenece us (likesDePublicacion pub) = pub : (pubsLikeadas pubs us)
                            | otherwise = pubsLikeadas pubs us
                          
-- 8 --

{- Describir qué hace la función -}

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red us1 us2 = mismosElementos (publicacionesQueLeGustanA red us1) (publicacionesQueLeGustanA red us2)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = (incluido xs ys) && (incluido ys xs)

incluido :: (Eq t) => [t] -> [t] -> Bool
incluido [] _ = True
incluido (x:xs) ys | pertenece x ys = mismosElementos xs ys
                   | otherwise = False
                   
-- 10 --

 {- existeSecuenciaDeAmigos determina si un usuario está relacionado con "us2".
  - Dicho usuario puede ser "us1". Si no lo es, se busca entre los "amigos de us1".
  - Se repite recursivamente con los "amigos de los amigos", en cada paso eliminando de la 
  - red quienes no son amigos de us2. Si se llega a algún "N-ésimo usuario" que satisface:
  - es amigo del "usuario anterior" y es amigo de "us2", entonces se puede concluir 
  - que es posible armar la cadena [us1,amigoDeUs1,amigoDeAmigoDeUs1,...,amigoDelAnterior,us2],
  - devolviendo True. De lo contrario, no existe una secuencia que sea solución 
  - del problema, devolviendo False. -}

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red us1 us2 = (pertenece us1 amigosU2) || (estanRelacionados red amigosU1 amigosU2)
  where 
    amigosU1 = amigosDe red us1
    amigosU2 = amigosDe red us2
-- No se elimina us1 por si los parámetros de entrada son iguales. --

estanRelacionados :: RedSocial -> [Usuario] -> [Usuario] -> Bool
estanRelacionados _ [] _ = False
estanRelacionados red (us:users) amigosU2 = (pertenece us amigosU2) || (estanRelacionados redSiguiente users amigosU2) || (estanRelacionados redSiguiente amigosUs amigosU2)
  where
    redSiguiente = eliminarUsuario red us
    amigosUs = amigosDe red us

eliminarUsuario :: RedSocial -> Usuario -> RedSocial
eliminarUsuario (users,rels,_) us = ((quitarTodos us users),(eliminarRelaciones rels us),[])
-- eliminarUsuario es auxiliar de existeSecuenciaDeAmigos. Como dicha funci

eliminarRelaciones :: [Relacion] -> Usuario -> [Relacion]
eliminarRelaciones [] _ = []
eliminarRelaciones (rel:rels) us | us == primero rel || us == segundo rel = eliminarRelaciones rels us
                                 | otherwise = rel : (eliminarRelaciones rels us)
