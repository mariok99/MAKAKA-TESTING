-- 1 --

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = eliminarRepetidos(armarListaNombres (usuarios red))

{- nombresDeUsuarios arma una lista con los nombres de los usuarios de red.
 - Una vez armada la lista elimina los repetidos, de haberlos.
 -}

armarListaNombres :: [Usuario] -> [String]
armarListaNombres [] = []
armarListaNombres (x:xs) = (nombreDeUsuario x) : (armarListaNombres xs)

-- 5 --

estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos ((us:uses),rels,pubs) | (us:uses) == [] = False
                                        | (cantidadDeAmigos red us) > (10^6) = True
                                        | otherwise = estaRobertoCarlos (uses,rels,pubs)
                                        where red = ((us:uses),rels,pubs)

{- estaRobertoCarlos devuelve True si un usuario tiene 
 - más de un millón de amigos.
 - -}

-- 10 --

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red us1 us2 = pertenece us1 amigosU2 
    || estanRelacionados red amigosU1 amigosU2
  where 
    amigosU1 = amigosDe red us1
    amigosU2 = amigosDe red us2

{- existeSecuenciaDeAmigos determina si us1 y us2 son amigos
 - De no serlos, se busca si están "relacionados lejanamente".
 - Es decir, va a buscar entre los amigos del primero. Si no encuentra
 - amigos de us2, va a buscar en los amigos de amigos (y así recursivamente).
 - Para no quedarse en un bucle, se eliminan de la red los usuarios que
 - no son amigos de us2. En el peor de los casos, se llega a una lista vacía
 - y se devuelve False.
 - -}

estanRelacionados :: RedSocial -> [Usuario] -> [Usuario] -> Bool
estanRelacionados _ [] _ = False
estanRelacionados red (us:uses) amigosU2 = pertenece us amigosU2 
    || estanRelacionados redSiguiente uses amigosU2
    || estanRelacionados redSiguiente amigosUs amigosU2
  where
    redSiguiente = eliminarUsuario red us
    amigosUs = amigosDe red us

eliminarUsuario :: RedSocial -> Usuario -> RedSocial
eliminarUsuario (uses,rels,_) us = ((quitar uses us),(eliminarRelaciones rels us),[])

quitar :: (Eq t) => [t] -> t -> [t]
quitar [] _ = []
quitar (x:xs) a
    | a == x = quitar xs a
    | otherwise = x : (quitar xs a)

eliminarRelaciones :: [Relacion] -> Usuario -> [Relacion]
eliminarRelaciones [] _ = []
eliminarRelaciones (rel:rels) us 
    | us == primero || us == segundo = eliminarRelaciones rels us
    | otherwise = rel : (eliminarRelaciones rels us)
  where
    primero = fst rel
    segundo = snd rel
