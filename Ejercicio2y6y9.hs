module Ejercicio2y6y9 where
import CasosDeTest
import Solucion
    ( likesDePublicacion,
      publicaciones,
      relaciones,
      usuarioDePublicacion,
      Publicacion,
      RedSocial,
      Relacion,
      Usuario ) 
import CasosDeTest (usuario1)


-- ejercicio 6 -- 

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red us = aux_publicacionesDe (publicaciones red) us  

aux_publicacionesDe :: [Publicacion] -> Usuario -> [Publicacion]
aux_publicacionesDe [] _ = []
aux_publicacionesDe (pub:pubs) us | us == usuarioDePublicacion pub = pub : aux_publicacionesDe pubs us 
                                  | otherwise = aux_publicacionesDe pubs us

-- ejercicio 9 -- 
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red us | pubs == [] = False
                           | otherwise = aparicionesDeLikeador (eliminarRepetidos (likesDePublicacion (head pubs))) (concatenarLikesDePublicaciones pubs) (longitud pubs)
                           where pubs = publicacionesDe red us

aparicionesDeLikeador :: [Usuario] -> [Usuario] -> Int -> Bool  -- (usl1:ussl1) es la lista con los likes de la primera publicacion de us --
aparicionesDeLikeador [] _ _ = False -- si nadie dio like en la primera publicacion, entonces False -- 
aparicionesDeLikeador _ [] _ = False -- si concateno todos los likes y es vacío, es que nadie dio like = False
aparicionesDeLikeador (usl1:ussl1) likesTotales pubsTotales | (cantidadDeApariciones usl1 likesTotales) == pubsTotales = True -- likesTotales es la lista concatenarLikesDePublicaciones -- 
                                                            | otherwise = aparicionesDeLikeador ussl1 likesTotales pubsTotales 

cantidadDeApariciones :: Usuario -> [Usuario] -> Int -- usX es un usuario genérico -- 
cantidadDeApariciones _ [] = 0
cantidadDeApariciones usX (us:users) | usX == us = 1 + (cantidadDeApariciones usX users) 
                                     | otherwise = cantidadDeApariciones usX users 

concatenarLikesDePublicaciones :: [Publicacion] -> [Usuario] -- agarro los likes de la primera publicación y los concateno, luego hago recursión sobre la lista sin la primera publicación, y así... --
concatenarLikesDePublicaciones [] = []
concatenarLikesDePublicaciones (pub:pubs) = eliminarRepetidos (quitarTodos (usuarioDePublicacion pub) (likesDePublicacion pub)) -- quito al autor de la publicacion porque no es valido como seguidorFiel -- 
                                            ++ concatenarLikesDePublicaciones pubs -- agregué eliminar repetidos por si alguien dio like DOS VECES -- 

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
