pertenece:: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs)  | n == x = True
                    | otherwise = pertenece n (xs)

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red user = publicacionesLista (publicaciones red) user

publicacionesLista:: [Publicacion] -> Usuario -> [Publicacion]
publicacionesLista [] user = []
publicacionesLista (x:xs) user  | pertenece user (likesDePublicacion x) = x:publicacionesLista(xs) user
                                | otherwise = publicacionesLista xs user

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red user1 user2 = mismosElementos (publicacionesQueLeGustanA red user1) (publicacionesQueLeGustanA red user2)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = (incluido xs ys) && (incluido ys xs)

incluido :: (Eq t) => [t] -> [t] -> Bool
incluido [] _ = True
incluido (x:xs) ys | pertenece x ys = mismosElementos xs ys
                   | otherwise = False
