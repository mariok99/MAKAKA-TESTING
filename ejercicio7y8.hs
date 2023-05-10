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


usuario1 = (1, "Juan")
usuario2 = (2, "Natalia")
usuario3 = (3, "Pedro")
usuario4 = (4, "Mariela")
usuario5 = (5, "Natalia")
usuario6 = (6, "Sol")
usuario7 = (7, "Roberto")

relacion1_2 = (usuario1, usuario2)
relacion1_3 = (usuario1, usuario3)
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 = (usuario3, usuario2)
relacion2_4 = (usuario2, usuario4)
relacion3_4 = (usuario4, usuario3)

publicacion1_1 = (usuario1, "Este es mi primer post", [usuario7, usuario4])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])

publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario2])

publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario7])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])


usuariosA = [usuario1, usuario2, usuario3, usuario4, usuario6, usuario7]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB = (usuariosB, relacionesB, publicacionesB)

-- test propios-------------------------------------------

userU1 = (1, "Sol")
userU2 = (2, "Guido")
userU3 = (3, "Azul")
userU4 = (4, "Juana")
userV1 = (5, "Mel")
userV2 = (6, "Jaz")
userV3 = (7, "Santi")
userX1 = (8, "Kevin")
userX2 = (9, "Brian")
userX3 = (10, "Anita")


-- publicaciones con likes
publicacionU1_1 = (userU1, "i need moneey", [userU3, userU2])
publicacionU1_2 = (userU2, "boca y nada mas", [userU1])
publicacionU1_3 = (userU1, "espero que ande", [userU3, userU2, userU1,userU4])

--publicaciones sin ningun like
publicacionX1_1 = (userV1, "lorem input", [])
publicacionX1_2 = (userV2, "zzzzz", [])
publicacionX1_3 = (userV3, "aloja", [])

--usuarios que dieron likes a alguna publicacion de su red
usersU = [userU1, userU2, userU3, userU4]

--usuarios que no dieron ningun like a ninguna publicacion de su red
usersV = [userV1, userV2, userV3]

--usuarios que no pueden likear nada porque no hay publicaciones en su red
usersX = [userX1, userX2, userX3]

relacionesU = []
relacionesV = []
relacionesX = []

publicacionesU = [publicacionU1_1, publicacionU1_2, publicacionU1_3]
publicacionesV = []
publicacionesX = [publicacionX1_1, publicacionX1_2, publicacionX1_3]


--red con publicaciones y con likes de usuarios
redU = (usersU, relacionesU, publicacionesU)

--red sin publicaciones
redV = (usersV, relacionesV, publicacionesV)

--red con publicaciones pero sin likes
redX = (usersX, relacionesX, publicacionesX)






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

--mismosElementos :: (Eq t) => [t] -> [t] -> Bool
--mismosElementos xs ys = (incluido xs ys) && (incluido ys xs)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] _ = True
mismosElementos (x:xs) ys   | pertenece x ys = mismosElementos xs ys
                            | otherwise = False


