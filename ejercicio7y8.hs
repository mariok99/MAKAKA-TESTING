module Ejercicio7y8 where
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




