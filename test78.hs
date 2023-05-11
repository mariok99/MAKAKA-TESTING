module Test78 where
import Test.HUnit
import Solucion

{-HLINT ignore-}


tests :: Test
tests = test [testSuiteEj7, testSuitEj8]
run :: IO Counts
run = runTestTT tests
testSuiteEj7 :: Test
testSuiteEj7 = test    [
    "Caso1 red con pubs y like de usuario" ~: (publicacionesQueLeGustanA redU userU1) ~?= [publicacionU1_2, publicacionU1_3],
    "Caso2 red sin publicaciones" ~: (publicacionesQueLeGustanA redV userV1) ~?= [],
    "Caso3 red con publicaciones pero sin likes" ~: (publicacionesQueLeGustanA redX userX1) ~?= []
    ]
testSuitEj8 :: Test
testSuitEj8 = test  [
    "Caso1 usuarios con mismos likes en pubs" ~: (lesGustanLasMismasPublicaciones redU userU3 userU2) ~?= True,
    "Caso2 usuarios con likes en distintas pubs" ~: (lesGustanLasMismasPublicaciones redU userU1 userU2) ~?= False,
    "Caso3 usuarios con ningun like en pubs" ~: (lesGustanLasMismasPublicaciones redX userX1 userX2) ~?= True
    ]


userU1 :: (Usuario)
userU1 = (1, "Sol")
userU2 :: (Usuario)
userU2 = (2, "Guido")
userU3 :: (Usuario)
userU3 = (3, "Azul")
userU4 :: (Usuario)
userU4 = (4, "Juana")
userV1 :: (Usuario)
userV1 = (5, "Mel")
userV2 :: (Usuario)
userV2 = (6, "Jaz")
userV3 :: (Usuario)
userV3 = (7, "Santi")
userX1 :: (Usuario)
userX1 = (8, "Kevin")
userX2 :: (Usuario)
userX2 = (9, "Brian")
userX3 :: (Usuario)
userX3 = (10, "Anita")


-- publicaciones con likes
publicacionU1_1 :: (Publicacion)
publicacionU1_1 = (userU1, "i need moneey", [userU3, userU2])
publicacionU1_2 :: (Publicacion)
publicacionU1_2 = (userU2, "boca y nada mas", [userU1])
publicacionU1_3 :: (Publicacion)
publicacionU1_3 = (userU1, "espero que ande", [userU3, userU2, userU1,userU4])

--publicaciones sin ningun like
publicacionX1_1 :: (Publicacion)
publicacionX1_1 = (userX1, "lorem input", [])
publicacionX1_2 :: (Publicacion)
publicacionX1_2 = (userX2, "zzzzz", [])
publicacionX1_3 :: (Publicacion)
publicacionX1_3 = (userX3, "aloja", [])

--usuarios que dieron likes a alguna publicacion de su red
usersU :: [Usuario]
usersU = [userU1, userU2, userU3, userU4]

--usuarios que no dieron ningun like a ninguna publicacion de su red
usersV :: [Usuario]
usersV = [userV1, userV2, userV3]

--usuarios que no pueden likear nada porque no hay publicaciones en su red
usersX :: [Usuario]
usersX = [userX1, userX2, userX3]

relacionesU :: [Relacion]
relacionesU = []
relacionesV :: [Relacion]
relacionesV = []
relacionesX :: [Relacion]
relacionesX = []

publicacionesU :: [Publicacion]
publicacionesU = [publicacionU1_1, publicacionU1_2, publicacionU1_3]
publicacionesV :: [Publicacion]
publicacionesV = []
publicacionesX :: [Publicacion]
publicacionesX = [publicacionX1_1, publicacionX1_2, publicacionX1_3]


--red con publicaciones y con likes de usuarios
redU :: ([Usuario], [Relacion], [Publicacion])
redU = (usersU, relacionesU, publicacionesU)

--red sin publicaciones
redV :: ([Usuario], [Relacion], [Publicacion])
redV = (usersV, relacionesV, publicacionesV)

--red con publicaciones pero sin likes
redX :: ([Usuario], [Relacion], [Publicacion])
redX = (usersX, relacionesX, publicacionesX)
