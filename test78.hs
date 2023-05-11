module Test78 where
import Test.HUnit
import Solucion


tests = test [testSuiteEj7, testSuitEj8]
run = runTestTT tests
testSuiteEj7 = test    [
    "Caso1 red con pubs y like de usuario" ~: (publicacionesQueLeGustanA redU userU1) ~?= [publicacionU1_2, publicacionU1_3],
    "Caso2 red sin publicaciones" ~: (publicacionesQueLeGustanA redV userV1) ~?= [],
    "Caso3 red con publicaciones pero sin likes" ~: (publicacionesQueLeGustanA redX userX1) ~?= []
    ]
testSuitEj8 = test  [
    "Caso1 usuarios con mismos likes en pubs" ~: (lesGustanLasMismasPublicaciones redU userU3 userU2) ~?= True,
    "Caso2 usuarios con likes en distintas pubs" ~: (lesGustanLasMismasPublicaciones redU userU1 userU2) ~?= False,
    "Caso3 usuarios con ningun like en pubs" ~: (lesGustanLasMismasPublicaciones redX userX1 userX2) ~?= True
    ]


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
publicacionX1_1 = (userX1, "lorem input", [])
publicacionX1_2 = (userX2, "zzzzz", [])
publicacionX1_3 = (userX3, "aloja", [])

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
