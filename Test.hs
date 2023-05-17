import Solucion
import Test.HUnit

-- Test Suites --

main = runTestTT tests

tests = test [testSuiteEj1,testSuiteEj2,testSuiteEj3,testSuiteEj4,testSuiteEj5,testSuiteEj6,testSuiteEj7,testSuiteEj8,testSuiteEj9,testSuiteEj10]

testSuiteEj1 = test [
  " Caso 1: lista usuario vacía" ~: (nombresDeUsuarios redUsVacio) ~?= [],
  " Caso 2: lista de usuarios sin nombres repetidos" ~: (nombresDeUsuarios redJ1) ~?= ["Marcelo","Pepe","Ronaldo"],
  " Caso 3: lista de usuarios con nombres repetidos" ~: (nombresDeUsuarios redNomRep) ~?= ["Marcelo","Carlos"]
 ]

testSuiteEj2 = test [
  " Caso 1: lista relaciones vacía" ~: (amigosDe redRelsVacio usJ1) ~?= [],
  " Caso 2: usuario no tiene amigos" ~: (amigosDe redJ1 usJ7) ~?= [],
  " Caso 3: usuario tiene amigos" ~: (amigosDe redJ2 usJ4) ~?= [usJ1,usJ2,usJ7]
 ]

testSuiteEj3 = test [
  " Caso 1: lista relaciones vacía" ~: (cantidadDeAmigos redRelsVacio usJ1) ~?= 0,
  " Caso 2: usuario no tiene amigos" ~: (cantidadDeAmigos redJ1 usJ7) ~?= 0,
  " Caso 3: usuario tiene amigos" ~: (cantidadDeAmigos redJ2 usJ2) ~?= 2
 ]

testSuiteEj4 = test [
  " Caso 1: lista relaciones vacía" ~: (usuarioConMasAmigos redRelsVacio) ~?= usJ1,
  " Caso 2: un único usuario con máximo de amigos" ~: (usuarioConMasAmigos redJ2) ~?= usJ4,
  " Caso 3: varios usuarios con el máximo de amigos" ~: (usuarioConMasAmigos redJ3) ~?= usJ2
 ]

testSuiteEj5 = test [
  " Caso 1: No hay usuario con más de 10 amigos" ~: (estaRobertoCarlos redJ1) ~?= False,
  " Caso 2: Hay usuario con más de 10 amigos" ~: (estaRobertoCarlos redRobertoTrue) ~?= True
 ]

testSuiteEj6 = test [
    "Caso 1: red sin pubs, con usuarios" ~: (publicacionesDe redM1 usJ1) ~?= [],
    "Caso 2: red con pubs y user sin pubs" ~: (publicacionesDe redM3 usJ7) ~?= [],
    "Caso 3: hay pubs de distintos usuarios en la red" ~: (publicacionesDe redM4 usJ4) ~?= [publicacionM4_1, publicacionM4_2],
    "Caso 4: todas las pubs son del mismo usuario" ~: (publicacionesDe redM5 usJ9) ~?= publicacionesM5, 
    "Caso 5: red tiene pubs pero ninguna son del usuario de entrada" ~: (publicacionesDe redM6 usJ9) ~?= []
  ]

testSuiteEj7 = test [
    "Caso 1: no hay publicaciones en la red" ~: (publicacionesQueLeGustanA redPubsVacio usJ5) ~?= [],
    "Caso 2: el usuario dio un solo like a las publicaciones." ~: (publicacionesQueLeGustanA redU usJ1) ~?= [publicacionU1_2, publicacionU2_1],
    "Caso 3: el usuario dio likes repetidas veces a las publicaciones" ~: (publicacionesQueLeGustanA redM11 usJ8) ~?= [publicacionM6_1,publicacionM1_5],
    "Caso 4: el usuario no dio like" ~: (publicacionesQueLeGustanA redX usJ8) ~?= []
 ]
    
testSuiteEj8 = test [
    "Caso 1: no hay publicaciones en la red" ~: (lesGustanLasMismasPublicaciones redPubsVacio usJ5 usJ6) ~?= True,
    "Caso 2: les gustan las mismas publicaciones" ~: (lesGustanLasMismasPublicaciones redU usJ3 usJ2) ~?= True,
    "Caso 3: idem que 2, a uno le gusta más veces una publicación" ~: (lesGustanLasMismasPublicaciones redV usJ1 usJ4) ~?= True,
    "Caso 4: les gustan distintas publicaciones" ~: (lesGustanLasMismasPublicaciones redU usJ1 usJ2) ~?= False,
    "Caso 5: idem que 4, a uno le gusta más veces una publicación" ~: (lesGustanLasMismasPublicaciones redV usJ1 usJ2) ~?= False,
    "Caso 6: a ninguno les gusta alguna publicación" ~: (lesGustanLasMismasPublicaciones redX usJ8 usJ9) ~?= True
 ]

testSuiteEj9 = test [
  "Caso 1: red sin publicaciones" ~: (tieneUnSeguidorFiel redM1 usJ1) ~?= False,
  "Caso 2: cantidad de pubs de us = 0" ~: (tieneUnSeguidorFiel redM6 usJ8) ~?= False,
  "Caso 3: usuario tiene pubs pero ninguna tuvo likes" ~: (tieneUnSeguidorFiel redM2 usJ1) ~?= False,
  "Caso 4: usuario tiene pubs pero nadie le dio likes a todas" ~: (tieneUnSeguidorFiel redM7 usJ1) ~?= False,
  "Caso 5: usuario tiene pubs con likes repetidos, nadie dio likes a todas" ~: (tieneUnSeguidorFiel redM8 usJ1) ~=? False,
  "Caso 6: usuario no tiene likes repetidos, tiene seguidor fiel" ~: (tieneUnSeguidorFiel redM9 usJ1) ~?= True,
  "Caso 7: usuario tiene likes repetidos, tiene seguidor fiel" ~: (tieneUnSeguidorFiel redM10 usJ5) ~?= True,
  "Caso 8: usuario se dio autolikes a todas sus pubs y no tiene seguidor fiel" ~: (tieneUnSeguidorFiel redM11 usJ6) ~?= False,
  "Caso 9: usuario tiene seguidor fiel y se dio autolike a todas sus pubs" ~: (tieneUnSeguidorFiel redM12 usJ11) ~?= True
  ]

testSuiteEj10 = test [
  " Caso especial 1: usuarios de entrada iguales, sin amigos" ~: (existeSecuenciaDeAmigos redJ1 usJ7 usJ7) ~?= False,
  " Caso especial 2: usuarios de entrada iguales, con amigos" ~: (existeSecuenciaDeAmigos redJ1 usJ1 usJ1) ~?= True,
  " Caso base 1: ambos usuarios son amigos entre sí" ~: (existeSecuenciaDeAmigos redJ4 usJ2 usJ5) ~?= True,
  " Caso base 2: primer usuario no tiene amigos" ~: (existeSecuenciaDeAmigos redJ4 usJ4 usJ1) ~?= False,
  " Caso base 3: segundo usuario no tiene amigos" ~: (existeSecuenciaDeAmigos redJ4 usJ1 usJ11) ~?= False,
  " Caso recursivo 1: ambos usuarios tienen amigos, pero no están relacionados" ~: (existeSecuenciaDeAmigos redJ4 usJ1 usJ2) ~?= False,
  " Caso recursivo 2: ambos usuarios están relacionados lejanamente" ~: (existeSecuenciaDeAmigos redJ3 usJ1 usJ11) ~?= True
 ]



-- Usuarios, relaciones, publicaciones y redes usadas --

usJ1=(1,"Marcelo")
usJ2=(2,"Carlos")
usJ3=(3,"Pepe")
usJ4=(4,"Francesco")
usJ5=(5,"Mariana")
usJ6=(6,"Sixto")
usJ7=(7,"Ronaldo")
usJ8=(8,"Angela")
usJ9=(9,"Fulana")
usJ10=(10,"Leon")
usJ11=(11,"Leopoldo")
usJ12=(12,"Alfredo")

usNomRep=(777,"Marcelo")

usuariosJ1=[usJ1,usJ3,usJ7]
usuariosJ2=[usJ1,usJ2,usJ4,usJ5,usJ7,usJ11]
--usuarios que dieron likes a alguna publicacion de su red
usersU = [usJ1, usJ2, usJ3, usJ4]
--usuarios que no pueden likear nada porque no hay publicaciones en su red
usersV = [usJ5, usJ6, usJ7]
--usuarios que no dieron ningun like a ninguna publicacion de su red
usersX = [usJ8, usJ9, usJ10]

relacionesJ1=[(usJ1,usJ3)]
-- Relaciones con un solo usuario teniendo max de amigos
relacionesJ2=[(usJ1,usJ4),(usJ2,usJ4),(usJ5,usJ2),(usJ4,usJ7),(usJ5,usJ11)]

-- Relaciones con varios usuarios teniendo máx de amigos
relacionesJ3=[(usJ1,usJ4),(usJ2,usJ4),(usJ5,usJ2),(usJ5,usJ11)]

-- Relaciones donde dos usuarios no tienen amigos y el resto sí
relacionesJ4=[(usJ2,usJ5),(usJ1,usJ7)]

-- publicaciones con likes
publicacionU1_1 = (usJ1, "i need moneey", [usJ3, usJ2])
publicacionU1_2 = (usJ1, "espero que ande", [usJ3, usJ2, usJ1,usJ4,usJ4])
publicacionU2_1 = (usJ2, "boca y nada mas", [usJ1])
publicacionU2_2 = (usJ2, "aguante mario bros", [usJ4,usJ3,usJ1,usJ1])

--publicaciones sin ningun like
publicacionX8_1 = (usJ8, "lorem input", [])
publicacionX9_1 = (usJ9, "zzzzz", [])
publicacionX10_1 = (usJ10, "aloja", [])

publicacionesU = [publicacionU1_1, publicacionU1_2, publicacionU2_1]
publicacionesX = [publicacionX8_1, publicacionX9_1, publicacionX10_1]
publicacionesV = [publicacionU1_2,publicacionU2_2]



redJ1=(usuariosJ1,relacionesJ1,[]) -- redes usadas para los ejs 1,2,3,4,5 y 10. Ninguno precisa de publicaciones.
redJ2=(usuariosJ2,relacionesJ2,[])
redJ3=(usuariosJ2,relacionesJ3,[])
redJ4=(usuariosJ2,relacionesJ4,[])
--red con publicaciones y con likes de usuarios
redU = (usersU, [], publicacionesU) -- redes usadas para los ejs 7 y 8. Ninguno precisa de relaciones.
--red con publicaciones pero sin likes
redX = (usersX, [], publicacionesX)
redV = (usersU, [], publicacionesV)

ususariosRobertoTrue=[usJ1,usJ2,usJ3,usJ4,usJ5,usJ6,usJ7,usJ8,usJ9,usJ10,usJ11,usJ12] -- lista usuarios donde estaRobertoCarlos pueda dar True.
usuariosNomRep=[usJ1,usJ2,usNomRep] -- lista usuarios donde dos repiten el mismo nombre.

relacionesRobertoTrue=[(usJ1,usJ2),(usJ1,usJ3),(usJ1,usJ4),(usJ1,usJ5),
 (usJ1,usJ6),(usJ1,usJ7),(usJ1,usJ8),(usJ1,usJ9),(usJ1,usJ10),(usJ1,usJ11),(usJ1,usJ12)]

redUsVacio=([],[],[])
redRelsVacio=(usuariosJ1,[],[])
redPubsVacio = (usersV, [], [])

redRobertoTrue=(usuariosJ1,relacionesRobertoTrue,[])
redNomRep=(usuariosNomRep,[],[])

-- En todas estas redes las relaciones entre los usuarios no importan, entonces uso la lista vacía -- 

-- redM1 sin publicaciones con usuarios --
redM1 = (usuariosM1, relacionesM1, publicacionesM1)
usuariosM1 = [usJ1, usJ2, usJ4,usJ9]
relacionesM1 = []
publicacionesM1 = []

-- redM2 con publicaciones, publicaciones de usJ1 >= 1, pero ninguna tiene likes --  
redM2 = (usuariosM2, relacionesM2, publicacionesM2)
usuariosM2 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM2 = []
publicacionesM2 = [publicacionM10_1, publicacionM10_2, publicacionM3_2, publicacionM1_1, publicacionM1_2]

-- redM3 con publicaciones y usM7 sin publicaciones--
redM3 = (usuariosM3, relacionesM3, publicacionesM3)
usuariosM3 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM3 = []
publicacionesM3 = [publicacionM1_2, publicacionM2_1, publicacionM3_1, publicacionM3_2]

-- red M4 usuario tiene publicaciones, no todas las publicaciones de la red son del usuario (con usuario4) -- 
redM4 = (usuariosM4, relacionesM4, publicacionesM4)
usuariosM4 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM4 = []
publicacionesM4 = [publicacionM4_1, publicacionM9_1, publicacionM2_1, publicacionM4_2]

-- redM5 usuario tiene publicaciones y todas las publicaciones de la red son del usuario (con usuario9) -- 
redM5 = (usuariosM5, relacionesM5, publicacionesM5)
usuariosM5 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM5 = []
publicacionesM5 = [publicacionM9_1, publicacionM9_2, publicacionM9_3, publicacionM9_4]

-- redM6 tiene publicaciones y ninguna de las publicaciones de la red son de usJ9 -- 
redM6 = (usuariosM6, relacionesM6, publicacionesM6)
usuariosM6 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM6 = []
publicacionesM6 = [publicacionM1_1, publicacionM1_2, publicacionM3_1]

---------------------------------------------------------------------------------------------------------------
--- a partir de ahora todas las redes tienen publicaciones del usJ1 y todas tienen likes -- 
---------------------------------------------------------------------------------------------------------------
-- redM7 usJ1 no tiene likes repetidos y  nadie dió like a todas -- 
redM7 = (usuariosM7, relacionesM7, publicacionesM7)
usuariosM7 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM7 = []
publicacionesM7 = [publicacionM1_2, publicacionM1_3, publicacionM1_1, publicacionM9_2, publicacionM9_4]

-- redM8 usJ1 tiene likes repetidos en sus publicaciones y nadie dió like a todas -- 
redM8 = (usuariosM8, relacionesM8, publicacionesM8)
usuariosM8 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM8 = []
publicacionesM8 = [publicacionM1_4, publicacionM1_5, publicacionM10_1, publicacionM2_2]

-- redM9 un us /= usJ1 dió like a todas sus publicaciones (tiene seguidor fiel), no hay likes repetidos en sus publicaciones --
redM9 = (usuariosM9, relacionesM9, publicacionesM9)
usuariosM9 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM9 = []
publicacionesM9 = [publicacionM1_6, publicacionM1_7, publicacionM1_8, publicacionM2_1, publicacionM10_1]

-- redM10 usJ5 tiene seguidor fiel y hay likes repetidos en sus publicaciones -- 
redM10 = (usuariosM10, relacionesM10, publicacionesM10)
usuariosM10 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM10 = []
publicacionesM10 = [publicacionM5_1, publicacionM5_2, publicacionM10_1, publicacionM9_1, publicacionM5_3]

-- redM11 usJ6 se dió autolikes a todas sus publicaciones, no tiene seguidor fiel, indistinto si hay likes repetidos --
redM11 = (usuariosM11, relacionesM11, publicacionesM11)
usuariosM11 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM11 = []
publicacionesM11 = [publicacionM6_1, publicacionM6_3, publicacionM1_5, publicacionM6_2, publicacionM3_2, publicacionM6_3, publicacionM3_1]

-- redM12 usJ11 tiene seguidor fiel y usJ11 se dio autolike a todas sus publicaciones, indistinto si hay likes repetidos -- 
redM12 = (usuariosM12, relacionesM12, publicacionesM12)
usuariosM12 = [usJ1, usJ2, usJ3, usJ4, usJ5, usJ6, usJ7, usJ8, usJ9, usJ10, usJ11, usJ12]
relacionesM12 = []
publicacionesM12 = [publicacionM11_1, publicacionM2_1, publicacionM3_2, publicacionM3_1, publicacionM11_2, publicacionM9_1, publicacionM11_3]

publicacionM1_1 = (usJ1, "primera primera", [usJ5, usJ4])
publicacionM1_2 = (usJ1, "segunda caballero", [usJ6, usJ9, usJ2])
publicacionM1_3 = (usJ1, "creo que no subiré nunca más nada", [usJ10, usJ2])
publicacionM1_4 = (usJ1, "no tengo bots lo prometo", [usJ10, usJ10, usJ3, usJ3, usJ5])
publicacionM1_5 = (usJ1, "no soy yo enserio", [usJ8, usJ8, usJ8, usJ2, usJ11, usJ11])
publicacionM1_6 = (usJ1, "creo que esta resulto", [usJ10, usJ11, usJ2])
publicacionM1_7 = (usJ1, "no me lo puedo creer", [usJ10, usJ11, usJ9, usJ2, usJ8])
publicacionM1_8 = (usJ1, "gracias LEOOOOOO", [usJ10, usJ9, usJ2, usJ8])

publicacionM2_1 = (usJ2, "relleno jijiji", [])
publicacionM2_2 = (usJ2, "no sé que subir", [])
publicacionM2_3 = (usJ3, "al fin sé que subir", [])

publicacionM3_1 = (usJ3, "basta de publicar", [])
publicacionM3_2 = (usJ3, "segunda publicación", [])

publicacionM4_1 = (usJ4, "hola gente", [])
publicacionM4_2 = (usJ4, "lalalala", []) 

publicacionM5_1 = (usJ5, "tengo sueño, bastante", [usJ1, usJ11, usJ10, usJ11,usJ2])
publicacionM5_2 = (usJ5, "tengo más sueño que antes", [usJ3, usJ2, usJ1, usJ2])
publicacionM5_3 = (usJ5, "me duermo...", [usJ6, usJ7, usJ7, usJ1])

publicacionM6_1 = (usJ6, "ping pon mejor deporte", [usJ1, usJ11, usJ2, usJ9, usJ8, usJ6])
publicacionM6_2 = (usJ6, "hoy jugué pin pong", [usJ6, usJ2, usJ10])
publicacionM6_3 = (usJ6, "Messi tendría que admitirlo", [usJ6, usJ11])

publicacionM9_1 = (usJ9, "soy el nueve", [])
publicacionM9_2 = (usJ9, "Terepín", [])
publicacionM9_3 = (usJ9, "Minecraft", [])
publicacionM9_4 = (usJ9, "botella roja", [])

publicacionM11_1 = (usJ11, "¿Qué me pasará dentro de 10 años?", [usJ2, usJ11, usJ10, usJ7])
publicacionM11_2 = (usJ11, "Tengo miedo...", [usJ9, usJ11, usJ2])
publicacionM11_3 = (usJ11, "Porque estas aquí, ya no", [usJ10, usJ7, usJ2, usJ8, usJ11])

publicacionM10_1 = (usJ10, "la diabetes es peligrosa", [usJ1, usJ12, usJ7])
publicacionM10_2 = (usJ10, "por favor cuidense", [usJ5, usJ12])
