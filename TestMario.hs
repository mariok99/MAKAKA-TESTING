module TestMario where
import Solucion 
import TestCatedra
import Test.HUnit 
import Test.HUnit (runTestTT)


testSuiteEj6 = test [
    "Caso1 red sin publicaciones con usuarios" ~: (publicacionesDe redM1 usJ1) ~?= [],
    "Caso2 red con pubs y user sin pubs" ~: (publicacionesDe redM3 usJ7) ~?= [],
    "Caso3 usJ4 tiene pubs, no es autor de todas" ~: (publicacionesDe redM4 usJ4) ~?= [publicacionM4_1, publicacionM4_2],
    "Caso4 usJ9 tiene pubs y son todas de la red" ~: (publicacionesDe redM5 usJ9) ~?= publicacionesM5, 
    "Caso5 red tiene pubs pero ninguno de us" ~: (publicacionesDe redM6 usJ9) ~?= []
  ]
run1 = runTestTT testSuiteEj6

testSuiteEj9 = test [
  "Caso1 red sin publicaciones" ~: (tieneUnSeguidorFiel redM1 usJ1) ~?= False,
  "Caso2 cantidad de pubs de us = 0" ~: (tieneUnSeguidorFiel redM6 usJ8) ~?= False,
  "Caso3 usJ1 tiene pubs, pero likes = 0" ~: (tieneUnSeguidorFiel redM2 usJ1) ~?= False,
  "Caso4 usJ1 tiene pubs y likes, ningún us dió likes a todas" ~: (tieneUnSeguidorFiel redM7 usJ1) ~?= False,
  "Caso5 usJ1 tiene likes repetidos, ningún us dió likes a todas" ~: (tieneUnSeguidorFiel redM8 usJ1) ~=? False,
  "Caso6 usJ1 sin likes repetidos, tiene seguidor fiel" ~: (tieneUnSeguidorFiel redM9 usJ1) ~?= True,
  "Caso7 usJ5 tiene likes repetidos, tiene seguidor fiel" ~: (tieneUnSeguidorFiel redM10 usJ5) ~?= True,
  "Caso8 usJ6 autolikes a todas sus pubs y no tiene seguidor fiel" ~: (tieneUnSeguidorFiel redM11 usJ6) ~?= False,
  "Caso9 usJ11 tiene seguidor fiel y se dio autolike a todas sus pubs" ~: (tieneUnSeguidorFiel redM12 usJ11) ~?= True
  ]
run2 = runTestTT testSuiteEj9
-- casos de test publicacionesDe --

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



usJ1=(1,"Marcelo")
usJ2=(2,"Carlos")
usJ3=(3,"Pepe")
usJ4=(4,"Francesco")
usJ5=(5,"Mariana")
usJ6=(6,"Sixto")
usJ7=(7,"Ronaldo")
usJ8=(8,"Angela")
usJ9=(9,"Fulana")
usJ10=(10,"Messi")
usJ11=(11,"Leopoldo")
usJ12=(12,"Alfredo")



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
