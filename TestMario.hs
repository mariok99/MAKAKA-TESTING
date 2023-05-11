module TestMario where
import Solucion 
import TestCatedra
import Test.HUnit 
import Test.HUnit (runTestTT)
import Ejercicio7y8

testSuitEj6 = test [
    "Caso1 red sin publicaciones con usuarios" ~: (publicacionesDe redM1 usuario1) ~?= [],
    "Caso2 red con pubs y user sin pubs" ~: (publicacionesDe redM3 usuario7) ~?= [],
    "Caso3 us tiene pubs, no es autor de todas" ~: (publicacionesDe redM4 usuario4) ~?= [publicacion4_1, publicacion4_2],
    "Caso4 us tiene pubs y son todas de la red" ~: (publicacionesDe redM5 usuario9) ~?= publicacionesM5, 
    "Caso5 red tiene pubs pero ninguno de us" ~: (publicacionesDe redM6 usuario9) ~?= []
  ]
run1 = runTestTT testSuitEj6

testSuitEj9 = test [
    "Caso1 red sin pubs" ~: (publicacionesQueLeGustanA redU userU1) ~?= [publicacionU1_2, publicacionU1_3]
     
    ]
run2 = runTestTT testSuitEj7
-- casos de test publicacionesDe --
-- redM7 sin publicaciones -- 



-- redM1 sin publicaciones con usuarios --
redM1 = (usuariosM1, relacionesM1, publicacionesM1)
usuariosM1 = [usuarioM1, usuarioM2, usuarioM4,usuarioM9]
relacionesM1 = [relacion2_3, relacion3_4]
publicacionesM1 = []

-- redM2 sin publicaciones y sin usuarios -- ningnun usuario cumple el requiere
redM2 = (usuariosM2, relacionesM2, publicacionesM2)
usuariosM2 = []
relacionesM2 = []
publicacionesM2 = []

-- redM3 con publicaciones y usuario sin publicaciones Usuario7--
redM3 = (usuariosM3, relacionesM3, publicacionesM3)
usuariosM3 = [usuarioM7, usuarioM3, usuarioM4, usuarioM5]
relacionesM3 = [relacion1_2]
publicacionesM3 = [publicacion1_2, publicacion2_1, publicacion3_1, publicacion3_2]

-- red M4 usuario tiene publicaciones, no todas las publicaciones de la red son del usuario (con usuario4) -- 
redM4 = (usuariosM4, relacionesM4, publicacionesM4)
usuariosM4 = [usuarioM4, usuarioM5, usuarioM9, usuarioM8]
relacionesM4 = [relacion1_4, relacion1_2]
publicacionesM4 = [publicacion4_1, publicacion9_1, publicacion2_1, publicacion4_2]

-- redM5 usuario tiene publicaciones y todas las publicaciones de la red son del usuario (con usuario9) -- 
redM5 = (usuariosM5, relacionesM5, publicacionesM5)
usuariosM5 = [usuarioM1, usuarioM2, usuarioM9, usuarioM7, usuarioM8]
relacionesM5 = [relacion3_4, relacion2_3]
publicacionesM5 = [publicacion9_1, publicacion9_2, publicacion9_3, publicacion9_4]

-- redM6 tiene publicaciones y ninguna de las publicaciones de la red son del usuario (con usuario9) -- 
redM6 = (usuariosM6, relacionesM6, publicacionesM6)
usuariosM6 = [usuarioM1, usuarioM9, usuarioM8]
relacionesM6 = []
publicacionesM6 = [publicacion4_1, publicacion1_2, publicacion3_1]


usuarioM1 = (10, "Mandarin")
usuarioM2 = (11, "Mariela")
usuarioM3 = (13, "Fede")
usuarioM4 = (14, "Pattaus")
usuarioM5 = (15, "Camarón")
usuarioM6 = (16, "Rulitos")
usuarioM7 = (17, "Carlitos")
usuarioM8 = (18, "Secante")
usuarioM9 = (19, "Nitro")
usuarioM10 = (20, "Pachuli")

publicacionM1_1 = (usuarioM1, "primera primera", [usuarioM5, usuarioM4])
publicacionM1_2 = (usuarioM1, "segunda caballero", [usuarioM6, usuarioM9, usuarioM2])
publicacionM1_3 = (usuarioM1, "creo que no subiré nunca más nada", [usuarioM10, usuarioM2])

publicacionM2_1 = (usuarioM2, "relleno jijiji", [])
publicacionM2_2 = (usuarioM2, "no sé que subir", [])
publicacionM2_3 = (usuarioM3, "al fin sé que subir", [])

publicacionM3_1 = (usuarioM3, "porfa basta")