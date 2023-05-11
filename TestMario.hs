module TestMario where
import Solucion 
import TestCatedra
import Test.HUnit 
import Test.HUnit (runTestTT)

testSuitEj6 = test [
    "Caso1 sin publicaciones con usuarios" ~: (publicacionesDe redM1 usuario1) ~?= [],
    "Caso2 con pubs y user sin pubs" ~: (publicacionesDe redM3 usuario7) ~?= [],
    "Caso3 us tiene pubs, no es autor de todas" ~: (publicacionesDe redM4 usuario4) ~?= [publicacion4_1, publicacion4_2],
    "Caso4 us tiene pubs y son todas de la red" ~: (publicacionesDe redM5 usuario9) ~?= publicacionesM5, 
    "Caso5 hay pubs pero ninguno de us" ~: (publicacionesDe redM6 usuario9) ~?= []
  ]
run1 = runTestTT testSuitEj6

-- casos de test publicacionesDe --

-- redM1 sin publicaciones con usuarios --
redM1 = (usuariosM1, relacionesM1, publicacionesM1)
usuariosM1 = [usuario1, usuario2, usuario4,usuario9]
relacionesM1 = [relacion2_3, relacion3_4]
publicacionesM1 = []

-- redM2 sin publicaciones y sin usuarios -- ningnun usuario cumple el requiere
redM2 = (usuariosM2, relacionesM2, publicacionesM2)
usuariosM2 = []
relacionesM2 = []
publicacionesM2 = []

-- redM3 con publicaciones y usuario sin publicaciones Usuario7--
redM3 = (usuariosM3, relacionesM3, publicacionesM3)
usuariosM3 = [usuario7, usuario3, usuario4, usuario5]
relacionesM3 = [relacion1_2]
publicacionesM3 = [publicacion1_2, publicacion2_1, publicacion3_1, publicacion3_2]

-- red M4 usuario tiene publicaciones, no todas las publicaciones de la red son del usuario (con usuario4) -- 
redM4 = (usuariosM4, relacionesM4, publicacionesM4)
usuariosM4 = [usuario4, usuario5, usuario9, usuario8]
relacionesM4 = [relacion1_4, relacion1_2]
publicacionesM4 = [publicacion4_1, publicacion9_1, publicacion2_1, publicacion4_2]

-- redM5 usuario tiene publicaciones y todas las publicaciones de la red son del usuario (con usuario9) -- 
redM5 = (usuariosM5, relacionesM5, publicacionesM5)
usuariosM5 = [usuario1, usuario2, usuario9, usuario7, usuario8]
relacionesM5 = [relacion3_4, relacion2_3]
publicacionesM5 = [publicacion9_1, publicacion9_2, publicacion9_3, publicacion9_4]

-- redM6 tiene publicaciones y ninguna de las publicaciones de la red son del usuario (con usuario9) -- 
redM6 = (usuariosM6, relacionesM6, publicacionesM6)
usuariosM6 = [usuario1, usuario9, usuario8]
relacionesM6 = []
publicacionesM6 = [publicacion4_1, publicacion1_2, publicacion3_1]


