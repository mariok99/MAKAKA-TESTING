module Test (redA,redB,redC,redD,redW,redX,redY) where


usuario1 :: (Integer, String)
usuario1 = (1, "Juan")
usuario2 :: (Integer, String)
usuario2 = (2, "Natalia")
usuario3 :: (Integer, String)
usuario3 = (3, "Pedro")
usuario4 :: (Integer, String)
usuario4 = (4, "Mariela")
usuario5 :: (Integer, String)
usuario5 = (5, "Natalia")

relacion1_2 = (usuario1, usuario2)
relacion1_3 :: ((Integer, String), (Integer, String))
relacion1_3 = (usuario1, usuario3)
relacion1_4 :: ((Integer, String), (Integer, String))
relacion1_4 = (usuario4, usuario1) -- Notar que el orden en el que aparecen los usuarios es indistinto
relacion2_3 :: ((Integer, String), (Integer, String))
relacion2_3 = (usuario3, usuario2)
relacion2_4 :: ((Integer, String), (Integer, String))
relacion2_4 = (usuario2, usuario4)
relacion3_4 :: ((Integer, String), (Integer, String))
relacion3_4 = (usuario4, usuario3)

publicacion1_0 :: ((Integer, String), String, [(Integer, String)])
publicacion1_0 = (usuario1, "quiero muchos likes", [usuario2, usuario3, usuario4, usuario5])
publicacion1_1 :: ((Integer, String), String, [(Integer, String)])
publicacion1_1 = (usuario1, "Este es mi primer post", [usuario2, usuario4])
publicacion1_2 :: ((Integer, String), String, [(Integer, String)])
publicacion1_2 = (usuario1, "Este es mi segundo post", [usuario4])
publicacion1_3 :: ((Integer, String), String, [(Integer, String)])
publicacion1_3 = (usuario1, "Este es mi tercer post", [usuario2, usuario5])
publicacion1_4 :: ((Integer, String), String, [a])
publicacion1_4 = (usuario1, "Este es mi cuarto post", [])
publicacion1_5 :: ((Integer, String), String, [(Integer, String)])
publicacion1_5 = (usuario1, "Este es como mi quinto post", [usuario5])
publicacion1_6 = (usuario1, "MI SESTO POS", [usuario5, usuario3])

publicacion2_1 :: ((Integer, String), String, [(Integer, String)])
publicacion2_1 = (usuario2, "Hello World", [usuario4])
publicacion2_2 :: ((Integer, String), String, [(Integer, String)])
publicacion2_2 = (usuario2, "Good Bye World", [usuario1, usuario4])

publicacion3_1 :: ((Integer, String), String, [a])
publicacion3_1 = (usuario3, "Lorem Ipsum", [])
publicacion3_2 :: ((Integer, String), String, [(Integer, String)])
publicacion3_2 = (usuario3, "dolor sit amet", [usuario2])
publicacion3_3 :: ((Integer, String), String, [(Integer, String)])
publicacion3_3 = (usuario3, "consectetur adipiscing elit", [usuario2, usuario5])

publicacion4_1 :: ((Integer, String), String, [(Integer, String)])
publicacion4_1 = (usuario4, "I am Alice. Not", [usuario1, usuario2])
publicacion4_2 :: ((Integer, String), String, [a])
publicacion4_2 = (usuario4, "I am Bob", [])
publicacion4_3 :: ((Integer, String), String, [(Integer, String)])
publicacion4_3 = (usuario4, "Just kidding, i am Mariela", [usuario1, usuario3])
publicacion4_4 = (usuario4, "Que funcione plis", [])
publicacion4_5 = (usuario4, "SIUIUIUIU", [])
publicacion4_6 = (usuario4, "otro ejemplo sin likes", [])

usuariosA :: [(Integer, String)]
usuariosA = [usuario1, usuario2, usuario3, usuario4]
relacionesA :: [((Integer, String), (Integer, String))]
relacionesA = [relacion1_2, relacion1_4, relacion2_3, relacion2_4, relacion3_4]
publicacionesA :: [((Integer, String), String, [(Integer, String)])]
publicacionesA = [publicacion1_1, publicacion1_2, publicacion2_1, publicacion2_2, publicacion3_1, publicacion3_2, publicacion4_1, publicacion4_2]
redA :: ([(Integer, String)], [((Integer, String), (Integer, String))],
 [((Integer, String), String, [(Integer, String)])])
redA = (usuariosA, relacionesA, publicacionesA)

usuariosB = [usuario1, usuario2, usuario3, usuario5]
relacionesB = [relacion1_2, relacion2_3]
publicacionesB = [publicacion1_3, publicacion1_4, publicacion1_5, publicacion3_1, publicacion3_2, publicacion3_3]
redB :: ([(Integer, String)], [((Integer, String), (Integer, String))], [((Integer, String), String, [(Integer, String)])])
redB = (usuariosB, relacionesB, publicacionesB)

redW = (usuariosA, relacionesA, publicacionesW)
publicacionesW = [publicacion1_0, publicacion1_3, publicacion1_5, publicacion1_6, publicacion4_1, publicacion4_3]
redC = (usuariosA, relacionesA, [])
publicacionesC :: [((Integer, String), String, [(Integer, String)])]
publicacionesC = [publicacion1_1]

redD = (usuariosA, relacionesA, publicacionesD)
publicacionesD = [publicacion2_1]


redX = (usuariosA, relacionesA, publicacionesX)
publicacionesX = [publicacion3_2, publicacion3_3,publicacion4_4, publicacion4_5, publicacion4_6]

redY= (usuariosA, relacionesY, publicacionesX)
publicacionesY = [publicacion3_2, publicacion3_3,publicacion4_3, publicacion4_4, publicacion4_5, publicacion4_6]
relacionesY = [relacion1_3, relacion1_4]