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

usNomRep=(12,"Marcelo")

ususariosRobertoTrue=[usJ1,usJ2,usJ3,usJ4,usJ5,usJ6,usJ7,usJ8,usJ9,usJ10,usJ11]
usuariosJ1=[usJ1,usJ3,usJ7]
usuariosJ2=[usJ1,usJ2,usJ4,usJ5,usJ7,usJ11]
usuariosNomRep=[usJ1,usJ2,usNomRep]

relacionesJ1=[(usJ1,usJ3)]
relacionesJ2=[(usJ1,usJ4),(usJ2,usJ4),(usJ5,usJ2),(usJ4,usJ7),(usJ5,usJ11)]
relacionesJ3=[(usJ1,usJ4),(usJ2,usJ4),(usJ5,usJ2),(usJ5,usJ11)]
relacionesJ4=[(usJ2,usJ5),(usJ1,usJ7)]
relacionesRobertoTrue=[(usJ1,usJ2),(usJ1,usJ3),(usJ1,usJ4),(usJ1,usJ5),(usJ1,usJ6),(usJ1,usJ7),(usJ1,usJ8),(usJ1,usJ9),(usJ1,usJ10),(usJ1,usJ11)]

redUsVacio=([],[],[])
redRelsVacio=(usuariosJ1,[],[])

redJ1=(usuariosJ1,relacionesJ1,[])
redJ2=(usuariosJ2,relacionesJ2,[])
redJ3=(usuariosJ2,relacionesJ3,[])
redJ4=(usuariosJ2,relacionesJ4,[])

redRobertoTrue=(usuariosJ1,relacionesRobertoTrue,[])
redNomRep=(usuariosNomRep,[],[])

tests = test [testSuiteEj1,testSuiteEj2,testSuiteEj3,testSuiteEj4,testSuiteEj5,testSuiteEj10]

run1 = runTestTT tests

testSuiteEj1 = test [
  " Caso 1: lista usuario vacía" ~: (nombresDeUsuarios redUsVacio) ~?= []
  " Caso 2: lista de usuarios sin nombres repetidos" ~: (nombresDeUsuarios redJ1) ~?= ["Marcelo","Pepe","Ronaldo"]
  " Caso 3: lista de usuarios con nombres repetidos" ~: (nombresDeUsuarios redNomRep) ~?= ["Marcelo","Roberto Carlos"]
]



testSuiteEj2 = test [
  " Caso 1: lista relaciones vacía" ~: (amigosDe redJ1 usJ1) ~?= []
  " Caso 2: usuario no tiene amigos" ~: (amigosDe redJ1 usJ7) ~?= []
  " Caso 3: usuario tiene amigos" ~: (amigosDe redJ2 usJ4) ~?= [usJ1,usJ2,usJ7]
]

testSuiteEj3 = test [
  " Caso 1: lista relaciones vacía" ~: (cantidadDeAmigos redRelsVacio usJ1) ~?= 0
  " Caso 2: usuario no tiene amigos" ~: (cantidadDeAmigos redJ1 usJ7) ~?= 0
  " Caso 3: usuario tiene amigos" ~: (cantidadDeAmigos redJ2 usJ2) ~?= 2
]

testSuiteEj4 = test [
  " Caso 1: lista relaciones vacía" ~: (usuarioConMasAmigos redRelsVacio) ~?= usJ1
  " Caso 2: un único usuario con máximo de amigos" ~: (usuarioConMasAmigos redJ2) ~?= usJ4
  " Caso 3: varios usuarios con el máximo de amigos" ~: (usuarioConMasAmigos redJ3) ~?= usJ2
]

testSuiteEj5 = test [
  " Caso 1: No hay usuario con más de 10 amigos" ~: (estaRobertoCarlos redJ1) ~?= False
  " Caso 2: Hay usuario con más de 10 amigos" ~: (estaRobertoCarlos redRobertoTrue) ~?= True
]

testSuiteEj10 = test [
  " Caso 1: usuarios de entrada iguales, sin amigos" ~: (existeSecuenciaDeAmigos redJ1 usJ7 usJ7) ~?= False
  " Caso 2: usuarios de entrada iguales, con amigos" ~: (existeSecuenciaDeAmigos redJ1 usJ1 usJ1) ~?= True
  " Caso 3: usuarios de entrada distintos, sin amigos" ~: (existeSecuenciaDeAmigos redJ4 usJ4 usJ11) ~?= False
  " Caso 4: primer usuario no tiene amigos" ~: (existeSecuenciaDeAmigos redJ4 usJ4 usJ1) ~?= False
  " Caso 5: segundo usuario no tiene amigos" ~: (existeSecuenciaDeAmigos redJ4 usJ1 usJ11) ~?= False
  " Caso 6: ambos usuarios tienen amigos, pero no están relacionados" ~: (existeSecuenciaDeAmigos redJ4 usJ1 usJ2) ~?= False
  " Caso 7: ambos usuarios son amigos entre sí" ~: (existeSecuenciaDeAmigos redJ4 usJ2 usJ5) ~?= True
  " Caso 8: ambos usuarios están relacionados lejanamente" ~: (existeSecuenciaDeAmigos redJ3 usJ1 usJ11) ~?= True
]
