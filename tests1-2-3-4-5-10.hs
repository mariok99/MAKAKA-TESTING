import Solucion
import Test.HUnit

-- RECORDATORIO: Al descargar de github, es necesario agregar "," al final de cada renglón de los tests.

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
-- Relaciones con un solo usuario con max de amigos
relacionesJ2=[(usJ1,usJ4),(usJ2,usJ4),(usJ5,usJ2),(usJ4,usJ7),(usJ5,usJ11)]

-- Relaciones con varios usuarios teniendo máx de amigos
relacionesJ3=[(usJ1,usJ4),(usJ2,usJ4),(usJ5,usJ2),(usJ5,usJ11)]

-- Relaciones donde dos usuarios no tienen amigos y el resto sí
relacionesJ4=[(usJ2,usJ5),(usJ1,usJ7)]

-- publicaciones con likes
publicacionU1_1 = (usJ1, "i need moneey", [usJ3, usJ2])
publicacionU1_2 = (usJ1, "espero que ande", [usJ3, usJ2, usJ1,usJ4])
publicacionU2_1 = (usJ2, "boca y nada mas", [usJ1])

--publicaciones sin ningun like
publicacionX8_1 = (usJ8, "lorem input", [])
publicacionX9_1 = (usJ9, "zzzzz", [])
publicacionX10_1 = (usJ10, "aloja", [])

publicacionesU :: [Publicacion]
publicacionesU = [publicacionU1_1, publicacionU1_2, publicacionU2_1]

publicacionesX :: [Publicacion]
publicacionesX = [publicacionX8_1, publicacionX9_1, publicacionX10_1]



redJ1=(usuariosJ1,relacionesJ1,[]) -- redes usadas para los ejs 1,2,3,4,5 y 10. Ninguno precisa de publicaciones.
redJ2=(usuariosJ2,relacionesJ2,[])
redJ3=(usuariosJ2,relacionesJ3,[])
redJ4=(usuariosJ2,relacionesJ4,[])
--red con publicaciones y con likes de usuarios
redU = (usersU, [], publicacionesU) -- redes usadas para los ejs 7 y 8. Ninguno precisa de relaciones.

--red sin publicaciones
redV = (usersV, [], [])

--red con publicaciones pero sin likes
redX = (usersX, [], publicacionesX)


ususariosRobertoTrue=[usJ1,usJ2,usJ3,usJ4,usJ5,usJ6,usJ7,usJ8,usJ9,usJ10,usJ11,usJ12] -- lista usuarios donde estaRobertoCarlos pueda dar True.
usuariosNomRep=[usJ1,usJ2,usNomRep] -- lista usuarios donde dos repiten el mismo nombre.

relacionesRobertoTrue=[(usJ1,usJ2),(usJ1,usJ3),(usJ1,usJ4),(usJ1,usJ5),
 (usJ1,usJ6),(usJ1,usJ7),(usJ1,usJ8),(usJ1,usJ9),(usJ1,usJ10),(usJ1,usJ11),(usJ1,usJ12)]

redUsVacio=([],[],[])
redRelsVacio=(usuariosJ1,[],[])

redRobertoTrue=(usuariosJ1,relacionesRobertoTrue,[])
redNomRep=(usuariosNomRep,[],[])

-- Test Suites --

tests = test [testSuiteEj1,testSuiteEj2,testSuiteEj3,testSuiteEj4,testSuiteEj5,testSuiteEj7,testSuiteEj8,testSuiteEj10]

run1 = runTestTT tests

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
 
 testSuiteEj7 = test [
    "Caso1 red con pubs y like de usuario" ~: (publicacionesQueLeGustanA redU usJ1) ~?= [publicacionU1_2, publicacionU2_1],
    "Caso2 red sin publicaciones" ~: (publicacionesQueLeGustanA redV usJ5) ~?= [],
    "Caso3 red con publicaciones pero sin likes" ~: (publicacionesQueLeGustanA redX usJ8) ~?= []
 ]
    
testSuiteEj8 = test [
    "Caso1 usuarios con mismos likes en pubs" ~: (lesGustanLasMismasPublicaciones redU usJ3 usJ2) ~?= True,
    "Caso2 usuarios con likes en distintas pubs" ~: (lesGustanLasMismasPublicaciones redU usJ1 usJ2) ~?= False,
    "Caso3 usuarios con ningun like en pubs" ~: (lesGustanLasMismasPublicaciones redX usJ8 usJ9) ~?= True
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
