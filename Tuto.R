# Calculer une intégrale

g = function(x){
  return(exp(-x^2));
}

I = integrate(g,0,1)
round(I$value,2)

# Tableau d'effectifs

X = c(2,1,5,3,3,4,3,5,3,5,3,1,4,6,4,1,3,6,1,1)
table(X)

# Comparaison de tableaux
V_1<-c(2, 1 ,5 ,3 ,3, 4, 3, 5, 3, 5, 3 ,1, 4, 6 ,4 ,1, 3, 6, 1, 1)
V_2<-c(1, 3 ,2 ,4 ,2, 1, 3, 3 ,1, 4, 4, 5, 2, 2, 4, 6, 1, 4, 5, 4)
result  <- c(V_1==V_2)
result

# définition d'un vecteur
v <-c(2,13,41)
v <- c(c1=2,c2=13,c3=41)
v
1/v

# différence entre les éléments d'un vecteur
diff(v)

# usage de apply
X_1 = V_1
X_1[X_1<5] <- 0
X_1
round(sapply(X_1, g),10)

# utilisation des dataframes
matri <- sample(100:200,10)
nom <- LETTERS[1:10]
matri
nom

notephys <- round(rnorm(10,10,3))
notemaths <- round(rnorm(10,8,3))
moy <- round(3*notephys + 2*notemaths)/5
resultat <- moy >= 10
affichage <-ifelse(resultat == TRUE, resultat<-"Admis", resultat<-"Non Admis")
examen <- data.frame(matri, nom, notemaths, notephys, moy, affichage)
examen

pgcd = function(a,b){
  c = a
  d = b
  
  a = as.integer(a)
  b = as.integer(b)
  while (a!=b) {
    # ifelse(a>b, a = a-b, b = b-a)
    if(a>b){
      a = a-b
    }else{
      b = b-a
    }
  }
  res = paste("Le pgcd de ",c ,"et",d,"est ",a)
  
  print(res)
}

pgcd(7,50)


# Statistique à deux variables
x = c(2,15,16,8,13,20,24,7,5,11)
y = c(3,13,17,12,10,8,20,7,2,8)
plot(x,y, main="Nuage de points de y en fonction de x")
r = cor(x,y)
r
resultats = lm(y~x)
resultats
abline(resultats)
grid(20)

# Manipulation des variables aléatoires
# Tirage avec remise de 10% de boules rouges, 35% de boules blanches, 55% de boules vertes
sample(urne <- c("R","B","V"), 10, replace = T, prob = c(0.10,0.35,0.55))

# Lancer d'un dé
