# toutes les lois de probablités implémentées on les 4 fonctionnalités suivantes
# 1. Calcul des quantiles
# 2. Calcul des probas
# 3. Génération des nombres suivant cette loi
# 4. FOnction de densité

qnorm(0.5,5,1.2)

qnorm(.25, 5, 1.2)

pnorm(4.190612,5,1.2)

# le paramètre log=TRUE permet de calculer le D en donnant son log directement 
# au lieu de faire log(dnorm(...)) qui est plus lent
dnorm(5,5,1.2,TRUE)

dnorm(5,5,1.2)


# générer un vecteur de taille n avec les paramètres demandés
v = rnorm(100,5,1.2)
# v[1] pour le premier, pas v[0]
summary(v)

# si on veut générer deux nombres aléatoires provenant de deux lois
# qui ont un lien entre elle, il faut initialiser le seed à la même valeur


# 1. Générateur de nombre aléatoire : changement de variable
set.seed(1)
x = runif(1)
set.seed(1)
y = runif(1,2,5)
# On construit u[2,5] à partir de u[0,1]
y1 = 5.*x+2. 
y2 = 3.*x+2 # cette transformation est la bonne
y # vaut 2.796526
y1 # vaut 3.327543
y2 # vaut 2.796526 (bon !!!)

# 2. Générateur de nombre aléatoire : inverse de la fonction de répartition
# il est démontré que : si u -> U[0,1] alors F^(-1) -> X 
# avec F qui est la fonction de répartition de la variable X (loi quelconque dont on 
# cherche à générer les valeurs)


# exemple de cette méthode avec la loi exponentielle

#  fonction de répartition de d'epsilon

Fforet = function(x){
  if(x<0){
    return(0)
  }
  else{
    if((x>=0)&&(x<pi/2))
      return (1-cos(x))
    return(1)
  }
}


# Fonction inverse de la fonction de répartition
invForest = function(x){
  if((x>=0)&&(x<=1))
    return(acos(1-x))
  return(0)
}

# fonction pour avoir la quantile d'ordre q
qForest = function(p){
  # car q = F^-1(p) et p = F(q)
  return(invForest(p))
}

# fonction pour avoir la probabilité
pForest = function(q){
  return(Fforet(q))
}

# fonction densité
dForest = function(x, log=FALSE){
  if((x>0)&&(x<pi/2))
    if(log==TRUE)
      return(log(sin(x)))
    return(sin(x))
  return(0)
}

# fonction pour générer les nombres aléatoires tirés de cette loi
rForest = function(n){
  return(invForest(runif(n)))
}

# x = rForest(100000000)
# hist(x)

D = 150
r=.3
X0 = rnorm(1,10)
X = rep(X0,51)
# listeBruit = rForest(50)
listeBruit = rlnorm(50,0,.02)
for(i in 2:51){
  X[i] = (D^(1-exp(-r))* X[i-1]^(exp(-r)))*listeBruit[i]
}
# plot(X,type = 'l')



# un modèle déterministe est un modèle dont on est sure qu'il produira toujours
# les même résultats si on lui passe les mêmes paramètres, ce qui est le contraire 
# d'un modèle stochastique (qui intègre de l'aléatoire)
# la diminution des paramètres du bruit fait en sorte que ce dernier se rapproche de 1

# Investigation du modèle
# Comment le bruit influence sur le modèle
# Si le bruit se comporte mal, comment y remédier ?
# Ce qui permet de mieux comprendre le bruit
# le choix du premier bruit n'est pas adapté, car il influence négativement le
# comportement du modèle
# un bon bruit devrait avoir des valeurs qui oscillent autour de 1
# il est aussi possible d'utiliser un bruit normal de paramètres 1 et 0.01



kmax = 50
genTrajectory = function(){
  X0 = rnorm(1,10)
  X = rep(X0,kmax+1)
  listeBruit = rlnorm(kmax,0,.02)
  for(i in 1:kmax){
    X[i+1] = (D^(1-exp(-r))* X[i]^(exp(-r)))*listeBruit[i]
  } 
  return(X)
}

M = matrix(0, nrow = 1000, ncol = kmax+1)
for(i in 1:1000){
  M[i,] = genTrajectory()
}

# plot(M[1,], type = 'l')
# for(i in 2:1000){
#   points(M[i,], type = 'l')
# }
