# le but de ce code est de contruire les 04 fonction fondamentales
# d'une variable aléatoire en se basant sur la fonction ci-dessous:


c = (3./124.)
b = 1./3.
FPoly = function(x){
  if(x<1){
    return(0)
  }
  else{
    if((x>=1)&&(x<=5))
      return((c/3.)*(125+(x-6)^3))
    return(1)
  }
}

part = function(x){
  return(124*x-125)  
}

invFPoly = function(x){
  if((x>=0)&&(x<=1))
    y = 6+(124*x-125)^(1/3)
    print(y)
    return(y)
  return(0)
}

qFPoly = function(p){
  return(invFPoly(p))
}

pFPoly = function(q){
  return(FPoly(q))
}

dFPoly = function(x, log= FALSE){
  if((x>=1)&&(x<=5))
    if(log==TRUE)
      return(log(c*(6-x)^2))
    return(c*(6-x)^2)
  return(0)
}


rFPoly = function(n){
  return(invFPoly(runif(n)))
}


