library(pracma)

FLoiNorm = function(x,m,s){
  b = erf((x-m)/s*sqrt(2))
  return(.5*(1+b))
}

invLoiNorm = function(x,m,s){
  return((s*sqrt(2)*erfinv((2*x)-1))+m)
}

pnorm1 = function(q,m,s){
  return(FLoiNorm(q,m,s))
}

qnorm1 = function(p,m,s){
  return(invLoiNorm(p,m,s))
}

dnorm1 = function(x,m,s){
  return ((1./(s*sqrt(2*pi)))* exp(-(x-m)^2/(2*(s^2))))
}

rnorm1 = function(n,m,s){
  return(invLoiNorm(runif(n),m,s))
}

