#DISCRETAS

#BINOMIAL:
#dbinom(k, n ,p) --->  p(x = k) = ?
#pbinom(k, n, p) --->  p(x <= k) = ?
#qbinom(q, n, p) --->  p(x <= ?) = q 
#rbinom(k ,n ,p) --->  genera k binomiales.


#GEOMATRICA:
#dgeom(k, p) --->  p(x = k) = ?
#pgeom(k, p) --->  p(x <= k) = ?
#qgeom(q, p) --->  p(x <= ?) = q 
#rgeom(n,p)  --->  genera n geometricas.

#BINOMIAL NEGATIVA:
#dnbinom(k, n ,p, r) --->  p(x = k) = ?
#pnbinom(k, n, p, r) --->  p(x <= k) = ?
#qnbinom(q, n, p, r) --->  p(x <= ?) = q 
#rnbinom(k, n ,p, r)  --->  genera k binomiales negativas.

#HiPERGEOMETRICA:
#H(n,N,D): x = lo q me piden, m = D, n= N-D, k = n.
#dhyper(k, D, N-D, n) --> P( X = K)
#phyper(k, D, N-D, n) --> P( X <= K)
#qhyper(p, D, N-D, n) --->  p(x <= ?) = p
#rhyper(k, D, N-D, n) --->  genera k hipergeometricas

#POISSON: 
#dpois(k, lambda) --> P( X = K)
#ppois(k, lambda) --> P( X <= K)
#qpois(q, lambda) --->  p(x <= ?) = q 
#rpois(n, lambda) --->  genera n poissons

#CONTINUAS

#UNIFORME:
#dunif(x,a,b) ---> densidad 
#punif(X,a,b) ---> Fx(X) = (X-a)/(b-a) 
#qunif(p,a,b) ---> p(x <= ?) = q 
#runif(n,a,b) ---> genera n uniformes.

#NORMAL STANDARD:
#dnorm(x,0,1) ---> densidad
#pnorm(c,0,1) ---> p(z <= c)
#qnorm(p,0,1) ---> p(z <= c) = p
#rnorm(n,0,1) ---> genera n normales.

#GAMAA:
#dgamma(t,a,lambda) ---> fx(t)
#pgamma(t,a,lambda) ---> Fx(t)
#qgamma(p,a,lambda) ---> p(t <= c) = p
#rgamma(n,a,lamda) ---> genera n gammas.

#EXPONENCIAL:
#dexp(t,lambda) ---> fx(t) = lambda*e^(-tlambda)
#pexp(t,lambda) ---> Fx(t) = 1 - e^(-tlambda)
#qexp(p,lambda) ---> Fx(x) = p  
#rexp(n,lambda) ---> genera n exponenciales.

#VECTORES

#MULTINOMIAL:
# x <- c(x1,x2,...,xn)
# p <- c(p1,p2,...,pn)
# n: number of random vectors to draw.
#size: integer, total number of objects that are put into K boxes.
#For dmultinom, size defaults to sum(x).

#rmultinom(n, size, prob)
#dmultinom(x, size = NULL, prob)

#ESTIMACIÓN
#S cuadrado (estimador de la varianza)
s_cuad <- function(n,x){
  div <- 1/(n-1)
  x_raya <- sum(x)/n
  suma <- sum((x-x_raya)^2)
  return(div*suma)
}

#EMV de la normal
#tambien sirve para EM y EMV poisson
EMV_normMu <- function(x){
  n <- length(x)
  return(sum(x)/n)
}

EMV_normVar <- function(x){
  n <- length(x)
  suma <- sum((x-mean(x))^2)
  cant <- 1/n
  return(cant*suma)
}

EMV_normVar_conMu <- function(x,mu){
  n <- length(x)
  suma <- sum((x-mu)^2)
  cant <- 1/n
  return(cant*suma)
}

#EM de la normal
#El EM de mu es el mismo que el EMV
EM_normVar <- function(x){
  n <- length(x)
  x_raya<- sum(x)/n
  suma <- sum(x^2)
  cant <- 1/n
  return(cant*suma - x_raya^2)
}

#EM y EMV exponencial
#tambien sirve para EM y EMV geométrica
est_exp <- function(x){
  n <- length(x)
  x_raya <- sum(x)/n
  return(1/x_raya)
}

#EM y EMV uniforme
EM_unif <- function(x){
  n <- length(x)
  x_raya <- sum(x)/n
  return(2*x_raya)
}

EMV_unif <- function(x){
  return(max(x))
}

##Intervalos de confianza bajo normalidad, funciones:

library(tidyverse)
set.seed(10)

longitudIC <- function(var,alpha,n){
  cuantil <- qnorm( p = 1 - ( alfa / 2 ) )
  return(2*cuantil*sqrt(var/n))
}


## IC de mu con varianza conocida
normalVarConocida_conMuestra <- function( muestra, var, alfa ) {
  poblacion <- length( muestra )
  media <- mean( muestra )
  cuantil <- qnorm( p = 1 - ( alfa / 2 ) )
  
  radio <- sqrt(var) * ( 1 / sqrt( poblacion ) ) * cuantil
  return(
    c(
      inferior = media - radio,
      superior = media + radio
    )
  )
}

normalVarConocida <- function(x_raya, n, var, alfa ) {
  cuantil <- qnorm( p = 1 - ( alfa / 2 ) )
  
  radio <- sqrt(var) * ( 1 / sqrt( n ) ) * cuantil
  return(
    c(
      inferior = x_raya - radio,
      superior = x_raya + radio
    )
  )
}

longitudIC <- function(var,alpha,n){
  cuantil <- qnorm( p = 1 - ( alpha / 2 ) )
  return(2*cuantil*sqrt(var/n))
}

tamañoMuestraIC <- function(var,alpha,long){
  cuantil <- qnorm( p = 1 - ( alpha / 2 ) )
  sigma <- sqrt(var)
  return((2*cuantil*sigma/long)^2)
}

##IC de mu con varianza desconocida
normalVarDesconocida_conMuestra <- function( muestra, alfa ) {
  poblacion <- length( muestra )
  media <- mean( muestra )
  desvioMuestral <- sd( muestra )
  cuantil <- qt( p = 1 - ( alfa / 2 ), df = poblacion - 1 )
  radio <- desvioMuestral * ( 1 / sqrt( poblacion ) ) * cuantil
  return(
    c(
      inferior = media - radio,
      superior = media + radio
    )
  )
}

normalVarDesconocida <- function(sd, x_raya, n, alfa ) {
  cuantil <- qt( p = 1 - ( alfa / 2 ), df = n - 1 )
  radio <- sd * ( 1 / sqrt( n ) ) * cuantil
  return(
    c(
      inferior = x_raya - radio,
      superior = x_raya + radio
    )
  )
}

## Ic para sigma^2 con media conocida 
normalMedConocida <- function( muestra,media, alfa) {
  poblacion <- length( muestra )
  sumatoria <- sum((muestra -media)^2)
  radio1 <- qchisq(1-alfa/2,poblacion)
  radio2 <- qchisq(alfa/2,poblacion)
  return(
    c(
      inferior = sumatoria / radio1,
      superior = sumatoria / radio2
    )
  )
}

## IC para sigma^2 con media desconocida

normalMedDesconocida <- function(muestra, alfa) {
  poblacion <- length( muestra )
  sumatoria <- sum((muestra-mean(muestra))^2)
  radio1 <- qchisq(1-alfa/2,poblacion-1)
  radio2 <- qchisq(alfa/2,poblacion-1)
  return(
    c(
      inferior = sumatoria / radio1,
      superior = sumatoria / radio2
    )
  )
  
}

#Distribucion desconocida con var conocida
distVarConocida <- function(muestra,var, alfa){
  poblacion <- length( muestra )
  media <- mean( muestra )
  s <- sqrt(var)
  cuantil <- qnorm( p = 1-abs(1-alfa)/2)
  radio <- s/sqrt(poblacion) * cuantil
  return(
    c(
      inferior = media - radio,
      superior = media + radio
    )
  )
  
}

#Distribucion desconocida con var desconocida
distVarDesconocida <- function(muestra, alfa){
  poblacion <- length( muestra )
  media <- mean( muestra )
  s <- sqrt(sum((muestra-media)^2)/(poblacion-1))
  cuantil <- qt( p = 1- abs(1-alfa)/2, df = poblacion - 1 )
  radio <- s/sqrt(poblacion) * cuantil
  return(
    c(
      inferior = media - radio,
      superior = media + radio
    )
  )
  
}

#IC uniforme[0,tita]
expICExacta <- function( muestra, alfa ) {
  n <- length( muestra )
  maximo <- max(muestra)
  return(
    c(
      inferior =  maximo/(alfa^(1/n)),
      superior =  maximo
    )
  )
}

#IC exponencial
expICExacta <- function( muestra, alfa ) {
  poblacion <- length( muestra )
  radio1 <- (qchisq(alfa/2, 2*poblacion))
  radio2 <- (qchisq(1-alfa/2, 2*poblacion))
  return(
    c(
      inferior =  radio1/(2*sum(muestra)),
      superior =  radio2/(2*sum(muestra))
    )
  )
}


expICAsintotica <- function(muestra, alfa){
  poblacion <- length( muestra )
  media <- mean( muestra )
  radio1 <- qnorm(alfa/2) 
  radio2 <- qnorm(1-alfa/2)
    c(
      inferior = (radio1/sqrt(poblacion) + 1)/media,
      superior = (radio2/sqrt(poblacion) + 1)/media
    )
  
}

#IC binomial
binomICAsintotica_conMuestra <- function(muestra,n,alfa){
  cuantil <- qnorm( p = 1 - ( alfa / 2 ) )
  psombrero <- sum(muestra) * 1/n
  radio <- sqrt((psombrero*(1-psombrero))/n)
  return(
    c(
      inferior = psombrero-cuantil*radio,
      superior = psombrero+cuantil*radio
    )
  )
}

binomICAsintotica <- function(n,p0,alfa){
  cuantil <- qnorm(1-alfa/2)
  radio <- sqrt((p0*(1-p0))/n) 
  return(
    c(
      inferior = p0-cuantil*radio,
      superior = p0+cuantil*radio
    )
  )
}


longitudICBinom <- function(p0,alpha,n){
  cuantil <- qnorm( p = 1 - ( alpha / 2 ) )
  return(2*cuantil*sqrt((p0*(1-p0))/n))
}

#acoto p0(1-p0) por 1/4, ya que depende de n --> sigma = sqrt(1/4) = 1/2
tamañoMuestraICBinom <- function(alpha,long){
  cuantil <- qnorm( p = 1 - ( alpha / 2 ) )
  sigma <- 1/2
  return(((2*cuantil*sigma)/long)^2)
}

#IC poisson
poissonICAsintotica_conMuestra <- function(muestra,alfa){
  n <- length(muestra)
  cuantil <- qnorm( p = 1 - ( alfa / 2 ) )
  lambdasombrero <- sum(muestra) * 1/n
  radio <- sqrt((lambdasombrero)/n)
  return(
    c(
      inferior = lambdasombrero-cuantil*radio,
      superior = lambdasombrero+cuantil*radio
    )
  )
  
}

poissonICAsintotica <- function(lambdasombrero,n,alfa){
  cuantil <- qnorm( p = 1 - ( alfa / 2 ) )
  radio <- sqrt((lambdasombrero)/n)
  return(
    c(
      inferior = lambdasombrero-cuantil*radio,
      superior = lambdasombrero+cuantil*radio
    )
  )
  
}

#TESTS DE HIPÓTESIS

# dist normal

#estadístico para la media con var conocida
est_mu_conVar <- function(n, x_raya, mu0, var){
  raiz <- sqrt(n)
  t <- raiz * (x_raya-mu0)/sqrt(var)
  return(t)
}

est_mu_conVar_muestra <- function(x, mu0, var){
  n <- length(x)
  x_raya <- sum(x)/n
  raiz <- sqrt(n)
  t <- raiz * (x_raya-mu0)/sqrt(var)
  return(t)
}

#estadístico para la media con var desconocida

est_mu_sinVar_muestra <- function(x_raya, n, mu0, var){
  raiz <- sqrt(n)
  t <- raiz * (x_raya-mu0)/sqrt(s_cuad(n,x))
  return(t)
}

est_mu_sinVar_muestra <- function(x, mu0, var){
  n <- length(x)
  x_raya <- sum(x)/n
  raiz <- sqrt(n)
  t <- raiz * (x_raya-mu0)/sqrt(s_cuad(n,x))
  return(t)
}

#estadístico para la var con media conocida
est_var_conMu <- function(x,n,mu0,var0){
  suma <- sum(x-mu0)
  return(suma/var0)
}

#estadístico para la var con media desconocida
est_var_sinMu <- function(x,var0){
  n<- lenght(x)
  size <- n-1
  s <- s_cuad(n,x)
  return(size*s/var0)
}

#Estadistico para el parámetro de una exponencial
estadistico_exp <- function(x,lambda0){
  suma <- sum(x)
  return(2*lambda0*suma)
}


#funcion de potencia H1: mu > mu0, var conocida
pi_mu_conVar <- function(mu0,mu,sigma_cuad0,n,alpha){
  zalpha <- qnorm(1-alpha)
  res <- (mu0 - mu)/(sqrt(sigma_cuad0/n))
  return(1-pnorm(zalpha + res))
}


#funcion de potencia H1: var < var0, media desconocida
pi_var_noMu <- function(sigma_cuad,sigma_cuad0,n,alpha){
  x <- qchisq(alpha,n)
  res <- (sigma_cuad0/sigma_cuad)*x
  return(pchisq(res,n))
}

