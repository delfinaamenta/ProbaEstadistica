#Aca le pongo los valores a la matriz de la tabla, especifico el numero de filas y 
# separo con un enter los valores como el ejemplo de aca.
tabla <- matrix(c(0,0.20,0.20,
                  2.0/15.0,0.4,0,
                  1.0/15.0,0,0),nrow =3 , byrow = TRUE)
#en el c() pongo los valores de X e Y, y esto me calcula la esperanza
#fijarme que la cantidad de numeros sea igual a la cantidad de filas o columnas
rangoX <- c(0,1,2)
rangoY <- c(0,1,2)

esperanzaY <- function(tabla,rangoX){
  sum(rangoY*rowSums(tabla))
} 
esperanzaX <- function(tabla,rangoY){
  sum(rangoX*colSums(tabla))
} 

#E(x^2)
esperanzaX2 <- function(tabla, rangoX){
  sum(rangoX^2 * rowSums(tabla))}
esperanzaY2 <- function(tabla, rangoY){
  sum(rangoY^2 * colSums(tabla))}

#multiplica por las filay columnas y las suma ----- E(X*Y)
esperanzaXY <- function(tabla){
  b = sweep(tabla, 2, rangoX, FUN="*")
  a = b*rangoY
  return(sum(a))
}
esperanzaXY(tabla)
#COVARIANZA
covarianza <- function(tabla,rangoY,rangoX){
  a = esperanzaXY(tabla)
  b = esperanzaY(tabla,rangoY)
  c = esperanzaX(tabla,rangoX)
  return(a-(b*c))
  
}
covarianza(tabla,rangoY,rangoX)

varianzaX <- function(tabla, rangoX){
  a = esperanzaX2(tabla, rangoX)
  b = esperanzaX(tabla, rangoX)^2
  return(a-b)
}
varianzaY <- function(tabla, rangoY){
  a = esperanzaY2(tabla, rangoY)
  b = esperanzaY(tabla, rangoY)^2
  return(a-b)
}
correlacion <- function(tabla,rangoY,rangoX){
  a = covarianza(tabla,rangoY, rangoX)
  b = varianzaY(tabla,rangoY)
  c = varianzaX(tabla,rangoX)
  return(a/(b*c))
}

#INTEGRAL DOBLE
InnerFunc <- function(x, y) { (x^3 * y + y^3 * x )* (3/38) }#ACA VA LA FUNCION
InnerIntegral <- function(y) { sapply(y,    #ACA VAN LOS LIMITES DE x
                      function(z) { integrate(InnerFunc, 2, 3, y=z)$value }) }
integrate(InnerIntegral, 2, 3) #ACA VAN LOS LIMITES DE Y
