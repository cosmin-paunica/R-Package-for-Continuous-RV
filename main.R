# 1
normalizing_constant <- function(f)
{
  I = integrate(f, -Inf, Inf)$value
  return(1/I)
}

# f <- function(x) {exp((-x**2))}
I = normalizing_constant(f)

# 2
allValuesPos <- function(vec) all(vec >= 0)
verifyPositive <- function(fAux){
  allValuesPos(fAux(seq(-100, 100, 0.1)))
}
is_prob_density <- function(faux)
{
  if(!verifyPositive(faux))
    F
  if (floor(integrate(faux, -Inf, Inf)$value) ==0)
    T
  else
    F
}


#3
CRV <- function(pdFuncion,type=1){
  if(!is_prob_density(pdFuncion))
    return(FALSE)
  object = pdFuncion
  attr(object,"pdf") = pdFuncion 
  attr(object,"type")= type
  class(object)="CRV"
  return(object)
}

i=is_prob_density(dnorm)
test <- CRV(dnorm)
class(test)
integrate(dnorm,-Inf,Inf)$value


expectedValue <- function(X){#intoarce valoare medie a distributiei
  if(class(X)!="CRV")
    return()
  functionFromX<- attr(X,"pdf")
  integrate(function(x){x*functionFromX(x)},-Inf,Inf)$value
}

meanOfFunction <- function(g, X){ #intoarce valoarea medie a lui g(X)
  if(class(X)!="CRV")
    return()
  functionFromX<- attr(X,"pdf")
  integrate(function(x){x*g(functionFromX(x))},-Inf,Inf)$value
}

varianceValue<-function(X){
  if(class(X)!="CRV")
    return()
  functionFromX<- attr(X,"pdf")
  toVariance <- function(x){
    (x^2)*functionFromX(x)
  }
  integrate(toVariance,-Inf,Inf)$value - (expectedValue(X)^2)
}

varianceOfFunction <- function(g,X){
  if(class(X)!="CRV")
    return()
  functionFromX<- attr(X,"pdf")
  toVariance <- function(x){
    (x^2)*g(functionFromX(x))
  }
  integrate(toVariance,-Inf,Inf)$value - (expectedValue(X)^2)
}

Pcont<-function(continua,a=-Inf,b){
  if(!class(continua)=="CRV")
    return(1)
  integrate(attr(continua,"pdf"),a,b)
}