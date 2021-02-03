# 1
normConstant <- function(f) {
  I = integrate(f, -Inf, Inf)$value
  return(1/I)
}

# 2
allValuesPos <- function(vec) all(vec >= 0)

verifyPositive <- function(fAux) {
  allValuesPos(fAux(seq(-100, 100, 0.1)))
}

isProbDensity <- function(faux) {
  if(!verifyPositive(faux))
    F
  if (round(integrate(faux, -Inf, Inf)$value, 4) == 1)
    T
  else
    F
}

#3
CRV <- function(pdFuncion,type=1) {
  if(!isProbDensity(pdFuncion))
    stop("function is not a probability density")
  object = pdFuncion
  attr(object,"pdf") = pdFuncion 
  attr(object,"type")= type
  class(object)="CRV"
  return(object)
}

# 5
expectedValue <- function(X) { # intoarce valoare medie a distributiei
  if(class(X)!="CRV")
    stop("X is not a continuous random variable")
  functionFromX<- attr(X,"pdf")
  integrate(function(x){x*functionFromX(x)},-Inf,Inf)$value
}

varianceValue<-function(X) {
  if(class(X)!="CRV")
    stop("X is not a continuous random variable")
  functionFromX<- attr(X,"pdf")
  toVariance <- function(x) {
    (x^2)*functionFromX(x)
  }
  integrate(toVariance,-Inf,Inf)$value - (expectedValue(X)^2)
}

moment <- function(X, k) {
  if (k < 1 || k > 4) {
    stop("k nu este corespunzator")
  }
  if (class(X) != "CRV") {
    stop("X nu este v.a. continua")
  }
  meanOfFunction(function(x) {x**k}, X)
}

centeredMoment <- function(X, k) {
  if (k < 1 || k > 4) {
    stop("k nu este corespunzator")
  }
  if (class(X) != "CRV") {
    stop("X nu este v.a. continua")
  }
  functionFromX <- attr(X,"pdf")
  E = expectedValue(X)
  meanOfFunction(function(x) {(x-E)**k}, X)
}

# 6
meanOfFunction <- function(g, X){ # intoarce valoarea medie a lui g(X)
  if(class(X)!="CRV")
    stop("X is not a continuous random variable")
  functionFromX<- attr(X,"pdf")
  integrate(function(x){x*g(functionFromX(x))},-Inf,Inf)$value
}

varianceOfFunction <- function(g,X){
  if(class(X)!="CRV")
    stop("X is not a continuous random variable")
  functionFromX<- attr(X,"pdf")
  toVariance <- function(x){
    (x^2)*g(functionFromX(x))
  }
  integrate(toVariance,-Inf,Inf)$value - (expectedValue(X)^2)
}

# 7
PCont<-function(X,a=-Inf,b){
  if(!class(X)=="CRV")
    stop("X is not a continuous random variable")
  integrate(attr(X,"pdf"),a,b)
}