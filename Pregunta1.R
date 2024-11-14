#PREDICCIONES 

#binomial -> encuesta

#binomial negativa -> hasta que ocurra algo, cuanto se resiste
# cuanta el numero de ceros hasta r unos

#poisson-> conteos en intervalors, cuenta el numero en un determindo rango

# hipergeometrica -> dentro de una muestra cuantificar 

# exponencial -> 

#normal -> campana de gauss




#PROBLEMA 1

#poblacion X <- N(mu, sigma^2)
mu <- 93.5
sigma <- 5.7

curve(dnorm(x, mu, sigma), xlim=c(80, 110), col="red")

#a)
set.seed(123)
rnorm(4, mu, sigma)
sum(rnorm(4, mu, sigma))

Y <- function(i){sum(rnorm(4, mu, sigma))}
Y(2)

Y4 <- sapply(1:4, Y)
hist(Y4)

Y1000000 <- sapply(1:1000000, Y)
hist(Y1000000)
mean(Y1000000) #suma 
4*mu #comprovacio, ha d'aproxiamr-se a l'anterior


#b)
Y <- function(i){sum(rnorm(100, mu, sigma))}
Y(100)

Y1000000 <- sapply(1:1000000, Y)
var(Y1000000)

100*sigma^2 #comprovacio, ha d'aproxiamr-se a l'anterior

hist(Y1000000, freq=FALSE)
curve(dnorm(x, 100*mu, sqrt(100)*sigma), add=TRUE, col="red")


#c)
1-pnorm(103, mu, sigma)


#d)
Xbar <- function(i){mean(rnorm(4, mu, sigma))}
Xbar500000 <- sapply(1:500000, Xbar)
mean(Xbar500000>98)

1-pnorm(98, mu, sigma/sqrt(4))

hist(Xbar500000, freq=FALSE)
curve(dnorm(x, mu, sigma/sqrt(4)), add=TRUE, col="red")


#e)
Ssq <- function(i){var(rnorm(100, mu, sigma))}
Ssq500000 <- sapply(1:500000, Ssq)
mean(Ssq500000>32)

1-pchisq(32*(100-1)/sigma^2, 100-1)

hist(Ssq500000*(100-1)/sigma^2, freq=FALSE)
32*(100-1)/sigma^2
curve(dchisq(x, 100-1), add=TRUE, col="red")
