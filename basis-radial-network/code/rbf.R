fd <- function(xd, Beta, Mu, Sigma)
{
   total = 0
   for(p in 1:ncol(Beta))
   {
	total = total + Beta[,p]*exp( - (norm( matrix(xd) - matrix(Mu[p,]) )^2)/Sigma[1,p]  )	
   }
   return (total)
}
E <- function( Beta, Mu, Sigma, X, Y)
{
#  return (norm( Y - fk(X, Beta, Mu, Sigma))^2)
	total = 0
	for(d in 1:nrow(X))
	{
	   total = total+ (Y[d] - fd(X[d, ], Beta, Mu, Sigma))^2
	}
   return (total)
}
df_dbeta <- function(xd, Beta, Mu, Sigma)
{
   total = 0
   for(p in 1:ncol(Beta))
   {
	total = total + exp( - (norm( matrix(xd) - matrix(Mu[p,]) )^2)/Sigma[1,p]  )	
   }
   return (total)
}
df_mu <- function(xd, Beta, Mu, Sigma)
{
   total = 0
   for(p in 1:ncol(Beta))
   {
	total = total + Beta[,p]*exp( - (norm( matrix(xd) - matrix(Mu[p,]) )^2)/Sigma[1,p]  )* norm( matrix(xd) - matrix(Mu[p,]) )*0.5
   }
   return (total)
}
df_sigma <- function(xd, Beta, Mu, Sigma)
{
   total = 0
   for(p in 1:ncol(Beta))
   {
	total = total + Beta[,p]*exp( - (norm( matrix(xd) - matrix(Mu[p,]) )^2)/Sigma[1,p]  )* norm( matrix(xd) - matrix(Mu[p,]) )^2*(1.0/Sigma[1,p])
   }
   return (total)
}
gradient_beta <- function(Beta, Mu, Sigma, X, Y)
{
	total = 0
	for(d in 1:nrow(X))
	{
	   total = total+ (Y[d] - fd(X[d, ], Beta, Mu, Sigma))*df_dbeta(X[d, ], Beta, Mu, Sigma)
	}
   	return (total)
}
gradient_mu<- function(Beta, Mu, Sigma, X, Y)
{
	total = 0
	for(d in 1:nrow(X))
	{
	   total = total+ (Y[d] - fd(X[d, ], Beta, Mu, Sigma))*df_mu(X[d, ], Beta, Mu, Sigma)
	}
   	return (total)
}
gradient_sigma<- function(Beta, Mu, Sigma, X, Y)
{
	total = 0
	for(d in 1:nrow(X))
	{
	   total = total+ (Y[d] - fd(X[d, ], Beta, Mu, Sigma))*df_sigma(X[d, ], Beta, Mu, Sigma)
	}
   	return (total)
}
##Función objetivo...
E <- function( Beta, Mu, Sigma, X, Y)
{
#  return (norm( Y - fk(X, Beta, Mu, Sigma))^2)
	total = 0
	for(d in 1:nrow(X))
	{
	   total = total+ (Y[d] - fd(X[d, ], Beta, Mu, Sigma))^2
	}
   return (total)
}

RBF <- function(Y, P, X)
{

   D=ncol(Y) ##calcular la dimension
   ##Vector pesos
   Beta = matrix(0,ncol=P)
   ##Vector varianzas
   Sigma = matrix(0,ncol=P)
   ##Matriz medias P X D
   Mu = matrix(0,ncol=D, nrow=P )

      
   ##Inicialización
   for(i in 1:P) {
    Beta[1,i] = runif(1,-10,10)
    Sigma[1,i] = runif(1,0,1)
   
   	for(j in 1:D) {
   		Mu[i,j] = runif(1,-1,1)
   	}
   }
   
   nite = 100
   eta = 0.1
   for( it in 1:nite)
   {
	##Obtener el gradiente de los pesos
	betag = gradient_beta(Beta, Mu, Sigma, X, Y)
	##Obtener el gradiente de la medias
	mug = gradient_mu(Beta, Mu, Sigma, X, Y)
	##Gradiente de la varianza
	varg =	gradient_sigma(Beta, Mu, Sigma, X, Y)
	Beta = Beta - eta*betag
	Mu = Mu - eta*mug
	Sigma = Sigma - eta*varg
        print(E( Beta, Mu, Sigma, X, Y))
   }
   Yp = X;
   for( i in 1:nrow(X))
   {
	Yp[i,] = fd( X[i,], Beta, Mu, Sigma)
   } 	
  
   return (Yp)
}
##

##Generar datos de entrenamiento
sX = runif(100, -1, 1)
Y = matrix(sX*sX  + rnorm(100, 0, 0.1), nrow=100)
N=100
X2 = matrix( runif( N, -1, 1),ncol=ncol(Y))

##Número de neuronas..
P=10
##Dimension

Y2= RBF(Y, P, X2 )
plot(sX, Y)
points(X2, Y2, col='red')
