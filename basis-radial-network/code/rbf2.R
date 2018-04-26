fd <- function(xd, Beta, Mu, Sigma)
{
   total = 0
   for(p in 1:ncol(Beta))
   {
	total = total + Beta[1,p]*exp( - (( xd[1] - Mu[p,1] )^2)/Sigma[1,p]  )	
   }
   return (total)
}
Phi <- function(Xd, Muj, Sigmaj)
{
   return (exp(- (Xd-Muj)^2/Sigmaj ))
}
E <- function( Beta, Mu, Sigma, X, Y)
{
	total = 0
	for(d in 1:nrow(X))
	{
	   total = total+ (Y[d] - fd(X[d,1], Beta, Mu, Sigma))^2
	}
   return (total)
}
gradient_beta <- function(Beta, Mu, Sigma, X, Y)
{
	total = 0
	for(d in 1:nrow(X))
	{
	   for( j in 1:ncol(Beta) )
	   {
	     total = total + (Y[d]-fd(X[d,1], Beta, Mu, Sigma))*Phi(X[d, 1], Mu[j,], Sigma[,j])
	   }
	}
   	return (total)
}
gradient_mu<- function(Beta, Mu, Sigma, X, Y)
{
	total = 0
	for(d in 1:nrow(X))
	{
	   for( j in 1:ncol(Beta) )
	   {
	     total = total + (Y[d]-fd(X[d,1], Beta, Mu, Sigma))*Phi(X[d, 1], Mu[j,], Sigma[,j])*( X[d,1]-Mu[j])*(1.0/(Sigma[,j])) 
	   }
	}
   	return (total)
}
gradient_sigma<- function(Beta, Mu, Sigma, X, Y)
{
	total = 0
	for(d in 1:nrow(X))
	{
	   for( j in 1:ncol(Beta) )
	   {
	     total = total + (Y[d]-fd(X[d,1], Beta, Mu, Sigma))*Phi(X[d, 1], Mu[j,], Sigma[,j])*((X[d,1]-Mu[j,])^2/(Sigma[,j]^2))
	   }
	}
   	return (total)
}
##Función objetivo...
E <- function( Beta, Mu, Sigma, X, Y)
{
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
   Beta = matrix(1,ncol=P)
   ##Vector varianzas
   Sigma = matrix(1,ncol=P)
   ##Matriz medias P X D
   Mu = matrix(1,ncol=D, nrow=P )

      
   ##Inicialización
   for(i in 1:P) {
    Beta[1,i] = runif(1,0,1)
    Sigma[1,i] = runif(1,1,1)
   
   	for(j in 1:D) {
   		Mu[i,j] = runif(1,-1,1)	
   	}
   }
   Beta = Beta/norm(Beta)
   nite = 1000
   eta = 0.001
   for( it in 1:nite)
   {
	Beta = Beta + eta*gradient_beta(Beta, Mu, Sigma, X, Y)
	Mu = Mu + eta*gradient_mu(Beta, Mu, Sigma, X, Y)
	Sigma = Sigma + eta*gradient_sigma(Beta, Mu, Sigma, X, Y)
#	pause(0.5)
	print(Beta)
#	Sys.sleep(1)
   }
   Yp = X;
   for( i in 1:nrow(X))
   {
	Yp[i,1]=0
#	Yp[i,] = E( X[i,], Beta, Mu, Sigma, Y)
	for( j in 1:ncol(Beta))
	{
	   Yp[i,1] = Yp[i,1]+ Beta[1,j]*Phi(X[i, 1], Mu[j,1], Sigma[1,j])
	}
   } 	
   return (Yp)
}
##
Sizetrain=100
##Generar datos de entrenamiento
sX = matrix( runif(Sizetrain, 0, 1), nrow=Sizetrain)
#Y = matrix(sX*sX  + rnorm(100, 0, 0.1), nrow=100)
Y = matrix(-sX*sX+1 , nrow=Sizetrain)
N=100

##Datos de prueba...
X2 = matrix( runif( N, -1, 1),ncol=ncol(Y))

##Número de neuronas..
P=4
##Dimension

Y2= RBF(Y, P, sX )
plot(sX, Y)
points(sX, Y2, col='red')
