Phi <- function(Xd, Muj, Sigmaj)
{
   return (exp(- norm( matrix(Xd)-matrix(Muj))^2/(2*Sigmaj^2) ))
}
fd <- function(xd, Beta, Mu, Sigma)
{
   total = 0
   for(p in 1:ncol(Beta))
   {
	total = total + Beta[1,p]* Phi(xd, Mu[p,], Sigma[,p])#  exp( - (  norm( matrix(xd) - matrix(Mu[p,]) )^2)/Sigma[1,p]  )	
   }
   return (total)
}

E <- function( Beta, Mu, Sigma, X, Y)
{
	total = 0
	for(d in 1:nrow(X))
	{
	   total = total+ (Y[d,] - fd(X[d,], Beta, Mu, Sigma))^2
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
	     total = total + (Y[d, ]-fd(X[d, ], Beta, Mu, Sigma))*Phi(X[d, ], Mu[j,], Sigma[,j])
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
	     total = total + (Y[d, ]-fd(X[d,], Beta, Mu, Sigma))*Phi(X[d, ], Mu[j,], Sigma[,j])*( X[d,]-Mu[j])*(1.0/(Sigma[,j]^2)) 
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
	     total = total + (Y[d, ]-fd(X[d,], Beta, Mu, Sigma))*Phi(X[d, ], Mu[j,], Sigma[,j])*(norm(matrix(X[d,])- matrix(Mu[j,]))^2)/(Sigma[,j]^3)
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
	   total = total+ (Y[d, ] - fd(X[d, ], Beta, Mu, Sigma))^2
	}
   return (total)
}

RBF <- function(Y, P, X, Yp)
{

   
   D=ncol(X) ##calcular la dimension
   ##Vector pesos
   Beta = matrix(1,ncol=P)
   ##Vector varianzas
   Sigma = matrix(1,ncol=P)
   ##Matriz medias P X D
   Mu = matrix(1,ncol=D, nrow=P )
      
   ##Inicialización
   for(i in 1:P) {
    Beta[1,i] = runif(1,0.1,1)
    Sigma[1,i] = runif(1,0.1,1)
   
   	for(j in 1:D) {
   		Mu[i,j] = runif(1,0,1)	
   	}
   }
#   Beta = Beta/norm(Beta)
   nite =5000
   delta = 0.00001
   etaSigma = delta
   etaMu = delta
   etaBeta = delta
 
 	cl=kmeans(Y, P, nstart = 10, iter.max=100)
	Mu = (cl$centers)
#	for( i in 1:P)
#	{
#	Sigma[,i] = 0;
#	   for( j in X[which(cl$cluster==i), ])
#	   {
#		Sigma[,i] = Sigma[,i] + sqrt(sum(j, Mu[i,])^2)
#	   }
#	   Sigma[,i] =  Sigma[,i] / sum(cl$cluster==i  )
#	}		
   for( it in 1:nite)
   {
	Beta = Beta + etaBeta*gradient_beta(Beta, Mu, Sigma, X, Y)
	Mu = Mu + etaMu*gradient_mu(Beta, Mu, Sigma, X, Y)
	Sigma = Sigma + etaSigma*gradient_sigma(Beta, Mu, Sigma, X, Y)
#	pause(0.5)
	print(Beta)
#	Sys.sleep(1)
   }
   for( i in 1:nrow(X))
   {
	Yp[i,]=0
#	Yp[i,] = E( X[i,], Beta, Mu, Sigma, Y)
	for( j in 1:ncol(Beta))
	{
	   Yp[i,] = Yp[i,]+ Beta[1,j]*Phi(X[i, ], Mu[j,], Sigma[1,j])
	}
   } 	
   return (Yp)
}
##
Sizetrain=100
##Generar datos de entrenamiento
sX = matrix( runif(Sizetrain, 0, 1), ncol=1)#nrow=Sizetrain)
#Y = matrix(sX*sX  + rnorm(100, 0, 0.1), nrow=100)
Y = matrix(sX , nrow=Sizetrain, ncol=1)
N=100

##Datos de prueba...
X2 = matrix( runif( 1000, -1, 1),ncol=ncol(Y))

##Número de neuronas..
P=3
##Dimension

Y2= RBF(Y, P, sX, X2 )
plot(sX, Y)
points(sX, Y2, col='red')

#write.table(sX,file= "datax", sep="\t", col.names=FALSE, row.names=FALSE)
#write.table(Y,file= "datay", sep="\t", col.names=FALSE, row.names=FALSE)
#write.table(Y2,file= "datayp", sep="\t", col.names=FALSE, row.names=FALSE)
