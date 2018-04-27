fd <- function(xd, Beta, Mu, Sigma)
{
   total = 0
   for(p in 1:ncol(Beta))
   {
	total = total + Beta[1,p]*exp( - (  norm( matrix(xd) - matrix(Mu[p,]) )^2)/Sigma[1,p]  )	
   }
   return (total)
}
Phi <- function(Xd, Muj, Sigmaj)
{
   return (exp(- norm( matrix(Xd)-matrix(Muj))^2/Sigmaj ))
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
	     total = total + (Y[d, ]-fd(X[d, ], Beta, Mu, Sigma))*Phi(X[d, ], Mu[j,], Sigma[,j])*2.0
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
	     total = total + (Y[d, ]-fd(X[d,], Beta, Mu, Sigma))*Phi(X[d, ], Mu[j,], Sigma[,j])*( X[d,]-Mu[j])*(1.0/(Sigma[,j])) *2.0
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
	     total = total + (Y[d, ]-fd(X[d,], Beta, Mu, Sigma))*Phi(X[d, ], Mu[j,], Sigma[,j])*(norm(matrix(X[d,])- matrix(Mu[j,]))^2)/(Sigma[,j]^2)*2.0
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

RBF <- function(Y, P, X)
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
   nite =500
   etaSigma = 0.001
   etaMu = 0.001
   etaBeta = 0.001
 
 	cl=kmeans(Y, P)
	Mu = (cl$centers)
	for( i in 1:P)
	{
	Sigma[,i] = 0;
	   for( j in 	Y[which(cl$cluster==i), ])
	   {
		Sigma[,i] = Sigma[,i] + sqrt(sum(j, Mu[i,])^2)
	   }
	   Sigma[,i] =  Sigma[,i] / sum(cl$cluster==i  )
	}		
   for( it in 1:nite)
   {
	Beta = Beta + etaBeta*gradient_beta(Beta, Mu, Sigma, X, Y)
	Mu = Mu + etaMu*gradient_mu(Beta, Mu, Sigma, X, Y)
	Sigma = Sigma + etaSigma*gradient_sigma(Beta, Mu, Sigma, X, Y)
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
X2 = matrix( runif( N, -1, 1),ncol=ncol(Y))

##Número de neuronas..
P=4
##Dimension

Y2= RBF(Y, P, sX )
plot(sX, Y)
points(sX, Y2, col='red')


#write.table(sX,file= "datax", sep="\t", col.names=FALSE, row.names=FALSE)
#write.table(Y,file= "datay", sep="\t", col.names=FALSE, row.names=FALSE)
#write.table(Y2,file= "datayp", sep="\t", col.names=FALSE, row.names=FALSE)
