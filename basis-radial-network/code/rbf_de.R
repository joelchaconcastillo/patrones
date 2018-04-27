library("DEoptimR")
#set.seed(30)
PI = 3.1416
Obj <- function(X)
{
  return (X*X)   
 return (1+cos(X*4*PI))#(cos(X*X)*4*3.1416)
}
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
	     total = total + (Y[d, ]-fd(X[d,], Beta, Mu, Sigma))*Phi(X[d, ], Mu[j,], Sigma[,j])*( X[d,]-Mu[j])*(1.0/(Sigma[,j])) 
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
	     total = total + (Y[d, ]-fd(X[d,], Beta, Mu, Sigma))*Phi(X[d, ], Mu[j,], Sigma[,j])*(norm(matrix(X[d,])- matrix(Mu[j,]))^2)/(Sigma[,j]^2)
	   }
	}
   	return (total)
}
##Función objetivo...
#E <- function( Beta, Mu, Sigma, X, Y)
#{
#	total = 0
#	for(d in 1:nrow(X))
#	{
#	   total = total+ (Y[d, ] - fd(X[d, ], Beta, Mu, Sigma))^2
#	}
#   return (total)
#}

RBF <- function(Y, P, X, Xp, min, max)
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
    Sigma[1,i] = 0.8#*runif(1,0.0,1.0)
   
   	for(j in 1:D) {
   		Mu[i,j] = runif(1,0,1)	
   	}
   }
	## [ P, D, Beta, Sigma, Mu, X, Y] --> 1 + 1 + P + P + PxD + DxN + N
	data = c(Beta, Sigma, Mu)
	sizeind =2*P+(D*P)
	min_region = rep(min,sizeind )
	max_region = rep(max, sizeind)
	max_region[(P+1):(2*P)] = 1 ##Variance bound
	max_region[1:P] = 2 ##Beta bounds
	
	best = JDEoptim( min_region, max_region, E <- function(Ind)
{
	myBeta = matrix(Ind[1:P], ncol=P)
	mySigma = matrix(Ind[ (P+1):(2*P)], ncol=P)
	myMu= matrix(Ind[ (2*P+1): (2*P+(D*P))], ncol=D)
	total = 0
	for(d in 1:nrow(X))
	{
	   total = total+ (Y[d, ] - fd(X[d, ], myBeta, myMu, mySigma))^2
	}
   return (total)
}  , tol = 1e-4, trace = FALSE, triter = 50, NP = 10, maxiter=100)
	best = best$par


#	print(best)
	Beta = matrix(best[1:P], ncol=P)
	Sigma = matrix(best[ (P+1):(2*P)], ncol=P)
	Mu= matrix(best[ (2*P+1): (2*P+(D*P))], ncol=D)

#   Beta = Beta/norm(Beta)
#   nite = 100
#   eta = 0.001
#   for( it in 1:nite)
#   {
#	Beta = Beta + eta*gradient_beta(Beta, Mu, Sigma, X, Y)
#	Mu = Mu + eta*gradient_mu(Beta, Mu, Sigma, X, Y)
#	Sigma = Sigma + eta*gradient_sigma(Beta, Mu, Sigma, X, Y)
##	pause(0.5)
##	print(Beta)
##	Sys.sleep(1)
#   }
   Yp = Xp;
   for( i in 1:nrow(Yp))
   {
	Yp[i,1]=0
	for( j in 1:ncol(Beta))
	{
	   Yp[i,] = Yp[i,]+ Beta[1,j]*Phi(Xp[i, ], Mu[j,], Sigma[1,j])
	}
   } 	
   return (Yp)
}
##
min = 0.0
max = 1
Sizetrain=100
##Generar datos de entrenamiento
sX = matrix( runif(Sizetrain, min, max), ncol=1)#nrow=Sizetrain)
#Y = matrix(sX*sX  + rnorm(100, 0, 0.1), nrow=100)
Y = matrix( Obj(sX) , nrow=Sizetrain, ncol=1)

##Datos de prueba...
X2 = matrix( runif( 1000, min, max),ncol=ncol(sX))

##Número de neuronas..
P=4
##Dimension

Y2= RBF(Y, P, sX, X2, min, max )
plot(sX, Y)
points(X2, Y2, col='red')
print(sum( (Obj(X2)-Y2)*(Obj(X2)-Y2)  ))
#write.table(sX,file= "datax", sep="\t", col.names=FALSE, row.names=FALSE)
#write.table(Y,file= "datay", sep="\t", col.names=FALSE, row.names=FALSE)
#write.table(X2,file= "dataxp", sep="\t", col.names=FALSE, row.names=FALSE)
#write.table(Y2,file= "datayp", sep="\t", col.names=FALSE, row.names=FALSE)
