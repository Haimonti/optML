# Demonstration of the Conjugate Gradient Method
require(numDeriv)
require(Rcgmin)

#create some values
xs <- seq(0,4,len=20)

#define the function we want to optimize
f<- function(x)
{
	1.2 * (x-2)^2 + 3.2
}

#Plot the function
plot(xs, f(xs), type="l", xlab="x",ylab=expression(1.2 * (x-2)^2 + 3.2))

#Calculate the gradient of the function
grad <- function(x)
{
	1.2*2*(x-2)
}
 
#Closed form solution
lines(c(2,2), c(3,8), col="red",lty=2)
text(2.1, 7, "Closed Form Solution", col="red", pos=4)

#Perform Conjugate Gradient
cgSol <-Rcgmin(fn=f,gr=grad, par=c(0))
print(cgSol)