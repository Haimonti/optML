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

secGrad <-function(x)
{
	2.4
}

#Closed form solution
lines(c(2,2), c(3,8), col="red",lty=2)
text(2.1, 7, "Closed Form Solution", col="red", pos=4)

#Implementation of gradient descent
#Initialize the first guess
x<-0.1
#store initial x values
xtrace<-x
#store the y-values
ftrace<-f(x)
for (step in 1: 100)
{	
	#gradient descent step
	x <- x - grad(x)/secGrad(x)
	xtrace <- c(xtrace,x)
	ftrace <- c(ftrace, f(x))
}
lines(xtrace, ftrace, type = "b", col="blue")
text(0.5, 6, "Newton's Method", col = "blue", pos=4)
#print final value of x
print(x)
