#James Millikan, SJ, Verbum Dei High School, Los Angeles, Spring 2022
#AMDG

#To introduce a unit on linear regression, 15 students recorded their heights (in inches) and shoe sizes.
#These are recorded in the following matrices.

Y<-matrix(c(71, 70, 69, 70, 67, 70, 67, 64, 64, 67, 68, 69, 68, 70, 74), ncol=1, nrow=15)
X<-matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 10.5, 10, 10, 13, 10, 11, 9.5, 8, 8, 12, 13, 8.5, 9, 11, 12.5), ncol=2, nrow=15)

#To calculate the y-intercept and slope of the least squares regression line:
XtX<-(t(X)%*%X)
XtY<-(t(X)%*%Y)
A<-solve(XtX)%*%XtY #a11=y-int, a21=slope
x<-"For the least-square regression line, the y-intercept is a11 and the slope is a12"
x
A

#To obtain a matrix of the residuals

E<-(Y-(X%*%A))

SSR<-sum(E^2) #Sum of squared residuals


#Mean of the Y values

Ymean<-mean(Y)

MeanVec<-matrix(rep(c(Ymean), times=length(E)))#Vector filled with mean height

Dev<-Y-(MeanVec) #Deviations from the mean
SqDev<-Dev^2 

SST<-sum(SqDev) #Total sum of squared deviations

Rsqr<-1-(SSR/SST) #R-square value

y<-"The R^2 value is..."
y
Rsqr #Displays R-square value in console.

#An R2 of 0.354996 means that 35.5% of the variation in heights can be accounted for by shoe sizes.
#Shoe sizes aren't a great predictor of heights. (The correlation coefficient is sqrt(R2)=0.5958)

#We can plot the data points and graph our least squared lines to check our work

Xvals<-c(10.5, 10, 10, 13, 10, 11, 9.5, 8, 8, 12, 13, 8.5, 9, 11, 12.5)
Yvals<-c(71, 70, 69, 70, 67, 70, 67, 64, 64, 67, 68, 69, 68, 70, 74)

b0<-A[1]
b1<-A[2]

h <- function(s) b0 + b1*s # Our model for heights as a function of shoe size: h(s)= b0 + b1*s

plot(Xvals,Yvals, main="Shoe sizes and height", xlab="Shoe Size", ylab="Height (in)")
plot(h, xlim=c(min(Xvals), c(max(Xvals))), add=T)

#The plot looks pretty good. Nice to have so sense of what's going on under the hood for R's lm(y~x) command.

#Feel free to email me at jmillikan@jesuits.org if you have any questions.
#AMDG