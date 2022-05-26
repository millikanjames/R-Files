#R script to accompany the "Sines of the Times" ecological justice project.
#James Millikan, SJ, Verbum Dei Jesuit High School, Los Angeles, 2022. AMDG

#Average monthly temperatures in Bethel, Alaska 1960s & 2010s

Temp1960s<-c(7.4, 6.6, 12.0, 22.5, 39, 50.7, 54.8, 51.5, 45.4, 28.5, 15.8, 5.7)
Temp2010s<-c(10.0, 15.2, 14.3, 29.6, 43.6, 53.9, 56.1, 54.2, 46.6, 35.0, 19.5, 12.1)

par(mfrow=c(1,2)) #Plot the temp graphs side by side. 
#Note: Make sure to make the plot window wide enough to see both plots.

t1<-c(1:12) #Assigns a vector of length 12 for the twelve months of the year.
t2<-c(1:12)

y1<-c(Temp1960s)
y2<-c(Temp2010s)

#Now we find the coefficients for the model y=a*cos(bx+c)+d

a1<-.5*(max(Temp1960s)-min(Temp1960s))#amplitude 1
a2<-.5*(max(Temp2010s)-min(Temp2010s))#amplitude 2

b1<-((2*pi)/length(t1))# recall that period = 2π/|b|
b2<-((2*pi)/length(t2))

PhaseShift1<-which(y1==max(y1)) #The graph cos(x) must be shifted 7 units right to align with the data 
PhaseShift2<-which(y2==max(y2))

c1<-(-1*PhaseShift1*abs(b1)) #Recall that phase shift = -c/|b|
c2<-(-1*PhaseShift2*abs(b2))

d1<-(1/2)*(max(Temp1960s)+min(Temp1960s)) #vertically shifts the cosine function
d2<-(1/2)*(max(Temp2010s)+min(Temp2010s))

f1 <- function(x) a1*cos(b1*x+c1)+d1 # Our model for the 1960s temperatures

f2 <-function(x) a2*cos(b2*x+c2)+d2 # Model for the 2010s temperature

#Plots the functions over the monthly temperature data.

plot(t1, y1, main="Monthly Avg Temp 1960s", xlab="Month (1=Jan, 2=Feb, ...)", ylab="Temp (F)")
plot(f1, xlim=c(0,12.5), add=T, main="Monthly Avg Temp 1960s", xlab="Month (1=Jan, 2=Feb, ...)", ylab="Temp (F)")
plot(t2, y2, main="Monthly Avg Temp 2010s", xlab="Month (1=Jan, 2=Feb, ...)", ylab="Temp (F)")
plot(f2, xlim=c(1,12), add=T, main="Monthly Avg Temp 2010s", xlab="Month (1=Jan, 2=Feb, ...)", ylab="Temp (F)")

#Questions? Shoot me an email at jmillikan@jesuits.org
#AMDG
