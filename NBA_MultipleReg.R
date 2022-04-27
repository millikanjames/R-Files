#James Millikan, SJ, Verbum Dei High School, Los Angeles, Spring 2022
#The Linear Algebra of Multiple Regression
#AMDG

#This lesson extends OLS regression to two independent variables ——heights and weights—— 
#and uses these factors to predict shoe sizes.

#Start by importing the data set BBall_Multiple_Reg.csv from Github.

data1<-read.csv("https://raw.githubusercontent.com/millikanjames/R-Files/main/BBall_Multiple_Reg.csv", header=TRUE)

#Taller Basketball players tend to have bigger shoe sizes, and many big guys' shoe sizes have increased as they put on weight over their careers.
#This gives us reason to believe we can predict NBA shoe sizes from the independent variables of heights and weights.
#My students already built linear models using their own shoe sizes and heights. This lesson extends those insights 
#to multivariate regression analysis.

#R can perform this analysis for us:

z<-data1$Shoe_Size
x<-data1$Height
y<-data1$Weight

model<-lm(z~ x + y)
summary(model)

#Examining the printout, we find that a one inch increase in height is associated with a 0.19 increase in shoe size. Shoe sizes increase by 1 size for every additional ~5 inches in height, on average.
#By the same token, a one pound increase in weight is associated with a .04611 inch increase in shoe size. In other words, a 1 size increase in shoe size is associated with a ~21.7 pound increase in weight, on average.
#Also note that the R^2 value is 0.8368359 and the adjusted R^2 value is 0.8005772
#Let's calculate all these values by hand.

#(As an aside, there is much more to say about the significance levels of the coefficients, as well 
#as important checks for multicollinearity and  heteroskedasticity, that need to be done before making predictions with our model).


#-------Part 1---------

#Determining the Z intercept and the coefficients for the independent variables. 

#Make a column vector of the shoe sizes

Z<-matrix(c(z), ncol=1)

#Now make a 12x3 matrix with 1s in the first columns, heights in the second column, and weights in the third column.

X<-matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, x, y), ncol=3)

b<-(solve(t(X)%*%X))%*%t(X)%*%Z #Just like in bivariate regression, the relevant values are given
#by the inverse of the transpose of X multiplied by X times the transpose of X multiplied by Z.
#Algebraically: b=inv[(transpose(X)]*[(transpose(X))*Z], where "*" indicates matrix multiplication

print1<-"The z-intercept, x coefficient, and y coefficients are..."
print1
b

#The console printout confirms that the model that best approximates the data
#is Z = 0.19167x+ 0.0461y -9.732. Substituting words for the variables gives us:
# PredShoeSize = 0.19167*(height) + 0.0461*(weight)- 9.732.

#-------Part 2---------

#Calculating the R^2 and the adjusted R^2 values.

Z_Bar<-mean(Z)
Z_Bar_Vec<-matrix(rep(c(Z_Bar), times=length(Z))) #Column vector filled with mean heights

Z_Dev<-Z - Z_Bar_Vec # Column vector of deviations from the mean height.
SqDev<-Z_Dev^2

SST<-sum(SqDev) #Total sum of squared deviations from the mean height.

#Next, we determine the predicted shoe sizes from the heights and weights of the 12 NBA players

ShoePred<-(X%*%b) # The predicted shoe sizes for NBA players with the heights and weights of the 12 players from our data set.

E<-(Z-(ShoePred)) #The residuals of the 12 NBA players

SSR<-sum(E^2) #Sum of squared residuals

R_Sq<-1-(SSR/SST) #Multiple R-Squared value. This is the same in our "by hand" calculations as with the model lm(z~ x + y)
Print2<-"The R-Squared Value is..."
Print2
R_Sq

#Finally, to calculate the adjusted R-Square value:

n<-length(Z) #Number of observations (n=12; one ordered triple for each of the twelve NBA athletes)
k<-ncol(X)-1 #Number of independent variables (k=2)

AdjR2<-1-(((1-R_Sq)*(n-1))/(n-k-1)) #Adjusted R-square formula.

Print3<-"The adjusted R-Square value is..."
Print3
AdjR2

#That's it. Shoot me an email at jmillikan@jesuits.org if something isn't clear.
#AMDG
