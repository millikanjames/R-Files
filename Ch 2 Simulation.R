#James Millikan, SJ - Verbum Dei High School - 18 September 2021 - AMDG

#Difference in Proportions Simulator 
#This script tests the difference in two proportions and assigns an approximate p-value
#for a given test statistics. Results of the simulation are recorded in a dot plot.
#This script was inspired by Ch. 2 of Tabor and Franklin's Statistical Reasoning in Sports

#=============/=============

#DESCRIPTION OF THE INITIAL CONDITIONS
#Test statistic. To show how the program works, we will use the Arizona Cardinals home and
#road record during the 2004-2008 seasons. During that time, they won 23/40 or 57.5% of their home games
#and won 10/40 or 25% of their road games. That gives us a test statistic of 32.5% (57.5% - 25%)

TestStatistic <-0.325 #This value can be changed to any test statistic you wish to test.

OverallRecord<-(.4125) #This is the proportion assumed by the null hypothesis,
# that is, the 2004-2008 Cardinals had the same ABILITY to win at home and
#on the road. The Cardinals won 33 of 80 overall, giving us the null hypothesis
#proportion of 33/80 = .4125.

Category1<-40 #Total number of results in the first category (e.g., home games, 
#clay surface, etc.). In the Cardinals example, we put 40 to represent the
#number of home games played.

Category2<-40 #Total number of results for the second category (e.g., away games
#other surfaces, etc.)

#We will run 1000 simulations. (e.g., 1000 simulation for the 40)
#home and 40 away games played between 2004 and 2008.

NumOfSims<-1000 #(See note on line 97 regarding the number of simulations and dot plot graphics.
#With these parameters set, we now proceed with the simulation.

#=============/=============

#SIMULATION

#First, we generate a 1 x (length NumOfSims) matrix. At this point, each element
#of the matrix is 0; i.e. a11 = 0, a12 = 0, ..., a1n=0)
EmptyList<-vector("numeric", NumOfSims)

#Next, we use a FOR loop to simulate the performances of the teams. The loop replaces
#each element of  EmptyList  with a simulated difference between the category 1 and category 2 performances.
#In the Cardinals example, 40 home and 40 away games are simulated, and the simulated difference
#in winning percentage is calculated. This process is repeated NumOfSims times, and NumOfSims dots (e.g., 1000 dots)
#are recorded on the dot plot.

for(n in seq_along(EmptyList)) #Iterates the simulation for each element of the empty list.
{(RandList1<-sample(1:10000, Category1, replace=FALSE)) #Generates random integers between 1 and 10000.
  #These random integers are saved in the RandList matrix.
  (RandList2<-sample(1:10000, Category2, replace=FALSE))
  #Next, the program searches the list of random numbers and replaces random numbers corresponding
  #to a simulated Win (i.e. RandInt ≤ OverallRecord*10000) with the number "1". In the 2004-2008 Cardinals example, all
  #random integers less than or equal to (33/80)*10000 or 4125 are changed to 1 to represent a win.
  AssignSuccess1<-replace(RandList1, RandList1<=OverallRecord*10000, 1)
  AssignSuccess2<-replace(RandList2, RandList2<=OverallRecord*10000, 1)
  
  #We repeat the process by assigning 0s to simulated games that represent a loss
  AssignFailure1<-replace(AssignSuccess1, AssignSuccess1>OverallRecord*10000, 0)
  AssignFailure2<-replace(AssignSuccess2, AssignSuccess2>OverallRecord*10000, 0)
  
  # We rename these matrices Outcomes1 and Outcomes 2.
  Outcomes1<-AssignFailure1
  Outcomes2<-AssignFailure2
  
  #By adding the elements of the Outcomes matrices, we obtain the simulated total number successes in each category.
  #In our example, this is the simulated number of home games and away games we would expect to see won from
  #2005-2008 assuming that the null hypothesis is true, i.e., the Cardinals had the same ABILITY to
  #at home and on the road.
  
  TotalSuccesses1<-sum(Outcomes1)
  TotalSuccesses2<-sum(Outcomes2)
  
  #We now convert the total number of wins in each category to proportions. In our example, this 
  #is the simulated proportion of home games won assuming the null hypothesis is true.
  TotalAttempts1<-length(Outcomes1)#The denominator of our fraction.
  TotalAttempts2<-length(Outcomes2)
  SimPerformance1<-(TotalSuccesses1/TotalAttempts1) #The simulated performance in category 1 (e.g, home record)
  SimPerformance2<-(TotalSuccesses2/TotalAttempts2) #The simulated performance in category 2 (e.g. road record)
  Difference<-(SimPerformance1-SimPerformance2) #The simulated difference in performances between the two categories.
  
  #We now iterate this process n (i.e., the NumOfSims) times
  x<-Difference
  EmptyList[n]<-x} #Iterates through the list and assigns simulated differences in proportions.
SimDiffs<-EmptyList #The previously empty list now contains the results of our simulation. Each
#element in list is a unique simulated difference between category 1 and 2 
#In our example, SimDiffs contains 1000 simulated differences between 40 home and 40 away performances


#=============/=============


#To display the results of the simulation as a dot plot (like the one presented on p. 51 of the 
#first edition of the Statistical Reasoning of Sports), we use use R's built-in dot plot:

#Note: you can adjust the offset to fit more or fewer dots on the chart.
stripchart(SimDiffs, method = "stack", offset = .05, at = 0, pch = 19,
           col = "steelblue", main = "A Greater Ability to Win at Home?", xlab = "Simulated Difference in Proportions of Wins")

abline(v = TestStatistic, col = 1, pch = 7, cex = 2, lwd = 2, lty = 2) #Marks the Test Statistic on the plot

pvalue<-(sum(SimDiffs >= TestStatistic, na.rm=TRUE))/(length(SimDiffs)) #Approximate p-value
sprintf("The approximate p value is: %f", pvalue)

#That's it. If you have questions about this script, feel free to send me an 
#email at jmillikan@jesuits.org
#AMDG





