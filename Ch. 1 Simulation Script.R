#James Millikan, SJ - Verbum Dei High School - 4 September 2021 - AMDG

#Categorical Data Simulator 
#This script creates a dot plot of simulated outcomes of categorical data.
#This is intended for teachers using Ch. 1 of Tabor and Franklin's Statistical Reasoning in Sports

#DESCRIPTION OF THE INITIAL CONDITIONS
#During the regular season, LeBron made 113/359 or 31.5% of his attempted 3-point shots.
#We will use that value as his true ABILITY.

TrueAbility <-0.315 #This value can be changed to any ABILITY

#In the Playoffs, LeBron made 18 of 70 (25.7%) of his attempted 3-point shots. We will use 70 as the
#number of attempts for one simulated playoff season. This is equivalent to spinning
#a spinner with a 31.5% "made shot" region 70 times.

NumOfAttempts<-70 #This, too, can be changed to any number of attempts.

#Finally, we select how many times we would like to simulate the outcome.
#We will run 100 simulations of LeBron James's 3-point shooting PERFORMANCE in the 2008 playoffs. 

NumOfSims<-100 #Also changable.


#With these parameters set, we now proceed to the simulation.

#First, we generate a 1 x (length NumOfSims) matrix. At this point, each element
#of the matrix is 0; i.e. a11 = 0, a12 = 0, ..., a1n=0)
EmptyList<-vector("numeric", NumOfSims)

#Next, we use a FOR loop to simulate the performances of the athlete. The loop replaces
#each element of the EmptyList matrix with a simulated performance. We will simulate
#LeBron James's 3-point shooting performance 100 times.

for(n in seq_along(EmptyList)) #Iterates the simulation for each element of the empty list.
  {(RandList<-sample(1:1000, NumOfAttempts, replace=FALSE)) #Generates random integers between 1 and 1000.
  #This is done NumOfAttempts times, and the integers are saved in the matrix RandList.

  #Next, the program searches the list of random numbers and replaces random numbers corresponding
  #to a made shot (i.e. RandInt ≤ True Ability*1000) with the number "1". In the LeBron example, all
  #random integers less than or equal to 315 are changed to 1.
  AssignSuccess<-replace(RandList, RandList<=TrueAbility*1000, 1)
  
  #We repeat the process by assigning 0s to the random integers that represent a miss.
AssignFailure<-replace(AssignSuccess, AssignSuccess>TrueAbility*1000, 0)

# We rename this matrix Outcomes.
Outcomes<-AssignFailure

#By adding the contents of the Outcomes vector, we obtain the simulated total number of successes.
#In our example, this is the simulated number of made 3-point shots we would expect to see in LeBron's
#2008 playoffs assuming his ability stayed the same.
TotalSuccesses<-sum(Outcomes)

#We now convert the total number of successes to a cumulative relative frequency, i.e., the simulated
#3-point shooting percentage in the playoffs.
TotalAttempts<-length(Outcomes) #The denominator of our fraction.
SimPerformance<-(TotalSuccesses/TotalAttempts) #The simulated performance in the playoffs

#We now iterate this process n (i.e., the NumOfSims) times
x<-SimPerformance
EmptyList[n]<-x} #Iterates through the list and assigns simulated performances.
Results<-EmptyList #The previously empty list now contains the results of our simulation. Each
#element in list is a unique simulated playoff performance.

#You can check that the results are correct by typing "Results" in the command window below.

#To display the results of the simulation as a dot plot (like the one presented on p. 13 of the 
#first edition of the Statistical Reasoning of Sports), we use the built-in stripchart plot:
stripchart(Results, method="stack")

#To make the dot chart more aesthetically pleasing and add a title and label:
stripchart(Results, method = "stack", offset = .5, at = 0, pch = 19,
           col = "steelblue", main = "LeBron Simulated 2008 Playoff Performances", xlab = "Simulated Percentages of Made 3-Point Shots")

#This dot plot gives us a way to see how likely it is that his 25.7% shooting PERFORMANCE in the playoffs was due
#to a decline in his ABILITY and low likely the decline was due to RANDOM CHANCE. Just count the number of dots plotted at or below
#his playoff performance and divide by the number of simulated performances. This is the probability
#that his playoff performance was due to random chance rather than a decline in his true ability.

#That's it. If you have questions about this script, feel free to send me an email at jmillikan@jesuits.org
#AMDG
  