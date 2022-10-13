install.packages("tidyverse")
install.packages("zoo")
install.packages("dplyr")
install.packages("lubridate")
install.packages("scales")

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(scales)

# Create a function to calculate the rate of change in each state variable
change.dt11 <- function(X, Lambda, theta1, sigma1, beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2){
  S1 <- X[1] ; E1 <- X[2] ; I1 <- X[3] ; R1 <- X[4]
  
  dS1dt <- Lambda*(1 - theta1 *sigma1) - beta1*c1*I1*S1 - (1 - theta2 *sigma2)*alpha1*S1 - d1*S1
  dE1dt <- beta1*c1*I1*S1 - (alpha1 + d1 + epsilon1)*E1
  dI1dt <- epsilon1*E1 - (alpha1 + gamma1)*I1 - (d1 + mu1)*I1
  dR1dt <- gamma1*I1 + (Lambda*theta1 *sigma1) - d1*R1 - alpha1*R1
  
  return(c(dS1dt, dE1dt, dI1dt, dR1dt))
  
}

# A fuction that will update the system at each time step
updateSystm11 <- function(X, Lambda, theta1, sigma1,beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2,deltaT){
  X <- X + change.dt11(X, Lambda, theta1, sigma1, beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2)*deltaT
  return(X)
}

# How many people are in S, E, I and R at time = 0
Systm <- c(200, 80, 30, 1)
# Per-capital birth rate
Lambda <- 650
theta1 <- 0.95
sigma1 <- 0.95
beta1 <- 0.00000167989
c1 <- 13.3
alpha1 <- 0.038
d1 <- 0.00029
epsilon1 <- 0.72
gamma1 <- 0.00275
mu1 <- 0.2
theta2 <- 0.764
sigma2 <- 0.95
# time step
deltaT <- 1
# time period
period <- 2554

# Simulations for age group 1
epidemic11 <- data.frame(time=0, S1=Systm[1], E1=Systm[2], I1=Systm[3], R1=Systm[4])

for(time in seq(from=deltaT, to=period, by=deltaT)){
  
  Systm <- updateSystm11(Systm, Lambda, theta1, sigma1,beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, 
                       deltaT)
  epidemic11 <- rbind(epidemic11, c(time, Systm))
  
}


epi11 <- epidemic11 %>%
  pivot_longer(S1:R1, values_to="Counts", names_to="State")

ggplot(epi) + geom_line(aes(x=time, y=Counts, col=State))
view(epi)
###################################################################################################################
view(epidemic)
epidemic11$Epidemic_size11 = rowSums(epidemic[,c("S1","E1")])

epidemic11$Total_population_size11 = rowSums(epidemic11[,c("S1","E1","I1", "R1")])


epidemic11$V1 =  epidemic11$R1 / epidemic11$Total_population_size11

par(mfrow=c(1,2))

plot(epidemic$V1, epidemic$Epidemic_size, type = "l", pch=19, lwd = 2, 
     main = "Vaccination proportion for age group 1", xlab = expression(paste("Vaccination proportion")),
     ylab = "Epidemic size", xlim = c(0.0, 1.0))

###################################################################################################################
epidemic$Epidemic_size1 = rowSums(epidemic[,c("I1","E1")])

plot(Epide, epidemic$Epidemic_size, type = "l", pch=19, lwd = 2, 
     main = "Vaccination proportion for age group 1", xlab = expression(paste("Vaccination proportion")),
     ylab = "Epidemic size")


###################################################################################################################

plot(rollmean(epidemic$V1, k = 7), rollmean(epidemic$I1, k = 7), type = "l", lwd = 2, main = "Vacc", 
     xlab = expression(paste("Vaccination proportion")),
     ylab = "Peak incidences", xlim = c(0.0, 1.0))

###################################################################################################################

start.date11 = "20170101"; end.date11 = "20231230"

Dates11 <- seq(ymd(start.date11), ymd(end.date11), by="days")

Datetable11 <- data.frame(Dates11)
View(Datetable11)
view(epidemic11)

EpidemicDate11 <- cbind(Datetable11, epidemic11)
colnames(EpidemicDate11)[1] <- "Year"
view(EpidemicDate11)







###################################################################################################################


plot(rollmean(EpidemicDate11$Year, k = 7), rollmean(EpidemicDate11$V1, k = 7), type = "l", pch = 19, 
     col = 'red', xlab = 'Year',  ylab = "Proportion of population", ylim = c(0.0, 1.0))

plot(EpidemicDate$Year, EpidemicDate$V1, type = "l", pch = 19, col = 'red', xlab = 'Year', 
     ylab = "Proportion of population", ylim = c(-1.0, 1.0))

legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("darkblue", "blue"), lty=1:2, cex=0.8)














start.date1 = "20170101"; end.date1 = "20251231"
da <- seq(ymd(start.date1), ymd(end.date1), by="days")
Da <- data.frame(da)
view(Da)
#####################################################################################################################

write.csv(EpidemicDate, file = "Epidemic1.csv", row.names = FALSE)

annual.inc <- data.frame(Dates=c("2017", "2018", "2019", "2020", "2021", "2022", "2023", "2024", "2025"),
                         Inc1=c(51, 41, 31, 28, 20, 16, 11, 9, 6),
                         Inc2=c(51, 38, 26, 19, 13, 8, 6, 4, 1))

view(annual.inc)
#annual.inc[order(as.Date(annual.inc$Dates, format="%Y")), ]
#####################################################################################################################

plot(annual.inc$Dates, annual.inc$Inc1, type="b", pch=19, col="red", xlab="Year", ylab="Annual Incidences")
# Add a line
lines(annual.inc$Dates, annual.inc$Inc2, pch=18, col="blue", type="b", lty=2)
# Add a legend
legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)



# Generate some data
x<-1:10; y1=x*x; y2=2*y1
plot(x, y1, type="b", pch=19, col="red", xlab="x", ylab="y")
# Add a line
lines(x, y2, pch=18, col="blue", type="b", lty=2)
# Add a legend
legend(1, 95, legend=c("Line 1", "Line 2"),
       col=c("red", "blue"), lty=1:2, cex=0.8)

