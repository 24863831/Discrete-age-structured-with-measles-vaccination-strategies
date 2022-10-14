install.packages("tidyverse")
install.packages("zoo")
install.packages("dplyr")
install.packages("lubridate")
install.packages("scales")
install.packages("r2symbols")

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(scales)
library(r2symbols)

# Create a function to calculate the rate of change in each state variable
change.dt <- function(X, Lambda, theta1, sigma1, beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2){
  S1 <- X[1] ; E1 <- X[2] ; I1 <- X[3] ; R1 <- X[4]
  
  dS1dt <- Lambda*(1 - theta1 *sigma1) - beta1*c1*I1*S1 - (1 - theta2 *sigma2)*alpha1*S1 - d1*S1
  dE1dt <- beta1*c1*I1*S1 - (alpha1 + d1 + epsilon1)*E1
  dI1dt <- epsilon1*E1 - (alpha1 + gamma1)*I1 - (d1 + mu1)*I1
  dR1dt <- gamma1*I1 + (Lambda*theta1 *sigma1) - d1*R1 - alpha1*R1
  
  return(c(dS1dt, dE1dt, dI1dt, dR1dt))
  
}

# A fuction that will update the system at each time step
updateSystm <- function(X, Lambda, theta1, sigma1,beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2,deltaT){
  X <- X + change.dt(X, Lambda, theta1, sigma1, beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2)*deltaT
  return(X)
}

# How many people are in S, E, I and R at time = 0
Systm <- c(200, 80, 30, 1)
# Per-capital birth rate
Lambda <- 650
theta1 <- 0.717

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
epidemic <- data.frame(time=0, S1=Systm[1], E1=Systm[2], I1=Systm[3], R1=Systm[4])

for(time in seq(from=deltaT, to=period, by=deltaT)){
  
  Systm <- updateSystm(Systm, Lambda, theta1, sigma1,beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, 
                       deltaT)
  epidemic <- rbind(epidemic, c(time, Systm))
  
}


epi <- epidemic %>%
  pivot_longer(S1:R1, values_to="Counts", names_to="State")

ggplot(epi) + geom_line(aes(x=time, y=Counts, col=State))
view(epi)
###################################################################################################################
view(epidemic)
epidemic$Epidemic_size = rowSums(epidemic[,c("S1","E1")])

epidemic$Total_population_size = rowSums(epidemic[,c("S1","E1","I1", "R1")])


epidemic$V1 =  epidemic$R1 / epidemic$Total_population_size

epidemic$Incidences =  epidemic$I1 / epidemic$Total_population_size


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

start.date = "20170101"; end.date = "20231230"

Dates <- seq(ymd(start.date), ymd(end.date), by="days")

Datetable <- data.frame(Dates)

EpidemicDate <- cbind(Datetable, epidemic)
colnames(EpidemicDate)[1] <- "Year"


###################################################################################################################
par(mfrow=c(1,2))

plot(rollmean(EpidemicDate$Year, k = 7), rollmean(EpidemicDate$V1, k = 7), type = "l", pch = 19, 
     col = 'darkblue', lty = 1, lwd = 3, xlab = 'Year',  ylab = "Proportion of population",
     main = "Infants Aged 6 - 12 months old", ylim = c(0.0, 1.0))

lines(rollmean(EpidemicDate11$Year, k = 7), rollmean(EpidemicDate11$V1, k = 7), type = "l", pch = 18,
      col = 'darkgreen', lty = 1, lwd = 3)

legend(x = "topleft", lty = c(1,1), text.font = 2, lwd = 3,
       col= c("darkblue","darkgreen"),text.col = "black", 
       legend=c("Vaccine coverange:0,717 ", "Vaccine coverage:0,95"))



#####################################################################################################################

plot(rollmean(EpidemicDate$Year, k = 365), rollmean(EpidemicDate$Incidences, k = 365), type = "l", pch = 19, 
     col = 'darkblue', lty = 1, lwd = 3, xlab = "Year",  ylab = "Annual Incidences",
     main = "Proportion of age group Incedences (6 - 12 months old)")

lines(rollmean(EpidemicDate11$Year, k = 365), rollmean(EpidemicDate11$Incidences1, k = 365), type = "l", pch = 18,
      col = 'darkgreen', lty = 1, lwd = 3)

legend(x = "topright", lty = c(1,1), text.font = 2, lwd = 3,
       col= c("darkblue","darkgreen"),text.col = "black", 
       legend=c("Vaccine coverange:0,717 ", "Vaccine coverage:0,95"))
#####################################################################################################################




