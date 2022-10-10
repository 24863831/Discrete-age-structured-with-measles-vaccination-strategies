install.packages("tidyverse")
install.packages("zoo")
install.packages("dplyr")
install.packages("lubridate")

library(tidyverse)

library(lubridate)
library(zoo)
library(dplyr)


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

# How many people are in S, E, I, R and V at time = 0
Systm <- c(59000, 20000, 500, 100, 300)
# Per-capita birth rate
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
##################
theta2 <- 0.764
sigma2 <- 0.95
#beta2 <- 0.515425
#c2 <- 6.02
#d2 <- 0.0241
#epsilon2 <- 0.72
#gamma2 <- 0.024368
#mu2 <- 0
# time step
deltaT <- 1
# time period
period <- 3652

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

plot(epidemic$V1, epidemic$Epidemic_size, type = "l", lwd = 2, main = "Vaccination proportion for age group 1",
     xlab = expression(paste("Vaccination proportion")),
     ylab = "Epidemic size", xlim = c(0.0, 1.0))

###################################################################################################################

plot(rollmean(epidemic$V1, k = 7), rollmean(epidemic$I1, k = 7), type = "l", lwd = 2, main = "Vacc", 
     xlab = expression(paste("Vaccination proportion")),
     ylab = "Peak incidences", xlim = c(0.0, 1.0))

###################################################################################################################

start.date = "20160101"; end.date = "20251231"

Dates <- seq(ymd(start.date), ymd(end.date), by="days")

Datetable <- data.frame(Dates)
View(Datetable)

EpidemicDate <- cbind(Datetable, epidemic)
colnames(EpidemicDate)[1] <- "Year"
view(EpidemicDate)
#####################################################################################################################

write.csv(EpidemicDate, file = "Epidemic1.csv", row.names = FALSE)

annual.inc <- data.frame(Dates=c("2016", "2017", "2018", "2019", "2020","2021", "2022", "2023", "2024", "2025"),
                  Incidences=c(397841.2737, 140755.6755, 136429.7851, 136421,6261, 136806.8353, 
                               136433.2252, 136433.1955, 136433.1947, 136806.9843, 136433.1947))
# display
annual.inc[order(as.Date(annual.inc$Dates, format="%Y")), ]
#####################################################################################################################



plot(EpidemicDate$Year, EpidemicDate$I1, type = "l", lwd = 2, main = "s = 2",
     xlab = expression(paste("Year")),
     ylab = "Annual Incidences", xlim = c(0.0, 1.0))
