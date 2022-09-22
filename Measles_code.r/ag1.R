# Create a function to calculate the rate of change in each state variable
change.dt <- function(X, Lambda, theta1, sigma1, beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, 
                      beta2, c2, d2, epsilon2, gamma2, mu2){
  S1 <- X[1] ; E1 <- X[2] ; I1 <- X[3] ; R1 <- X[4]
  
  dS1dt <- Lambda*(1 - theta1 *sigma1) - beta1*c1*I1*S1 - (1 - theta2 *sigma2)*alpha1*S1 - d1*S1
  dE1dt <- beta1*c1*I1*S1 - (alpha1 + d1 + epsilon1)*E1
  dI1dt <- epsilon1*E1 - (alpha1 + gamma1)*I1 - (d1 + mu1)*I1
  dR1dt <- gamma1*I1 + (Lambda*theta1 *sigma1) - d1*R1
  
  return(c(dS1dt, dE1dt, dI1dt, dR1dt))
  
}

# A fuction that will update the system at each time step
updateSystm <- function(X, Lambda, theta1, sigma1,beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2,
                        beta2, c2, d2, epsilon2, gamma2, mu2, deltaT){
  X <- X + change.dt(X, Lambda, theta1, sigma1, beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, 
                     beta2, c2, d2, epsilon2, gamma2, mu2)*deltaT
  return(X)
}

# How many people are in S, E, I, R and V at time = 0
Systm <- c(59000, 20000, 500, 100, 300)
# Per-capita birth rate
Lambda <- 650
theta1 <- 0.717
sigma1 <- 0.93
beta1 <- 0.167989
c1 <- 13.3
alpha1 <- 0.2859
d1 <- 0.0.241
epsilon1 <- 0.72
gamma1 <- 0.024368
mu1 <- 0.2
##################
theta2 <- 0.764
sigma2 <- 0.95
beta2 <- 0.515425
c2 <- 6.02
d2 <- 0.0241
epsilon2 <- 0.72
gamma2 <- 0.024368
mu2 <- 0
# time step
deltaT <- 1
# time period
period <- 365

# Simulations for age group 1
epidemic <- data.frame(time=0, S1=Systm[1], E1=Systm[2], I1=Systm[3], R1=Systm[4])

for(time in seq(from=deltaT, to=period, by=deltaT)){
  
  Systm <- updateSystm(Systm, Lambda, theta1, sigma1,beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, 
                       beta2, c2, d2, epsilon2, gamma2, mu2, deltaT)
  epidemic <- rbind(epidemic, c(time, Systm))
  
}

install.packages("tidyverse")
library(tidyverse)

epi <- epidemic %>%
  pivot_longer(S1:R1, values_to="Counts", names_to="State")

view(epi)

ggplot(epi) + geom_line(aes(x=time, y=Counts, col=State))

###################################################################################################################
install.packages("lubridate")
library(lubridate)

start.date = "20210101"; end.date = "20220101"

Dates <- seq(ymd(start.date), ymd(end.date), by="days")

Datetable1 <- data.frame(Dates)

#Vaccination rates

V1 <- seq(from = 0.0, to = 0.85, le = 366)

EpidemicDate1 <- cbind(Datetable1, epidemic, V1)
colnames(EpidemicDate1)[1] <- "Year"
view(EpidemicDate1)


###################################################################################################################
plot(EpidemicDate1$I1, EpidemicDate1$V1, type = "l", lwd = 2, main = "Vaccination rate vs Peak Incidence",
     xlab = expression(paste("Peak Incidence")),
     ylab = "Vaccination rate")

###################################################################################################################

EpidemicDate1$N = rowSums(EpidemicDate1[,c(3,4,5,6)])

plot(EpidemicDate1$N, EpidemicDate1$V1, type = "l", lwd = 2,  main = "Vaccination rate vs Total epidemic size",
     xlab = expression(paste("Total Epidemic size")),
     ylab = "Vaccination rate")
####################################################################################################################

plot(EpidemicDate1$Year, EpidemicDate1$I1, type = "l", lwd = 2,
     xlab = expression(paste("Year")),
     ylab = "Incidence")
abline(v = EpidemicDate1[152, "Year"], lty = 3, col = "purple", lwd = 4)

rect(xleft = EpidemicDate1[152, "Year"], xright = EpidemicDate1[366, "Year"], ybottom = EpidemicDate1[1, "I1"], 
     ytop =  EpidemicDate1[9, "I1"], 
     border = NA, col = adjustcolor("blue", alpha = 0.3))

text(EpidemicDate1[250, "Year"], EpidemicDate1[12, "I1"], "Vaccine Era", col = "blue", cex = 2)

####################################################################################################################

####################################################################################################################

