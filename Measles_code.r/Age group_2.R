# Create a function to calculate the rate of change in each state variable
changed.dt <- function(X, Lambda, theta1, sigma1, beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2){
  
  S1 <- X[1] ; E1 <- X[2] ; I1 <- X[3] ; R1 <- X[4]
  S2 <- X[1] ; E2 <- X[2] ; I2 <- X[3] ; R2 <- X[4] ; V2 <- X[5]
  
  dS2dt <- (1 - theta2 *sigma2)*alpha1*S1 - beta2*c2*I2*S2 - d2*S2
  dE2dt <- beta2*c2*I2*S2 + alpha1*E1 - (d2 + epsilon2)*E2
  dI2dt <- epsilon2*E2 + alpha1*I1 - gamma2*I2 - (d2 + mu2)*I2
  dR2dt <- gamma2*I2 + theta2*sigma2*alpha1*S1 + alpha1*R1 - d2*R2
  
  
  
  return(c(dS2dt, dE2dt, dI2dt, dR2dt))
  
}

# A fuction that will update the system at each time step
updateSystm <- function(X, Lambda, theta1, sigma1,beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2, deltaT){
  X <- X + change.dt(X, Lambda, theta1, sigma1, beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2)*deltaT
  return(X)
}

# How many people are in S, E, I, R and V at time = 0
Systm <- c(79000, 20000, 500, 100, 300)
# Per-capita birth rate
Lambda <- 650
theta1 <- 0
sigma1 <- 0.01
beta1 <- 0.00000167989
c1 <- 13.3
alpha1 <- 0.00385
d1 <- 0.00029
epsilon1 <- 1/3
gamma1 <- 0.024368
mu1 <- 0.2
##################
theta2 <- 1
sigma2 <- 0.95
beta2 <- 0.000000515425
c2 <- 6.02
d2 <- 0.00029
epsilon2 <- 1/3
gamma2 <- 0.024368
mu2 <- 0
# time step
deltaT <- 1
# time period
period <- 365

# Simulations for age group 1
epidemic <- data.frame(time=0, S2=Systm[1], E2=Systm[2], I2=Systm[3], R2=Systm[4])

for(time in seq(from=deltaT, to=period, by=deltaT)){
  
  Systm <- updateSystm(Systm, Lambda, theta1, sigma1,beta1, c1, alpha1, d1, epsilon1, gamma1, mu1, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2, deltaT)
  epidemic <- rbind(epidemic, c(time, Systm))
  
}

library(tidyverse)

epi <- epidemic %>%
  pivot_longer(S2:R2, values_to="Counts", names_to="State")

ggplot(epi) + geom_line(aes(x=time, y=Counts, col=State))

#######################################################################################################################

start.date = "20210101"; end.date = "20220101"

Dates <- seq(ymd(start.date), ymd(end.date), by="days")

YearEndMonth = 6

Datetable <- data.frame(Dates)

view(Datetable)
view(epidemic)


colnames(EpidemicDate2)[1] <- "Year"
colnames(EpidemicDate2)[5] <- "Peak_Incidence"
colnames(EpidemicDate2)[7] <- "Vaccination_rate"


#Vaccination rates
#Let's use this function for several possible values ofVaccination rates
vacc_rate1 <- seq(from = 0.0, to = 0.95, le = 366)
view(vacc_rate1)

EpidemicDate2 <- cbind(Datetable, epidemic, vacc_rate1)
view(EpidemicDate2)

ggplot(EpidemicDate2)+geom_line(aes(x=Peak_Incidence, y=Vaccination_rate))
