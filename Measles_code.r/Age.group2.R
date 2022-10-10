# Create a function to calculate the rate of change in each state variable
changed.dt2 <- function(X, alpha1, alpha2, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2){
  
  S1 <- X[1] ; E1 <- X[2] ; I1 <- X[3] ; R1 <- X[4]
  S2 <- X[1] ; E2 <- X[2] ; I2 <- X[3] ; R2 <- X[4]
  
  dS2dt <- (1 - theta2 *sigma2)*alpha1*S1 - beta2*c2*I2*S2 - (d2 + alpha2)*S2
  dE2dt <- beta2*c2*I2*S2 + alpha1*E1 - (d2 + epsilon2 + alpha2)*E2
  dI2dt <- epsilon2*E2 + alpha1*I1 - gamma2*I2 - (d2 + mu2)*I2 - alpha2
  dR2dt <- gamma2*I2 + theta2*sigma2*alpha1 + alpha1*R1 - (d2*R2 + alpha2)*R2
  
  
  
  return(c(dS2dt, dE2dt, dI2dt, dR2dt))
  
}

# A fuction that will update the system at each time step
updateSystm2 <- function(X, alpha1, alpha2, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2, deltaT){
  X <- X + changed.dt2(X, alpha1, alpha2, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2)*deltaT
  return(X)
}

# How many people are in S, E, I, R and V at time = 0
Systm2 <- c(79000, 20000, 500, 100, 300)
# Per-capita birth rate
alpha1 <- 0.0385
alpha2 <- 0.00385
theta2 <- 0.764
sigma2 <- 0.95
beta2 <- 0.000000515425
c2 <- 6.02
d2 <- 0.00029
epsilon2 <- 0.72
gamma2 <- 0.024368
mu2 <- 0.01
# time step
deltaT <- 1
# time period
period <- 365


# Simulations for age group 2
epidemic2 <- data.frame(time=0, S2=Systm2[1], E2=Systm2[2], I2=Systm2[3], R2=Systm2[4])

for(time in seq(from=deltaT, to=period, by=deltaT)){
  
  Systm2 <- updateSystm2(Systm2, alpha1, alpha2, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2, deltaT)
  epidemic2 <- rbind(epidemic2, c(time, Systm2))
  
}

library(tidyverse)

epi2 <- epidemic2 %>%
  pivot_longer(S2:R2, values_to="Counts", names_to="State")

ggplot(epi2) + geom_line(aes(x=time, y=Counts, col=State))

######################################################################################################################
options(scipen = 999)
view(epidemic2)
epidemic2$Epidemic_size2 = rowSums(epidemic2[,c("S2","E2")])

epidemic2$Total_population_size = rowSums(epidemic2[,c("S2","E2","I2", "R2")])

#epidemic$s1 <- epidemic$S1 / epidemic$Total_population_size
#epidemic$r1 <- epidemic$R1 / epidemic$Total_population_size


epidemic2$V2 =  epidemic2$R2 / epidemic2$Total_population_size

plot(epidemic2$V2, epidemic2$Epidemic_size, type = "l", lwd = 2,  main = "Vaccination Proportion", 
     xlab = expression(paste("Vaccination proportion")),
     ylab = "Epidemic size", xlim = c(0.0, 1.0))

######################################################################################################################

plot(rollmean(epidemic2$V2, k = 7), rollmean(epidemic2$I2, k = 7), type = "l", lwd = 2,  main = "h", 
     xlab = expression(paste("Vaccination proportion")),
     ylab = "Peak incidences", xlim = c(0.0, 1.0))

######################################################################################################################