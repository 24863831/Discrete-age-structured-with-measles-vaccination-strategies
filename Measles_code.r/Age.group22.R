# Create a function to calculate the rate of change in each state variable
changed.dt22 <- function(X, alpha1, alpha2, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2){
  S1 <- X[1] ; E1 <- X[2] ; I1 <- X[3] ; R1 <- X[4]
  S2 <- X[1] ; E2 <- X[2] ; I2 <- X[3] ; R2 <- X[4]
  
  dS2dt <- (1 - theta2 *sigma2)*alpha1*S1 - beta2*c2*I2*S2 - (d2 + alpha2)*S2
  dE2dt <- beta2*c2*I2*S2 + alpha1*E1 - (d2 + epsilon2 + alpha2)*E2
  dI2dt <- epsilon2*E2 + alpha1*I1 - gamma2*I2 - (d2 + mu2)*I2 - alpha2*I2
  dR2dt <- gamma2*I2 + theta2*sigma2*alpha1*S1 + alpha1*R1 - (d2 + alpha2)*R2
  
  
  
  return(c(dS2dt, dE2dt, dI2dt, dR2dt))
  
}

# A fuction that will update the system at each time step
updateSystm22 <- function(X, alpha1, alpha2, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2, deltaT){
  X <- X + changed.dt22(X, alpha1, alpha2, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2)*deltaT
  return(X)
}

# How many people are in S, E, I, R and V at time = 0
Systm2 <- c(200, 80, 30, 1)
# Per-capita birth rate
alpha1 <- 0.0385
alpha2 <- 0.00385
theta2 <- 0.95
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
period <- 2554


# Simulations for age group 2
epidemic22 <- data.frame(time=0, S2=Systm2[1], E2=Systm2[2], I2=Systm2[3], R2=Systm2[4])

for(time in seq(from=deltaT, to=period, by=deltaT)){
  
  Systm2 <- updateSystm22(Systm2, alpha1, alpha2, theta2, sigma2, beta2, c2, d2, epsilon2, gamma2, mu2, deltaT)
  epidemic22 <- rbind(epidemic22, c(time, Systm2))
  
}

epi22 <- epidemic22 %>%
  pivot_longer(S2:R2, values_to="Counts", names_to="State")

######################################################################################################################
epidemic22$Epidemic_size22 = rowSums(epidemic22[,c("S2","E2")])

epidemic22$Total_population_size2 = rowSums(epidemic22[,c("S2", "E2","I2","R2")])


epidemic22$V2 =  epidemic22$R2 / epidemic22$Total_population_size2
epidemic22$Incidences22 =  epidemic22$I2 / epidemic22$Total_population_size2

######################################################################################################################

######################################################################################################################

######################################################################################################################
start.date = "20170101"; end.date = "20231230"

Dates <- seq(ymd(start.date), ymd(end.date), by="days")

Datetable <- data.frame(Dates)

EpidemicDate22 <- cbind(Datetable, epidemic22)
colnames(EpidemicDate22)[1] <- "Year"


