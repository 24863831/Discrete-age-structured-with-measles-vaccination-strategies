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
c1 <- 15
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
###################################################################################################################

epidemic11$Epidemic_size11 = rowSums(epidemic[,c("S1","E1")])

epidemic11$Total_population_size11 = rowSums(epidemic11[,c("S1","E1","I1", "R1")])


epidemic11$V1 =  epidemic11$R1 / epidemic11$Total_population_size11
epidemic11$Incidences1 =  epidemic11$I1 / epidemic11$Total_population_size

###################################################################################################################

###################################################################################################################

###################################################################################################################

start.date11 = "20170101"; end.date11 = "20231230"

Dates11 <- seq(ymd(start.date11), ymd(end.date11), by="days")

Datetable11 <- data.frame(Dates11)

EpidemicDate11 <- cbind(Datetable11, epidemic11)
colnames(EpidemicDate11)[1] <- "Year"

###################################################################################################################


