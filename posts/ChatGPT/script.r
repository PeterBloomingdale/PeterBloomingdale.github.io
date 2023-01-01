library('deSolve')



pbpkModel <- function(time, init, params) {
  with(as.list(c(init, params)), {
    # Calculate rate of change in concentrations in each compartment
    dA1 <- -(k12 + k10) * A1 + k21 * A2
    dA2 <- k12 * A1 - k21 * A2
    
    # Calculate liver and kidney clearance
    liverClearance <- Q * kLiver
    kidneyClearance <- Q * kKidney
    
    # Calculate rate of change in drug amount in liver and kidneys
    dLiver <- liverClearance - k10 * A1
    dKidney <- kidneyClearance - k10 * A2
    
    # Return rates of change
    return(list(c(dA1, dA2, dLiver, dKidney)))
  })
}

# Define initial conditions and parameters
init <- c(A1 = 100, A2 = 0, liver = 0, kidney = 0)
params <- c(k12 = 0.1, k21 = 0.2, k10 = 0.1, kLiver = 0.3, kKidney = 0.4, Q = 1)
time <- seq(0, 24, 0.1)

# Use ode to solve the system of ODEs
output <- ode(func = pbpkModel, y = init, times = time, parms = params)

# Extract concentration values from output
concentrations1 <- output[, "A1"]
concentrations2 <- output[, "A2"]
liverConcentrations <- output[, "liver"]
kidneyConcentrations <- output[, "kidney"]

##### Code I Added
png("pbpk.png")
par(mfrow=c(2,2))
plot(time, concentrations1)
plot(time, concentrations2)
plot(time, liverConcentrations)
plot(time, kidneyConcentrations)
dev.off()
#####