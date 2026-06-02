Ages <- 0:15
Length <- seq(10, 100, 10)
Weight <- seq(1, 50, 5)

M_age <- MortalityAtAge(Ages, M = 0.2)

M_length_lor <- LorenzenMortalityLength(Length, Mref = 0.3, Lref = 50)

M_weight_lor <- LorenzenMortalityWeight(Weight, Mref = 0.3, Wref = 20)

# Plot M-at-age
plot(Ages, M_age, type = "l", lwd = 2, col = "blue", 
     xlab = "Age", ylab = "M", main = "Constant M-at-Age",
     ylim=c(0,0.25))

# Plot M-at-length
plot(Length, M_length_lor, type = "l", lwd = 2, col = "green", 
     xlab = "Length", ylab = "M", main = "Lorenzen M-at-Length",
     ylim=c(0, 0.5))

# Plot M-at-weight
plot(Weight, M_weight_lor, type = "l", lwd = 2, col = "purple", 
     xlab = "Weight", ylab = "M", main = "Lorenzen M-at-Weight", 
     ylim=c(0,0.8))
