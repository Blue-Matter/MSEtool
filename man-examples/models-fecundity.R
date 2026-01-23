Ages <- 0:15
Length <- seq(10, 100, 10)
Weight <- seq(1, 50, 5)

F_age <- FecundityAtAge(Ages, A50 = 5, A50_95 = 3, MaxFec=1e6)

F_length <- FecundityAtLength(Length, L50 = 50, L50_95 = 20, MaxFec=1e6)

F_weight <- FecundityAtWeight(Weight, W50 = 20, W50_95 = 15, MaxFec=1e6)


plot(Ages, F_age, type = "l", lwd = 2,
     xlab = "Age (years)", ylab = "Fecundity", 
     main = "Fecundity-at-Age")

plot(Length, F_length, type = "l", lwd = 2,
     xlab = "Length", ylab = "Fecundity", 
     main = "Fecundity-at-Length")

plot(Weight, F_weight, type = "l", lwd = 2, 
     xlab = "Weight", ylab = "Fecundity", 
     main = "Fecundity-at-Weight")
