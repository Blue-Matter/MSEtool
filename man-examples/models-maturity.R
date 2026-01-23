Ages <- 0:15
Length <- seq(10, 100, 10)
Weight <- seq(1, 50, 5)

M_age <- MaturityAtAge(Ages, A50 = 5, A50_95 = 3)

M_length <- MaturityAtLength(Length, L50 = 50, L50_95 = 20)

M_weight <- MaturityAtWeight(Weight, W50 = 20, W50_95 = 15)


plot(Ages, M_age, type = "l", lwd = 2,
     xlab = "Age (years)", ylab = "Proportion Mature", 
     main = "Maturity-at-Age")

plot(Length, M_length, type = "l", lwd = 2, 
     xlab = "Length", ylab = "Proportion Mature", 
     main = "Maturity-at-Length")

plot(Weight, M_weight, type = "l", lwd = 2,
     xlab = "Weight", ylab = "Proportion Mature", 
     main = "Maturity-at-Weight")
