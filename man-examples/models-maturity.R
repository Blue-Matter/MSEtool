Ages <- 0:15
Length <- seq(10, 100, 10)
Weight <- seq(1, 50, 5)

M_age <- MaturityAtAge(Ages, A50 = 5, A50_95 = 8)

M_length <- MaturityAtLength(Length, L50 = 50, L50_95 = 70)

M_weight <- MaturityAtWeight(Weight, W50 = 20, W50_95 = 35)


plot(Ages, M_age, type = "l", lwd = 2, col = "blue",
     xlab = "Age (years)", ylab = "Proportion Mature", main = "Maturity-at-Age")

plot(Length, M_length, type = "l", lwd = 2, col = "green",
     xlab = "Length", ylab = "Proportion Mature", main = "Maturity-at-Length")

plot(Weight, M_weight, type = "l", lwd = 2, col = "red",
     xlab = "Weight", ylab = "Proportion Mature", main = "Maturity-at-Weight")
