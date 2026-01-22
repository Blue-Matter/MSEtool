Ages <- 0:10
MeanLength <- vonBert(Ages, 100, 0.2)
Length <- seq(10, 100, 10)

W_age <- WeightatAge(Ages, a = 59.56, b = 2)
W_mean <- WeightatMeanLength(MeanLength, alpha = 0.01, beta = 3)
W_length <- WeightatLength(Length, Alpha = 0.0001, Beta = 3)

df <- data.frame(
  Age = Ages,
  W_age = W_age,
  W_mean = W_mean
)

# Plot Weight-at-Age and Weight-at-Mean-Length
matplot(df$Age, df[,-1], type = "l", lty = 1, lwd = 2,
        col = c("blue", "purple"), xlab = "Age (years)", ylab = "Weight",
        main = "Weight-at-Age Models")
legend("topleft", legend = colnames(df)[-1], col = c("blue", "purple"), lty = 1, lwd = 2)

# Plot Weight-at-Length
plot(Length, W_length, type = "l", lwd = 2, col = "red",
     xlab = "Length", ylab = "Weight", main = "Weight-at-Length Model")
