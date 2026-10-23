
Ages <- 0:15

L_vb <- vonBert(Ages, Linf = 100, K = 0.25, t0 = -0.5)
L_brody <- Brody(Ages, L0 = 10, Linf = 100, K = 0.3)
L_gompertz <- Gompertz(Ages, Linf = 100, g = 0.35, a = 3)
L_richards <- Richards(Ages, Linf = 100, K = 0.3, t0 = -0.5, d = 0.8)

df <- data.frame(
  Age = Ages,
  vonBert = L_vb,
  Brody = L_brody,
  Gompertz = L_gompertz,
  Richards = L_richards
)

matplot(df$Age, df[,-1], type = "l", lty = 1, lwd = 2,
        col = c("blue", "red", "green", "purple"),
        xlab = "Age (years)", ylab = "Length",
        main = "Length-at-Age Models")
legend("bottomright", legend = colnames(df)[-1],
       col = c("blue", "red", "green", "purple"), lty = 1, lwd = 2)

