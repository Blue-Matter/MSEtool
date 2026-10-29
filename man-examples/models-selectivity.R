
ages   <- 0:10
lengths <- seq(0, 100, by = 1)
weights <- seq(0.1, 10, by = 0.5)

# ---- Logistic Selectivity ----
sel_age_log   <- SelectivityAtAge(ages, SA50 = 3, SA50_95 = 2)
sel_length_log <- SelectivityAtLength(lengths, SL50 = 40, SL50_95 = 10)
sel_weight_log <- SelectivityAtWeight(weights, SW50 = 5, SW50_95 = 2)

# ---- Knife-Edge Selectivity ----
sel_age_ke    <- SelectivityKnifeEdgeAge(ages, SA = 4)
sel_length_ke <- SelectivityKnifeEdgeLength(lengths, SL = 50)


# ---- Double-Normal Selectivity ----
sel_dn_length <- DoubleNormal(lengths, L5 = 20, LFS = 50, Vmaxlen = 0.8)
sel_dn_weight <- DoubleNormalWeight(weights, W5 = 1, WFS = 5, Vmaxweight = 0.8)


# ---- Plots ----
# Age-based
plot(ages, sel_age_log, type='l', lwd=2, col='blue', ylim=c(0,1),
     main='Selectivity-at-Age', xlab='Age', ylab='Selectivity')
lines(ages, sel_age_ke, type='s', lwd=2, col='red')
legend("bottomright", legend=c("Logistic","Knife-Edge"), col=c("blue","red"), lwd=2)

# Length-based
plot(lengths, sel_length_log, type='l', lwd=2, col='blue', ylim=c(0,1),
     main='Selectivity-at-Length', xlab='Length', ylab='Selectivity')
lines(lengths, sel_length_ke, type='s', lwd=2, col='red')
lines(lengths, sel_dn_length, lwd=2, col='green')
legend("bottomright", legend=c("Logistic","Knife-Edge","Double-Normal"),
       col=c("blue","red","green"), lwd=2)

# Weight-based
plot(weights, sel_weight_log, type='l', lwd=2, col='blue', ylim=c(0,1),
     main='Selectivity-at-Weight', xlab='Weight', ylab='Selectivity')
lines(weights, sel_dn_weight, lwd=2, col='green')
legend("bottomright", legend=c("Logistic","Double-Normal"),
       col=c("blue","green"), lwd=2)
