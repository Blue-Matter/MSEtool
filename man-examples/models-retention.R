
ages   <- 0:10
lengths <- seq(0, 100, by = 1)
weights <- seq(0.1, 10, by = 0.5)

# ---- Logistic Retention ----
ret_age_log   <- RetentionAtAge(ages, RA50 = 3, RA50_95 = 2)
ret_length_log <- RetentionAtLength(lengths, RL50 = 40, RL50_95 = 10)
ret_weight_log <- RetentionAtWeight(weights, RW50 = 5, RW50_95 = 2)

# ---- Knife-Edge Retention ----
ret_age_ke    <- RetentionKnifeEdgeAge(ages, RA = 4)
ret_length_ke <- RetentionKnifeEdgeLength(lengths, RL = 50)


# ---- Double-Normal Retention ----
ret_dn_length <- DoubleNormalRetention(lengths, R5 = 20, RFS = 50, Rmaxlen = 0.8)
ret_dn_weight <- DoubleNormalRetentionWeight(weights, RW5 = 1, RWFS = 5, Rmaxweight = 0.8)


# ---- Plots ----
# Age-based
plot(ages, ret_age_log, type='l', lwd=2, col='blue', ylim=c(0,1),
     main='Retention-at-Age', xlab='Age', ylab='Retention')
lines(ages, ret_age_ke, type='s', lwd=2, col='red')
legend("bottomright", legend=c("Logistic","Knife-Edge"), col=c("blue","red"), lwd=2)

# Length-based
plot(lengths, ret_length_log, type='l', lwd=2, col='blue', ylim=c(0,1),
     main='Retention-at-Length', xlab='Length', ylab='Retention')
lines(lengths, ret_length_ke, type='s', lwd=2, col='red')
lines(lengths, ret_dn_length, lwd=2, col='green')
legend("bottomright", legend=c("Logistic","Knife-Edge","Double-Normal"),
       col=c("blue","red","green"), lwd=2)

# Weight-based
plot(weights, ret_weight_log, type='l', lwd=2, col='blue', ylim=c(0,1),
     main='Retention-at-Weight', xlab='Weight', ylab='Retention')
lines(weights, ret_dn_weight, lwd=2, col='green')
legend("bottomright", legend=c("Logistic","Double-Normal"),
       col=c("blue","green"), lwd=2)
