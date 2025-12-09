# Case Studies
# 1. Condition RCM
# 2. Import SS3 
# 3. Import BAM
# 4. Data-Limited - Expert Judgement
# 5. Convert




x <- Convert(testOM)
x@Stock$Albacore@CommonName


x@Stock$Albacore@Ages



x@Stock$Albacore@Ages


MinAge(x)
MaxAge(x) <- list(5)
MaxAge(x)


MaxAge(x@Stock)
MaxAge(x@Stock) <- list(15)
MaxAge(x@Stock)


MaxAge(x@Stock$Albacore)
MaxAge(x@Stock$Albacore) <- 12
MaxAge(x@Stock$Albacore)

MaxAge(x@Stock$Albacore@Ages)  <- 10
MaxAge(x)



