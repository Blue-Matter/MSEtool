# Case Studies
# 1. Condition RCM
# 2. Import SS3 
# 3. Import BAM
# 4. Data-Limited - Expert Judgement
# 5. Convert


TestOM <- Convert(testOM)

MaxAge(TestOM@Stock$Albacore@Ages)  <- 10
MaxAge(TestOM@Stock$Albacore)

# UP TO HERE
MaxAge(TestOM@Stock) <- list(5)
MaxAge(TestOM@Stock)

MaxAge(TestOM) <- list(4)
MaxAge(TestOM) 

