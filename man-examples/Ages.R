# Basic construction
a <- Ages(MaxAge = 20)
a

# Seasonal model: quarterly age classes
a_qtr <- Ages(MaxAge = 20, Units = "quarter")
Classes(a_qtr)

# No plus group
a_exact <- Ages(MaxAge = 20, PlusGroup = FALSE)

# Slot accessors
MaxAge(a)
MaxAge(a) <- 25
PlusGroup(a) <- FALSE

# Pass-through access from a stock
s <- Stock(Name = "Cod", Ages = Ages(MaxAge = 15))
Ages(s)
Ages(s) <- Ages(MaxAge = 20)
