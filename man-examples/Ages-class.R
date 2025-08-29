
# Populate an `Ages` object
Ages <- Ages(MaxAge=20)

# Change units
Units(Ages)
Units(Ages) <- 'month'

# Check valid units
ValidUnits(Ages)

# Check `Ages` object is complete
Check(Ages)



