library(MSEtool)

OM <- ExampleOM

OM <- Populate(OM)


# ---------------------- DEBUG ----------------------


OM <- Convert(testOM)


OM@Fleet$Albacore$Generic_Fleet@Selectivity@MeanAtAge |> dim()
OM@Fleet$Albacore$Generic_Fleet@Retention@MeanAtAge |> dim()
OM@Fleet$Albacore$Generic_Fleet@DiscardMortality@MeanAtAge |> dim()



# - Add option for Selectivity, Retention, DiscardMortality by Area ...
# - 

LoadArgs('Simulate_om')

Hist <- Simulate_om(OM)


OM@Fleet$Albacore$`Stock:Albacore  Fleet:Generic_Fleet  Obs model:Generic_Obs  Imp model:Perfect_Imp`


# - Add Selectivity, Retention, DiscardMortality by Area ...
# - finalize OM object  - HERM !! & MoveStock
# - finalize Hist object

# - update internal for new Fleet structure
# - test Simulate


# - finalise MSE object
# - test Project

# - merge back into pre-release when ready ... 



# -------------------- END DEBUG --------------------





slotNames(OM)










# - try optimize cpp if possible

# - finalize MSE object 


# - write Fleet section of manual
# - update NPSWO 

