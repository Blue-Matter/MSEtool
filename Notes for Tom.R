
# Changed Slots
Fleet <- Fleet()
Fleet@Effort@Value # previously Fleet@Effort
Fleet@Effort@Distribution # previously Fleet@Distribution

Fleet@Catchability@Value # previously Fleet@Catchability
Fleet@Catchability@qArea # previously Fleet@qArea

Hist@OM is now the same as OM - ie Hist@OM@Fleet[[1]] is now a list of `Fleet` objects rather than all fleets combined 