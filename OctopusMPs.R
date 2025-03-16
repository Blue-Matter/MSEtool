
SpatioTemporalClosure <- function(Data=NULL, 
                                  MonthsClosed=NULL,
                                  AreasClosed=NULL,
                                  AnnEffortInc=NULL) {
  
  Advice <- Advice()
  
  if (all(c(is.null(MonthsClosed),
            is.null(AreasClosed),
            is.null(AnnEffortInc))))
    return(Advice)
  
  # projection time-step
  projTS <- length(Data$TimeSteps[Data$TimeSteps>Data$TimeStepLastHist])+1
  month <- projTS %% 12
  if (month==0)
    month <- 12
  
  if (!is.null(AnnEffortInc)) {
    if (month==12) {
      Advice@Effort@Effort <- (1+AnnEffortInc/100)^(projTS/12)
    } 
  }
  
  if (!is.null(MonthsClosed)) {
    if (month %in% MonthsClosed) {
      Advice@Distribution@Closure <- AreasClosed
    } else {
      Advice@Distribution@Closure <- rep(1,2)
    }
  }
  Advice
}

# Open all the time (status quo)
Open <- function(Data=NULL) {
  SpatioTemporalClosure(Data, AnnEffortInc=2.5)
}
class(Open) <- 'mp'

# Close Area 1 for month 6
Spatial_1 <- function(Data=NULL) {
  SpatioTemporalClosure(Data,
                        MonthsClosed=6,
                        AreasClosed=c(0,1),
                        AnnEffortInc=2.5)
}
class(Spatial_1) <- 'mp'

# Close whole fishery for 1 month
Seasonal_1 <- function(Data=NULL) {
  SpatioTemporalClosure(Data,
                        MonthsClosed=6,
                        AreasClosed=c(0,0),
                        AnnEffortInc=2.5)
}
class(Seasonal_1) <- 'mp'


Spatial_2 <- function(Data=NULL) {
  SpatioTemporalClosure(Data,
                        MonthsClosed=5:6,
                        AreasClosed=c(0,1),
                        AnnEffortInc=2.5)
}
class(Spatial_2) <- 'mp'


Spatial_3 <- function(Data=NULL) {
  SpatioTemporalClosure(Data,
                        MonthsClosed=4:6,
                        AreasClosed=c(0,1),
                        AnnEffortInc=2.5)
}
class(Spatial_3) <- 'mp'

Spatial_6 <- function(Data=NULL) {
  SpatioTemporalClosure(Data,
                        MonthsClosed=4:9,
                        AreasClosed=c(0,1),
                        AnnEffortInc=2.5)
}
class(Spatial_6) <- 'mp'


Spatial_12 <- function(Data=NULL) {
  SpatioTemporalClosure(Data,
                        MonthsClosed=1:12,
                        AreasClosed=c(0,1),
                        AnnEffortInc=2.5)
}
class(Spatial_12) <- 'mp'



