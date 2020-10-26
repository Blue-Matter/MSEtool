#' Get part of an MP specific data-list
#'
#' @param MSElist A hierarchical list [Stock][Fleet][MP]
#' @param mm integer the MP number
#'
#' @return a sublist of MSElist for a specific MP
getDataList<-function(MSElist,mm){
  DataList<-new('list')
  for(ss in 1:length(MSElist)){
    DataList[[ss]]<-new('list')
    for(ff in 1:length(MSElist[[ss]]))DataList[[ss]][[ff]]<-MSElist[[ss]][[ff]][[mm]]
  }
  DataList
}

#' Apply multi Management Procedures (class MMP) to a hierarchical list of Data class objects
#'
#' @param DataList A hierarchical list of \linkS4class{Data} objects (Fleets nested in Stocks)
#' @param MP Name(s) of the MPs to run
#' @param reps Number of samples
#' @param nsims Optional. Number of simulations.
#' @param silent Logical. Should messages be suppressed?
#'
#' @return A hierarchical list of management recommendations (object class Rec), Fleets nested in Stocks
#'
applyMMP <- function(DataList, MP = NA, reps = 1, nsims=NA, silent=FALSE) {

  if (is.na(nsims)) nsims <- length(DataList[[1]][[1]]@Mort)
  nMPs <- length(MP)
  if (.hasSlot(DataList[[1]][[1]], "nareas")) {
    nareas <- DataList[[1]][[1]]@nareas
  } else {
    nareas <- 2
  }

  DataList_F <-DataList  # formatted data list

  for(ss in 1:length(DataList)){
    for(ff in 1:length(DataList[[ss]])){
      DataList_F[[ss]][[ff]] <- updateMSE(DataList[[ss]][[ff]])
    }
  }

  temp <- lapply(1:nsims, MP, DataList = DataList, reps = reps)
  recList<-

    #if (!silent && any(apply(is.na(recList$TAC), 2, sum) > rep(0.5 * reps, nsims)))
    #  message("Method ", MPs[mp], " produced greater than 50% NA values")

    #Data@MPs <- MPs
    CombineMMP(temp,nareas)

}


#' Create a blank MP recommendations object (class Rec) of the right dimensions
#'
#' @param temp A list of nsim simulations.
#' @param nareas The number of areas.
#' @author T. Carruthers
#' @export
CombineMMP<-function(temp,nareas){
  
  slots <- slotNames(temp[[1]][[1]][[1]]) # sim stock fleet
  nsim<-length(temp)
  np<-length(temp[[1]])
  nf<-length(temp[[1]][[1]])
  
  recList<-new('list')
  
  for(pp in 1:np){
    
    recList[[pp]]<-new('list')
    
    for(ff in 1:nf){
      
      recList[[pp]][[ff]]<-new('list')
      
      for (X in slots) { # sequence along recommendation slots
        
        if (X == "Misc") { # convert to a list nsim by nareas
          rec <- lapply(temp,getfirstlev,name=X,pp=pp,ff=ff)
        } else {
          rec <- matrix(unlist(lapply(temp,getfirstlev,name=X,pp=pp,ff=ff)),ncol=nsim) # unlist(lapply(temp, slot, name=X))
        }
        if (X == "Spatial") { # convert to a matrix nsim by nareas
          rec <- matrix(rec, nareas, nsim)
        }
        recList[[pp]][[ff]][[X]] <- rec
        recList$Misc <- NULL
        
      } # end of Rec slots
      
    } # end of fleets
    
  } # end of stocks
  
  recList
  
}

#' Extract the first dimension of a hierachical list of recommendation objects
#' 
#' @param x Simulation number
#' @param name Character. The slot name to extract.
#' @param pp Integer. The stock number (second level list)
#' @param ff Integer. The fleet number (third level list)
#' @author T. Carruthers
#' @export
getfirstlev<-function(x, name,pp,ff) slot(x[[pp]][[ff]], name)
