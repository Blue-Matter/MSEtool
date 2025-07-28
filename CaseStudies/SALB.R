library(MSEtool)

la <- devtools::load_all

la()


SSDir <- 'C:/Users/Admin/Documents/GitHub/SALB-MSE/Condition/SS3/ALB-S_Stochastic/Condition/SS3'
StochasticDirs <- list.dirs(file.path(SSDir), full.names = TRUE, recursive = FALSE)
StochasticDirs <- StochasticDirs[!grepl('Base', StochasticDirs)]

RepList <- ImportSSRepList(StochasticDirs[1:5])

OM <- ImportSS(RepList)


Hist <- SimulateDEV(OM)





 


t <- ImportSS(file.path(SSDir, 'Base'))
t <- ImportSS(StochasticDirs)
t <- ImportSS(RepList)






ImportSS_Dirs <- function(Dirs, RepList=NULL, pYear=50, Name = "Imported SS3 Model", silent=FALSE) {
  AllSSFiles <- lapply(Dirs, list.files)
  
  ReportExists <- lapply(AllSSFiles, function(x) sum(grepl('^Report.sso', x))) |> 
    unlist()
  
  ind <- which(ReportExists<1)
  if (length(ind)>0) {
    cli::cli_alert_warning('Warning: SS3 output is not available in {?directory/directories}: {.val {basename(Dirs[ind])}}. \nSkipping {?this/these} {?directory/directories} ...')
  }
  Dirs <- Dirs[-ind]
  
  # TODO - option to pass RepList as first argument
  if (is.null(RepList)) {
    RepList <- purrr::map(Dirs, r4ss::SS_output, verbose=FALSE, printstats=FALSE,
                          .progress=  list(
                            caller = environment(),
                            format =  'Reading SS3 Output from {.val {length(Dirs)}} directories {cli::pb_bar} {cli::pb_percent}')
    )
  }
  
  replist <- RepList[[1]] 
  nStock <- replist$nsexes
  nFleet <- replist$nfishfleets
  
  if (!silent)
    cli::cli_alert('{.val {nStock}-sex} and {.val {nFleet}-fleet} model detected.')
  
  
  OM <- OM(Name=Name)
  OM@nSim <- length(RepList)
  mainyrs <- replist$startyr:replist$endyr
  OM@nYear <- length(mainyrs)
  OM@pYear <- pYear
  OM@CurrentYear <- max(mainyrs)
  ProYears <- seq(max(mainyrs)+1, by=1, length.out=pYear)
  OM@TimeSteps <- c(mainyrs, ProYears)
  
  if (nStock==1) {
    StockNames <- 'Female'
  } else if (nStock==2) {
    StockNames <- c('Female', 'Male')
  } else {
    cli::cli_abort('`nStock` should be {.val {1} or {2}}')
  }
  
  
  

  
  
  ssstock <- function(st, RepList){
    mainyrs <- RepList[[1]]$startyr:RepList[[1]]$endyr
    nyears <- length(mainyrs)
    
    Stock <- new('stock')
    Stock@Name <- ifelse(st == 1, "Female", "Male")  # TODO - option for single sex combined models
    
    
    endgrowth <- purrr::map(RepList, \(replist) GetEndGrowth(st, replist))
    
    
    M_at_age <- purrr::map(RepList, \(replist) {
      replist$M_at_age[replist$M_at_age$Sex == st & replist$M_at_age$Year %in% mainyrs, ]
    })
    
    M_at_age <- purrr::map(RepList, \(replist) {
      replist$M_at_age[replist$M_at_age$Sex == st & replist$M_at_age$Yr %in% mainyrs, ]
    })
    
    M_at_age <- purrr::map(RepList, \(replist) {
      replist$M_at_age[replist$M_at_age$Gender == st & replist$M_at_age$Year %in% mainyrs, ]
    })
    
    
    M_at_age <- replist$M_at_age[replist$M_at_age$Gender == st & replist$M_at_age$Year %in% mainyrs, ]
    
    
  }
  
  r = lapply(RepList, '[[', 'natage')

  r[[1]] |> dim()
  r[[2]] |> dim()

  


 
}



MOM <- readRDS("C:\\Users\\Admin\\AppData\\Local\\Temp\\RtmpM5YKdS\\file1ccc33a58d9")

Hist <- SimulateMOM(MOM)

