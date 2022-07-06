

#' Converts a Data object into a .csv data file
#'
#' @description A function that writes a correctly formatted .csv file from a MSEtool Data object
#' @param Data An object of class 'Data'.
#' @param file Character string. The name of the location and file you wish to create (e.g. "C:/temp/mydata.csv")
#' @param simno Integer. An optional argument to specify the simulation number if writing simulated data
#' @param overwrite Boolean. Should existing data files be automatically overwritten.
#' @param keepNAs Boolean. Should slots with NAs still be written to the data file.
#' @author T. Carruthers
#' @export
Data2csv<-function(Data, file=NULL, simno = 1, overwrite=F, keepNAs=T) {

  if(!inherits(Data,'Data')) stop("First argument 'Data' not an object of class 'Data' ")

  if(is.null(file)){
    file=paste0(getwd(),"/",deparse(substitute(Data)),".csv")
    message(paste0("Second argument 'file' not specified - data to be written to ",file))
  }

  if(substr(file,nchar(file),nchar(file))=="/"){
    file=paste0(file,deparse(substitute(Data)),".csv")
    message(paste0("Second argument 'file' appears to be only a folder, no file name specified - data to be written to ",file))
  }

  if(substr(file,nchar(file)-3,nchar(file))!=".csv") file=paste0(file,".csv")

  dirsplit<-unlist(strsplit(file,split="/"))
  folder<-paste(dirsplit[1:(length(dirsplit)-1)],collapse="/")
  if (folder != file) {
    if(!dir.exists(folder)){
      file=paste0(getwd(),"/",deparse(substitute(Data)),".csv")
      message(paste0("Folder < ",folder," > does not exist -  data to be written to ",file))
    }
  } else {
    message("Data to be written to ", getwd(), "/", file)
  }

  if(file.exists(file)){
    if(!overwrite){
      ANSWER<- readline("This file already exists. Is it ok to continue and overwrite it (y/n) ?")
      ## a better version would check the answer less cursorily, and
      ## perhaps re-prompt
      if (substr(ANSWER, 1, 1) == "n"){
        stop("Data file writing aborted")
      }
    }
    message(paste0("File < ",file," > is being over-written"))
  }

  slots<-slotNames(Data)
  ns<-length(slots)

  appendy<-c(F,rep(T,ns-1))

  yrs<-Data@Year

  slottest<-function(obj){
    #if(sum(!is.na(obj))==0){
    #  return(TRUE)
    if(inherits(obj,"character")){
      return(FALSE)
    }else if(sum(obj,na.rm=T)==0){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }

  lexicon <- as.matrix(MSEtool::DataSlots[,1:2])

  nsim <- length(Data@Mort)
  addInd <- FALSE
  for(i in 1:ns){
    obj<-slot(Data,slots[i])
    if (slots[i] %in% c('AddInd', "CV_AddInd", "AddIndV", "AddIunits", "AddIndType")) {
      # add additional indices
      if (slots[i] %in% c('AddInd', 'CV_AddInd')) {
        if (length(obj)>0) {
          addInd <- TRUE
          nind <- dim(obj)[2]
          for (j in 1:nind) {
            if (slots[i] == 'AddInd') name <- paste('Index', j)
            if (slots[i] == 'CV_AddInd') name <- paste('CV Index', j)
            write(paste(c(name,obj[simno, j, ]),collapse=","),file,1,append=appendy[i])
          }
        }
      }
      if (addInd && slots[i] == 'AddIndV') {
        if (dim(obj)[2] == nind) {
          for (j in 1:nind) {
            name <- paste('Vuln Index', j)
            write(paste(c(name,obj[simno, j, ]),collapse=","),file,1,append=appendy[i])
          }
        } else {
          for (j in 1:nind) {
            name <- paste('Vuln Index', j)
            write(paste(c(name,NA),collapse=","),file,1,append=appendy[i])
          }
        }
      }
      if (addInd && slots[i] == 'AddIunits') {
        write(paste(c('AddIunits',obj),collapse=","),file,1,append=appendy[i])
      }
      if (addInd && slots[i] == 'AddIndType') {
        write(paste(c('AddIndType',obj),collapse=","),file,1,append=appendy[i])
      }

    } else {
      lex<-lexicon[match(slots[i],lexicon[,2]),1]
      if(!is.na(lex)){
        if(!inherits(obj,"list") & !inherits(obj,"data.frame")){
          allNA<-sum(!is.na(obj))==0
          if(!(allNA & !keepNAs)){ # write NA values
            if(slottest(obj)){ # is the slot empty or all NAs?
              write(paste(c(lex,"NA"),collapse=","),file,1,append=appendy[i])
            }else{
              if(is.null(dim(obj))){  # vector or single value
                if (length(obj)==nsim) {
                  write(paste(c(lex,obj[simno]),collapse=","),file,1,append=appendy[i])
                } else {
                  write(paste(c(lex,obj),collapse=","),file,1,append=appendy[i])
                }
              }else if(length(dim(obj))==2){ # a matrix (time series)
                write(paste(c(lex,obj[simno,]),collapse=","),file,1,append=appendy[i])

              }else if(length(dim(obj))==3){ # 3d array of composition data (CAL, CAA)
                ny<-dim(obj)[2]
                if (slots[i] == "RInd") {
                  for(yy in 1:ny){
                    if (yy ==1) write(paste(c(lex,obj[simno,yy,]),collapse=","),file,1,append=appendy[i])
                    if (yy !=1) write(paste(c('',obj[simno,yy,]),collapse=","),file,1,append=appendy[i])
                  }
                } else {
                  for(yy in 1:ny){
                    write(paste(c(paste(lex,yrs[yy]),obj[simno,yy,]),collapse=","),file,1,append=appendy[i])
                  }
                }

              }
            } # end of slottest
          } # end of removeNAs
        }
      }#end of na lex
    }

  }
}


#' Simplifies the CAL slot of data object
#'
#' @description A function that condenses the number of catch-at-length bins in a data object
#' @param Data An object of class 'Data'.
#' @param nbins Integer. The target number of catch at length bins
#' @param simno Integer. An optional argument to specify the simulation number if writing simulated data
#' @author T. Carruthers
#' @export
CALsimp<-function(Data,nbins=10,simno=1){

  oldbins<-Data@CAL_bins
  nold<-length(oldbins)
  ind<-rep((1:nold),each=floor(nold/nbins))[1:nold]
  maxbin<-max(ind)
  newCAL_bins<-c(Data@CAL_bins[match(1:maxbin,ind)],Data@CAL_bins[nold])
  ny<-dim(Data@CAL)[2]
  newCAL<-array(0,c(1,ny,maxbin))
  for(b in 1:(nold-1)) newCAL[1,,ind[b]]<-newCAL[1,,ind[b]]+Data@CAL[simno,,b]

  Data@CAL_bins<-newCAL_bins
  Data@CAL<-newCAL
  Data
}

