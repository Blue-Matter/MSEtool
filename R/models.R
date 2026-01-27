PrintModelTable <- function(models, print=TRUE, Independent=NULL) {
  df <- list()
  Independent <- c(Independent, slotNames('stock'), slotNames('fleet'))
  for (i in seq_along(models)) {
    mod <- get(models[i])
    args <- names(formals(mod))
    other_index <- rep(FALSE, length(args))
    other_index[args %in% Independent] <- TRUE

    df[[i]] <- data.frame(Model=models[i],
                          Arguments=paste(names(formals(mod)), collapse=', '),
                          Class=class(mod))
    if (print) {
      cli::cli_par()
      cli::cli_text(paste0("{.strong Model:} {.help MSEtool::", models[i],"}"))
      cli::cli_text("{.strong Pars:} {.code {args[!other_index]}}")

      if (sum(other_index))
        cli::cli_text("{.strong Other argument(s):} {.code {args[other_index]}}")

      cli::cli_text("{.strong Class:} {.code {class(mod)}}")
      cli::cli_end()
    }
  }
  do.call('rbind', df)
}

ReturnModels <- function(ModelClass, full=TRUE, print=TRUE, Independent='Independent') {
  models <- FindModels(ModelClass)
  if (!full)
    return(models)

  df <- PrintModelTable(models, print, Independent)
  if (!print)
    return(df)
  invisible(df)
}


dnormal<-function(lens,lfs,sl,sr){
  cond<-lens<=lfs
  sel<-rep(NA,length(lens))
  sel[cond]<-2.0^-((lens[cond]-lfs)/sl*(lens[cond]-lfs)/sl)
  sel[!cond]<-2.0^-((lens[!cond]-lfs)/sr*(lens[!cond]-lfs)/sr)
  sel
}

allometric <- function(x, scale, exponent) {
  scale * x^exponent
}


logistic_50_95 <- function(x, x50, x50_95, asymp=1) {
  slope <- log(19) / (x50_95)
  asymp / (1 + exp(-slope * (x - x50)))
}

double_normal <- function(x, x5, xF, xMax) {
  
  if (all(x5 ==0) & all(xF==0)) {
    return(rep(1, length(x)))
  }
  
  if (x5 >= xF) {
    cli::cli_abort("`x5` ({.val {x5}}) must be less than `xF` ({.val {xF}}).")
  }
  if (xF >= max(x)) {
    cli::cli_abort("`xF` ({.val {xF}}) must be less than max(x) ({.val {max(x)}}).")
  }
  
  if (xMax < 0 || xMax > 1) {
    cli::cli_abort("`xMax` ({.val {xMax}}) must be in [0, 1].")
  }
  

  ref <- max(x)
  sr <- (ref - xF) / ((-log(xMax,2))^0.5)
  sr[!is.finite(sr)] <- Inf
  sl <- (xF - x5) /((-log(0.05,2))^0.5)
  dnormal(x, xF, sl, sr)
}




