
FindModels <- function(ModelClass) {
  objects_name <- ls.str("package:MSEtool", mode = "function")
  objects <- lapply(objects_name, get, envir=rlang::ns_env('MSEtool'))
  objects_class <- lapply(objects, class)
  objects_class <- unlist(lapply(objects_class, '[[', 1))
  ind <- which(objects_class%in%ModelClass)
  objects_name[ind]
}


FindModel <- function(object, ignore=c('Ages', 'Length', 'Weight', 'nage', 'AtAge', 'MaxLen',
                                       'S0', 'S', 'R0')) {

  
  if (inherits(object@Model,'function'))
    return(object@Model)

  if (any(is.na(object@Pars)))
    return(NULL)

  if (length(object@Pars)<1)
    return(NULL)
  
  # if (inherits(object@Model,'character'))
  #   return(object@Model)

  cl <- class(object)
  slots <- c(slotNames('stock'), slotNames('fleet'))
  ind <- match(cl, tolower(slots))

  fun <- get(paste0(slots[ind], 'Models'))
  models <- fun(FALSE, FALSE)

  matching_parameters <- rep(TRUE, length(models))

  ParNames <- names(object@Pars)
  ind <- which(tolower(ParNames) |> substrRight(2) == 'sd')
  if (length(ind)>0)
    ParNames <- ParNames[-ind]
  
  for (i in seq_along(matching_parameters)) {
    formals <- formals(get(models[i]))
    formal_names <- NA
    for (j in seq_along(formals)) {
      formal_names[j] <- names(formals[j])
    }
    formal_names <- formal_names[!formal_names%in% ignore]
    if (!(all(ParNames %in% formal_names)
        & all(formal_names %in% ParNames)))
      matching_parameters[i] <- FALSE
  }
  model <- models[matching_parameters]

  if (length(model)<1) {
    CheckModel(object)
  }

  model
}
