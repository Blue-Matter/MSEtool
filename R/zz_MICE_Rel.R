
#' Utility for making multi-OMs
#' 
#' Converts an OM to a single stock, single fleet MOM.
#' 
#' @param ... An \linkS4class{OM}.
#' @param silent Should messages be printed out to the console?
#' @return A class \linkS4class{MOM} object.
#' @examples 
#' MOM <- makeMOM(testOM)
#' @author Q. Huynh
#' @export
makeMOM <- function(..., silent = FALSE) {
  dots <- list(...)
  
  if (length(dots) == 1 && is(dots[[1]], "OM")) {
    OM2MOM(dots[[1]])
  } else {
    stop("OM object was not found.")
  }
}

OM2MOM <- function(OM) {
  MOM <- new("MOM",
             Stocks = list(SubOM(OM, "Stock")), 
             Fleets = list(list(SubOM(OM, "Fleet"))),
             Obs = list(list(SubOM(OM, "Obs"))),
             Imp = list(list(SubOM(OM, "Imp"))))
  
  vars <- intersect(slotNames("MOM"), slotNames("OM")) # setdiff(slotNames("MOM"), slotNames("OM"))
  lapply(vars, function(i) slot(MOM, i) <- slot(OM, i))
  
  # Override cpars
  MOM@cpars <- list(list(OM@cpars))
  return(MOM)
}



#' MICE relationships for multi-OM
#' 
#' Generate a MICE \code{Rel} object, with \code{predict} and \code{simulate} methods, for \link{multiMSE}. 
#' Currently implements intra-stock dynamics via density-dependent processes.
#' 
#' @param type String to indicate the type of stock interaction. \code{"DDM"} for
#' density-dependent natural mortality.
#' @param stock The index position of the stock in the MOM.
#' @param CV Coefficient of variation of the predicted value for \code{simulate}. Used to pass
#' values to the operating model.
#' @param ... Additional arguments depending on \code{type}. See details below.
#' @author Q. Huynh
#' @return A class "Rel" object to pass to \code{MOM@@Rel}.
#' 
#' @section Density-dependent M ("DDM"):
#' Natural mortality (M) is a linear function of stock depletion in terms to total biomass (B) in year y 
#' (Forrest et al. 2018):
#' \deqn{M_y = M_0 + (M_1 + M_0) (1 - B_y/B_0)}{M_y = M_0 + (M_1 + M_0) (1 - B_y/B_0)}
#' with a constraint that \eqn{M_y = M_0} if \eqn{B_y > B_0}
#' 
#' Provide the following arguments:
#' \itemize{
#' \item \code{M0}: Natural mortality as B approaches B0. Vector `[nsim]`
#' \item \code{M1}: Natural mortality as B approaches zero. Vector `[nsim]`
#' \item Optional \code{B0}: Unfished biomass. Calculated from stock-recruit \code{alpha} and \code{beta} and unfished 
#' biomass per recruit at M = M0. Vector `[nsim]`
#' }
#'  
#' @examples
#' # Depensatory natural mortality
#' Rel <- makeRel(type = "DDM", M0 = 0.8, M1 = 0.2, CV = 0.1)
#' 
#' # Predict M when B/B0 = 0.1
#' pred <- predict(Rel, newdata = data.frame(B_1 = 0.1, B0_1 = 1))
#' 
#' # Simulate values of M with CV = 0.1
#' Rel$fitted.values <- pred
#' simulate(Rel, nsim = 10, seed = 1)
#' 
#' # Add Rel to MOM
#' MOM <- makeMOM(testOM)
#' MOM@Rel <- list(Rel)
#' @references 
#' Forrest, R., Holt, K., and Kronlund, A. 2018. Performance of alternative harvest control rules for two 
#' Pacific groundfish stocks with uncertain natural mortality: Bias, robustness and trade-offs. 
#' Fisheries Research 206: 259–286. \doi{10.1016/j.fishres.2018.04.007}
#' @export
makeRel <- function(type = "DDM", stock = 1, CV = 0, ...) {
  type <- match.arg(type)
  dots <- list(...)
  
  if(type == "DDM") {
    if(is.null(dots$M0)) stop("M0 not found. See help(\"make_Rel\")")
    if(is.null(dots$M1)) stop("M1 not found. See help(\"make_Rel\")")
    
    if(length(dots$M0) != length(dots$M1)) {
      stop("M0 and M1 need to be equal length vectors")
    }
    if(!is.null(dots$B0)) {
      if(length(dots$B0) != length(dots$M0)) {
        stop("M0 and B0 need to be equal length vectors")
      }
      B0_OM <- FALSE
    } else {
      B0_OM <- TRUE
      dots$B0 <- 1
    }
    f <- function(B, B0 = dots$B0, M0 = dots$M0, M1 = dots$M1, x = 1) {
      dep <- pmin(B/B0[x], 1)
      M0[x] + (M1[x] - M0[x]) * (1 - dep)
    }
    model <- data.frame(M = f(seq(0, 1, 0.1) * dots$B0[1], x = 1),
                        B = seq(0, 1, 0.1) * dots$B0[1])
    terms <- paste0(c("M", "B"), "_", stock)
    if(B0_OM) {
      terms <- c(terms, paste0("B0", "_", stock))
      model$B0 <- dots$B0[1]
    }
    if(length(dots$M0) > 1) {
      terms <- c(terms, "x")
      model$x <- 1
    }
    
    out <- list(f = f,
                model = structure(model, names = terms),
                fitted.values = model$M,
                CV = CV,
                terms = terms,
                type = type,
                Rel = "Density-dependent natural mortality")
  } else {
    out <- list()
  }
  structure(out, class = "Rel")
}

#' @rdname makeRel
#' @param x For \code{print.Rel}, a \code{Rel} class object from \code{make_Rel}.
#' @export
print.Rel <- function(x, ...) {
  cat("MOM Rel:", x$Rel, "\n\n")
  
  cat("Predictor variables from MOM:", paste(x$terms[-1], collapse = ", "), "\n")
  if("x" %in% x$terms) cat("(x is the simulation number)\n\n")
  
  cat("Response variable:", x$terms[1], "\n")
  
  invisible(x)
}


#' @rdname makeRel
#' @param object A \code{Rel} class object from \code{make_Rel}.
#' @param newdata A data frame to provide values of predictor variables with which to calculate the 
#' response variable.
#' 
#' @export
predict.Rel <- function(object, newdata, ...) {
  
  if(missing(newdata)) {
    return(object$fitted.values)
  }
  
  vars <- names(newdata)
  vars_Rel <- names(object$model)[-1]
  var_match <- vars_Rel %in% vars
  
  if (any(!var_match)) {
    stop(paste("Provide a data frame with column names:", paste(vars_Rel, collapse = ", "), "in newdata."))
  }
  
  vars_call <- sapply(vars, function(x) strsplit(x, "_")[[1]][1])
  args <- lapply(vars, function(x) getElement(newdata, x)) %>% structure(names = vars_call)
  do.call(object$f, args)
}

#' @rdname makeRel
#' @param nsim The number of simulations.
#' @param seed Integer to specify the seed for the random number generator.
#' @export
simulate.Rel <- function(object, nsim = 1, seed = 1, ...) {
  set.seed(seed)
  stdev <- sqrt(log(1 + object$CV^2))
  do_sim <- object$fitted.values * rlnorm(nsim * length(object$fitted.values), -0.5 * stdev^2, stdev)
  val <- matrix(do_sim, length(object$fitted.values), nsim) %>% as.data.frame() %>%
    structure(names = paste0("sim_", 1:nsim), seed = seed)
  val
}
