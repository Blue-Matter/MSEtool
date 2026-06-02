#' Selectivity, Retention, Weight, Fecundity, and Maturity Model Helpers
#'
#' Low-level curve functions used internally by
#' [FecundityModels()], [MaturityModels()], [RetentionModels()],
#' [SelectivityModels()], and [WeightModels()].
#'
#' @param x Numeric vector of values (ages, lengths, or weights) at which to
#'   evaluate the curve.
#' @param x5 Value of `x` at 5% of the maximum (ascending limb of
#'   `double_normal`).
#' @param x50 Value of `x` at 50% of the asymptote (`logistic_50_95`).
#' @param x50_95 Interval between the 50% and 95% points (`logistic_50_95`).
#' @param xF Value of `x` at the peak of the `double_normal` curve.
#' @param xMax Maximum value on the descending limb of `double_normal`,
#'   expressed as a proportion between 0 and 1.
#' @param lens Numeric vector of lengths (`dnormal`).
#' @param lfs Length at full selection, the peak of the `dnormal` curve.
#' @param sl Standard deviation of the ascending limb of `dnormal`.
#' @param sr Standard deviation of the descending limb of `dnormal`. Use
#'   `Inf` for a flat-topped (one-sided) curve.
#' @param scale Multiplicative scale parameter (`allometric`).
#' @param exponent Power exponent (`allometric`).
#' @param asymp Asymptote of the logistic curve. Defaults to `1`.
#'
#' @details
#' **`allometric(x, scale, exponent)`**
#'
#' Returns `scale * x ^ exponent`. Suitable for length-weight and
#' length-fecundity relationships.
#'
#' **`dnormal(lens, lfs, sl, sr)`**
#'
#' Double-normal (dome-shaped) selectivity curve evaluated at `lens`.
#' The ascending limb uses standard deviation `sl` and the descending limb
#' uses `sr`. Set `sr = Inf` for a flat-topped curve.
#'
#' \deqn{S(l) = 2^{-\left(\frac{l - l_{fs}}{s}\right)^2}}
#'
#' where \eqn{s = sl} if \eqn{l \le l_{fs}} and \eqn{s = sr} otherwise.
#'
#' **`double_normal(x, x5, xF, xMax)`**
#'
#' Parameterises and evaluates `dnormal` from three inputs: the
#' 5% ascending point `x5`, the peak `xF`, and the descending plateau
#' `xMax`. Returns a flat vector of ones if both `x5` and `xF` are zero.
#'
#' **`logistic_50_95(x, x50, x50_95, asymp)`**
#'
#' Standard logistic curve parameterised by the 50% point and the
#' 50-to-95% interval:
#'
#' \deqn{S(x) = \frac{a}{1 + \exp\!\left(-\frac{\ln 19}{x_{50,95}}(x - x_{50})\right)}}
#'
#' where \eqn{a} is `asymp`.
#'
#' @return
#' Each function returns a numeric vector of the same length as the primary
#' input (`x` or `lens`).
#'
#' @seealso
#' [FecundityModels()], [MaturityModels()], [RetentionModels()],
#' [SelectivityModels()], [WeightModels()]
#'
#' @name model-helpers
#' @rdname model-helpers
#' @export
allometric <- function(x, scale, exponent) {
  scale * x ^ exponent
}

#' @rdname model-helpers
#' @export
dnormal <- function(lens, lfs, sl, sr) {
  ifelse(lens <= lfs,
         2 ^ -((lens - lfs) / sl) ^ 2,
         2 ^ -((lens - lfs) / sr) ^ 2
  )
}

#' @rdname model-helpers
#' @export
double_normal <- function(x, x5, xF, xMax) {
  if (all(x5 == 0) && all(xF == 0)) return(rep(1, length(x)))
  
  if (x5 >= xF)
    cli::cli_abort("`x5` ({.val {x5}}) must be less than `xF` ({.val {xF}}).")
  
  if (xF >= max(x))
    cli::cli_abort("`xF` ({.val {xF}}) must be less than `max(x)` ({.val {max(x)}}).")
  
  if (xMax < 0 || xMax > 1)
    cli::cli_abort("`xMax` ({.val {xMax}}) must be in [0, 1].")
  
  sl <- (xF - x5)  / sqrt(-log2(0.05))
  sr <- (max(x) - xF) / sqrt(-log2(xMax))
  sr[!is.finite(sr)] <- Inf
  
  dnormal(x, xF, sl, sr)
}

#' @rdname model-helpers
#' @export
logistic_50_95 <- function(x, x50, x50_95, asymp = 1) {
  asymp / (1 + exp(-log(19) / x50_95 * (x - x50)))
}

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

