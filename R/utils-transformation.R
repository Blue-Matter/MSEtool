#' Logit and Inverse Logit (Sigmoid) Functions
#'
#' `logit()` transforms a probability to the log-odds scale. `ilogit()` is its
#' inverse — the sigmoid function — mapping any real number back to \[0, 1\].
#'
#' @param p A numeric vector of probabilities. Values must be in the open
#'   interval (0, 1); `NaN` is returned for values outside this range.
#' @param x A numeric vector of real-valued log-odds.
#'
#' @return
#' - `logit(p)` returns a numeric vector of log-odds in (-Inf, Inf).
#' - `ilogit(x)` returns a numeric vector of probabilities in (0, 1).
#'
#' @details
#' The two functions are mutual inverses:
#'
#' \deqn{\text{logit}(p) = \log\!\left(\frac{p}{1-p}\right)}
#' \deqn{\text{ilogit}(x) = \frac{1}{1 + e^{-x}}}
#'
#' so that `ilogit(logit(p)) == p` and `logit(ilogit(x)) == x` (up to
#' floating-point precision).
#'
#' @examples
#' logit(0.5)   # 0
#' logit(0.9)   # ~2.197
#'
#' ilogit(0)    # 0.5
#' ilogit(2)    # ~0.880
#'
#' # Round-trip
#' p <- c(0.1, 0.5, 0.9)
#' all.equal(p, ilogit(logit(p)))  # TRUE
#'
#' @name logit
#' @aliases ilogit
#' @export 
logit <- function(p) {
  log(p / (1 - p))
}

#' @rdname logit
#' @export 
ilogit <- function(x) {
  1 / (1 + exp(-x))
}
#' Multinomial inverse logit (softmax)
#'
#' Maps a vector of real values, or each row of a matrix, to proportions that
#' sum to 1.
#'
#' @param x A numeric vector, or a matrix whose rows are transformed
#'   independently.
#' @return An object of the same shape as `x`, summing to 1 overall (vector) or
#'   by row (matrix).
#' @keywords internal
ilogitm <- function(x) {
  if (inherits(x, "matrix")) return(exp(x) / apply(exp(x), 1, sum))
  exp(x) / sum(exp(x))
}

#' Lognormal moment conversion
#'
#' Convert a mean and standard deviation expressed in normal space to the
#' parameters of the corresponding lognormal distribution.
#'
#' @param m Mean in normal space.
#' @param sd Standard deviation in normal space.
#' @author T. Carruthers
#' @return numeric
#' @describeIn sdconv Returns sigma of the lognormal distribution
#' @keywords internal
#' @export
sdconv <- function(m, sd) (log(1 + ((sd^2)/(m^2))))^0.5

#' @describeIn sdconv Returns mu of the lognormal distribution
#' @export
mconv <- function(m, sd) log(m) - 0.5 * log(1 + ((sd^2)/(m^2)))

# Small positive constant used to keep divisions and log-scale arithmetic
# finite where a quantity may legitimately be zero.
tiny <- 1e-15
