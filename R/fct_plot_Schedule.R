
#' Plot At-Age or At-Length Schedules
#' 
#' @method plot Schedule
#' @export
plot.Schedule <- function(x, TimeLab='Year', color='TimeStep',
                          ColorLab=NULL,
                          xlab=NULL,
                          ylab=NULL) {
  
  if (is.null(ylab))
    ylab <- x$Variable |> unique()
  
  x[[color]] <- as.factor(x[[color]])
  
  nColor <- unique(x[[color]]) |> length()
  ColNames <- colnames(x)
  if ("Age" %in% ColNames)
    XVar <- "Age"
  if ("Class" %in% ColNames)
    XVar <- "Class"
  
  if (is.null(xlab))
    xlab <- XVar
  
  nStock <- unique(x$Stock) |> length()
  nFleet <- suppressWarnings(unique(x$Fleet)) |> length()
  
  # TO DO - average over sims
  
  if (nColor<2) {
    p <- ggplot(x, aes(x=.data[[XVar]], y=Value))
  } else {
    p <- ggplot(x, aes(x=.data[[XVar]], y=Value, color=.data[[color]]))
  }
  
  if (nFleet<=1 & nStock>1 & color!='Stock')
    p <- p + facet_wrap(~Stock)
  
  if (nFleet>1 & nStock<=1 & color!='Fleet')
    p <- p + facet_wrap(~Fleet)
  
  if (nFleet>1 & nStock>1) {
    if (color!='Stock' & color!='Fleet') {
      p <- p + facet_grid(Stock~Fleet)
    } else if (color=='Stock') {
      p <- p + facet_wrap(~Fleet)
    } else if (color=='Fleet') {
      p <- p + facet_wrap(~Stock)
    }
  }
  
  if (is.null(ColorLab)) {
    if (color=='TimeStep') {
      ColorLab <- TimeLab
    } else {
      ColorLab <- color 
    }
  }
  
  p <- p + geom_line() +
    expand_limits(y=0) +
    theme_bw() +
    labs(x=xlab, y=ylab,  color=ColorLab)
  
  if (nColor<2)
    p <- p + guides(color='none')
  p
  
}
