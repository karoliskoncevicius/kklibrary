#' Add an oscillation curve
#'
#' Adds a cosine-based oscillation curve to an existing plot.
#'
#' @param x0 a numeric value specifying the starting x-coordinate.
#' @param x1 a numeric value specifying the ending x-coordinate.
#' @param osc a named list or vector containing oscillation parameters:
#'   `mesor`, `acrophase`, `amplitude`, and `period`.
#' @param ... additional parameters passed to \code{\link[graphics]{lines}}.
#'
#' @details
#' The curve is based on 1000 points placed between \code{x0} and \code{x1}.
#'
#' Requried oscillation parameters:\cr
#' 1. mesor - mean value of the curve.\cr
#' 2. acrophase - timing of the oscillation peak (maximum).\cr
#' 3. amplitude - half the difference between peak and trough.\cr
#' 4. period - length of one complete cycle.
#'
#' @examples
#' plot.new()
#' plot.window(xlim = c(0,24), ylim = c(0,10))
#' grid()
#' osc_params <- list(mesor = 5, amplitude = 2, acrophase = 6, period = 24)
#' oscline(0, 24, osc_params, col = "blue", lwd = 2)
#'
#' @author Karolis Koncevičius
#' @export
oscline <- function(x0, x1, osc, ...) {

  missing <- setdiff(c("mesor", "acrophase", "amplitude", "period"), names(osc))
  if (length(missing) > 0) {
    stop("Missing required parameters: ", paste(missing, collapse = ", "))
  }

  mesor     <- osc[["mesor"]]
  acrophase <- osc[["acrophase"]]
  amplitude <- osc[["amplitude"]]
  period    <- osc[["period"]]

  x <- seq(x0, x1, length.out = 1000)
  y <- mesor + amplitude * cos(2 * pi * (x - acrophase) / period)
  lines(x, y, ...)
}

