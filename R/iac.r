#' Inter-Array Correlation
#'
#' Inter-Array Correlation (IAC) method for outlier detection.
#' The idea behind IAC is to measure the average correlation between each
#' observation and the rest of the dataset and then mark the observations with
#' low average correlation as potential outliers.
#'
#' Technically the function first calculates the average correlations for each
#' available observation and then transforms those correlations into z-scores.
#' Only rows with no missing values are used for estimating the correlation
#' coefficients. For the next iteration all the z-scores below the specified
#' SD threshold will not be included in the calculations of mean and standard
#' deviation of average correlation coefficients. However, the actual average
#' correlation for those observations will still be transformed into a z-score,
#' reflecting the distance (in standard deviations) from the rest of the group.
#'
#' When `groups` argument is provided the IAC procedure is performed for each
#' group separately. When no observations fall below the specified SD threshold
#' the z-score values returned by IAC will be the same as in returned by the
#' previous iteration.
#'
#' @param x a matrix where features are represented by rows and observations by columns.
#' @param groups an optional vector of group membership for each column of \code{x}.
#' @param sdcutoff standard deviation cutoff used for outlier detection (default = -3).
#' @param niter number of iterations (default = 3).
#'
#' @return a matrix of IAC distances with iterations listed in rows.
#'
#' @examples
#' # a toy dataset
#' dat <- data.matrix(t(iris[,-5]))
#'
#' # 10 iterations with sd cutoff of -2
#' res <- iac(dat, sdcutoff = -2, niter = 10)
#'
#' # only the iterations that found additional outliers
#' unique(res)
#'
#' @author Karolis Koncevičius
#' @export
iac <- function(x, groups, sdcutoff = -3, niter = 3) {
  if(missing(groups)) groups <- rep("all", ncol(x))
  dists <- matrix(nrow = niter, ncol = ncol(x))
  rownames(dists) <- paste0("iteration", 1:nrow(dists))
  colnames(dists) <- colnames(x)
  for(g in unique(na.omit(groups))) {
    inds <- which(groups == g)
    cors <- cor(x[, inds], use = "complete.obs")
    diag(cors) <- NA
    outs <- FALSE
    for(i in 1:niter) {
      means <- colMeans(cors[!outs,], na.rm = TRUE)
      dists[i, inds] <- (means - mean(means[!outs])) / sd(means[!outs])
      outs  <- dists[i, inds] < sdcutoff
    }
  }
  dists
}
