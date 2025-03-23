#' Convert Sex Labels to Colors
#'
#' Converts sex labels to colors.
#' Currently the color palette is hard-coded within the function.
#' Gray color is used for undefined and missing labels.
#'
#' @param x a vector of sex labels.
#'
#' @return a vector of colors for each element in x
#'
#' @examples
#' sex <- c("F", "M", "Female", "Male", "unknown")
#' sex2col(sex)
#'
#' @author Karolis Koncevičius
#' @export
sex2col <- function(x) {
  if(is.null(x)) return(NULL)
  cols <- c(m      = "cornflowerblue",
            male   = "cornflowerblue",
            man    = "cornflowerblue",
            f      = "pink",
            female = "pink",
            woman  = "pink"
            )

  x <- tolower(x)
  x <- setNames(cols[x], x)
  x[is.na(x)] <- "gray"
  x
}
