#' Convert Cell Type Names to Colors
#'
#' Converts cell type names to colors.
#' Currently the color palette is hard-coded within the function.
#' Black color is used for undefined and missing cell types.
#'
#' @param x a vector of cell type names.
#'
#' @return a vector of colors for each element in x
#'
#' @examples
#' cells <- c("blood", "pbmc", "bcell", "bn", "neu", "mix", "mono")
#' cell2col(cells)
#'
#' @author Karolis Koncevičius
#' @export
cell2col <- function(x) {
  cols <- c(blood = "tomato3",
            wb    = "tomato3",
            pbmc  = "salmon2",
            gran  = "darkorange1",
            neu   = "#FAA43A",
            mono  = "#DECF3F",
            eos   = "#FFF804",
            baso  = "#C4A484",
            nk    = "#B276B2",
            tcell = "blue",
            tcd4  = "dodgerblue",
            tcd4n = "deepskyblue",
            tcd4m = "dodgerblue",
            tcd8  = "skyblue3",
            tcd8n = "lightblue2",
            tcd8m = "skyblue3",
            treg  = "cyan4",
            bcell = "green3",
            bn    = "lawngreen",
            bm    = "green3"
            )

  x <- tolower(x)
  x <- setNames(cols[x], x)
  x[is.na(x)] <- "black"
  x
}
