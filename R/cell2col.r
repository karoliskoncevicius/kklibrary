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
  cols <- c(blood = "darkred",
            wb    = "darkred",
            pbmc  = "tomato3",
            gran  = "darkorange1",
            neu   = "#FAA43A",
            mono  = "#DECF3F",
            eos   = "#FFF804",
            baso  = "#C4A484",
            nk    = "mediumvioletred",
            tcell = "blue",
            tcd4  = "dodgerblue",
            tcd4n = "deepskyblue",
            tcd4m = "steelblue",
            treg  = "skyblue",
            tcd8  = "purple",
            tcd8n = "mediumpurple1",
            tcd8m = "slateblue3",
            bcell = "green3",
            bn    = "lightgreen",
            bm    = "#60BD68"
            )

  x <- tolower(x)
  x <- setNames(cols[x], x)
  x[is.na(x)] <- "black"
  x
}
