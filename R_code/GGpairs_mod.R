## Code by: Jacob Herschberger
## Date: September 2025
## Email: j.herschberger@ufl.edu
## Project: Culprits of plant defense variation across a latitudinal gradient.

# Remove specific rows and columns from a GGally ggmatrix (e.g., from ggpairs)
remove_ggmatrix_rows_cols <- function(gm, rows = integer(), cols = integer()) {
  stopifnot(inherits(gm, "ggmatrix"))
  nr <- gm$nrow; nc <- gm$ncol
  
  rows <- sort(unique(as.integer(rows)))
  cols <- sort(unique(as.integer(cols)))
  
  if (length(rows) && (any(rows < 1) || any(rows > nr))) stop("rows out of range")
  if (length(cols) && (any(cols < 1) || any(cols > nc))) stop("cols out of range")
  if (length(rows) >= nr) stop("cannot remove all rows")
  if (length(cols) >= nc) stop("cannot remove all columns")
  if (length(rows) == 0 && length(cols) == 0) return(gm)
  
  keep_rows <- setdiff(seq_len(nr), rows)
  keep_cols <- setdiff(seq_len(nc), cols)
  
  # keep plots by row-major indices based on original nc
  keep_idx <- as.integer(unlist(
    lapply(keep_rows, function(r) (r - 1L) * nc + keep_cols),
    use.names = FALSE
  ))
  gm$plots <- gm$plots[keep_idx]
  
  # update dimensions
  gm$nrow <- length(keep_rows)
  gm$ncol <- length(keep_cols)
  
  # update per-row metadata
  for (nm in c("yAxisLabels", "yProportions", "rowHeights")) {
    if (!is.null(gm[[nm]])) gm[[nm]] <- gm[[nm]][keep_rows]
  }
  # update per-column metadata
  for (nm in c("xAxisLabels", "xProportions", "columnWidths", "columns")) {
    if (!is.null(gm[[nm]])) gm[[nm]] <- gm[[nm]][keep_cols]
  }
  
  gm
}

# example:
# library(GGally)
# g <- ggpairs(iris, columns = 1:4)
# g2 <- remove_ggmatrix_rows_cols(g, rows = c(1, 3), cols = c(2, 4))
# g2