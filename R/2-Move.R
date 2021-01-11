#' Move Cursor
#'
#' Move cursor relative to its current position on the screen. Screen coordinates are given by \code{(row, column)} with the position of the screen being \code{(1, 1)}.
#'
#' The user must be in a terminal to use the functionality; it will not work in RStudio or the R GUI.
#'
#' @param row number of rows in which to move the cursor. Positive values move the cursor down; negative values move the cursor up. If \code{row} has two or more values, the second value replaces \code{col}.
#' @param col number of columns in which to move the cursor. Positive values move the cursor forward; negative values move the cursor backwards.
#'
#' @return \code{NULL}
#' @seealso \code{\link{mv_to}} to move to a specific location on the screen.
#' @export
#'
#' @family moving functions
#' @examples
#' # move the cursor down one and forward two
#' mv(1, 2)
#'
#' # Alternatively, you can specify the coordinates as a single vector.
#' loc <- c(1, 2)
#' mv(loc)
#'
#' # to move to the left one unit (only works if the current column is > 1)
#' mv(, -1)
#'
mv <- function(row=0L, col=0L){
  if (in.term()){
    if (length(row) > 1){col <- row[2]; row <- row[1]
    } else {col <- col[1]; row <- row[1]}
    z <- ""
    if (row > 0){z <- paste0(z, "\033[", row, 'B')
    } else if (row < 0){z <- paste0(z, "\033[", abs(row), "A")}
    if (col > 0){z <- paste0(z, "\033[", col, "C")
    } else if (col < 0){z <- paste0(z, "\033[", abs(col), "D")}
    cat(z)
  }
}
#' Move Cursor to Specified Location
#'
#' Move cursor relative to its current position on the screen. Screen coordinates are given by \code{(row, column)} with the position of the screen being \code{(1, 1)}.
#'
#' The user must be in a terminal to use the functionality; it will not work in RStudio or the R GUI.
#'
#' @param row positive integer specifying the console row. If \code{row} has two or more values, the second value replaces \code{col}.
#' @param col positive integer specifying the console column.
#'
#' @return \code{NULL}
#' @seealso \code{\link{mv}} to move relative to the current location on the screen.
#' @export
#'
#' @family moving functions
#' @examples
#' # move the cursor to the 2nd row, 4th column
#' mv_to(2, 4)
#'
#' # alternatively, you can specify the coordinates as a vector.
#' loc <- c(2, 4)
#' mv_to(loc)
#'
mv_to <- function(row=1L,col=1L){
  if (in.term()){
    if (length(row) > 1){col <- row[2]; row <- row[1]
    } else {col <- col[1]; row <- row[1]}
    loc <- abs(round(c(row, col)))
    cat(paste0("\033[", loc[1], ';', loc[2], 'H'))
  }
}
#' Move Cursor to Row
#'
#' Moves cursor to the beginning of the row relative to its current location.
#'
#' The user must be in a terminal to use the functionality; it will not work in RStudio or the R GUI.
#'
#' @param n number of rows to move. Positive values indicate the next rows; negative values indicate the previous rows
#'
#' @return \code{NULL}
#' @export
#'
#' @family moving functions
#' @examples
#' # move the cursor to the beginning of the previous line
#' mv_row(-1)
#'
mv_row <- function(n=1L){
  if (in.term()){
    if (n > 0){cat(paste0("\033[", n[1], 'E'))
    } else if (n < 0){cat(paste0("\033[", n[1], 'F'))}
  }
}
#' Move Cursor to Column
#'
#' Move the cursor to the specified column, while maintaining the same row.
#'
#' The user must be in a terminal to use the functionality; it will not work in RStudio or the R GUI.
#'
#' @param n positive integer specifying the column
#'
#' @return \code{NULL}
#' @export
#'
#' @family moving functions
#' @examples
#' # Position cursor at the beginning of the row
#' mv_col(1)
#'
#' # Move cursor to the 10th column in the row
#' mv_col(10)
#'
mv_col <- function(n=1L){if (in.term()){cat(paste0("\033[", abs(n[1]), 'G'))}}
