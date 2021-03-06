% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2-Move.R
\name{mv_to}
\alias{mv_to}
\title{Move Cursor to Specified Location}
\usage{
mv_to(row = 1L, col = 1L)
}
\arguments{
\item{row}{positive integer specifying the console row. If \code{row} has two or more values, the second value replaces \code{col}.}

\item{col}{positive integer specifying the console column.}
}
\value{
\code{NULL}
}
\description{
Move cursor relative to its current position on the screen. Screen coordinates are given by \code{(row, column)} with the position of the screen being \code{(1, 1)}.
}
\details{
The user must be in a terminal to use the functionality; it will not work in RStudio or the R GUI.
}
\examples{
# move the cursor to the 2nd row, 4th column
mv_to(2, 4)

# alternatively, you can specify the coordinates as a vector.
loc <- c(2, 4)
mv_to(loc)

}
\seealso{
\code{\link{mv}} to move relative to the current location on the screen.

Other moving functions: 
\code{\link{mv_col}()},
\code{\link{mv_row}()},
\code{\link{mv}()}
}
\concept{moving functions}
