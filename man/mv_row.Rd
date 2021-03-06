% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2-Move.R
\name{mv_row}
\alias{mv_row}
\title{Move Cursor to Row}
\usage{
mv_row(n = 1L)
}
\arguments{
\item{n}{number of rows to move. Positive values indicate the next rows; negative values indicate the previous rows}
}
\value{
\code{NULL}
}
\description{
Moves cursor to the beginning of the row relative to its current location.
}
\details{
The user must be in a terminal to use the functionality; it will not work in RStudio or the R GUI.
}
\examples{
# move the cursor to the beginning of the previous line
mv_row(-1)

}
\seealso{
Other moving functions: 
\code{\link{mv_col}()},
\code{\link{mv_to}()},
\code{\link{mv}()}
}
\concept{moving functions}
