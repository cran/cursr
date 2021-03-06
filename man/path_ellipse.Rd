% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/7-Path.R
\name{path_ellipse}
\alias{path_ellipse}
\title{Ellipse Path}
\usage{
path_ellipse(yx = c(0, 0), rx = 1, ry = 1, n = 50)
}
\arguments{
\item{yx}{\code{(row, col)} coordinate of the center of the ellipse}

\item{rx}{radius along the x-axis in grid points}

\item{ry}{radius along the y-axis in grid points}

\item{n}{number of points along curve to calculate}
}
\value{
Nx2 matrix of \code{(row, column)} coordinates
}
\description{
Calculate the path of an ellipse within a grid.
}
\examples{
path_ellipse(yx=c(10,10), rx=8, ry = 4)

}
\seealso{
Other path-fitting functions: 
\code{\link{path_arc}()},
\code{\link{path_bezier}()},
\code{\link{path_circle}()},
\code{\link{path_fill}()},
\code{\link{path_fn}()},
\code{\link{path_intersection}()},
\code{\link{path_lerp}()},
\code{\link{path_ray}()},
\code{\link{path_rect}()},
\code{\link{path_shape}()}
}
\concept{path-fitting functions}
