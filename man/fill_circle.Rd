% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/8-Draw.R
\name{fill_circle}
\alias{fill_circle}
\title{Draw a Filled-In Circle}
\usage{
fill_circle(yx, r = 1, n = 50, text = "x", ...)
}
\arguments{
\item{yx}{center \code{(row, col)} coordinate}

\item{r}{radius of the circle in grid points}

\item{n}{number of points along curve to calculate}

\item{text}{character value drawn at coordinate}

\item{...}{parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}}
}
\value{
\code{NULL}
}
\description{
Calculate the path of a circle in a grid and draw it to screen.
}
\examples{
draw_circle(yx=c(10,10), r=5)

}
\seealso{
Other drawing functions: 
\code{\link{box_at}()},
\code{\link{draw_arc}()},
\code{\link{draw_bezier}()},
\code{\link{draw_circle}()},
\code{\link{draw_ellipse}()},
\code{\link{draw_fn}()},
\code{\link{draw_lerp}()},
\code{\link{draw_path}()},
\code{\link{draw_ray}()},
\code{\link{draw_rect}()},
\code{\link{draw_shape}()},
\code{\link{fill_ellipse}()},
\code{\link{fill_rect}()},
\code{\link{fill_shape}()},
\code{\link{grid_at}()},
\code{\link{grid_mat}()},
\code{\link{hline_at}()},
\code{\link{hline}()},
\code{\link{vline_at}()},
\code{\link{vline}()}
}
\concept{drawing functions}
