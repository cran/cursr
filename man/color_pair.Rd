% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3-Style.R
\name{color_pair}
\alias{color_pair}
\title{Create Background & Foreground Color Combination}
\usage{
color_pair(fg, bg)
}
\arguments{
\item{fg}{character or numeric value for the foreground color}

\item{bg}{character or numeric value for the background color}
}
\value{
ANSI character string
}
\description{
Returns the ANSI codes for the specified colors. \code{color_pair} accepts numeric values (RGB or 8-bit color code), hexadecimal characters, or the name of the color.
}
\examples{
# Blue background with white text
color_pair("white", "blue")
color_pair("#FFFFFF", "#0000FF")
color_pair(0, 12)
color_pair(c(255, 255, 255), c(0,0,255))

}
\seealso{
Other style functions: 
\code{\link{attr_off}()},
\code{\link{attr_on}()},
\code{\link{bg_off}()},
\code{\link{bg_on}()},
\code{\link{color_off}()},
\code{\link{fg_off}()},
\code{\link{fg_on}()},
\code{\link{make_bg}()},
\code{\link{make_fg}()},
\code{\link{make_style}()},
\code{\link{reset}()},
\code{\link{style}()}
}
\concept{style functions}
