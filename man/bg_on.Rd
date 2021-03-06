% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3-Style.R
\name{bg_on}
\alias{bg_on}
\title{Turn On Background Color}
\usage{
bg_on(...)
}
\arguments{
\item{...}{character or numeric value}
}
\value{
\code{NULL}
}
\description{
Specifies the background color of all future text written in the terminal \code{bg_on} accepts numeric values (RGB or 8-bit color code), hexadecimal characters, or the name of the color. Not all terminals support each possible color.
}
\details{
Background color is turned off with \code{\link{bg_off}}.
}
\examples{
# Different methods of specifying yellow
bg_on("yellow")
bg_on("#FFFF00")
bg_on(11)
bg_on(255, 255, 0)

# Turn off color
bg_off()

}
\seealso{
Other style functions: 
\code{\link{attr_off}()},
\code{\link{attr_on}()},
\code{\link{bg_off}()},
\code{\link{color_off}()},
\code{\link{color_pair}()},
\code{\link{fg_off}()},
\code{\link{fg_on}()},
\code{\link{make_bg}()},
\code{\link{make_fg}()},
\code{\link{make_style}()},
\code{\link{reset}()},
\code{\link{style}()}
}
\concept{style functions}
