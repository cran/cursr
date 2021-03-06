% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3-Style.R
\name{attr_off}
\alias{attr_off}
\title{Attributes Off}
\usage{
attr_off(...)
}
\arguments{
\item{...}{characters indicating attributes to turn off. \code{"bf"} for bold face; \code{"ft"} for faint; \code{"it"} for italics; \code{"ul"} for underline; \code{"sb"} for slow blink; \code{"fb"} for fast blink; \code{"rv"} for reverse video (invert bg and fg colors); \code{"st"} for strike-through. All attributes are turned off if left blank.}
}
\value{
\code{NULL}
}
\description{
Turns off text attributes in the terminal, including bold text, italics, underline, etc.
}
\details{
Use \code{\link{attr_on}} to turn on attributes.
}
\examples{
cat("hello world!\n")
attr_on("bf", "ul")
cat("hello world!\n")
attr_off("bf")
cat("hello world!\n")
attr_off()
cat("hello world!\n")

}
\seealso{
Other style functions: 
\code{\link{attr_on}()},
\code{\link{bg_off}()},
\code{\link{bg_on}()},
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
