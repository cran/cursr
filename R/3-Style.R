#' Reset Console Style
#'
#' Turns off all text attributes and colors in the terminal.
#'
#' @return \code{NULL}
#' @export
#'
#' @family style functions
#' @examples
#' attr_on("ul")
#' fg_on("red")
#' bg_on(c(10,60,205))
#' cat("Hello World!\n")
#'
#' reset()
#' cat("Hello World!\n")
#'
reset <- function(){cat("\033[0m")}
map_attr <- function(x, on=TRUE){
  if (all(is.na(x))){return("")}
  if (on == TRUE){
    attr_list <- list(
      bf = "\033[1m",
      ft = "\033[2m",
      it = "\033[3m",
      ul = "\033[4m",
      sb = "\033[5m",
      fb = "\033[6m",
      rv = "\033[7m",
      st = "\033[9m"
    )
  } else {
    attr_list <- list(
      bf = "\033[21m",
      ft = "\033[22m",
      it = "\033[23m",
      ul = "\033[24m",
      sb = "\033[25m",
      fb = "\033[26m",
      rv = "\033[27m",
      st = "\033[29m"
    )
  }
  paste(sapply(x, function(x){attr_list[[x]]}), collapse="")
}
#' Attributes On
#'
#' Turns on text attributes in terminal, including bold text, italics, underline, etc. Note that not all terminals support each attribute.
#'
#' Use \code{\link{attr_off}} to turn off the attributes.
#'
#' @param ... characters indicating attributes to turn on. \code{"bf"} for bold face; \code{"ft"} for faint; \code{"it"} for italics; \code{"ul"} for underline; \code{"sb"} for slow blink; \code{"fb"} for fast blink; \code{"rv"} for reverse video (invert bg and fg colors); \code{"st"} for strike-through
#'
#' @return \code{NULL}
#' @export
#'
#' @family style functions
#' @examples
#' cat("hello world!\n")
#' attr_on("bf", "ul")
#' cat("hello world!\n")
#' attr_off()
#'
attr_on <- function(...){cat(paste(map_attr(unlist(eval(substitute(alist(...)))), on=TRUE), collapse=""))}
#' Attributes Off
#'
#' Turns off text attributes in the terminal, including bold text, italics, underline, etc.
#'
#' Use \code{\link{attr_on}} to turn on attributes.
#'
#' @param ... characters indicating attributes to turn off. \code{"bf"} for bold face; \code{"ft"} for faint; \code{"it"} for italics; \code{"ul"} for underline; \code{"sb"} for slow blink; \code{"fb"} for fast blink; \code{"rv"} for reverse video (invert bg and fg colors); \code{"st"} for strike-through. All attributes are turned off if left blank.
#'
#' @return \code{NULL}
#' @export
#'
#' @family style functions
#' @examples
#' cat("hello world!\n")
#' attr_on("bf", "ul")
#' cat("hello world!\n")
#' attr_off("bf")
#' cat("hello world!\n")
#' attr_off()
#' cat("hello world!\n")
#'
attr_off <- function(...){
  x <- unlist(eval(substitute(alist(...))))
  if (is.null(x)){cat("\033[21m\033[22m\033[23m\033[24m\033[25m\033[27m\033[29m")
  } else {cat(paste(map_attr(x, on=FALSE), collapse=""))}
}
map_color <- function(x, type="fg"){
  UseMethod("map_color")
}
map_color.character <- function(x, type="fg"){
  y <- substr(x[1], 1, 1)
  if (y == "#"){
    z <- sapply(1:3, function(i){
      start <- 2*i
      as.numeric(as.hexmode(substr(x, start, start+1)))
    })
    if (type == "fg"){return(paste0("\033[38;2;", z[1], ";", z[2], ';', z[3], 'm'))
    } else {return(paste0("\033[48;2;", z[1], ";", z[2], ';', z[3], 'm'))}
  } else {
    z <- switch(x[1],
                black = 30,
                red = 31,
                green = 32,
                yellow = 33,
                blue = 34,
                magenta = 35,
                cyan = 36,
                white = 37)
    if (type == 'fg'){return(paste0("\033[",z, 'm'))
    } else {return(paste0("\033[",z+10, 'm'))}
  }
}
map_color.numeric <- function(x, type="fg"){
  if (length(x) > 2){
    if (type == 'fg'){return(paste0("\033[38;2;", x[1], ';', x[2], ';', x[3], 'm'))
    } else {return(paste0("\033[48;2;", x[1], ';', x[2], ';', x[3], 'm'))}
  } else {
    if (type == 'fg'){return(paste0("\033[38;5;", x[1], 'm'))
    } else {return(paste0("\033[48;5;", x[1], 'm'))}
  }
}
map_color.logical <- function(x, type='fg'){""}
map_color.NULL <- function(x, type='fg'){""}
input_color <- function(..., type = 'fg'){
  x <- unlist(eval(substitute(alist(...))))
  if (is.list(x)){x <- eval(x[[1]])}
  map_color(x, type=type)
}

#' Turn On Foreground Color
#'
#' Specifies the color of all future text written in the terminal \code{fg_on} accepts numeric values (RGB or 8-bit color code), hexadecimal characters, or the name of the color. Not all terminals support each possible color.
#'
#' Foreground color is turned off with \code{\link{fg_off}}.
#'
#' @param ... character or numeric value
#'
#' @return \code{NULL}
#' @export
#'
#' @family style functions
#' @examples
#' # Different methods of specifying red
#' fg_on("red")
#' fg_on("#FF0000")
#' fg_on(9)
#' fg_on(255, 0, 0)
#'
#' # Turn off color
#' fg_off()
#'
fg_on <- function(...){cat(input_color(..., type='fg'))}
#' Turn On Background Color
#'
#' Specifies the background color of all future text written in the terminal \code{bg_on} accepts numeric values (RGB or 8-bit color code), hexadecimal characters, or the name of the color. Not all terminals support each possible color.
#'
#' Background color is turned off with \code{\link{bg_off}}.
#'
#' @param ... character or numeric value
#'
#' @return \code{NULL}
#' @export
#'
#' @family style functions
#' @examples
#' # Different methods of specifying yellow
#' bg_on("yellow")
#' bg_on("#FFFF00")
#' bg_on(11)
#' bg_on(255, 255, 0)
#'
#' # Turn off color
#' bg_off()
#'
bg_on <- function(...){cat(input_color(..., type='bg'))}
#' Turn Off Foreground Color
#'
#' Return future terminal text to the default color. Foreground color is turned on with \code{\link{fg_on}}.
#'
#' @return \code{NULL}
#' @export
#'
#' @family style functions
#' @examples
#' # Different methods of specifying red
#' fg_on("red")
#' fg_on("#FF0000")
#' fg_on(1)
#' fg_on(255, 0, 0)
#'
#' # Turn off color
#' fg_off()
#'
fg_off <- function(){cat("\033[39m")}
#' Turn Off Background Color
#'
#' Return the background of future terminal text to the default color. Background color is turned on with \code{\link{bg_on}}.
#'
#' @return \code{NULL}
#' @export
#'
#' @family style functions
#' @examples
#' # Different methods of specifying yellow
#' bg_on("yellow")
#' bg_on("#FFFF00")
#' bg_on(11)
#' bg_on(255, 255, 0)
#'
#' # Turn off color
#' bg_off()
#'
bg_off <- function(){cat("\033[49m")}
#' Turn Off Colors in Terminal
#'
#' Return the background and foreground of future terminal text to the default colors.
#'
#' @return \code{NULL}
#' @export
#'
#' @family style functions
#' @examples
#' bg_on("red")
#' fg_on("yellow")
#'
#' # Turn off color
#' color_off()
#'
color_off <- function(){cat("\033[39m\033[49m")}
#' Create Foreground Color
#'
#' Returns the ANSI code for the specified foreground color. \code{make_fg} accepts numeric values (RGB or 8-bit color code), hexadecimal characters, or the name of the color.
#'
#' @param ... character or numeric value
#'
#' @return ANSI character string
#' @export
#'
#' @family style functions
#' @examples
#' # Different methods of specifying red
#' make_fg("red")
#' make_fg("#FF0000")
#' make_fg(9)
#' make_fg(255, 0, 0)
#'
make_fg <- function(...){input_color(..., type='fg')}
#' Create Background Color
#'
#' Returns the ANSI code for the specified background color. \code{make_bg} accepts numeric values (RGB or 8-bit color code), hexadecimal characters, or the name of the color.
#'
#' @param ... character or numeric value
#'
#' @return ANSI character string
#' @export
#'
#' @family style functions
#' @examples
#' # Different methods of specifying cyan
#' make_bg("cyan")
#' make_bg("#00FFFF")
#' make_bg(14)
#' make_bg(0, 255, 255)
#'
make_bg <- function(...){input_color(..., type='bg')}
#' Create Background & Foreground Color Combination
#'
#' Returns the ANSI codes for the specified colors. \code{color_pair} accepts numeric values (RGB or 8-bit color code), hexadecimal characters, or the name of the color.
#'
#' @param fg character or numeric value for the foreground color
#' @param bg character or numeric value for the background color
#'
#' @return ANSI character string
#' @export
#'
#' @family style functions
#' @examples
#' # Blue background with white text
#' color_pair("white", "blue")
#' color_pair("#FFFFFF", "#0000FF")
#' color_pair(0, 12)
#' color_pair(c(255, 255, 255), c(0,0,255))
#'
color_pair <- function(fg, bg){paste0(map_color(fg, type='fg'), map_color(bg, type='bg'))}
#' Create Color & Attribute Style
#'
#' Returns the ANSI codes for the specified colors and text attributes.
#'
#' @param fg character or numeric value for the foreground color. See \code{\link{fg_on}} for more details.
#' @param bg character or numeric value for the background color. See \code{\link{bg_on}} for more details.
#' @param attr character vector describing attributes to turn on. See \code{\link{attr_on}} for more details.
#'
#' @return ANSI character string
#' @export
#'
#' @family style functions
#' @examples
#' cat(make_style(fg="blue", bg=c(192,192,192), attr=c("ul", "st")))
#' cat("Hello World!\n")
#' reset()
#'
make_style <- function(fg = NA, bg = NA, attr = NA){
  fg <- map_color(fg, type='fg')
  bg <- map_color(bg, type='bg')
  attr <- map_attr(attr)
  gsub(" ", "", paste(fg, bg, paste(attr, collapse=""), collapse=""))
}
#' Add Color & Attributes to a Character
#'
#' Add color and other text attributes to a character vector. Attributes can be seen after text is passed to \code{cat}, though it may only show up in a terminal. Note that terminal attributes and colors are automatically reset to default after text is printed.
#'
#' @param x character vector to be styled
#' @param fg character or numeric value for the foreground color. See \code{\link{fg_on}} for more details.
#' @param bg character or numeric value for the background color. See \code{\link{bg_on}} for more details.
#' @param attr character vector describing attributes to turn on. See \code{\link{attr_on}} for more details.
#'
#' @return character vector
#' @export
#'
#' @family style functions
#' @examples
#' x <- style("Hello World!\n", fg="blue", bg=c(192,192,192), attr=c("ul", "st"))
#' cat(paste(x, "It is nice to meet you!"))
#'
style <- function(x, fg = NA, bg = NA, attr = NA){
  val <- make_style(fg=fg, bg=bg, attr=attr)
  return(paste0(val, x, "\033[0m"))
}
