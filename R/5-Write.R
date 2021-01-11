#' Write Character to Terminal
#'
#' Writes a single character to the terminal at the current cursor position. \code{wr} accepts text colors and attributes, but these are reset to default afterwards if used.
#'
#' @param chr character to be printed to the Console
#' @param fg foreground color. See \code{\link{fg_on}} for more details.
#' @param bg background color. See \code{\link{bg_on}} for more details.
#' @param attr character attribute. See \code{\link{attr_on}} for more details.
#'
#' @return \code{NULL}
#' @export
#'
#' @family writing functions
#' @examples
#' mv_to(5,4)
#' wrch("h")
#' wrch("e", fg="red")
#' wr("llo World")
#'
wrch <- function(chr, fg=NA, bg=NA, attr=NA){
  chr <- substr(chr[1], 1, 1)
  if (!is.na(fg)){chr <- paste0(map_color(fg, type='fg'), chr, "\033[39m")}
  if (!is.na(bg)){chr <- paste0(map_color(bg, type='bg'), chr, "\033[49m")}
  if (!any(is.na(attr))){chr <- paste0(map_attr(attr), chr, "\033[21m\033[22m\033[23m\033[24m\033[25m\033[27m\033[29m")}
  cat(chr)
}
#' Write String to Terminal
#'
#' Writes a string of characters to the terminal at the current cursor position. \code{wr} accepts text colors and attributes, but these are reset to default afterwards if used.
#'
#' @param text string to be printed to the Console
#' @param fg foreground color. See \code{\link{fg_on}} for more details.
#' @param bg background color. See \code{\link{bg_on}} for more details.
#' @param attr character attribute. See \code{\link{attr_on}} for more details.
#'
#' @return \code{NULL}
#' @export
#'
#' @family writing functions
#' @examples
#' mv_to(5,4)
#' wrch("h")
#' wrch("e", fg="red")
#' wr("llo World")
#'
wr <- function(text, fg=NA, bg=NA, attr=NA){
  text <- paste(text, collapse="")
  if (!is.na(fg)){text <- paste0(map_color(fg, type='fg'), text, "\033[39m")}
  if (!is.na(bg)){text <- paste0(map_color(bg, type='bg'), text, "\033[49m")}
  if (!any(is.na(attr))){text <- paste0(map_attr(attr), text, "\033[21m\033[22m\033[23m\033[24m\033[25m\033[27m\033[29m")}
  cat(text)
}
#' Echo Keypress to Screen
#'
#' Detect keypress and print it to the terminal screen, while invisibly returning the keypress. The user can specify which characters to ignore, and can also map keys to a list of functions. Any keypress mapped to a function will not be echoed to the screen.
#'
#' @param ignore vector of keypresses to ignore.
#' @param fn list of functions, named by key, to be called when key is pressed.
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @import keypress
#' @export
#'
#' @family writing functions
#' @examples
#' \dontrun{
#' wrkp(
#'  ignore="escape",
#'  fn = list(
#'    enter = function(){mv_row(1)},
#'    left = function(){mv(0, -1)},
#'    right = function(){mv(0, 1)},
#'    up = function(){mv(-1,0)},
#'    down = function(){mv(1,0)},
#'    space = function(){cat(" ")}
#'  )
#' )
#' }
#'
wrkp <- function(ignore = 'escape', fn = list(), ...){
  if (in.term()){
    kp <- keypress()
    if (kp %in% names(fn)){fn[[kp]]()
    } else if (!(kp %in% ignore)) {wrch(kp, ...)}
    return(invisible(kp))
  }
}
#' Echo Keypress to Screen in a Loop
#'
#' Detect keypress and print it to the terminal screen, while invisibly returning the keypress. The user can specify which characters to ignore, and can also map keys to a list of functions. Any keypress mapped to a function will not be echoed to the screen.
#'
#' @param escape vector of keypresses to escape the loop.
#' @param ignore vector of keypresses to ignore.
#' @param fn list of functions, named by key, to be called when key is pressed.
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @import keypress
#' @export
#'
#' @family writing functions
#' @examples
#' \dontrun{
#' wrkpl(
#'   escape = "escape",
#'   fn = list(
#'     enter = function(){mv_row(1)},
#'     left = function(){mv(0, -1)},
#'     right = function(){mv(0, 1)},
#'     up = function(){mv(-1,0)},
#'     down = function(){mv(1,0)},
#'     space = function(){cat(" ")}
#'   )
#' )
#' }
#'
wrkpl <- function(escape = c("escape"), ignore = NA_character_, fn = list(), ...){
  if (in.term()){
    while(TRUE){
      kp <- keypress()
      if (kp %in% escape){break;
      } else if (kp %in% names(fn)){fn[[kp]]()
      } else if (!(kp %in% ignore)){wrch(kp, ...)
      }
    }
  }
}
#' Write At a Specific Location
#'
#' Move cursor to specified location in the terminal screen, then print the supplied text. This function will only work in terminal, not the RStudio Console or R GUI.
#'
#' The coordinates are given in matrix notation: \code{(row, column)}, with the top-left corner of the screen being \code{(1,1)}.
#'
#' @param yx numeric vector specifying the \code{(row, col)} coordinates to print at
#' @param text text to be written at \code{yx}
#' @param ... colors and attributes added to text. See \code{\link{wr}}, \code{\link{fg_on}}, \code{\link{bg_on}}, and \code{\link{attr_on}} for more details.
#'
#' @return \code{NULL}
#' @export
#'
#' @family writing functions
#' @examples
#' wrat(c(10,6), "CURSR")
#' wrat(c(4,1), "Hello World!", fg="red", attr=c("bf", "ul"))
#'
#' mat <- rbind(c(5,2), c(10,5), c(1,19))
#' wrat(mat, "HI", fg="yellow")
#'
wrat <- function(yx, text, ...){UseMethod("wrat")}
#' @method wrat numeric
#' @export
wrat.numeric <- function(yx, text, ...){
  if (in.term()){
    mv_to(abs(yx[1]), abs(yx[2]))
    wr(text, ...)
  }
}
#' @method wrat matrix
#' @export
wrat.matrix <- function(yx, text=NA, ...){
  if (in.term()){
    if (!is.na(text)){yx <- cbind(yx, text)}
    invisible(sapply(1:nrow(yx), function(i){
      mv_to(abs(as.numeric(yx[i,1])), abs(as.numeric(yx[i,2])))
      wr(yx[i,3], ...)
    }))
  }
}
#' @method wrat data.frame
#' @export
wrat.data.frame <- function(yx, text=NA, ...){
  if (in.term()){
    if (!is.na(text)){yx <- cbind(yx, text)}
    invisible(sapply(1:nrow(yx), function(i){
      mv_to(abs(yx[i,1]), abs(yx[i,2]))
      wr(yx[i,3], ...)
    }))
  }
}
#' Write Character to Terminal at Specified Location
#'
#' Move cursor to specified location in the terminal screen, then print the supplied character. This function will only work in terminal, not the RStudio Console or R GUI.
#'
#' The coordinates are given in matrix notation: \code{(row, column)}, with the top-left corner of the screen being \code{(1,1)}.
#'
#' @param row row in which character is printed. If length of \code{row} is greater than one, the second value replaces \code{col}.
#' @param col column in which character is printed
#' @param chr character to be printed to the Console
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#' wrchat(5, 4, "h")
#'
wrchat <- function(row, col, chr, ...){
  if (in.term()){
    if (length(row) > 1){
      if (missing(col)){col <- row[2]
      } else if (missing(text)){
        text <- col
        col <- row[2]
      }
    }
    mv_to(row[1], col[1])
    wrch(chr, ...)
  }
}
#' Repeat a Character
#'
#' Repeat a character \code{n} times and concatenate into a single value.
#'
#' @param x character to be repeated
#' @param n number of times to be repeated
#'
#' @return character vector
#' @export
#'
#' @examples
#' repch("abc", 5)
#'
repch <- function(x, n){
  return(paste(rep(x,n),collapse=""))
}
