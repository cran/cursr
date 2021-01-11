#' Determine Terminal Size
#'
#' Function determines the size of the terminal in number of rows and columns. The value may not be accurate in RStudio or the R GUI.
#'
#' @return numeric vector (# of rows, # of columns)
#' @export
#'
#' @examples
#' term_dim()
#'
term_dim <- function(){
  as.numeric(c(system("tput lines", intern=TRUE),
               system("tput cols", intern=TRUE)))
}
#' Determine whether in Terminal
#'
#' Tests whether the session is in terminal and returns TRUE or FALSE. Many of the \code{cursr} functions require being in terminal and will not work with RStudio or the R GUI application.
#'
#' @return logical value; \code{TRUE} or \code{FALSE}
#' @export
#'
#' @examples
#' in.term()
#'
in.term <- function(){isatty(stdin())}
#' Clear Text
#'
#' Clear text from the terminal. Passing values \code{"start"} or \code{"end"} allow the user to clear specific portions of the screen relative to the cursor.
#'
#' @param x character describing console location to clear. The default, \code{"screen"}, clears the entire screen; \code{"start"} clears all text from the beginning of the screen until the cursor's position; \code{"end"} clears all text from the cursor's position to the bottom of the screen.
#' @param ... objects passed to/from methods
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#' clear()
#'
#' cat(paste(LETTERS[1:10], collapse="\n"))
#'
#' clear("start")
#' clear("end")
#'
clear <- function(x = c("screen", "end", "start"), ...){
  if (x[1] == "screen"){cat("\033[2J\033[0;0H")
  } else if (x[1] == "end"){cat("\033[0J")
  } else if (x[1] == "start"){cat("\033[1J\033[0;0H")}
}
#' Erase Text
#'
#' Clear text from the cursor's row . Passing values \code{"start"} and \code{"end"} allow the user to erase specific portions of the row relative to the cursor.
#'
#' @param x character describing location to clear. The default, \code{"row"}, clears the entire row; \code{"start"} clears all text from the beginning of the row until the cursor's position; \code{"end"} clears all text from the cursor's position until the end of the row.
#' @param ... objects passed to/from methods
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#' cat('hello world!')
#' erase('row')
#'
erase <- function(x = c("row", "start", "end"), ...){
  if (x[1] == "row" | x[1] == "line"){cat("\033[2K")
  } else if (x[1] == "start"){cat("\033[1K")
  } else if (x[1] == "end"){cat("\033[0K")}
}
#' Save Cursor Position
#'
#' Save the position of the cursor so that the position can be restored for later with \code{\link{load_cursor}}.
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#' save_cursor()
#' cat("\n\nHello World!")
#' load_cursor()
#'
save_cursor <- function(){cat("\033[s")}
#' Load Cursor
#'
#' Restore cursor to its previously saved location from \code{\link{save_cursor}}.
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#' save_cursor()
#' cat("\n\nHello World!")
#' load_cursor()
#'
load_cursor <- function(){cat("\033[u")}
#' Hide Cursor
#'
#' Make the cursor invisible. The cursor can be revealed with \code{\link{show_cursor}}
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#' hide_cursor()
#' cat("\n\nHello World!")
#' show_cursor()
#'
hide_cursor <- function(){cat("\033[?25l")}
#' Show Cursor
#'
#' Reveal the cursor after it has been hidden by \code{\link{hide_cursor}}.
#'
#' @return \code{NULL}
#' @export
#'
#' @examples
#' hide_cursor()
#' cat("\n\nHello World!")
#' show_cursor()
#'
show_cursor <- function(){cat("\033[?25h")}
#' Get Keypress
#'
#' Listen for a keypress, then apply keypress to a function or echo it to the terminal screen. The user must be in a terminal to use \code{getkp}; it will not work in RStudio or the R GUI. All actions within R are halted until the keypress is returned.
#'
#' @param fn list of named functions
#' @param echo whether the keypress should be echoed to the screen if not found in list
#'
#' @return character naming the key that was pressed \emph{(invisibly)}.
#' @importFrom keypress keypress
#' @export
#'
#' @examples
#' f <- list(
#' 	 'up'    = function(){mv(row=-1)},
#'   'down'  = function(){mv(row=-1)},
#' 	 'left'  = function(){mv(col=-1)},
#' 	 'right' = function(){mv(col=1)}
#' )
#' \dontrun{
#' getkp(fn=f, echo=FALSE)
#' }
#'
getkp <- function(fn = list(), echo=FALSE){
  if (in.term()){
    kp <- keypress()
    if (kp %in% names(fn)){
      kp <- fn[[kp]]()
    } else {
      if (echo){cat(kp)}
    }
    return(invisible(kp))
  }
}
#' Loop a Keypress
#'
#' Maintain a loop that listens for a keypress, then applies the keypress to a function or echoes it to the terminal screen. The user must be in a terminal to use \code{getkp}; it will not work in RStudio or the R GUI. All actions within R are halted until the keypress is returned.
#'
#' @param escape vector of character keypresses that escape the loop. The default is "escape" key.
#' @param fn list of named functions
#' @param echo whether the keypress should be echoed to the screen if not found in list
#'
#' @return \code{NULL}
#' @importFrom keypress keypress
#' @export
#'
#' @examples
#' f <- list(
#' 	 'up'    = function(){mv(row=-1)},
#'   'down'  = function(){mv(row=-1)},
#' 	 'left'  = function(){mv(col=-1)},
#' 	 'right' = function(){mv(col=1)}
#' )
#' \dontrun{
#' getkpl(escape = c("escape", "enter"), fn=f, echo=FALSE)
#' }
#'
getkpl <- function(escape = "escape", fn = list(), echo=FALSE){
  if (in.term()){
    kp <- ""
    while (!(kp %in% escape)){
      if (kp %in% names(fn)){
        kp <- fn[[kp]]()
      } else {
        if (echo){cat(kp)}
      }
      kp <- keypress()
    }
  }
}
#' Return Screen to Blank State
#'
#' Function to be used at the end of a terminal function. It resets the colors and attributes to their default values, clears the screen, and reveals the cursor.
#'
#' @return \code{NULL}
#' @export
wrapup <- function(){
  reset()
  clear()
  show_cursor()
  invisible(NULL)
}
