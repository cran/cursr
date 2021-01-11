#' Example Program From Vignette
#'
#' Simple program that asks for a letter and a number and returns another value to screen.
#'
#' @return \code{NULL}
#' @export
example_luckynumber <- function(){
  clear()
  hide_cursor()

  dimensions <- term_dim()
  box_at(yx=c(1,1), dim=dimensions, fill=" ", fill.bg="blue")

  text <- "Find Your LUCKY NUMBER!"
  yx   <- c(3, floor((dimensions[2] - nchar(text))/2))
  wrat(yx, text)

  bg_on("blue")
  fg_on("white")

  wrat(c(5,3), "What is your favorite letter?  ")
  lett <- getkp(echo=TRUE)

  wrat(c(7,3), "What is your favorite number?  ")
  numb <- getkp(echo=TRUE)

  result <- which(letters == tolower(lett))*as.integer(numb)+sample(-10:10, 1)
  wrat(c(9,3), "Your lucky number is ...")
  wrat(c(10,10), result, fg = "red", attr="bf")

  wrat(c(14,3), "Press ANY Key to Quit.")
  getkp()

  wrapup()
}
