## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(cursr)

## ----echo=FALSE---------------------------------------------------------------
top <- "+----------------+"
row <- "|                |"
rw1 <- "|  X             |"
rw2 <- "|        Z       |"
cat(top, rw1, row, row, row, row, rw2, row, row,top, sep="\n")

## ----eval=FALSE---------------------------------------------------------------
#  library(cursr)
#  mv_to(6, 9)
#  
#  # OR ...
#  
#  new_location <- c(6,9)
#  mv_to(new_location)

## ----eval=FALSE---------------------------------------------------------------
#  mv(5, 6)

## ----eval=FALSE---------------------------------------------------------------
#  mv_to(1,3)
#  wr("Hello")
#  
#  wrat(6,9, "World")

## ----echo=FALSE---------------------------------------------------------------
top <- "+----------------+"
row <- "|                |"
rw1 <- "|  Hello         |"
rw2 <- "|        World   |"
cat(top, rw1, row, row, row, row, rw2, row, row,top, sep="\n")

## ----eval=FALSE---------------------------------------------------------------
#  # Change foreground color to be "red" with any of the following
#  fg_on("red")
#  fg_on("#FF0000")
#  fg_on(9)
#  fg_on(255, 0, 0)
#  
#  # Return foreground color to default
#  fg_off()

## ----eval=FALSE---------------------------------------------------------------
#  # Write at cursor in bold, blue on red background
#  wr("Hello World!", bg="red", fg="blue", attr="bf")

