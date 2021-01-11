#' Horizontal Line
#'
#' @param n integer describing the character length of the line
#' @param text character to be repeated
#'
#' @return character string of length \code{n}
#' @export
#'
#' @family drawing functions
#' @examples
#' hline(10, "*") # **********
#' hline(5, "$")  # $$$$$
#'
hline <- function(n, text="-"){paste(rep(text, n), collapse="")}
#' Vertical Line
#'
#' @param n integer describing the character length of the line
#' @param text character to be repeated
#'
#' @return character string of length \code{n}, separated by \code{"\n"}
#' @export
#'
#' @family drawing functions
#' @examples
#' vline(4, "*") # "*\n*\n*\n*"
#'
vline <- function(n, text="|"){paste(rep(text, n), collapse="\n")}
#' Draw Horizontal Line
#'
#' Draws a horizontal line of length \code{n} at \code{(row, col)}
#'
#' @param yx \code{(row, col)} coordinates where line should be drawn.
#' @param n integer describing the character length of the line
#' @param text character to be repeated
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' hline_at(c(3,4),6,"-") # print "------" at (3,4)
#'
hline_at <- function(yx, n, text="-", ...){
  if (in.term()){
    nc <- nchar(text[1])
    if (nc > 1){
      txt <- paste(rep(text[1], floor(n/nc)), collapse="")
      remain <- n %% nc
      if (remain > 0){txt <- paste0(txt, substr(text[1], 1, remain))}
    } else {txt <- paste(rep(text[1], n), collapse="")}
    wrat(yx, txt, ...)
  }
}
#' Draw Vertical Line
#'
#' Draws a vertical line of length \code{n} at \code{(row, col)}
#'
#' @param yx \code{(row, col)} coordinates where top of the line should be drawn.
#' @param n integer describing the character length of the line
#' @param text character to be repeated
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' vline_at(c(3,4),6,"|") # print "|" at (3,4), ..., (8,4)
#'
vline_at <- function(yx, n, text="|",...){
  if (in.term()){
    txt_split <- unlist(strsplit(text[1],""))
    nc <- length(txt_split)
    if (nc > 1){
      txt <- rep(txt_split, floor(n/nc))
      remain <- n %% nc
      if (remain > 0){
        txt <- c(txt, txt_split[1: remain])
      }
    } else {txt <- rep(text[1], n)}
    for (i in 1:n){wrat(c(yx[1]+i-1, yx[2]), txt[i],...)}
  }
}
#' Draw Box
#'
#' Draws a box of size \code{dim=c(height, width)} at \code{yx=c(row,col)}.
#'
#' @param yx starting console row and column of top-left corner of box
#' @param dim box dimensions in \code{c(height, width)}. If \code{NA}, defaults to the terminal's screen width.
#' @param text repeated character used for box. \code{text} can either be a single character or a vector of 8 characters (left side, right side, top, bottom, 4 corners: upper left, upper right, lower left, lower right).
#' @param fg foreground color. See \code{\link{fg_on}} for more details.
#' @param bg background color. See \code{\link{bg_on}} for more details.
#' @param attr border text attributes. See \code{\link{attr_on}} for details.
#' @param fill character object to fill box. Only the first character in the first element is used. If \code{NA} (the default), the box is not filled.
#' @param fill.fg foreground color of the fill character.
#' @param fill.bg background color of the fill character.
#' @param fill.attr text attributes of the fill character.
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' box_at(yx=c(4,4), dim=c(5,10), text="X")
#'
box_at <- function(yx=c(1,1), dim=NULL, text= c("|", "|", "-", "-", rep("+", 4)),
                   fg = NA, bg = NA, attr = NA, fill = NA,
                   fill.bg = NA, fill.fg = NA, fill.attr = NA){
  if (in.term()){
    if (length(text) < 8){
      if (length(text)==1){
        if (text[1] %in% c("box", "double", "round", "heavy", "dash", "quad")){
          if (text[1] == "box"){text <- c("\u2502","\u2502", "\u2500", "\u2500", "\u250c", "\u2510", "\u2514", "\u2518","\u253c", "\u2524", "\u2534", "\u251c","\u252c")
          } else if (text[1] == "double"){text <- c("\u2551", "\u2551", "\u2550", "\u2550", "\u2554", "\u2557", "\u255a", "\u255d", "\u256c","\u2563","\u2569","\u2560","\u2566")
          } else if (text[1] == "round"){text <- c("\u2502", "\u2502", "\u2500", "\u2500", "\u256d", "\u256e", "\u2570", "\u256f","\u253c", "\u2524", "\u2534", "\u251c","\u252c")
          } else if (text[1] == "heavy"){text <- c("\u2503", "\u2503", "\u2501", "\u2501", "\u250f", "\u2513", "\u2517", "\u251b","\u254b","\u252b","\u253b","\u2523","\u2533")
          } else if (text[1] == "dash"){text <- c("\u2506", "\u2506", "\u2504", "\u2504", "\u250c", "\u2510", "\u2514", "\u2518","\u253c", "\u2524", "\u2534", "\u251c","\u252c")
          } else if (text[1] == "quad"){text <- c("\u258c","\u2590","\u2580","\u2584","\u259b","\u259c","\u2599","\u259f",rep("\u2588",5))}

        } else {text <- rep(text[1], 8)}
      } else if (length(text)==3){c(text[1], text[1], text[2], text[2], rep(text[3], 4))
      } else {text <- rep(text, ceiling(8/length(text)))}
    }
    if (is.null(dim)){dim <- term_dim()}
    wrat(yx, style(paste0(text[5], hline(dim[2]-2, text=text[3]), text[6]), fg=fg, bg=bg, attr=attr))
    vline_at(c(yx[1]+1, yx[2]), dim[1]-2, text[1], fg=fg, bg=bg, attr=attr)
    vline_at(c(yx[1]+1, yx[2]+dim[2]-1), dim[1]-2, text[2], fg=fg, bg=bg, attr=attr)
    wrat(c(yx[1]+dim[1]-1, yx[2]), style(paste0(text[7], hline(dim[2]-2, text=text[4]), text[8]), fg=fg, bg=bg, attr=attr))
    if (!is.na(fill)){
      fill <- style(paste(rep(substr(fill[1], 1,1), dim[2]-2), collapse = ""), fg=fill.fg, bg=fill.bg, attr=fill.attr)
      sapply(2:(dim[1]-1), function(i){
        mv_to(yx[1]+i-1, yx[2]+1)
        cat(fill)
      })
    }
    mv_to(yx[1]+1, yx[2]+1)
  }
}
calc_grid_vertices <- function(y,x){
  Reduce('rbind', lapply(1:length(y), function(i){
    cbind(y[i], x)
  }))
}
#' Create a Character Grid Matrix
#'
#' Constructs a grid with provided dimensions \code{(row, col)}, character values for gridlines, and a step parameter noting the number of rows and columns between each gridline.
#'
#' @param dim \code{(row, column)} vector for size of grid.
#' @param step numeric vector describing grid step across \code{(rows, columns)}
#' @param text character vector of values for the grid, in order: horizontal grid line, vertical grid line, grid intersection, left border, right border, top border, bottom border, corners (upper-left, upper-right, lower-left, lower-right), ticks (right, bottom, left, top)
#' @param border logical value for whether a border should be included.
#'
#' @return \code{row}x\code{col} matrix
#' @export
#'
#' @family drawing functions
#' @examples
#' grid_mat(dim=c(11,13), step=c(2,4), border=TRUE)
#'
grid_mat <- function(dim, step=c(2,2), text= c(".", ".", "+", "|", "|", "-", "-", rep("+", 8)), border=TRUE){
  if (border){dim <- dim - c(2,2)}
  mat <- matrix(" ", nrow=dim[1], ncol=dim[2])
  if (length(step)==1){step <- rep(step,2)}
  if (any(step == 1)){
    mat[,] <- text[which(step==1)[1]]
    return(mat)
  }
  on_hstep <- rep(text[1], dim[2])
  cal_vstep <- c(rep(" ", step[2]-1), text[2])
  on_vstep <- rep(cal_vstep, floor(dim[2]/step[2]))
  if (length(on_vstep) < dim[2]){
    nc <- dim[2] - length(on_vstep)
    on_vstep <- c(on_vstep, cal_vstep[1:nc])
  }

  for (i in 1:dim[1]){
    if ((i %% step[1])==0){mat[i,] <- on_hstep
    } else {mat[i,] <- on_vstep}
  }

  y <- seq(step[1], dim[1], by=step[1])
  x <- seq(step[2], dim[2], by=step[2])
  vert <- calc_grid_vertices(y, x)
  mat[vert] <- text[3]
  if (border){
    top_vstep <- on_vstep
    bot_vstep <- on_vstep
    top_vstep[which(on_vstep == text[2])] <- text[15]
    bot_vstep[which(on_vstep == text[2])] <- text[13]
    top_vstep[which(on_vstep == " ")] <- text[6]
    bot_vstep[which(on_vstep == " ")] <- text[7]
    mat <- rbind(top_vstep, mat, bot_vstep)

    cal_hstep <- c(rep(text[4], step[1]-1), text[14])
    left_step <- rep(cal_hstep, floor(dim[1]/step[1]))
    if (length(left_step) < dim[1]){
      nc <- dim[1] - length(left_step)
      left_step <- c(left_step, cal_hstep[1:nc])
    }
    right_step <- left_step
    right_step[which(right_step == text[14])] <- text[12]
    left_step <- c(text[8], left_step, text[10])
    right_step <- c(text[9], right_step, text[11])
    mat <- cbind(left_step, mat, right_step)
  }
  return(mat)
}
#' Draw a Character Grid Matrix
#'
#' Constructs a grid with given dimension, character values, and step parameter, and prints it to screen
#'
#' @param yx \code{(row,column)} on screen or window where the upper-left corner of the grid is to be printed
#' @param dim \code{(row, column)} vector for size of grid.
#' @param step numeric vector describing grid step across \code{(rows, columns)}
#' @param text character vector of values for the grid, in order: horizontal grid line, vertical grid line, grid intersection, left border, right border, top border, bottom border, corners (upper-left, upper-right, lower-left, lower-right), ticks (right, bottom, left, top)
#' @param border logical value for whether a border should be included.
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' grid_at(yx=c(2,2), dim=c(11,13), step=c(2,4), border=TRUE)
#'
grid_at <- function(yx=c(1,1), dim=NULL, step=c(2,2), text= c(".", ".", "+", "|", "|", "-", "-", rep("+", 8)),border=TRUE){
  if (is.null(dim)){dim <- term_dim() - yx + c(1,1)}
  mat <- grid_mat(dim = dim, step=step, text= text, border=border)
  invisible(sapply(1:dim[1], function(i){
    mv_to(yx[1]+i-1, yx[2])
    cat(paste(mat[i,], collapse=""))
  }))
  return(invisible(mat))
}


#' Draw Path
#'
#' Draws text at each supplied coodinate.
#'
#' @param coord matrix or list containing \code{(row, col)} coordinates.
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' c <- path_circle(yx = c(5,5), r=3)
#' draw_path(c, text="0")
#'
draw_path <- function(coord, text="x", ...){UseMethod("draw_path")}
#' @export
draw_path.numeric <- function(coord, text="x", ...){wrat(coord, text, ...)}
#' @export
draw_path.matrix <- function(coord, text="x", ...){
  invisible(sapply(1:nrow(coord), function(i){
    wrat(coord[i,], text, ...)
  }))
}
#' @export
draw_path.list <- function(coord, text="x", ...){
  invisible(sapply(1:length(coord), function(i){
    draw_path(coord[[i]], text=text, ...)
  }))
}
#' Draw a Bezier Curve
#'
#' Calculate the path of a Bezier Curve with up to two control points in a grid and draw to screen.
#'
#' @param start starting \code{(row, col)} coordinate
#' @param end ending \code{(row, col)} coordinate
#' @param c1 coordinate of first control point
#' @param c2 coordinate of second control point
#' @param n number of points along curve to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_bezier(start=c(10,1), end=c(10,10), c1=c(1, 3))
#'
draw_bezier <- function(start, end, c1, c2=NULL, n=50, text="x", ...){draw_path(path_bezier(start, end, c1, c2, n), text, ...)}
#' Draw a Line
#'
#' Interpolate between two points in a grid and draw to sreen.
#'
#' @param start starting \code{(row, col)} coordinate
#' @param end ending \code{(row, col)} coordinate
#' @param n number of points along curve to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_lerp(start=c(10,1), end=c(1,3))
#'
draw_lerp <- function(start, end, n=50, text="x", ...){draw_path(path_lerp(start, end, n), text, ...)}
#' Draw a Shape
#'
#' Calculate the path of a shape given supplied vertices and draw to screen.
#'
#' @param mat an Nx2 matrix of \code{(row, col)} coordinates
#' @param cycle logical value determining whether to the first and last coordinates
#' @param n number of points along each edge to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' # Right Triangle
#' draw_shape(rbind(
#'   c(10,1),
#'   c(10,10),
#'   c(1,1)
#' ), cycle=TRUE)
#'
draw_shape <- function(mat, cycle=TRUE, n=30, text="x",...){draw_path(path_shape(mat,cycle,n), text,...)}
#' Draw Ellipse
#'
#' Calculate the path of an ellipse within a grid and draw to screen.
#'
#' @param yx \code{(row, col)} coordinate of the center of the ellipse
#' @param rx radius along the x-axis in grid points
#' @param ry radius along the y-axis in grid points
#' @param n number of points along curve to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_ellipse(yx=c(10,10), rx=8, ry = 4)
#'
draw_ellipse <- function(yx= c(0,0), rx = 1, ry = 1, n=50, text="x",...){draw_path(path_ellipse(yx, rx, ry, n), text,...)}
#' Draw a Rectangle
#'
#' Calculate the path of a rectangle in a grid and draw to screen.
#'
#' @param yx1 upper-left \code{(row, col)} coordinate
#' @param yx2 lower-right \code{(row, col)} coordinate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_rect(c(5,5), c(9,9))
#'
draw_rect <- function(yx1, yx2, text="x",...){draw_path(path_rect(yx1, yx2), text,...)}
#' Draw a Circle
#'
#' Calculate the path of a circle in a grid and draw it to screen.
#'
#' @param yx center \code{(row, col)} coordinate
#' @param r radius of the circle in grid points
#' @param n number of points along curve to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_circle(yx=c(10,10), r=5)
#'
draw_circle <- function(yx, r=1, n=50, text="x",...){draw_path(path_circle(yx, r, n), text,...)}
#' Draw an Arc
#'
#' Calculate the path of an arc within a grid and print to screen.
#'
#' @param yx center \code{(row, col)} coordinate of circle
#' @param start starting angle in radians
#' @param end ending angle in radians
#' @param r radius of circle
#' @param n number of points along curve to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_arc(yx=c(10,10), start=pi/2, end=pi, r=6)
#'
draw_arc <- function(yx, start, end, r=1, n=50, text="x",...){draw_path(path_arc(yx, start, end, r, n), text,...)}
#' Draw a Function
#'
#' Calculate the path within a grid of an user-supplied function and print to screen.
#'
#' @param x1 starting column value of the path
#' @param x2 ending column value of the path
#' @param fn function returning row value for a column input
#' @param n number of points along curve to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_fn(x1=1, x2=10,
#'   function(x){sqrt(x)}
#' )
#'
draw_fn <- function(x1, x2, fn, n=50, text="x",...){draw_path(path_fn(x1, x2, fn, n), text,...)}
#' Draw a Ray
#'
#' Calculate the path of a ray extending and print to screen.
#'
#' @param start start \code{(row, col)} coordinate of the ray
#' @param end either an ending coording, an angle in radians, or a character direction (u, d, l, r, ul, ur, dl, dr)
#' @param lim bounding box dimensions used to calculate ray
#' @param n number of points along curve to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_ray(start=c(10,10), end=pi/6)
#' draw_ray(start=c(10,10), end=pi/6, lim=c(15,15))
#' draw_ray(start=c(10,10), end=c(4,2))
#'
draw_ray <- function(start, end, lim=c(64,128), n=200, text="x",...){draw_path(path_ray(start, end, lim, n), text,...)}
#' Draw a Filled-In Shape
#'
#' Calculate the path of a shape given supplied vertices and draw to screen.
#'
#' @param mat an Nx2 matrix of \code{(row, col)} coordinates
#' @param cycle logical value determining whether to the first and last coordinates
#' @param n number of points along each edge to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' # Right Triangle
#' fill_shape(rbind(
#'   c(10,1),
#'   c(10,10),
#'   c(1,1)
#' ), cycle=TRUE)
#'
fill_shape <- function(mat, cycle=TRUE, n=30, text="x",...){draw_path(path_fill(path_shape(mat,cycle,n)), text,...)}
#' Draw a Filled-In Ellipse
#'
#' Calculate the path of an ellipse within a grid and draw to screen.
#'
#' @param yx \code{(row, col)} coordinate of the center of the ellipse
#' @param rx radius along the x-axis in grid points
#' @param ry radius along the y-axis in grid points
#' @param n number of points along curve to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_ellipse(yx=c(10,10), rx=8, ry = 4)
#'
fill_ellipse <- function(yx= c(0,0), rx = 1, ry = 1, n=50, text="x",...){draw_path(path_fill(path_ellipse(yx, rx, ry, n)), text,...)}
#' Draw a Filled-In Rectangle
#'
#' Calculate the path of a rectangle in a grid and draw to screen.
#'
#' @param yx1 upper-left \code{(row, col)} coordinate
#' @param yx2 lower-right \code{(row, col)} coordinate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_rect(c(5,5), c(9,9))
#'
fill_rect <- function(yx1, yx2, text="x",...){draw_path(path_fill(path_rect(yx1, yx2)), text,...)}
#' Draw a Filled-In Circle
#'
#' Calculate the path of a circle in a grid and draw it to screen.
#'
#' @param yx center \code{(row, col)} coordinate
#' @param r radius of the circle in grid points
#' @param n number of points along curve to calculate
#' @param text character value drawn at coordinate
#' @param ... parameters that are passed to \code{style()}, including the foreground color \code{fg}, background color \code{bg}, and attribute \code{attr}
#'
#' @return \code{NULL}
#' @export
#'
#' @family drawing functions
#' @examples
#' draw_circle(yx=c(10,10), r=5)
#'
fill_circle <- function(yx, r=1, n=50, text="x",...){draw_path(path_fill(path_circle(yx, r, n)), text,...)}

