#' Bezier Curve Path
#'
#' Calculate the path of a Bezier Curve with up to two control points in a grid.
#'
#' @param start starting \code{(row, col)} coordinate
#' @param end ending \code{(row, col)} coordinate
#' @param c1 coordinate of first control point
#' @param c2 coordinate of second control point
#' @param n number of points along curve to calculate
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' path_bezier(start=c(10,1), end=c(10,10), c1=c(1, 3))
#'
path_bezier <- function(start, end, c1, c2=NULL, n=50){
	t <- seq(0, 1, by=1/n)
	if (is.null(c2)){
		return(unique(round(cbind(
			y = (1-t)*((1-t)*start[1] + t*c1[1]) + t*((1-t)*c1[1] + t*end[1]),
			x = (1-t)*((1-t)*start[2] + t*c1[2]) + t*((1-t)*c1[2] + t*end[2])
		))))
	} else {
		return(unique(round(cbind(
			y = start[1]*(1-t)^3 + 3*t*c1[1]*(1-t)^2 + 3*c2[1]*(1-t)*t^2 + end[1]*t^3,
			x = start[2]*(1-t)^3 + 3*t*c1[2]*(1-t)^2 + 3*c2[2]*(1-t)*t^2 + end[2]*t^3
		))))
	}
}
#' Linear Interpolation Path
#'
#' Interpolate between two points in a grid.
#'
#' @param start starting \code{(row, col)} coordinate
#' @param end ending \code{(row, col)} coordinate
#' @param n number of points along curve to calculate
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' path_lerp(start=c(10,1), end=c(1,3))
#'
path_lerp <- function(start, end, n=50){
	t <- seq(0, 1, by=1/n)
	return(unique(round(cbind(
		y = start[1] + t*(end[1]-start[1]),
		x = start[2] + t*(end[2]-start[2])
	))))
}
#' Path along a Shape
#'
#' Calculate the path of a shape given supplied vertices.
#'
#' @param mat an Nx2 matrix of \code{(row, col)} coordinates
#' @param cycle logical value determining whether to the first and last coordinates
#' @param n number of points along each edge to calculate
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' # Right Triangle
#' path_shape(rbind(
#'   c(10,1),
#'   c(10,10),
#'   c(1,1)
#' ), cycle=TRUE)
#'
path_shape <- function(mat, cycle=TRUE, n=30){
	if (cycle){mat <-rbind(mat, mat[1,])}
	return(unique(round(Reduce(f = 'rbind', x = lapply(1:(nrow(mat)-1), function(i){
		path_lerp(as.numeric(mat[i,]), as.numeric(mat[i+1,]), n=n)
	})))))
}
#' Ellipse Path
#'
#' Calculate the path of an ellipse within a grid.
#'
#' @param yx \code{(row, col)} coordinate of the center of the ellipse
#' @param rx radius along the x-axis in grid points
#' @param ry radius along the y-axis in grid points
#' @param n number of points along curve to calculate
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' path_ellipse(yx=c(10,10), rx=8, ry = 4)
#'
path_ellipse <- function(yx= c(0,0), rx = 1, ry = 1, n=50){
	x <- seq(yx[2]-abs(rx), yx[2]+abs(rx), by = 2*abs(rx)/n)
	y <- c(
		sqrt(abs(ry)*(1 - ((x-yx[2])^2)/(rx^2))) + yx[1],
		-sqrt(abs(ry)*(1 - ((x-yx[2])^2)/(rx^2))) + yx[1]
	)
	return(unique(round(cbind(y, c(x, x)))))
}

#' Rectangle Path
#'
#' Calculate the path of a rectangle in a grid.
#'
#' @param yx1 upper-left \code{(row, col)} coordinate
#' @param yx2 lower-right \code{(row, col)} coordinate
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' path_rect(c(5,5), c(9,9))
#'
path_rect <- function(yx1, yx2){
	if (yx1[2] > yx2[2]){x <- seq(floor(yx2[2]), ceiling(yx1[2]), by=1)
	} else {x <- seq(floor(yx1[2]), ceiling(yx2[2]), by=1)}
	if (yx1[1] > yx2[1]){y <- seq(floor(yx2[1]), ceiling(yx1[1]), by=1)
	} else {y <- seq(floor(yx1[1]), ceiling(yx2[1]), by=1)}
	top <- cbind(yx1[1], x)
	bot <- cbind(yx2[1], x)
	lef <- cbind(y, yx1[2])
	rig <- cbind(y, yx2[2])
	return(rbind(lef,top,rig,bot))
}
#' Path of a Circle
#'
#' Calculate the path of a circle in a grid.
#'
#' @param yx center \code{(row, col)} coordinate
#' @param r radius of the circle in grid points
#' @param n number of points along curve to calculate
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' path_circle(yx=c(10,10), r=5)
#'
path_circle <- function(yx, r=1, n=50){
  x <- seq(yx[2]-r, yx[2]+r, by=2*r/n)
  y <- c(sqrt(r^2 - (x-yx[2])^2) + yx[1],
		-sqrt(r^2 - (x-yx[2])^2) + yx[1])
  return(unique(round(cbind(y, c(x, x)))))
}
#' Arc Path
#'
#' Calculate the path of an arc within a grid.
#'
#' @param yx center \code{(row, col)} coordinate of circle
#' @param start starting angle in radians
#' @param end ending angle in radians
#' @param r radius of circle
#' @param n number of points along curve to calculate
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' path_arc(yx=c(10,10), start=pi/2, end=pi, r=6)
#'
path_arc <- function(yx, start, end, r=1, n=50){

  path_arc_ray <- function(yx, angle, r){
    if (angle == 0 | angle == 2*pi){return(c(yx[1], yx[2]+r))
    } else if (angle == pi){return(c(yx[1], yx[2]-r))
    } else if (angle == pi/2){return(c(yx[1]-r, yx[2]))
    } else if (angle == 3/2*pi){return(c(yx[1]+r, yx[2]))
    } else if (angle < pi/2){return(round(c(yx[1]-r*sin(angle), yx[2]+r*cos(angle))))
    } else if (angle > 3/2*pi){return(round(c(yx[1]+r*sin(2*pi - angle), yx[2]+r*cos(2*pi - angle))))
    } else if (angle < pi){return(round(c(yx[1]-r*sin(pi-angle), yx[2]-r*cos(pi-angle))))
    } else {return(round(c(yx[1]+r*sin(angle - pi), yx[2]-r*cos(angle-pi))))}
  }

  theta <- seq(start, end, by=(end-start)/n)
  return(unique(t(sapply(theta, function(i){path_arc_ray(yx, i, r=r)}))))
}
#' Function Path
#'
#' Calculate the path within a grid of an user-supplied function.
#'
#' @param x1 starting column value of the path
#' @param x2 ending column value of the path
#' @param fn function returning row value for a column input
#' @param n number of points along curve to calculate
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' path_fn(x1=1, x2=10,
#'   function(x){sqrt(x)}
#' )
#'
path_fn <- function(x1, x2, fn, n=50){
  x <- seq(x1, x2, by=(x2-x1)/n)
  return(unique(round(cbind(sapply(x, fn), x))))
}
#' Ray Path
#'
#' Calculate the path of a ray extending
#'
#' @param start start \code{(row, col)} coordinate of the ray
#' @param end either an ending coording, an angle in radians, or a character direction (u, d, l, r, ul, ur, dl, dr)
#' @param lim bounding box dimensions used to calculate ray
#' @param n number of points along curve to calculate
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' path_ray(start=c(10,10), end=pi/6)
#' path_ray(start=c(10,10), end=pi/6, lim=c(15,15))
#' path_ray(start=c(10,10), end=c(4,2))
#'
path_ray <- function(start, end, lim=c(64,128), n=200){
  if (length(end) == 1){
    if (is.character(end)){
      end <- switch(end,
        u = pi/2,
        d = pi*1.5,
        l = pi,
        r = 2*pi,
        ul = pi*0.75,
        ur = pi/4,
        dl = pi*1.25,
        dr = pi*1.75
      )
    }

    # Convert angle, then raycast!
    if (end < 0){end <- abs(end)}
    if (end > 2*pi){end <- end %% (2*pi)}
    if (end <= pi){
      if (end == 0){
        end <- c(start[1], lim[2])
      } else if (end < pi/2){
        len_x <- lim[2] - start[2]
        len_y <- len_x*tan(end)
        end <- c(round(start[1]-len_y), lim[2])
      } else if (end == pi/2){
        end <- c(1, start[2])
      } else if (end < pi){
        len_x <- start[2]-1
        len_y <- len_x*tan(pi - end)
        end <- c(round(start[1]-len_y), 1)
      } else {
        end <- c(start[1], 1)
      }
    } else {
      if (end < 3/2*pi){
        len_x <- start[2]-1
        len_y <- len_x*tan(end-pi)
        end <- c(round(start[1]+len_y), 1)
      } else if (end == 3/2*pi){
        end <- c(lim[1], start[2])
      } else if (end < 2*pi){
        len_x <- lim[2]-start[2]
        len_y <- len_x*tan(2*pi - end)
        end <- c(round(start[2]+len_y), lim[2])
      } else {
        end <- c(start[1], lim[2])
      }
    }
  }

  if (all(start == end)){return(NA)}
  if (start[2] == end[2]){return(cbind(start[1]:end[1], start[2]))}
  if (start[1] == end[1]){return(cbind(start[1], start[2]:end[2]))}

  Xs <- seq(start[2], end[2], by=(end[2] - start[2])/n)
  # Equation of Line
  m <- (end[1]-start[1])/(end[2]-start[2])
  b <- start[1] - m*start[2]
  Ys <- b+m*Xs
  path <- unique(round(cbind(Ys, Xs)))
  to_remove <- which(path[,1] < 1 | path[,1] > lim[1])
  if (length(to_remove)> 0){
    path <- path[-to_remove,]
  }
  return(path)
}
#' Fill In Path
#'
#' Calculate the coordinates of all points inside of a path.
#'
#' @param mat Nx2 matrix of \code{(row, column)} path coordinates
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' c0 <- path_circle(c(10,10), r=5)
#' path_fill(c0)
#'
path_fill <- function(mat){
	Xs <- unique(mat[,2])
	return(Reduce("rbind", lapply(Xs, function(i){
		mat2 <- mat[mat[,2] == i,,drop=FALSE]
		maxy <- max(mat2[,1])
		miny <- min(mat2[,1])
		range <- maxy - miny
		if (range > 0){return(cbind(y = seq(miny, maxy, by=1), x = i))
		} else {return(mat2)}
	})))
}
#' Intersection between Two Paths
#'
#' Calculate the points of intersection between two paths.
#'
#' @param path list containing two coordinate \code{(row, column)} matrices.
#'
#' @return Nx2 matrix of \code{(row, column)} coordinates
#' @export
#'
#' @family path-fitting functions
#' @examples
#' c1 <- path_circle(c(4,4), r=3)
#' c2 <- path_circle(c(6,6), r=3)
#' path_intersection(list(c1, c2))
#'
path_intersection <- function(path){
	if (!is.list(path) && length(path) > 1){return(NULL)}
	mat <- rbind(unique(path[[1]]), unique(path[[2]]))
	return(mat[duplicated(mat),])
}
