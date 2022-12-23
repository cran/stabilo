#' @title Points of the Confidence Ellipse of a Statokinesigram
#' @name sttkellipseplot
#'
#' @description Computes the contour of the confidence ellipse of a given statokinesigram, containing 95 percent of statokinesigram's points.
#'
#' @param dados data frame with two columns "x" and "y"
#'
#' @details 'dados' is a data frame containing two columns named "x" and "y". The pairs (x, y) are the coordinates of the center of pressure acquired in a period of time.
#'
#' @return The contour of the ellipse fitted to the given statokinesigram \code{ellctr}.
#'
#' @author Jose Magalhaes de Oliveira
#'
#' @seealso \code{\link[stabilo]{sttkangle}}, \code{\link[stabilo]{sttkellipseplot}}
#'
#' @examples
#' x <- c(1,3,7,5,9,4,3,6,8,2,8,9,4,5,7,3,4,7,9,3,2,5,3,4,8,2,9,7,4,2)
#' y <- c(6,3,9,1,3,7,4,9,6,1,7,3,9,7,2,6,3,4,8,1,9,3,6,8,1,6,2,9,8,3)
#'
#' COP <- data.frame(x, y)
#'
#' COPellipseplot <- sttkellipseplot(COP)
#'
#'
#' @export
sttkellipseplot = function (dados) {
  invp <- 4.8955/2
  V <- cov(dados,y= NULL, use = "everything",method = "pearson")
  RR <- chol(V)
  angles <- seq(0, 2*pi, length.out=length(dados$x))
  ell <- invp * cbind(cos(angles), sin(angles)) %*% RR
  ctr <- apply(dados, 2, mean)
  ellCtr <- sweep(ell, 2, ctr, "+")
  # plot(ellCtr, type="l", lwd=2, asp=1)
  # points(ctr[1], ctr[2], pch=4, lwd=2)
  return(ellCtr)
}
