#' @title Quantifies the Angle of a Statokinesigram
#' @name sttkangle
#'
#' @description Computes the angle of of a given statokinesigram, with respect to the x axis, by fitting an ellipse containing 95 percent of statokinesigram's points.
#'
#' @param dados data frame with two columns "x" and "y"
#'
#' @details 'dados' is a data frame containing two columns named "x" and "y". The pairs (x, y) are the coordinates of the center of pressure acquired in a period of time.
#'
#' @return The angle, in degrees, of the fitted ellipse on the given statokinesigram \code{sttkangle}.
#'
#' @author Jose Magalhaes de Oliveira
#'
#' @seealso \code{\link[stabilo]{sttkarea}}, \code{\link[stabilo]{sttkellipseplot}}
#'
#' @examples
#' x <- c(1,3,7,5,9,4,3,6,8,2,8,9,4,5,7,3,4,7,9,3,2,5,3,4,8,2,9,7,4,2)
#' y <- c(6,3,9,1,3,7,4,9,6,1,7,3,9,7,2,6,3,4,8,1,9,3,6,8,1,6,2,9,8,3)
#'
#' COP <- data.frame(x, y)
#'
#' COPangle <- sttkangle(COP)
#'
#' @export
sttkangle = function (dados) {
  V <- cov(dados,y= NULL, use = "everything",method = "pearson")
  resultado <- eigen(V, TRUE, only.values = FALSE, EISPACK = FALSE)
  vetores <- t(apply(t(resultado$vectors), 2, rev))
  angles <- -atan2( vetores[1,],vetores[2,] )
  angulo <- round(angles[1]*180/pi*10)/10
  angulo <- round(angulo*10)/10
  if(angulo<0) angulo <- 180+angulo
  return (angulo);
}
