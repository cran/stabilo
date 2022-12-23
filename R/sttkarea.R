#' @title Quantifies the Area of a Statokinesigram
#' @name sttkarea
#'
#' @description Computes the area of of a given statokinesigram by fitting an ellipse containing 95 percent of statokinesigram's points.
#'
#' @param dados data frame with two columns "x" and "y"
#'
#' @details 'dados' is a data frame containing two columns named "x" and "y".    The pairs (x, y) are the coordinates of the center of pressure acquired in a period of time.
#'
#' @return The area of the given statokinesigram \code{sttkarea}.
#'
#' @author Jose Oliveira
#'
#' @seealso \code{\link[stabilo]{sttkangle}}, \code{\link[stabilo]{sttkellipseplot}}
#'
#' @examples
#' x <- c(1,3,7,5,9,4,3,6,8,2,8,9,4,5,7,3,4,7,9,3,2,5,3,4,8,2,9,7,4,2)
#' y <- c(6,3,9,1,3,7,4,9,6,1,7,3,9,7,2,6,3,4,8,1,9,3,6,8,1,6,2,9,8,3)
#'
#' COP <- data.frame(x, y)
#'
#' COParea <- sttkarea(COP)
#'
#' @export
sttkarea = function (dados) {
  invp <- 4.8955/2
  V <- cov(dados,y= NULL, use = "everything",method = "pearson")
  resultado <- eigen(V, TRUE, only.values = FALSE, EISPACK = FALSE)
  valores <- resultado$values
  val <- rev(valores)*diag(2)
  msvd <- svd(val)
  axes <- invp*sqrt(msvd$d)
  area <- pi*prod(axes)
  area <- round(area*10)/10
  return (area);
}
