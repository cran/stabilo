#' @title Quantifies the front-and-back Sway Amplitude of a Statokinesigram
#' @name sttksdy
#'
#' @description Computes the standard deviation of front-and-back displacement of the center of pressure.
#'
#' @param dados data frame with two columns "x" and "y"
#'
#' @details 'dados' is a data frame containing two columns named "x" and "y".    The pairs (x, y) are the coordinates of the center of pressure acquired in a period of time.
#'
#' @return The standard deviation of y \code{sdy}.
#'
#' @author Jose Oliveira
#'
#' @seealso \code{\link[stabilo]{sttksdx}}, \code{\link[stabilo]{sttkellipseplot}}
#'
#' @examples
#' x <- c(1,3,7,5,9,4,3,6,8,2,8,9,4,5,7,3,4,7,9,3,2,5,3,4,8,2,9,7,4,2)
#' y <- c(6,3,9,1,3,7,4,9,6,1,7,3,9,7,2,6,3,4,8,1,9,3,6,8,1,6,2,9,8,3)
#'
#' COP <- data.frame(x, y)
#'
#' COPysd <- sttksdy(COP)
#'
#' @export
sttksdy = function (dados) {
  sdy<-sd(dados$y)
  return(sdy)
}
