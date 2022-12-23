#' @title Quantifies the Lateral Sway Amplitude of a Statokinesigram
#' @name sttksdx
#'
#' @description Computes the standard deviation of lateral displacement of the center of pressure.
#'
#' @param dados data frame with two columns "x" and "y"
#'
#' @details 'dados' is a data frame containing two columns named "x" and "y".    The pairs (x, y) are the coordinates of the center of pressure acquired in a period of time.
#'
#' @return The standard deviation of x \code{sdx}.
#'
#' @author Jose Magalhaes de Oliveira
#'
#' @seealso \code{\link[stabilo]{sttksdy}}, \code{\link[stabilo]{sttkangle}}
#'
#' @examples
#' x <- c(1,3,7,5,9,4,3,6,8,2,8,9,4,5,7,3,4,7,9,3,2,5,3,4,8,2,9,7,4,2)
#' y <- c(6,3,9,1,3,7,4,9,6,1,7,3,9,7,2,6,3,4,8,1,9,3,6,8,1,6,2,9,8,3)
#'
#' COP <- data.frame(x, y)
#'
#' COPxsd <- sttksdx(COP)
#'
#' @export
sttksdx = function (dados) {
  sdx<-sd(dados$x)
  return(sdx)
}
