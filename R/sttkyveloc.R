#' @title Mean front-to-back velocity of Center-of-pressure displacement
#'
#' @name sttkyveloc
#'
#' @description Computes the mean front-to-back velocity of a given Center-of-pressure displacement.
#' @param dados Data frame with two columns "x" and "y"
#' @param fs The sampling frequency used in data recording
#'
#' @details 'dados' is a data frame containing two columns named "x" and "y".    The pairs (x, y) are the coordinates of the center-of-pressure acquired in a period of time.
#'
#' @return The velocity of the front-to-back COP displacement \code{sttkyveloc}.
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
#' fs <- 50
#'
#' COPyvelocity <- sttkyveloc(COP,fs)
#'
#'
#' @export
sttkyveloc = function (dados,fs) {
  myvel = sum(sqrt(diff(dados$y)^2)*fs)/(length(dados$y)-1)
  return(myvel)
}
