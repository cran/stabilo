#' @title Velocity of a Center-of-pressure displacement
#'
#' @name sttkveloc
#'
#' @description Computes the mean velocity of a given Center-of-pressure displacement in the horizontal plane.
#'
#' @param dados Data frame with two columns "x" and "y"
#' @param fs The sampling frequency used in data recording
#'
#' @details 'dados' is a data frame containing two columns named "x" and "y".    The pairs (x, y) are the coordinates of the center-of-pressure acquired in a period of time.
#'
#' @return The velocity of the COP displacement \code{sttkveloc}.
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
#' COPvelocity <- sttkveloc(COP,fs)
#'
#'
#' @export
sttkveloc = function (dados,fs) {
  mvel = sum(sqrt(diff(dados$x)^2 + diff(dados$y)^2)*fs)/(length(dados$x)-1)
  return(mvel)
}
