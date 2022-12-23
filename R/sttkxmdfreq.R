#' @title Quantifies the Median Frequency of the Lateral Displacement of COP.
#' @name sttkxmdfreq
#'
#' @description Computes the median frequency of the lateral displacement of the center of pressure.
#'
#' @param dados data frame with two columns "x" and "y"
#' @param sampfreq number  The sampling frequency
#'
#' @details 'dados' is a data frame containing two columns named "x" and "y".    The pairs (x, y) are the coordinates of the center of pressure acquired in a period of time.
#'
#' @return The median frequency of the x displacement for the given statokinesigram \code{FMx}.
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
#' COPxmdfreq <- sttkxmdfreq(COP, 50)
#'
#'
#' @export
sttkxmdfreq = function (dados,sampfreq) {

  periodo = 1/sampfreq
  aa = detrend(dados$x, tt = 'linear')
  Z = fft (aa, inverse = FALSE)
  Pxx = Z*Conj (Z)
  Pxx = Pxx*periodo/length(dados$x)
  g1 = seq(0, 2, by = 1/(periodo*length(Pxx)))
  PHx=Pxx[1:length(g1)]
  FMx=Real((g1%*%PHx)/sum(PHx))
  return(FMx)
}
