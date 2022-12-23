#' @title Standardizes Statokinesigrams
#' @name sttknorm
#'
#' @description confines a given statokinesigram in a circumference of radius equal to 1, without spatially distorting its shape. The circumference contains 95 percent of statokinesigram's points.
#'
#' @param dados data frame with two columns "x" and "y"
#'
#' @details 'dados' is a data frame containing two columns named "x" and "y". The pairs (x, y) are the coordinates of the center of pressure acquired in a period of time.
#'
#' @return The normalized statokinesigram \code{stknorm}.
#'
#' @author Jose Magalhaes de Oliveira
#'
#' @seealso \code{\link[stabilo]{sttkarea}}, \code{\link[stabilo]{sttklength}}
#'
#' @examples
#' x <- c(1,3,7,5,9,4,3,6,8,2,8,9,4,5,7,3,4,7,9,3,2,5,3,4,8,2,9,7,4,2)
#' y <- c(6,3,9,1,3,7,4,9,6,1,7,3,9,7,2,6,3,4,8,1,9,3,6,8,1,6,2,9,8,3)
#'
#' COP <- data.frame(x, y)
#'
#' COPnorm <- sttknorm(COP)
#'
#'
#' @export
sttknorm = function (dados) {
  x<-dados$x
  y<-dados$y
  x<-x-mean(x)
  y<-y-mean(y)
  MODULO <- sqrt(x^2+y^2)
  ANGULO <- atan(y/x)
  sortMOD <- sort(MODULO)
  sortMOD95 <- sortMOD[1:round(length(sortMOD)*.95)]
  MOD95 <- max(sortMOD95)
  MODULO_NORM95 <- MODULO/MOD95
  X_NORM95 <- MODULO_NORM95*cos(ANGULO)
  X_NORM <- X_NORM95*(x/abs(x))
  Y_NORM95 <- MODULO_NORM95*sin(ANGULO)
  Y_NORM <- Y_NORM95*(x/abs(x))
  coordxyN <- cbind(X_NORM,Y_NORM)
  stknorm <- as.data.frame(coordxyN)
  names(stknorm)<-c("x","y")
  stknormplot <- ggplot(stknorm,aes(x,y,xmin=-2,xmax=2,ymin=-2,ymax=2)) + geom_path(colour = "red") +
    labs(x='xnorm', y='ynorm', title='Normalized COP displacement')

  eixox = seq(-1.0, 1.0, ((2)/(length(x) - 1)))
  circmenos = -sqrt(1-eixox^2)
  circ = sqrt(1-eixox^2)
  comcirculo<- stknormplot + geom_path(aes(eixox,circ),colour = "blue",linewidth=1) +
    geom_path(aes(eixox,circmenos),colour = "blue",linewidth=1)
  print(comcirculo)
  return(stknorm)
}
