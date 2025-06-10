#' Formula 123.32, p. 66
#'
#' Calculate the head change Phi at distance x and time t due to a stress
#' change h (= sudden drawdown) at t=0 at x=0.
#'
#' @section Schematization:
#'
#' \itemize{
#'   \item BI. One-dimensional groundwater flow
#'   \item BI-2. One-dimensional groundwater flow in a semi-infinite field
#'   \item The soil is assumed to be homogeneous
#'   \item The flow is non-periodic
#'   \item The boundary condition at x = 0 a is given head or drawdown
#'   \item Leaky aquifers with variable head at x = 0
#'   \item Sudden drawdown of the surface water level, which is kept constant thereafter.
#' }
#'
#' @param x Distance (L)
#' @param t Time (T)
#' @inheritParams xt_function
#' @inheritParams Labda
#' @inheritParams Eta
#' @param h Constant in the definition of the stress (= sudden drawdown) (L)
#' @return Phi Head change (L)
#' @export
Br_123_32 <- function(x, t, S, kD, c, h) {
  labda = Labda(c, kD)
  eta = Eta(c, S)
  Phi <- h * brmath::P(x / (2 * labda), sqrt(eta * t))
  return(Phi)
}

#' Formula 123.32 p. 66 (modified)
#'
#' First Derivative of Phi with respect to x
#'
#' @inheritSection Br_123_32 Schematization
#'
#' @inheritParams Br_123_32
#' @return First Derivative of Phi with respect to x (-)
#' @export
d_Br_123_32_dx <- function(x, t, S, kD, c, h) {
  labda = Labda(c, kD)
  eta = Eta(c, S)
  h * brmath::dPdx(x / (2 * labda), sqrt(eta * t)) / (2 * labda)
}
