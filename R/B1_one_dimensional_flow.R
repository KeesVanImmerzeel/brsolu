#' Bruggeman Formula 123.32, p. 66
#'
#' BI. One-dimensional groundwater flow
#' BI-2. One-dimensional groundwater flow in a semi-infinite field
#' The soil is assumed to be homogeneous
#' The flow is non-periodic
#' The boundary condition at x -- 0 a is given head or drawdown
#' Leaky aquifers with variable head at x = 0
#' Sudden drawdown of the surface water level, which is kept constant thereafter.
#'
#' Calculate the Head change Phi at distance x and time t due to a stress
#' change h (= sudden drawdown) at t=0 at x=0.
#'
#' @param x Distance (L)
#' @param t Time (T)
#' @inheritParams Labda
#' @inheritParams Eta
# @param S Storage coefficient [-]
# @param kD Hydraulic conductivity [L2/T]
# @param c Hydraulic resistance [T]
#' @param h Constant in the definition of the stress (= sudden drawdown) (L)
#' @return Phi Head change (L)
#' @export
Br_123_32 <- function(x, t, S, kD, c, h) {
  labda = Labda(c, kD)
  eta = Eta(c, S)
  Phi <- h * brmath::P(x / (2 * labda), sqrt(eta * t))
  return(Phi)
}

#' Bruggeman Formula 123.32 p. 66
#' First Derivative with respect to x [-]
#d_Br_123_32_dx <- function(x, t, S, kD, c, h) {
#  Labda = sqrt(c * kD)
#  Eta = 1 / (c * S)
#  h * brmath::dPdx(x / (2 * Labda), sqrt(Eta * t)) / (2 * Labda)
#}
