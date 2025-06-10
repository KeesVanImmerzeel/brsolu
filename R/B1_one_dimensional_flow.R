################################################################################

#' Formula 123.32, p. 66.
#'
#' Calculate the head change Phi at distance x and time t due to a stress
#' change h (= sudden drawdown which is kept constant thereafter) at t=0 at x=0.
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
#' }
#'
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

################################################################################

#' First derivative of formula 123.32 p. 66 with respect to x.
#'
#' @inheritSection Br_123_32 Schematization
#'
#' @inheritParams Br_123_32
#' @return First derivative of formula 123.32 p. 66 with respect to x (-)
#' @export
d_Br_123_32_dx <- function(x, t, S, kD, c, h) {
  labda = Labda(c, kD)
  eta = Eta(c, S)
  h * brmath::dPdx(x / (2 * labda), sqrt(eta * t)) / (2 * labda)
}

################################################################################

#' Formula 123.33, p. 66
#'
#' Calculate the head change Phi at distance x due to a stress h at x=0.
#'
#' @inheritSection Br_123_32 Schematization
#'
#' @details Steady state of formula 123.32 p. 66
#' @inheritParams x_function
#' @inheritParams Labda
#' @param h Constant in the definition of the stress (drawdown) (L)
#' @return Phi Head change (L)
#' @export
Br_123_33 <- function(x, kD, c, h) {
  labda = Labda(c, kD)
  Phi <- h * exp(-x/labda)
  return(Phi)
}

################################################################################

#' Formula 123.34, p. 67.
#'
#' Calculate the head change Phi at distance x and time t due to a stress
#' change F(t)=at: linear drawdown of the surface water level at x=0.
#'
#' @inheritSection Br_123_32 Schematization
#'
#' @inheritParams xt_function
#' @inheritParams Labda
#' @inheritParams Beta
#' @param a Constant in the linear drawdown (F(t)=at) (L/T)
#' @return Phi Head change (L)
#' @export
Br_123_34 <- function(x, t, S, kD, c, a) {
  labda <- Labda(c, kD)
  eta <- Eta(c, S)
  beta <- Beta(kD, S)
  x1 <- x / (2 * labda)
  y1 <- sqrt(eta * t)
  Phi <- a * t * brmath::P(x1, y1) +
    0.5 * a * beta ^ 2 * x * labda * brmath::Pconj(x1, y1)
  return(Phi)
}

