#' Calculate the Leakage factor
#'
#' @param kD Hydraulic conductivity (L2/T)
#' @param c Hydraulic resistance (T)
#' @return Leakage factor (L)
#' @export
Labda <- function(c, kD) {
  sqrt(c * kD)
}

#' Calculate Eta
#'
#' @inheritParams Labda
#' @param S Storage coefficient (-)
#' @return Eta (T-1)
#' @export
Eta <- function(c, S) {
  1 / (c * S)
}

