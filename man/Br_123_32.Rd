% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/B1_one_dimensional_flow.R
\name{Br_123_32}
\alias{Br_123_32}
\title{Formula 123.32, p. 66.}
\usage{
Br_123_32(x, t, S, kD, c, h)
}
\arguments{
\item{x}{Distance (L)}

\item{t}{Time (T)}

\item{S}{Storage coefficient (-)}

\item{kD}{Hydraulic conductivity (L2/T)}

\item{c}{Hydraulic resistance (T)}

\item{h}{Constant in the definition of the stress (= sudden drawdown) (L)}
}
\value{
Phi Head change (L)
}
\description{
Calculate the head change Phi at distance x and time t due to a stress
change h (= sudden drawdown which is kept constant thereafter) at t=0 at x=0.
}
\section{Schematization}{


\itemize{
\item BI. One-dimensional groundwater flow
\item BI-2. One-dimensional groundwater flow in a semi-infinite field
\item The soil is assumed to be homogeneous
\item The flow is non-periodic
\item The boundary condition at x = 0 a is given head or drawdown
\item Leaky aquifers with variable head at x = 0
}
}

