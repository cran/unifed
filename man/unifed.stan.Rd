% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doc-unifed_stan.R
\name{unifed.stan}
\alias{unifed.stan}
\title{Stan functions for working with the unifed distribution}
\description{
Stan functions for working with the unifed distribution
}
\details{
A script with stan functions of the unifed is provided. The script
can be included in stan code. The full path to the script can be
obtained with the function \code{\link{unifed.stan.path}}. The
following list are the names of functions that take one real value:

\describe{
\item{\code{real unifed_kappa(real theta)}}{Computes the cumulant generator of the
    unifed distribution.}
\item{\code{real unifed_kappa_prime(real theta)}}{Computes the first derivative of
    the cumulant generator.}
\item{\code{real unifed_kappa_double_prime(real theta)}}{Computes the second
    derivative of the cumulant generator.}
\item{\code{real unifed_lpdf(real x,real theta)}}{Computes the
    logarithm of the probability density function of a unifed
    distribution. \code{theta} is the value of the canonical
    parameter of the unifed and \code{x} if the value where the
    density is evaluated.}
\item{\code{real unifed_quantile(real p,real theta)}}{Returns the
    p-th quantile of a unifed distribution with canonical parameter
    \code{theta}}.
\item{\code{real unifed_rng(real theta)}}{Returns a simulated value
    of a unifed distribution with canonical parameter
    \code{theta}.}
\item{\code{real unifed_lcdf(real x,real theta)}}{Computes the
    logarithm of the cumulative density function of a unifed
    distribution. \code{theta} is the value of the canonical
    parameter of the unifed and \code{x} if the value where the
    density is evaluated.}
\item{\code{real unifed_kappa_prime_inverse(real mu)}}{Returns the
    inverse of the derivative of the unifed cumulant generator}
\item{\code{real unifed_unit_deviance(real y,real mu)}}{Unit
    deviance function of the unifed.}
}

The following functions take vectors as arguments

\describe{
\item{\code{vector unifed_kappa_v(vector theta)}}{Vectorized
    version of \code{unifed_kappa}.}
\item{\code{vector unifed_kappa_prime_inverse_v(vector
    mu)}}{Vectorized version of \code{unifed_kappa_prime_inverse}.}
\item{\code{void unifed_glm_lp(vector y, vector theta, vector
    weights)}}{Adds to the Log Probability Accumulator the
    logarithm of the likelihood function of a GLM with observed
    response \code{y}, estimated canonical parameter \code{theta}
    and weights \code{weights}.}  }
}
