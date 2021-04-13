#' Car insurance claims
#'
#' This data set is based on one-year vehicle insurance policies taken
#' out in 2004 or 2005. There are 67856 policies, of which 4624
#' (6.8\%) had at least one claim.
#'
#' @format A data frame with  67,856 rows and 11 columns:
#' \describe{
#' \item{veh_value}{vehicle value, in $10,000s}
#' \item{exposure}{Values between 0 and 1}
#' \item{clm}{occurrence of claim (0 = no, 1 = yes)}
#' \item{numclaims}{number of claims}
#' \item{claimcst0}{claim amount (0 if no claim)}
#' \item{veh_body}{vehicle body, coded as
#'   \describe{
#'   \item{BUS}{ }
#'   \item{CONVT}{convertible}
#'   \item{COUPE}{ }
#'   \item{HBACK}{hatchback}
#'   \item{HDTOP}{hardtop}
#'    \item{MCARA}{motorized caravan}
#'    \item{MIBUS}{minibus}
#'    \item{PANVN}{panel van}
#'    \item{RDSTR}{roadster}
#'    \item{SEDAN}{ }
#'    \item{STNWG}{station wagon}
#'    \item{TRUCK}{ }
#'    \item{UTE}{utility}}}
#' 
#' \item{veh_age}{age of vehicle: 1 (youngest), 2, 3, 4}
#' \item{gender}{gender of driver: M, F}
#' \item{area}{driver's area of residence: A, B, C, D, E, F}
#' \item{agecat}{driver's age category: 1 (youngest), 2, 3, 4, 5, 6}
#' }
#'
#' @source \url{http://www.businessandeconomics.mq.edu.au/our_departments/Applied_Finance_and_Actuarial_Studies/research/books/GLMsforInsuranceData}
#'
#' @references{
#' De Jong, P., and G.Z. Heller. 2008. Generalized Linear Models for Insurance Data. Cambridge University Press.
#' http://dx.doi.org/10.1017/CBO9780511755408
#' 
#' }
"car.insurance"
