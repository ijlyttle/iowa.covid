#' Iowa population 2019
#' 
#' Estimate of Iowa population by county, 2019.
#' 
#' @format A data frame with 99 rows and 6 variables:
#' 
#' \describe{ 
#'   \item{fips}{FIPS code}
#'   \item{county}{county name}
#'   \item{population}{population}
#'   \item{cumulative_population}{population in this and smaller counties}
#'   \item{quantile_population}{proportion of state population in this and smaller counties}
#'   \item{population_group}{groups counties by populations such that each group has about a quarter of the state's population}
#' }
#' 
#' @source [Iowa Community Indicators Program](https://www.icip.iastate.edu/tables/population/counties-estimates)
#' 
"iowa_county_population"

