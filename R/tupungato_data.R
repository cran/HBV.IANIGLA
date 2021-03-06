#' Tupungato River basin data
#'
#' A dataset containing a minimal information to simulate the streamflow  discharge
#' of the Tupungato catchment. The basin is located in the north of the Mendoza province
#' (Argentina - 32.90º S; 69.76º W) and has an area of about 1769 \eqn{km^2}. This catchment is
#' the main tributary of the Mendoza River basin (~50 \% of the annual discharge), a stream that
#' supplies with water to most of the province population (~64 \%).
#'
#'
#' @format A list with four elements
#' \describe{
#'   \item{hydro_meteo}{data frame with the air temperature, precipitation and streamflow (mean, lower and upper bounds) series.}
#'   \item{snow_cover}{data frame containing the snow cover (from MODIS) series for each elevation band.}
#'   \item{topography}{data frame with: elevation zone number, minimum, maximum and mean altitude
#'   values for the elevation range and the relative area of each polygon.}
#'   \item{station_height}{numeric vector with the station (Toscas) height (in masl).}
#' }
#'
"tupungato_data"
