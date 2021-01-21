## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(HBV.IANIGLA)

data("glacio_hydro_hbv")

str(glacio_hydro_hbv)

## ----definition, echo = TRUE--------------------------------------------------
## brief arguments description
  # basin: data frame with the same structure of the data("glacio_hydro_hbv) (colnames included).
  # tair: numeric matrix with air temperature inputs. 
  # precip: numeric matrix with precipitation inputs. 
  # pet: numeric matrix with potential eavapotranspiration inputs. 
  # param_snow: numeric vector with snow module parameters.
  # param_ice: numeric vector with glacier parameters.
  # param_soil: numeric vector with soil moisture parameters.
  # param_route: numeric vector with the routing parameters.
  # param_route_ice: numeric vector with the glacier routing parameters.
  # param_tf: numeric vector with the transfer function parameter.
  # init_snow: numeric value with initial snow water equivalent. Default value being 20 mm.
  # init_ice: numeric value with initial snow water equivalent of the glaciers. Default value
  # being 20 mm.
  # init_soil: numeric value with initial soil moisture content. Default value being 0 mm.
  # init_route: numeric vector with bucket water initial values. Default values are 0 mm.
  # init_route_ice: numeric value with glacier bucket initial value. Default values are 0 mm.
## output
  # simulated streamflow series.
glacio_hydrological_hbv <- function(basin,
                                    tair,
                                    precip,
                                    pet,
                                    param_snow,
                                    param_ice,
                                    param_soil,
                                    param_route,
                                    param_route_ice,
                                    param_tf,
                                    init_snow = 20,
                                    init_ice = 20,
                                    init_soil = 0,
                                    init_route = c(0, 0, 0),
                                    init_route_ice = 0
                                    ){
  n_it <- nrow(basin)

  # create output lists
  snow_module   <- list()
  ice_module    <- list()
  soil_module   <- list()
  route_module  <- list()
  route_ice_mod <- list()
  tf_module     <- list()

  # snow and soil module in every elevation band
  for(i in 1:n_it){
    snow_module[[ i ]] <-
      SnowGlacier_HBV(model = 1, inputData = cbind(tair[ , i], precip[ , i]),
                      initCond =  c(init_snow, 2), param = param_snow)

    ice_module[[ i ]] <-
      SnowGlacier_HBV(model = 1, inputData = cbind(tair[ , i], precip[ , i]),
                      initCond =  c(init_ice, 1, basin[i, 'rel_ice']), param = param_ice)

    soil_module[[ i ]] <-
      Soil_HBV(model = 1, inputData = cbind(snow_module[[i]][ , 5] , pet[ , i]),
               initCond = c(init_soil, basin[i, 'rel_soil']), param = param_soil )

  } # end for

  # get total soil discharge
  soil_disch <- lapply(X = 1:n_it, FUN = function(x){
    out <- soil_module[[x]][ , 1]
  })
  soil_disch <- Reduce(f = `+`, x = soil_disch)

  # get swe and total ice melt for all glacier area
  ice_disch <- lapply(X = 1:n_it, FUN = function(x){
    out <- ice_module[[x]][ , 9]
  })
  ice_disch <- Reduce(f = `+`, x = ice_disch)

  ice_swe   <- lapply(X = 1:n_it, FUN = function(x){
    out <- ice_module[[x]][ , 3] *  (basin[x, 'rel_ice'] / sum(basin[ , 'rel_ice']) )
  })
  ice_swe <- Reduce(f = `+`, x = ice_swe)

  # route module
  route_module <- Routing_HBV(model = 1, lake = F, inputData = as.matrix(soil_disch),
                              initCond = init_route, param = param_route )

  route_ice    <- Glacier_Disch(model = 1, inputData = cbind(ice_swe, ice_disch),
                                initCond = init_route_ice, param = param_route_ice  )

  # transfer function
  tf_soil <- round(
    UH(model = 1, Qg = route_module[ , 1], param = param_tf), 4  )

  tf_ice  <- round(
    UH(model = 1, Qg = route_ice[ , 1], param = param_tf), 4  )

  tf_out  <- tf_soil + tf_ice

  return( cbind(total = tf_out, soil = tf_soil, glacier = tf_ice) )


}# end fun

