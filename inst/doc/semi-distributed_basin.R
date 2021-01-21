## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(HBV.IANIGLA)

data("semi_distributed_hbv")

str(semi_distributed_hbv)

## ----definition, echo = TRUE--------------------------------------------------
## brief arguments description
  # basin: data frame with the same structure of the data("semi_distributed_hbv) (colnames included).
  # tair: numeric matrix with air temperature inputs. 
  # precip: numeric matrix with precipitation inputs. 
  # pet: numeric matrix with potential eavapotranspiration inputs. 
  # param_snow: numeric vector with snow module parameters.
  # param_soil: numeric vector with soil moisture parameters.
  # param_routing: numeric vector with the routing parameters.
  # param_tf: numeric vector with the transfer function parameter.
  # init_snow: numeric value with initial snow water equivalent. Default value being 20 mm.
  # init_soil: numeric value with initial soil moisture content. Default value being 100 mm.
  # init_routing: numeric vector with bucket water initial values. Default values are 0 mm.
## output
  # simulated streamflow series.
hydrological_hbv <- function(basin,
                             tair,
                             precip,
                             pet,
                             param_snow,
                             param_soil,
                             param_route,
                             param_tf,
                             init_snow = 20,
                             init_soil = 0,
                             init_routing = c(0, 0, 0) 
                             ){
  n_it <- nrow(basin)

  # create output lists
  snow_module  <- list()
  soil_module  <- list()
  route_module <- list()
  tf_module    <- list()

  # snow and soil module in every elevation band
  for(i in 1:n_it){
    snow_module[[ i ]] <-
      SnowGlacier_HBV(model = 1, inputData = cbind(tair[ , i], precip[ , i]),
                      initCond =  c(init_snow, 2), param = param_snow)
    soil_module[[ i ]] <-
      Soil_HBV(model = 1, inputData = cbind(snow_module[[i]][ , 5] , pet[ , i]),
               initCond = c(init_soil, basin[i, 'rel_area']), param = param_soil )

  } # end for

  # get total soil discharge
  soil_disch <- lapply(X = 1:n_it, FUN = function(x){
    out <- soil_module[[x]][ , 1]
  })
  soil_disch <- Reduce(f = `+`, x = soil_disch)

  # route module
  route_module <- Routing_HBV(model = 1, lake = F, inputData = as.matrix(soil_disch),
                              initCond = init_routing, param = param_route )

  # transfer function
  tf_module <- round(
    UH(model = 1, Qg = route_module[ , 1], param = param_tf), 4
  )

  return(tf_module)


}# end fun

## -----------------------------------------------------------------------------
# first we are going to create set the parameter range
  # snow module
snow_range <- rbind(
  sfcf = c(0, 1.5),
  tr   = c(-1, 1),
  tt   = c(0, 3),
  fm   = c(1.5, 4)
)

  # soil module
soil_range <- rbind(
  fc   = c(100, 200),
  lp   = c(0.5, 1),
  beta = c(1, 3)
)

  # routing module (here I will give you the correct values)
routing_range <- rbind(
  k0   = c(0.09, 0.09),
  k1   = c(0.07, 0.07),
  k2   = c(0.05, 0.05),
  uzl  = c(5, 5),
  perc = c(2, 2)
)

  # transfer function module (I will give the correct value)
tf_range <- rbind(
  bmax = c(2.25, 2.25)
)


## -----------------------------------------------------------------------------
param_range <- 
  rbind(
    snow_range,
    soil_range,
    routing_range,
    tf_range
  )

head(param_range)

## -----------------------------------------------------------------------------
# set the number of model runs that you want to try
n_run <- 1000

# build the matrix
n_it <- nrow(param_range)

param_sets <- matrix(NA_real_, nrow = n_run, ncol = n_it)

colnames(param_sets) <- rownames(param_range)
for(i in 1:n_it){
  
  param_sets[ , i] <- runif(n = n_run, 
                            min = param_range[i, 1],
                            max = param_range[i, 2]
                            )
    
}

head(param_sets)


## -----------------------------------------------------------------------------
# goodness of fit vector
gof <- c()

# make a loop
for(i in 1:n_run){
  streamflow <- hydrological_hbv(
                             basin = semi_distributed_hbv$basin,
                             tair = semi_distributed_hbv$tair,
                             precip = semi_distributed_hbv$prec,
                             pet = semi_distributed_hbv$pet,
                             param_snow = param_sets[i, rownames(snow_range) ],
                             param_soil = param_sets[i, rownames(soil_range)],
                             param_route = param_sets[i, rownames(routing_range)],
                             param_tf = param_sets[i, rownames(tf_range)]
                             )
  
  gof[i] <- cor(x = streamflow, y = semi_distributed_hbv$qout)
}

param_sets <- cbind(param_sets, gof)

head(param_sets)

## -----------------------------------------------------------------------------
# get the row index
max_gof   <- which.max(param_sets[ , "gof"])

# extract the parameter set
param_opt <- param_sets[max_gof, ]

param_opt

