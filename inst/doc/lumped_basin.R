## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(HBV.IANIGLA)

data("lumped_hbv")

head(lumped_hbv)
summary(lumped_hbv)


## -----------------------------------------------------------------------------
# we first consider the SnowGlacier module to take into account 
# precipitation partioning and the snow accumulation/melting. 
snow_module <-
  SnowGlacier_HBV(model = 1, 
                  inputData = as.matrix( lumped_hbv[ , c('T(ºC)', 'P(mm/d)')] ),
                  initCond = c(20, 2), 
                  param = c(1.20, 1.00, 0.00, 2.5) )

# is always advisable to take a look at the output (unless until you get familiarised 
# with the model) 
head(snow_module)

# now see the function documentation...you should be able to relate the model option
# with the matrix output!

## -----------------------------------------------------------------------------
soil_module <-
  Soil_HBV(model = 1,
           inputData = cbind(snow_module[ , "Total"], lumped_hbv$`PET(mm/d)`),
           initCond = c(100, 1),
           param = c(200, 0.8, 1.15) )

head(soil_module)

## -----------------------------------------------------------------------------
routing_module <-
  Routing_HBV(model = 1, 
              lake = F, 
              inputData = as.matrix(soil_module[ , "Rech"]),
              initCond = c(0, 0, 0), 
              param = c(0.9, 0.01, 0.001, 0.5, 0.01) )

head(routing_module)

## ----fig.width = 7------------------------------------------------------------
tf_module <-
  round( 
    UH(model = 1,
       Qg = routing_module[ , "Qg"],
       param = c(1.5) ),
    2)

# let's plot the "true" and simulated hydrographs
plot( x = lumped_hbv[ , "Date"], 
      y = tf_module,
      type = "l", col = "dodgerblue",
      xlab = "Date", ylab = "q(mm/d)")
lines(x = lumped_hbv[ , "Date"],
      y = lumped_hbv[ , "qout(mm/d)"], 
      col = "red")

# you can get a goodness of fit measurement by, 
cor(x = lumped_hbv[ , "qout(mm/d)"], y = tf_module)

# hint: to use hydrological goodness of fit functions (e.g.: NSE - Nash-Sutcliffe
# Efficiency) you can install the hydroGOF package.

## ----definition, echo = TRUE--------------------------------------------------
## brief arguments desription
  # basin: data frame with the same structure of the data("lumped_hbv") (colnames included).
  # param_snow: numeric vector with snow module parameters.
  # param_soil: numeric vector with soil moisture parameters.
  # param_routing: numeric vector with the routing parameters.
  # param_tf: numeric vector with the transfer function parameter.
  # init_snow: numeric value with initial snow water equivalent. Default value being 20 mm.
  # init_soil: numeric value with initial soil moisture content. Default value being 100 mm.
  # init_routing: numeric vector with bucket water initial values. Default values are 0 mm.
## output
  # simulated streamflow series.
hbv_lumped <- function(basin, 
                       param_snow,
                       param_soil,
                       param_routing,
                       param_tf, 
                       init_snow = 20, 
                       init_soil = 100,
                       init_routing = c(0, 0, 0)
                       ){
  
  snow_module <-
  SnowGlacier_HBV(model = 1, 
                  inputData = as.matrix( basin[ , c('T(ºC)', 'P(mm/d)')] ),
                  initCond = c(init_snow, 2), 
                  param = param_snow )
  
  soil_module <-
  Soil_HBV(model = 1,
           inputData = cbind(snow_module[ , "Total"], basin$`PET(mm/d)`),
           initCond = c(init_soil, 1),
           param = param_soil )
  
 routing_module <-
  Routing_HBV(model = 1, 
              lake = F, 
              inputData = as.matrix(soil_module[ , "Rech"]),
              initCond = init_routing, 
              param = param_routing )
 
 tf_module <-
     UH(model = 1,
       Qg = routing_module[ , "Qg"],
       param = param_tf )
 
 
 out <- round(tf_module, 2)
 
 return(out)
  
                       }

## ----fig.width = 7------------------------------------------------------------
# one of the ideas behind a function is to succinctly express 
# operations that will look complicated or too large. In our 
# case, by modifying (in the same line of arguments) just some
# parameters we obtain the basin discharge

streamflow <- 
  hbv_lumped(basin = lumped_hbv, 
             param_snow = c(1.20, 1.00, 0.00, 2.5),
             param_soil = c(200, 0.8, 1.15),
             param_routing = c(0.2, 0.01, 0.005, 0.75, 0.15),
             param_tf = c(1.5))

plot( x = lumped_hbv[ , "Date"], 
      y = streamflow,
      type = "l", col = "dodgerblue",
      xlab = "Date", ylab = "q(mm/d)")
lines(x = lumped_hbv[ , "Date"],
      y = lumped_hbv[ , "qout(mm/d)"], 
      col = "red")

cor(x = streamflow, y =  lumped_hbv[ , "qout(mm/d)"] )


## -----------------------------------------------------------------------------
# first we are going to generate a vector with the possible values
# of our parameter. Let's say that we want to evaluate the effect of
# changing the K0 value.

target_param <- seq(from = 0.1, to = 0.9, by = 0.01)

# now we get the number fo required iterations and we create an empty 
# vector to store the evaluation function results
n_it     <- length(target_param)
response <- c()

# finally run our function n_it times and we plot the results
for(i in 1:n_it){
  
  streamflow <- 
  hbv_lumped(basin = lumped_hbv, 
             param_snow = c(1.20, 1.00, 0.00, 2.5),
             param_soil = c(200, 0.8, 1.15),
             param_routing = c(target_param[i], 0.05, 0.002, 0.9, 0.1),
             param_tf = c(1.5))

  response[i] <- cor(x = streamflow, y =  lumped_hbv[ , "qout(mm/d)"] )
  
}

plot(x = target_param, y = response, type = "p",
     ylim = c(0, 1), pch = 20, cex = 0.5)



