library(dplyr); library(ggplot2); library(tidyverse); library(tidycensus); library(sf); library(fasterize); library(raster)
library(reshape)


census_api_key(Sys.getenv('census_api')) # get an API key from api.census.gov/data/key_signup.html



philaPopulation <- get_decennial(geography = 'block', # also try tract
                                 variables = 'H010001', geometry = T, 
                                 state = 'PA', county = 'Philadelphia', year = 2010) %>%
  # Add column for population density
  mutate(density = value/st_area(.))

# reproject sf object
philaPopulation <- st_transform(philaPopulation, "+proj=lcc +lat_1=39.93333333333333 +lat_2=40.96666666666667 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs")

# create empty raster and rasterize sf polygons
r <- raster(ncol = 100, nrow = 100)
extent(r) <- extent(philaPopulation)
popDensRaster <- fasterize(philaPopulation, r, field = 'density', fun = 'last')
# plot(popDensRaster)


coords <- as.data.frame(coordinates(popDensRaster))
values <- data.frame(pop = values(popDensRaster),
                     x = coords$x,
                     y = coords$y)


# standardize values for easier visualization
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

values <- values %>%
  mutate(pop = if_else(is.na(pop), 0, pop),
         # this scalar will affect how peaky the peaks are. Higher value = peakier
         pop_st = 0.10*range01(pop),
         # make maximum 0...a weird block NW of fairmount park. maybe high rises in overbrook?
         pop_st = if_else(pop > 0.0828908, 0, pop),
         x_st = range01(x),
         y_st = range01(y))


xquiet <- scale_x_continuous("", breaks=NULL)
yquiet <- scale_y_continuous("", breaks=NULL)
quiet <- list(xquiet, yquiet)

values$ord <- values$y
values_s <- values[order(-values$ord),]


# Create an empty plot called p
p <- ggplot()

backgroundColor <- '#F88379' # other colors I like: blue #1D3A7D, gray #B2B6BF, coral #F88379
lineColor <- 'white'

# This loops through each line of latitude and produced a filled polygon that will mask out the 
# lines beneath and then plots the paths on top.The p object becomes a big ggplot2 plot.
for (i in unique(values_s$ord)){
  p <- p + geom_polygon(data = values_s[values_s$ord == i,], aes(x_st, pop_st + y_st, group = y_st), 
                        size = 0.2, fill = backgroundColor, col = backgroundColor) + 
    geom_path(data = values_s[values_s$ord == i,],aes(x_st, pop_st + y_st, group = y_st), size = 0.2, 
              lineend = 'round', col = lineColor)
}


# Display plot and save as pdf
# pdf(file="H:/populationLines_coral.pdf", width=10, height=10)
p + theme(panel.background = element_rect(fill = backgroundColor,colour = backgroundColor)) + quiet
# dev.off()
