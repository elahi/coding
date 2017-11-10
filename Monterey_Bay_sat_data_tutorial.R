##### Extracting and plotting NOAA Satellite data #####
## Julia Mason
## Oct 18, 2017

## For more information and examples, see https://rmendels.github.io/UsingrerddapXtracto.nb.html
## For info about available datasets and dataset ID codes, see http://coastwatch.pfeg.noaa.gov/

# Install/load libraries
library (devtools)
devtools::install_github ("ropensci/rerddap")
devtools::install_github ("rmendels/rerddapXtracto")
devtools::install_github ("dgrtwo/gganimate")
devtools::install_github ("ropensci/plotdap")

library (rerddap)
library (rerddapXtracto)
library (ggplot2)
library (mapdata)
library (gganimate)
library (plotdap)
library (sf)

######### Map Chl-a in Monterey Bay

### Define area of interest and time range

# Longitude min and max:
xcoord_1 <- c(-122.5, -120)
# Latitude min and max: 
ycoord_1 <- c(36, 37.5)
# Date min and max (put same day if you only want one):
tcoord_1 <- c("2017-09-01", "2017-09-01")

### Get datasets 

# Primary productivity: 8-day MODIS Chl for the West Coast. Dataset ID is erdMWchla8day.

# what's in this dataset?
Chl_info <- info ("erdMWchla8day")
Chl_info
# Note the number of dimensions. Many have 3 (time, lat, lon), but some, like this one, have 4 (altitude). This means we also have to specifiy a null "z-coordinate" in our extraction command.

# Our variable of interest is "chlorophyll." Different datasets have inconsistent labels. 
param_1 <- "chlorophyll"

### Extract data from the ERDDAP server for our day and location of interest
chl <- rxtracto_3D (Chl_info, 
                    parameter = param_1,
                    xcoord = xcoord_1, 
                    ycoord = ycoord_1, 
                    tcoord = tcoord_1, 
                    zcoord = 0 # note empty zcoord
                    )

str (chl)

### Map it!
# Common practice is to work with log (Chl-a), which is easier to interpret. 
log_func <- function (x) log (x)

chla_plot <- plotBBox (chl, 
                       plotColor = "chlorophyll", 
                       myFunc = log_func, 
                       maxpixels = 30000
                       )
# plotColor options come with the rerddap palettes, http://matplotlib.org/cmocean/

# use add_ggplot to customize plot with ggplot features, separated by commas
add_ggplot (chla_plot, 
            ggtitle (paste ("Log Chl-a,", chl$time, sep = " "))
)

###### Map bathymetry in the bay

# Data from ETOPO dataset, http://coastwatch.pfeg.noaa.gov/erddap/griddap/etopo180.html
bathy_info <- info ("etopo180")
mb_bathy <- rxtracto_3D (bathy_info, 
                         parameter = "altitude", 
                         xcoord = xcoord_1,
                         ycoord = ycoord_1
                         ) # note: no date needed

# Depth is somewhat counterintuitive, so we're taking the negative (so "deeper" means a larger value)
neg_func <- function (x) -x

bathy_plot <- plotBBox (mb_bathy, 
                        plotColor = "density", 
                        myFunc = neg_func, 
                        name = "Depth", 
                        maxpixels = 50000
                        )

bathy_plot
# this isn't too helpful bc can't see the coast. What we need is a polygon of just the ocean area of interest. 


###### Use "rxtractogon" to extract data from a polygon

# The Monterey Bay National Marine Sanctuary boundaries are included in the rerddapXtracto packages

xcoord_mbnms <- mbnms$Longitude
ycoord_mbnms <- mbnms$Latitude

tcoord_last <- c("last", "last") # "Last" will automatically update to the most recent available data.

# SST from GHRSST, 1 day composite
sst_info <- info ("jplG1SST")
sst_info

# Extract sst for MBNMS
mbnms_sst <- rxtractogon (sst_info, 
                          parameter = "SST", 
                          xcoord = xcoord_mbnms, 
                          ycoord = ycoord_mbnms, 
                          tcoord = tcoord_last
                          ) # no z coord necessary

mbnms_sst_plot <- plotBBox (mbnms_sst, 
                            plotColor = "temperature", 
                            maxpixels = 50000
                            ) 
add_ggplot (mbnms_sst_plot,
            ggtitle (paste ("SST,", mbnms_sst$time, sep = " ")) 
            )
# sometimes I get a "polygon edge not found" error here, but it will work on a subsequent attempt. 

###### Make an animation
# Note: you need to install ImageMagick, which is somewhat involved

t_anim <- c("2017-09-01", "2017-09-03") # Take a range of several days.  Since this is is a daily dataset, interval will be daily. Monthly datasets will do monthly, etc. 

# Extract the data in our polygon
mbnms_sst_multi <- rxtractogon (sst_info, 
                                parameter = "SST", 
                                xcoord = xcoord_mbnms, 
                                ycoord = ycoord_mbnms, 
                                tcoord = t_anim
                                ) 

# Plot
mbnms_sst_anim <- plotBBox (mbnms_sst_multi,
                            plotColor = "temperature", 
                            maxpixels = 50000, 
                            time = identity, 
                            animate = TRUE
                            )
mbnms_sst_anim 
