##' Robin Elahi
##' 25 March 2016
##' Intro to 'raster'
##' From vignette by Hijmans, Dec 18, 2015

library(raster)
library(rgdal)

##### Intro stuff #####
# Make rasterlayer object from scratch
x <- raster()
x

# With other parameters
x <- raster(ncol = 36, nrow = 18, xmn = -1000, xmx = 1000, 
            ymn = -100, ymx = 900)

x
res(x)
ncol(x)

# Change resolution
res(x) <- 100
res(x)
ncol(x)

# Change number of columns (affects resolution)
ncol(x) <- 18
ncol(x)
res(x)

projection(x) <- "+proj=utm +zone=48 +datum=WGS84"
x

r <- raster(ncol = 10, nrow = 10)
r
hasValues(r)

set.seed(0)
values(r) <- runif(ncell(r))
hasValues(r)

inMemory(r)

values(r)[1:10]
plot(r, main = "raster with 100 cells")

res(r)
dim(r)
xmax(r)

# Change max x coordinate of the bounding box
xmax(r) <- 0
hasValues(r)
res(r)
plot(r)

ncol(r) <- 6
hasValues(r)
res(r)
plot(r)
dim(r)

# Multi-layer objects
r1 <- r2 <- r3 <- raster(nrow = 10, ncol = 10)
# assign random cell values
values(r1) <- runif(ncell(r1))
values(r2) <- runif(ncell(r2))
values(r3) <- runif(ncell(r3))

# Combine three rasterlayer objects into a raster stack
s <- stack(r1, r2, r3)
s
nlayers(s)

# Combine into brick
b <- brick(r1, r2, r3)
# same as:
b2 <- brick(s)

##### Load adriatic depth layer #####

# This will only import one layer (the 1st)
adr <- raster("../evp_vms/ignoreFolder/adria_depth/C3_rgb.tif")
adr
plot(adr)

adr2 <- raster("../evp_vms/ignoreFolder/adria_depth/C3_rgb_crop.tif")
adr; adr2 # note that the cropped map has lost a bunch of important info (e.g., lat longs, coordinate reference system)

# create raster brick from file
adr <- raster::brick("../evp_vms/ignoreFolder/adria_depth/C3_rgb.tif")
adr

# extract a single layer
adr1 <- raster(adr, layer = 1)
adr1; plot(adr1)

##### Raster algebra #####
r <- raster(ncol = 10, nrow = 10)
# assign values to cells
values(r) <- 1:ncell(r)
values(r)
s <- r + 10; values(s)
s <- sqrt(s); values(s)
s <- s * r + 5; values(s)
r[] <- runif(ncell(r))
values(r)
r <- round(r); values(r)
r <- r == 1
values(r)

# Can use replacement functions
s[r] <- -0.5; values(s)
s[!r] <- 5
values(s)
s[s == 5] <- 15
values(s)

# adding objects with different numbers of layers (shorter objects get recycled)
r <- raster(ncol = 5, nrow = 5)
r[] <- 1; values(r)
s <- stack(r, r + 1); s; values(s)
q <- stack(r, r+2, r+4, r+6); q; values(q)
x <- r + s + q; x; values(x)

# summary functions return raster layer
a <- mean(r, s, 10)
a; values(a)
b <- sum(r, s)
b; values(b)
st <- stack(r, s, a, b)
st; values(st)
sst <- sum(st)
sst

# use cell status if instead of a raster layer you want a single number summarizing the cell values of each layter
cellStats(st, 'sum')
cellStats(sst, 'sum')

##### High level functions #####

# modifying raster objects
adr

plot(adr) 

plotRGB(adr, r = 1, g = 2, b = 3)

# Manually set crop extent
# crop_adria <- drawExtent()

# Set crop extent with coordinates
cropExtent <- c(12.0, 19.9, 40.0, 46.0)

adrCrop <- crop(adr, cropExtent)
adrCrop
plotRGB(adrCrop, r = 1, g = 2, b = 3, axes = TRUE)

adrCrop@extent
adrCrop@crs


# Plot using ggplot2

library(ggplot2)
library(rgeos)
adrF <- fortify(adrCrop)
head(lnd_f)


xmin = round(min(punti2$lon,na.rm=T),2)-0.01
xmax = round(max(punti2$lon,na.rm=T),2)+0.01
ymin = round(min(punti2$lat,na.rm=T),2)-0.01
ymax = round(max(punti2$lat,na.rm=T),2)+0.01

xt = cut(punti2$lon,seq(xmin,xmax,0.02))
yt = cut(punti2$lat,seq(ymin,ymax,0.02))



# Save in 'native' raster format (.grd) - also results in .gri file (350mb!)
# writeRaster(adrCrop, "../evp_vms/ignoreFolder/adria_depth/adriaCrop_rgb.grd", overwrite = TRUE)

# To save as geotif, need rgdal (dosn't work, not sure why)
writeRaster(adrCrop, "../evp_vms/ignoreFolder/adria_depth/adriaCrop_rgb.tif",
            format = "GTiff", overwrite = TRUE)

##' STOPPED LESSON AT 5.1 
