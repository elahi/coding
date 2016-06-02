# Get buffer around points


# http://gis.stackexchange.com/questions/131168/buffer-and-union-in-r-without-reducing-to-a-multi-polygon

library(sp)
library(rgeos)
pts_df <- data.frame(lon = c(1,1,2,3), lat = c(1,2,1.5,2.5))
pts <- SpatialPoints(cbind(c(1, 1, 2, 3), c(1, 2, 1.5, 2.5))) 
pts

plot(gBuffer(pts, width=0.6), lwd=2)
points(pts, pch=20, cex=2)
text(coordinates(pts), labels=seq_len(length(pts)), pos=4, font=2)


b <- gBuffer(pts, width = 0.6)
b
plot(b)
over(pts, b)


b2 <- gBuffer(pts, width = 0.6, byid = TRUE)
plot(b2)
over(pts, b2)

b3 <- gUnaryUnion(b2)
plot(b3)
over(pts, b3)


# From Fra
library(spatstat)

pts_df

polys<-list() 

for(i in 1:nrow(pts_df)) {
  discbuff <- disc(radius = 0.009, centre = c(pts_df$lon[i], pts_df$lat[i]))
  discpoly<-Polygon(rbind(cbind(discbuff$bdry[[1]]$x, 
                                y=discbuff$bdry[[1]]$y), c(discbuff$bdry[[1]]$x[1], 
                                                           y=discbuff$bdry[[1]]$y[1]))) 
  polys<-c(polys, discpoly) 
}

polys

spolys<-list() 
for(i in 1:length(polys)) { 
  spolybuff<-Polygons(list(polys[[i]]), ID=row.names(pts_df)[i]) 
  spolys<-c(spolys, spolybuff) 
} 

spolys

polybuffs <- SpatialPolygons(spolys)
polybuffs
plot(polybuffs)
plot(b)
