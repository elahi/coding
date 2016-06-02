#######################################################################################
#
# AssignClosestTempPoints(): R function
#  
# Abstract:
# This R script assigns the closest of a set of point temparature measurements 
# to each of a second set of point density measurements.
# The sp package 'spDistsN1' function is used to calculate Euclidean distances
# between points.
# 
# 
# Input Files (2): Comma-separated-value (CSV) files containing, respectively,
#                  point temperature and density measurements. 
# 
# Output File: Comma-separated-value (CSV) file containing the results table:
#              point density measurements and their location, and the location 
#              index, closest temparature measurement, and distance to the 
#              assigned measurement point.
#
# Notes:  To execute this script: 1) Start R,
#                                 2) Load this script into the workspace,
#                                 3) At R prompt, enter > AssignClosestTempPoints()
#               
# Author: Rick Reeves
# Date created: August 2007    
# Date revised: July 2010
# National Center for Ecological Analysis and Synthesis (NCEAS),
# University of California, Santa Barbara, CA USA
# http://www.nceas.ucsb.edu/scicomp
#
#######################################################################################
#                                 
AssignClosestTempPoints <- function()
{

# Latest version: Assign closest points from a second point set

   require(sp)
   
   temperaturePoints <- read.csv("AssignClosestPointsToPoints/temperaturePoints.csv")                                      
   densityPoints <- read.csv("AssignClosestPointsToPoints/densityPoints.csv")

# promote the input lists to SpatialPointsDataFrames

   coordinates(temperaturePoints) <- c("longitude", "latitude")
   coordinates(densityPoints) <- c("longitude", "latitude")             

# Remove temparature points with 'invalid' value of -199

   validTempPoints <- temperaturePoints[!temperaturePoints$temperature %in% c(-199),]

#  Define these vectors, used in the loop.

   closestSiteVec <- vector(mode = "numeric",length = nrow(densityPoints))
   minDistVec     <- vector(mode = "numeric",length = nrow(densityPoints))

# Get the vector index of the temperature station closest to each field station.
# Use the spDistsN1 function to compute the distance vector between each
# field station site and all of the temperature stations. Then, find and
# retain the actual temperature, and the index of the closest temperature
# to each transect station.
#
# spDistsN1 usage: spDistsN1(pointList, pointToMatch, longlat)
#
# where:
#         pointList   : List of candidate points.
#         pointToMatch: Single point for which we seek the closest point in pointList.
#         longlat     : TRUE  computes Great Circle distance in km,
#                       FALSE computes Euclidean distance in units of input geographic coordinates
#
# We use Great Circle distance to increase distance calculation accuracy at high latitudes
# See the discussion of distance units in the header portion of this file
#
# minDistVec stores the distance from to the closest temperature station to each density measurement point.
# closestSiteVec stores the index of the closest temperature station to each density measurement point.
#
   for (i in 1 : nrow(densityPoints))
   {
      distVec <- spDistsN1(validTempPoints,densityPoints[i,],longlat = TRUE)
      minDistVec[i] <- min(distVec)
      closestSiteVec[i] <- which.min(distVec)
   }
#
# Create the output file: merge the temperature point list with the transect point list
# into a five-column table by merging the temperature point and transect point lists.
#
   PointAssignTemps <- as(validTempPoints[closestSiteVec,]$temperature,"numeric")
   FinalTable = data.frame(coordinates(densityPoints),densityPoints$density,closestSiteVec,minDistVec,PointAssignTemps)
#
# Update the FinalTable column names 
#
   names(FinalTable) <- c("Long","Lat","Density","CloseTempIndex","Distance","Temperature")
#
# And, at the end, write the point assignment file.
#
  message("Write temperature/density assignment table to disk in .csv format")
  write.csv(FinalTable,file="FinalTempAssignments.csv")
}