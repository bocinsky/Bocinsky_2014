## This is a script for processing the
## CanVec digital cartographic reference product
## produced by Natural Resources Canada.

## The script loads the waterbodies component
## of the CanVec dataset, extracts only natural 
## water bodies, filters the islands and waterbodies
## by area (if requested by the user) and outputs
## the waterbodies as a polygon shapefile.

## Raw CanVec dataset should be put in a folder
## whose path is set below by the user.

## The CanVec dataset may be downloaded at 
## http://geogratis.cgdi.gc.ca
## or directly at 
## ftp://ftp2.cits.rncan.gc.ca/pub/canvec/

## Extrinsic defensibility is defined in Bocinsky 2013:
## Bocinsky, R. Kyle. 2013. Extrinsic site defensibility and landscape-based archaeological
## inference: An example from the Northwest Coast. Journal of Anthropological Archaeology nn(nn):nnn--nnn.

## Author: R. Kyle Bocinsky
## Date: 04/25/2013


library(sp)
library(maptools)
library(rgeos)
library(rgdal)
library(raster)

########## PARAMETERS ##########
# Set this to the directory where the CanVec data is stored.
canvecDirectory <- "/PATH/TO/CanVEC"

# Set this to the path to where the dem data is stored.
# This should be the FINAL_DEM made using the DEM.R script.
demPath <- "/PATH/TO/FINAL_DEM"

# Set this to the directory where the waterbodies data should be output.
outputDirectory <- "/PATH/TO/output"

# Set the minimum water body size.
# This was set to zero in Bocinsky 2013.
minArea <- 0

########## END PARAMETERS ##########

########## SOURCE ##########
# Load the DEM
dem <- raster(demPath)

# Set the north, south, east, and west boundaries of the simulation area
# This is done from the DEM
UTM_North <- ymax(dem)
UTM_South <- ymin(dem)
UTM_East <- xmax(dem)
UTM_West <- xmin(dem)

# Create matrix of coordinates
datainUTM<-matrix(c(UTM_East, UTM_West, UTM_West, UTM_East,UTM_East, UTM_North,UTM_North,UTM_South,UTM_South,UTM_North),nrow=5)

# Set universal projection from the DEM
master.proj <- CRS(projection(dem))

# Create SpatialPolygon of simulation area
sim.poly <- Polygons(list(Polygon(datainUTM, hole=FALSE)),ID='A')
sim.poly <- SpatialPolygons(list(sim.poly), proj4string=master.proj)

# Load all available waterbodies information from the CanVec dataset
waterbodies <- NULL
directories <- list.dirs(path=canvecDirectory)
for(i in directories[c(2:length(directories))]){
	zips <- list.files(i)
	for(j in zips){
		rm(waterbodies.temp)
		
		unzip(paste(i,'/',j,sep=''),exdir='../temp')
		files <- list.files('../temp')
		if(any(grepl('1480009_2.shp',files)==T)){
			waterbodies.temp <- suppressWarnings(readOGR("../temp/", layer=ogrListLayers(paste('../temp/',files[grep('1480009_2.shp',files)],sep=''))))
			waterbodies.temp <- spChFIDs(waterbodies.temp,paste(i,j,row.names(waterbodies.temp),sep=''))
			if(!is.null(waterbodies)){
				waterbodies <- spRbind(waterbodies, waterbodies.temp)
			}else{
				waterbodies <- waterbodies.temp
			}
		}
		unlink('../temp',recursive=T)
	}
}

orig.projection <- proj4string(waterbodies)

# Drop all man-made waterbodies
waterbodies <- waterbodies[waterbodies$CODE %in% c(1481312, 1483312, 1482402, 1481402, 1481362, 1483362, 1481372, 1483372) & waterbodies$ISOLATED==0,]

# Merge line segments that meet at their ends.
waterbodies <- gUnaryUnion(waterbodies)

# Drop waterbodies that are smaller than minArea.
waterbodies <- waterbodies@polygons[[1]]@Polygons
areas <- vector(length=length(waterbodies))
for(i in 1:length(waterbodies)){
	if(waterbodies[[i]]@area >= minArea){
		areas[i] <- TRUE
	}
}
waterbodies <- waterbodies[areas]

# Create the spatial polygons data.
waterbodies <- Polygons(waterbodies,"waterbodies")
waterbodies <- SpatialPolygons(list(waterbodies), proj4string=CRS(orig.projection))

# Transform to the DEM projection
waterbodies <- spTransform(waterbodies, master.proj)

# Crop to the study area
waterbodies <- gIntersection(waterbodies,sim.poly)

# Create the SpatialPolygonsDataFrame
waterbodies <- SpatialPolygonsDataFrame(waterbodies, data.frame(NAMES=c(1:length(waterbodies))), match.ID = TRUE)

# Shift to align with DEM.
# The CanVec data is projected in the NAD83CSRS (Canadian Spatial Reference System),
# which is not in the Proj4J library. The shift is about 30 meters north in the Bocinsky 2013
# study area.
waterbodies <- elide(waterbodies, shift=c(0,30))

# Reset the projection information
proj4string(waterbodies) <- projection(dem)

# Write the data.
writeOGR(waterbodies, outputDirectory,"waterbodies","ESRI Shapefile", overwrite_layer=TRUE)