library(threejs)
library(proj4)
library(plyr)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgdal)
library(rgeos)
library(ggmap)


#earth <- "http://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.jpg"



#delete everything... start with a clean slate:
rm(list=ls())

setwd("~/scripts/mappingflows/")

#GBshapefiledir <- "~/scripts/mappingflows/CTRY_DEC_2013_GB_BGC.shp"

#GBshape <- readOGR(dsn=".",layer="CTRY_DEC_2013_GB_BGC")
EWshape <- readOGR(dsn=".",layer="CTRY_DEC_2013_EW_BGC")
#UK is EPSG27700


fEWshape <- fortify(EWshape)
#load the matrix, careful to have stringsAsFactors as FALSE

df<-read.csv("RMatrixsEW.csv",header = F,stringsAsFactors = F)
rownames(df)<-df[,1]    # msoa  codes as rownames
colnames(df)<-df[1,]    # msoa  codes as columns
df = df[-1,]            # delete  first row
df = df[,-1]            # delete  first column 


# tranform it to edgelist of the form origin,destination, total number of trips
input <- melt(as.matrix(df))

# name columns created by melt
names(input) <- c("origin","destination","total")


#load centroid data,

centroids <- read.csv ("MSOA_based_LAD_PWCs.csv",header = T,stringsAsFactors = F)


# merge the edgelist with the centroid dataframes by common cell 
# (called origin in edgelist and GSS_code in the MSOA file) 


orig.xy <- merge (input,centroids,by.x="origin",by.y="GSS_code")
#name columns of resulting DF as follows("origin","destination","trips","oX","oY","o_name")
names(orig.xy) <- c("origin","destination","trips","oX","oY","o_name")

#in order to avoid problems force fields to characters
orig.xy$origin <- as.character(orig.xy$origin)
orig.xy$destination <- as.character(orig.xy$destination)


# merge the resultant DF  with the centroid dataframes again by common cell 
# (called destination in edgelist and GSS_code in the MSOA file) 

destination.xy <- merge (orig.xy,centroids,by.x="destination",by.y="GSS_code")
#name columns of resulting DF and force fields to characters
names(destination.xy) <- c("origin","destination","trips","oX","oY","o_name","dX","dY","d_name")
destination.xy$origin <- as.character(destination.xy$origin)
destination.xy$destination <- as.character(destination.xy$destination)
destination.xy$trips <- as.numeric(destination.xy$trips)


destination.xy$origin<-NULL
destination.xy$destination<-NULL

destination.xy$o_name<-NULL
destination.xy$d_name<-NULL


#destination.xy$trips<-NULL


latlong <- "+init=epsg:4326"
ukgrid  <- "+init=epsg:27700"


outm <-SpatialPoints(cbind(destination.xy$oX,destination.xy$oY), proj4string=CRS(ukgrid))
dutm <-SpatialPoints(cbind(destination.xy$dX,destination.xy$dY), proj4string=CRS(ukgrid))
olonglatcoor<-spTransform(outm,CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0 +no_defs"))
dlonglatcoor<-spTransform(dutm,CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0 +no_defs"))

dforigin <- as.data.frame(olonglatcoor)
colnames(dforigin)<-c("ox","oy")
dfdest   <- as.data.frame(dlonglatcoor)
colnames(dfdest)<-c("dx","dy")


mylocation <-c(lat=52.88356,long=-1.97406)
mymap<-get_map(mylocation,source="google",zoom=6)

JSarc <- as.data.frame(c(dforigin,dfdest))
JSarc$trips <- destination.xy$trips


alltogether= ggmap(mymap)+
  #geom_polygon(data=fEWshape, aes(x = long, y = lat, group = group))

  geom_point(data=JSarc,size = 0.1, aes(x=ox, y=oy, color="red"))+
  geom_segment(data=JSarc,size = 0.1,aes(x=ox,y=oy,xend=dx,yend=dy,alpha=(trips)),col="white")+
  stat_density2d(data=JSarc,aes(x=ox,y=oy,color=trips,alpha=0.1),size=2,bins=8,geom="polygon") +
  scale_alpha_continuous(range=c(0.03,0.43)) 
alltogether


ggsave(alltogether, file="sampleEWdensity3.jpg", scale=3, dpi = 600)



#globejs(img=earth, bg="white",arcs=JSarc, emissive="#aaaacc",atmosphere = TRUE)