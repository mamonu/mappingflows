library(plyr)
library(ggplot2)
library(sp)
library(maptools)
#library(threejs)
library(reshape2)
library(rgdal)
library(rgeos)
library(ggmap)


#library(tools)
#write_PACKAGES("~/scripts/mappingflows/")



#delete everything... start with a clean slate:
rm(list=ls())

setwd("~/scripts/mappingflows/")

GBshapefiledir <- "~/scripts/mappingflows/CTRY_DEC_2013_GB_BGC.shp"
GBshape <- readOGR(dsn=".",layer="CTRY_DEC_2013_GB_BGC")

#UK is EPSG27700


fGBshape <- fortify(GBshape)
#load the matrix, careful to have stringsAsFactors as FALSE

df<-read.csv("RMatrixs.csv",header = F,stringsAsFactors = F)
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


#hence now we have a DF with origin & destination ids + coords

#time for ggplot


xquiet <- scale_x_continuous("",breaks=NULL)
yquiet <- scale_y_continuous("",breaks=NULL)

quiet <-list(xquiet,yquiet)

UKm <- get_map(location = c(-2.65, 53.7), source = 'stamen', maptype = "toner")


#UK_map
#(UK_map) 

  myplot <- ggmap(UKm) + ggplot (destination.xy[which(destination.xy$trips>25),],aes(oX,oY)) +
          # ggmap(UKm) + coord_map()+
  
  
  
  geom_polygon(data=fGBshape, aes(x = long, y = lat, group = group))+
  geom_segment(size = 0.1,aes(x=oX,y=oY,xend=dX,yend=dY,alpha=(trips)),col="white") +
  scale_alpha_continuous(range=c(0.03,0.63)) +
  #geom_point(data=destination.xy,size = 0.1, aes(x=oX, y=oY, color="red"))+
#  geom_text(data=destination.xy,size = 0.5,
#            aes(label = o_name))+
  guides(color="none",alpha="none") +
  theme (panel.background= element_rect(fill='grey40'))+ 
  quiet + coord_equal()

print(myplot, newpage = FALSE)

ggsave(myplot, file="sample3nodots.jpg", scale=3, dpi = 600)

