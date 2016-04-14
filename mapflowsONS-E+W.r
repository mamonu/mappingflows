library(plyr)
library(dplyr)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgdal)
library(rgeos)
library(ggmap)


#delete everything in the environment... start with a clean slate:
rm(list=ls())


#declare the current working directory

setwd("~/scripts/mappingflows/")

#read the shapefile 
#GBshape <- readOGR(dsn=".",layer="CTRY_DEC_2013_GB_BGC")
EWshape <- readOGR(dsn=".",layer="CTRY_DEC_2013_EW_BGC")
# make the shapefile easily plottable with ggplot with fortify()
fEWshape <- fortify(EWshape)


#load the matrix, careful to have stringsAsFactors as FALSE

df<-read.csv("RMatrixsEW.csv",header = F,stringsAsFactors = F)
rownames(df)<-df[,1]    # msoa  codes as rownames
colnames(df)<-df[1,]    # msoa  codes as columns
df = df[-1,]            # delete  first row
df = df[,-1]            # delete  first column 

##### matrix operations for the net matrix

tdf <- t(df)    # from trip matrix of type dataframe to transposed matrix (of type matrix)  
ddf <- t(tdf)   # the transose of the transposed matrix (so the original matrix) in type matrix
class (tdf) <- "numeric"     # make sure that what is in the matrix is numeric
class (ddf) <- "numeric"     # make sure that what is in the matrix is numeric

# in order to create net matrix subtract transposed lower tri from upper matrix 
netmatrix <- ddf - tdf  
 
#put 0 to lower submatrix including diagonal
netmatrix[lower.tri(netmatrix,diag=TRUE)] <- 0

#######end of net matrix operations###

######### now decide on the input matrix ####################
# and tranform it to an edgelist of the form origin,destination, total number of trips


# case 2 : the whole matrix
mdf <-as.matrix(df)
class (mdf) <- "numeric" 
input <- melt(mdf)

# case 3:  only the top sub-matrix
udf <-as.matrix(df)
class (udf) <- "numeric" 
udf[lower.tri(udf,diag=TRUE)] <- 0
input <- melt(udf)
  
# case 4:  only the lower submatrix
ldf <-as.matrix(df)
class (ldf) <- "numeric" 
ldf[!lower.tri(ldf,diag=TRUE)] <- 0
input <- melt(ldf)


# certain sanity checks
# name columns created by melt 
names(input) <- c("origin","destination","total")

#make sure origin & destination are strings and total is numeric
input$origin <- as.character(input$origin)
input$destination <- as.character(input$destination)
input$total <- as.numeric(input$total)


#############end of input selected#######################

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


#the following 3 lines remove the gridlines in the map. 

xquiet <- scale_x_continuous("",breaks=NULL)
yquiet <- scale_y_continuous("",breaks=NULL)
quiet <-list(xquiet,yquiet)



destination_gg <- destination.xy %>% filter(abs(trips)>1) %>%
 # mutate(trips = pmin(pmax(trips, -60), 60)) %>%
  arrange(abs(trips))





  myplot <-  ggplot (destination_gg,aes(oX,oY)) +
  
  geom_polygon(data=fEWshape,fill="black",colour="grey20",aes(x = long, y = lat, group = group))+
  
  geom_segment(size = 0.15,aes(x=oX,y=oY,xend=dX,yend=dY,alpha=(trips)),color="#80be2c") +
    
    
  #green #80be2c                pink #f5d6d6              #blue  #7895f0
    

  scale_alpha_continuous(range=c(0.03,0.33)) +
    
  #scale_color_gradient2(low="#80be2c",high="#7895f0",mid="black",midpoint=0)+
    
  #geom_point(data=destination.xy,size = 0.1, aes(x=dX, y=dY,size=(trips), color="white"))+    #plot points option
#  geom_text(data=destination.xy,size = 0.5,colour="white"  ,                      #plot names option
#            aes(label = o_name))+

  guides(color="none",alpha="none") +
    
  theme (panel.background= element_rect(fill='black'))+ 
  quiet + coord_equal()

  myplot
  


ggsave(myplot, file="sampleEW-net-cols.pdf", scale=3, dpi = 600)

