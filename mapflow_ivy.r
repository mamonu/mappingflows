#library(plyr)
library(dplyr)
library(ggplot2)
library(sp)
library(maptools)
library(reshape2)
library(rgdal)
#library(rgeos)
#library(ggmap)


#read the shapefile 
#GBshape <- readOGR(dsn=".",layer="CTRY_DEC_2013_GB_BGC")
EWshape <- readOGR(dsn=".",layer="CTRY_DEC_2013_EW_BGC")
# make the shapefile easily plottable with ggplot with fortify()
fEWshape <- fortify(EWshape)


#load the matrix, careful to have stringsAsFactors as FALSE

df <- read.csv("RMatrixsEW.csv",header = T, stringsAsFactors = F)
rownames(df) <- df[ , 1]    # msoa  codes as rownames
df = df[ , -1]            # delete  first column 

# case 1 : create net matrix subtract transposed lower tri from upper matrix 
# and tranform it to an edgelist of the form origin,destination, total number of trips
netmatrix <- as.matrix(df - t(df) )
net_edges <- melt(netmatrix)  %>% filter(value>0)
names(net_edges) <- c("origin","destination","total")

# case 2 : the whole matrix
full_edges <- melt(as.matrix(df))  %>% filter(value>0)
names(full_edges) <- c("origin","destination","total")

#make sure origin & destination are strings and total is numeric
net_edges$origin <- as.character(net_edges$origin)
net_edges$destination <- as.character(net_edges$destination)
full_edges$origin <- as.character(full_edges$origin)
full_edges$destination <- as.character(full_edges$destination)

#############end of input selected#######################

#load centroid data,
centroids <- read.csv ("MSOA_based_LAD_PWCs.csv", header = T, stringsAsFactors = F)

# merge the edgelist with the centroid dataframes by common cell 
# (called origin in edgelist and GSS_code in the MSOA file) 


net_df <- net_edges %>% merge (centroids,by.x="origin",by.y="GSS_code")  %>%
  merge (centroids,by.x="destination",by.y="GSS_code")
names(net_df) <- c("origin","destination","trips","oX","oY","o_name","dX","dY","d_name")

full_df <- full_edges %>% merge (centroids,by.x="origin",by.y="GSS_code")  %>%
  merge (centroids,by.x="destination",by.y="GSS_code")
names(full_df) <- c("origin","destination","trips","oX","oY","o_name","dX","dY","d_name")

# define function to split segments
interp_points <- function(data, n = 10){
  data <- data %>% mutate(noX = oX, noY = oY, ndX = dX, ndY = dY) 
  df_list <- lapply(1:n, function(k) {
    data %>% mutate(seg_id = k,
                    oX = (n-k+1)/n*noX + (k-1)/n*ndX,
                    oY = (n-k+1)/n*noY + (k-1)/n*ndY,
                    dX = (n-k)/n*noX + k/n*ndX,
                    dY = (n-k)/n*noY + k/n*ndY)
  })
  do.call('rbind', df_list)
}

#trimming/transforming the values:
net_interp  <- net_df %>% interp_points %>% mutate(trips = log(log(trips)))
net_both <- rbind(net_interp %>% mutate(dir = 'Net flow from:'), 
                  net_interp %>% mutate (seg_id = 11 - seg_id, dir = 'Net flow to:')) %>%
            arrange( seg_id, trips)

full_interp  <- full_df %>% interp_points %>% mutate(trips = log(log(trips)))
full_both <- rbind(full_interp %>% mutate(dir = 'Total flow from:'), 
                  full_interp %>% mutate (seg_id = 11 - seg_id, dir = 'Total flow to:')) %>%
  arrange( seg_id, trips)

#time for ggplot: colors and style:
#green #80be2c                pink #f5d6d6              #blue  #7895f0
xquiet <- scale_x_continuous("",breaks=NULL)
yquiet <- scale_y_continuous("",breaks=NULL)
dark <- theme (panel.background= element_rect(fill='black'), 
               plot.background=element_rect(color="black",fill="black"), 
               strip.background=element_rect(fill="black",color="black"), 
               strip.text.x=element_text(color="grey70") )
my_style <-list(xquiet, yquiet, dark,   guides(color="none",alpha="none"), coord_equal())


# two images where the gradient goes from light blue to dark
myplot <- ggplot(net_both %>% filter(trips>0)) +  my_style +
  geom_polygon(data = fEWshape, fill = "black", colour = "grey20", aes(x = long, y = lat, group = group)) +
  geom_segment(size = 0.05, aes(x = oX, y = oY, xend = dX, yend = dY, alpha = trips, color = seg_id)) +
  scale_alpha_continuous(range=c(0.01,0.4)) +
  facet_wrap(~dir)
myplot
ggsave('net flow4.pdf', scale = 1)

# one image using blue-green gradient 
myplot <- ggplot(net_both %>% filter(trips>1, dir=='Net flow to:')) +  my_style +
  geom_polygon(data = fEWshape, fill = "black", colour = "grey20", aes(x = long, y = lat, group = group)) +
  geom_segment(size = 0.05, aes(x = oX, y = oY, xend = dX, yend = dY, alpha = trips, color = seg_id)) +
  scale_color_gradient(high = '#7895f0', low = '#80be2c') +
  scale_alpha_continuous(range=c(0.1,0.4)) 
myplot
ggsave('net flow3.pdf', scale = 1)





