

# ---- DAY 2: BASIC SPATIAL OPERATIONS IN R ----
rm(list = ls()) # clear out any existing data taking up room in your working environment

#install.packages(c("terra", "mapview", "dplyr", "sf", "ggplot2", "tidyterra")) # install these packages if you don't already have them
library(terra)
library(mapview)
library(ggplot2)
library(tidyterra)
library(sf)

###YOU CAN WORK WITH SPATIAL DATA AS DATA FRAME 
##let's create unprojected spatial data 
data <- data.frame(long=c(-76.13332,-76.86515,-76.851651), # c() concatenates values separated by commas 
                 lat=c(42.85632,42.65465,42.51311))
data #Inspect to see what data looks like

#plot spatial data
plot(data)
#note if "Error in plot.new() : figure margins too large" resize the plot window 
#in the (default lower right window) to be larger
#Here we are using data created in R but could also create spatial points from any .csv, .txt, etc. with coordinates

# ---- SPATIAL DATA TYPES IN TERRA ----

##Create projected spatial data from data.frame with terra
#define coordinate system using EPSG code
coord_ref <- "EPSG:4326"

#create SpatVector class object named pts from data
?vect  #use the ? before a function to see which arguments are needed and their format!! 
pts <-  vect(data,geom = c("long", "lat"), crs = coord_ref)

#inspect pts
pts
plot(pts)

# ---- CREATE A SpatVector OBJECT WITH DATA ----

# Create attributes corresponding to the rows from data 
# (alternatively you would pull from your database/csv etc.)
# here creating sites pond, river, and forest, and ID for each row in data
attributes <-data.frame(site=c("Pond","River","Forest"),ID=1:nrow(data))

#look at attributes
attributes

#use good old cbind() function to add attributes to points
spatvector.df<-cbind(pts,attributes)

#look at spatvector.df
spatvector.df

#write sv.df to a shapefile using function writeVector() 
writeVector(spatvector.df,"output/mypoints.geojson",overwrite=T)
#Does it have to be in shapefile format? NO!! https://gdal.org/drivers/vector/index.html 75+ drivers! 
  
#read in myshapefile using the vect() function 
pts <- vect("output/mypoints.geojson")

#Inspect and check loaded simple features object created from the shapefile
pts #look at pts
str(pts) #look at data structure
crs(pts) #look at coordinate reference system - can be saved to object & applied to other datasets
ext(pts) #look at extent of spatial data 

#wow thats a lot -how about we try this
crs(pts,describe=T)
#much better!!

#lets plot it
plot(pts,col=as.factor(pts$site))#make each site a different color

##Convert the spatVector object to data frame
pts_df <-data.frame(pts)

#look at pts_df
pts_df

#aww no coordinates -what if we want them??
#get coordinates
coords<-data.frame(crds(pts))

#and combine/view
(pts_df_coords<-cbind(pts_df,coords))


# ---- WHAT ABOUT OTHER VECTOR PACKAGES? ----

#sp is out of date but sf still good!

#convert sp/sf to spatVector object using vect()
#Create a sp object using as(shp,"Spatial")
#(need to load both packages sp and raster)

#convert from spatVector to simplefeatures using st_as_sf()
library(sf)#load the sf library
library(dplyr)

#Check your spatVector object
pts

#and convert & inspect!
(simple_features<-st_as_sf(pts))

# plot
plot(simple_features) # see how there are two attributes, and two plots?
plot(simple_features["geometry"], axes = TRUE) # here's just how to plot the geometry

# manipulate
simple_features[2,] # you can subset an sf object like a regular dataframe
river<- simple_features %>%
  filter(site == "River") # and filter objects based on their attribute
river # see how it's only 1 feature now? 

#convert a simple features (back) to a spatVector using vect()
(spat_vector<-vect(simple_features))
#seem familiar?

# ---- WHAT ABOUT CREATING A RASTER? ----

# Create a matrix of values using the terra package

?rast #see what arguments are required to make a raster note :: calls package terra

#let's copy our crs from 'shp'
crs.shp<-crs(shp)

#let's inspect
crs.shp

#okay lets make our raster from scratch & give it 10 rows and 13 columns 
#plotted in WGS84
raster <- rast(nrows=10, ncols=13, crs=crs.shp)

#create 10 x 13 =130 values and assign values to raster
values(raster)<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,
                   0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,1,1,1,0,1,1,0,0,0,1,1,1,1,
                   1,1,1,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,0,1,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,
                   0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

#inspect raster
raster

#look at its crs
crs(raster,describe=T)

#plot raster
plot(raster,col=c("black","green"))

#save raster to file
writeRaster(raster,"output/myraster.tif",overwrite=T)

#load raster from file
raster_2 <- rast("output/myraster.tif")

#plot raster to inspect
plot(raster_2,col=c("black","purple"))


# ---- SPATIAL TRANSFORMATIONS ---- #

# let's take some real data straight out of California...
areas<- vect("data/lesson02/CA_protected_areas.gpkg")
roads<- vect("data/lesson02/CA_roads.geojson")
# and visualize
mapview(areas) + mapview(roads) # toggle between map types for a different base layer

# wow that's all of California! What if we just wanted to look at a smaller area?

tree_cover<- rast("data/lesson02/clipped_treecover_utm.tif")
(tree_cover) # notice this is now a raster
plot(tree_cover) # this raster gives us percent tree cover for every 27.3782m by 27.3782m pixel

# the shape of our raster looks slightly different- how does it compare to the other two?
ext(tree_cover)
st_bbox(areas)
st_bbox(roads) 

# now why are these numbers so different??

# 1) tree_cover is a much smaller extent than "areas"- let's see 
  # First let's get the extent of our raster as its own object
e <- ext(tree_cover)
  # and we can covert the extent to a polygon
p <- as.polygons(e)
  # We can assign it a CRS from the raster
crs(p) <- crs(tree_cover)
  # and plot it with the "areas" layer to compare
mapview(p) + mapview(areas) # and you should see a square box in the middle of California

# 2) we have two different spatial projections!!!
    # this is a reminder that we have to be careful with mapview- it will show us our spatial data
    # without us realizing that we're actually using two different projection systems. Let's fix that
crs(roads, describe=T)
crs(areas, describe=T)
crs(tree_cover, describe=T)

# you should notice that roads has a code of 4269, while areas and our raster layer have a code of 32610.
# If we wanted to only look at the roads and protected areas within our square box on our map of California,
# could we do it without changing our spatial projection? Let's try...

# we can use either "crop" (which crops by our object's bounding box) or "mask" (which will crop exactly by the geometry)
clipped <- crop(roads, p) # remember, p is our square in the middle of California
clipped2 <- mask(roads, p) 

plot(clipped) 
plot(clipped2) # and both are totally empty
  # it looks like it can't clip if the spatial projections don't match

# How can we update our roads layer to match the projection of "p"? 
roads_utm<- project(roads, crs(areas))

# now check it...
crs(roads_utm, describe=T)
# or you can check this way
crs(roads_utm) == crs(areas)
# looks good! Let's try clipping our layers

# and since our "p" is just a big square, we can use crop
clipped_roads <- crop(roads_utm, p) # remember, p is our square in the middle of California
plet(clipped_roads) # perfect! let's do the same with our proected areas

# Let's use the sf package to manipulate our "areas" data, that way we can learn
  # multiple ways to do the same thing
p<- st_as_sf(p) # first, we must make our "terra" objects into "sf" objects
areas<- st_as_sf(areas)
st_is_valid(areas) # this is a good thing to check for spatial data we get online, as sometimes
  # layers with many overlapping polygons are "not valid" due to human error
areas_valid<- st_make_valid(areas) # this can take a little while...

# now let's try cropping using "st_intersection"
clipped_areas<- st_intersection(areas_valid, p)
mapview(clipped_areas) + mapview(clipped_roads) # ah, much better!

# let's save these two smaller layers for later
st_write(clipped_areas, "output/CA_protected_areas_clipped.geojson")
st_write(clipped_roads, "output/CA_roads_clipped.geojson") 
# looks like we forgot that our roads layer is a vector!

# this is common when processing spatial data- sometimes we prefer commands in one package vs. another, and need to switch
# back and forth between them. We could do one of two things: either make "clipped_roads" an sf object, or write it as 
# a vector file, such as:

st_write(st_as_sf(clipped_roads), "output/CA_roads_clipped.geojson") # here's making it an sf object
writeVector(clipped_roads, "output/CA_roads_clipped.geojson", overwrite = T) # and here's what we used above
  # we add the "overwrite = T" because this layer already exists (we just wrote it!)
  # this is a good feature to add if you are continually updating spatial data

# you'll notice that however we read it in, the only thing that changes the shapefile is if we read it in
# using the sf library, or the terra library- the object itself contains the same data

# zooming in on our layers, let's get a better idea of what's going on
#let's see what the columns of the attribute table are
names(clipped_areas)
#alternatively, to see the top six rows of data (same as L. 27)
head(clipped_areas)
#how many features are in each object?
nrow(clipped_roads)
nrow(clipped_areas) # looks like it's just 1 polygon made up of several little ones- a multipolygon
                    # so let's split it up
clipped_areas_split <- st_cast(clipped_areas, "POLYGON")
nrow(clipped_areas_split) # better!

# let's make a column that tells us how large each protected area is
clipped_areas_split$area<- st_area(clipped_areas_split)
  # since this is in meters squared, let's make it kilometers squared so it's easier to understand
clipped_areas_split$area_km2<- as.numeric(clipped_areas_split$area/1e6)

#what if we wanted to display our protected areas as a function of their size?
ggplot() + 
  #adds protected areas, with fill by AREA
  geom_spatvector(data = clipped_areas_split, aes(fill = area_km2), lwd = 0) +
  #add color palette
  viridis::scale_fill_viridis()+
  #label protected areas in legend
  #NOTE USE OF BQUOTE FOR SUPERSCRIPT
  labs(fill = bquote("Area (km"^2*")")) +
  #add title and subtitle
  ggtitle("Protected Areas in California", subtitle = "Subtitle option if you want it!")

# looks like quite a spread but most are quite small- let's take a look at our data another way
hist(clipped_areas_split$area_km2) # it looks like the vast majority of protected areas are quite small

big_PAs<- clipped_areas_split %>%
  filter(area_km2 > 200) # here we're treating the vector like a dataframe, and filtering by size

#what if we wanted to display just our little protected areas as a function of their size?
ggplot() + 
  #adds protected areas, with fill by AREA
  geom_spatvector(data = big_PAs, aes(fill = area_km2), lwd = 0) +
  #add color palette
  viridis::scale_fill_viridis()+
  #label protected areas in legend
  #NOTE USE OF BQUOTE FOR SUPERSCRIPT
  labs(fill = bquote("Area (km"^2*")")) +
  #add title and subtitle
  ggtitle("Protected Areas in California", subtitle = "Subtitle option if you want it!")

# similarly, we can use the "perim()" function to look at road length:
clipped_roads$length <- perim(clipped_roads)/1000 # divide by 1000 so it goes from meters to km
long_roads<- clipped_roads %>%
  filter(length > 40)

#what if we are interested in selecting only those little PAs that intersect with roads?
#what if we wanted to display just our little protected areas as a function of their size?
ggplot() + 
  #adds protected areas, with fill by AREA
  geom_spatvector(data = long_roads, aes(color = length), lwd = 1) +
  coord_sf() +
  #add color palette
  viridis::scale_color_viridis()+
  #label protected areas in legend
  #NOTE USE OF BQUOTE FOR SUPERSCRIPT
  labs(color = "Length (km)") +
  #add title and subtitle
  ggtitle("Long Roads in California", subtitle = "Subtitle option if you want it!")

# now let's read in some county information





