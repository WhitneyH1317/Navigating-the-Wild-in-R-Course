

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

### SPATIAL DATA TYPES IN TERRA ###

##Create projected spatial data from data.frame with terra
#define coordinate system using EPSG code
coord_ref <- "EPSG:4326"

#create SpatVector class object named pts from data
?vect  #use the ? before a function to see which arguments are needed and their format!! 
pts <-  vect(data,geom = c("long", "lat"), crs = coord_ref)

#inspect pts
pts
plot(pts)

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


### WHAT ABOUT OTHER VECTOR PACKAGES? ###

#let's check out the sf package
library(sf)

#convert sf to spatVector object using vect()
#create an sf object using as(x,"Spatial")
#convert from spatVector to simplefeatures using st_as_sf()

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

#~#~# Now you try #~#~#
### Exercise 1 ###
# pick 5 points on GoogleMaps- a place in your hometown, your study site, or somewhere you want to visit, and create a dataframe

# assign an attribute name to each point

# assign a CRS (hint- what projection does GoogleMaps automatically use?)

# plot the points

# now filter to a specific point, and plot that one

#~#~# #~#~#

# ---- WHAT ABOUT CREATING A RASTER? ----

# Create a matrix of values using the terra package

?rast #see what arguments are required to make a raster note :: calls package terra

#let's copy our crs from 'shp'
crs.pts<-crs(pts)

#let's inspect
crs.pts

#okay lets make our raster from scratch & give it 10 rows and 13 columns 
#plotted in WGS84
raster <- rast(nrows=10, ncols=13, crs=crs.pts)

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


# ---- SPATIAL MANIPULATIONS ---- #

# let's take some real data straight out of California...
areas<- vect("data/lesson02/CA_protected_areas.gpkg")
roads<- vect("data/lesson02/CA_roads.geojson")
# and visualize
mapview(areas) + mapview(roads) # toggle between map types for a different base layer

# wow that's all of California! What if we just wanted to look at a smaller area?

tree_cover<- rast("data/lesson02/treecover.tif")
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
plot(clipped2) # but wait, both are totally empty
  # it looks like it can't clip if the spatial projections don't match!!!

# How can we update our roads layer to match the projection of "p"? 
roads_utm<- project(roads, crs(areas))

# now check it...
crs(roads_utm, describe=T)
# or you can check this way
crs(roads_utm) == crs(areas)
# looks good! Let's try clipping our layers

# and since our "p" is just a big square, we can use crop
clipped_roads <- crop(roads_utm, p) # remember, p is our square in the middle of California

## we're going to try a new function to view our data- plet is a "terra" function, and works like mapview
plet(clipped_roads) # perfect! 

#~#~# Now you try #~#~#
### Exercise 2 ###

# take our "areas" layer, and create an "areas_4326" object by transforming "areas" to the same crs projection as "roads"

# take our "p" layer, and do the same. Name it p_4326

# check all projections are equal

# now clip the "roads" layer using the "p_4326" to crop instead of "p"

# plot the roads, areas_4326, and p_4326 layers all at once using mapview()

#~#~# #~#~#

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

# sometimes it's easy to confuse our spatial data types- we may prefer commands in one package vs. another, and need to switch
# back and forth between them. We could do one of two things: either make "clipped_roads" an sf object, or write it as 
# a vector file, such as:

writeVector(clipped_roads, "output/CA_roads_clipped.geojson", overwrite = T) # here's writing it as a vector object directly
st_write(st_as_sf(clipped_roads), "output/CA_roads_clipped.geojson") # here's making it an sf object

  # we add the "overwrite = T" because this layer already exists (we just wrote it!)
  # this is a good feature to add if you are continually updating spatial data

# you'll notice that however we read it in, the only thing that changes the shapefile is if we read it in
# using the sf library, or the terra library- the object itself contains the same data

# ---- SPATIAL INVESTIGATIONS ---- # 

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

#what if we wanted to display just our big protected areas as a function of their size?
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

#what if we wanted to display just our long roads as a function of their size?
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
counties<- vect("data/lesson02/CA_counties.geojson")

#~#~# Exercise 3 #~#~#

# use commands above, such as "name()", "ext()", etc. to understand what our 'counties' layer is

# plot it with clipped_roads and clipped_areas

# make sure it's the same crs as the above two layers (if needed, create a new object: counties_utm)

# create a new column in 'counties' that has the area of each county

#~#~# #~#~#

# now let's try understanding what our data looks like across counties...
clipped_areas<- st_as_sf(clipped_areas)
counties_utm<- st_as_sf(counties_utm)

county_PAs<- st_intersection(clipped_areas_split, counties_utm)
# take a look at this object...
head(county_PAs) # now our protected areas have county names, so we can classify them! 
# what different county names do we have? 
unique(county_PAs$COUNTY_NAM)

# let's plot all protected areas in Monterey county that are greater than 10km2
monterey_PAs<- county_PAs %>%
  filter(COUNTY_NAM == "Monterey" & area_km2 > 10)
mapview(counties_utm, zcol = "COUNTY_NAM") + mapview(monterey_PAs, col.regions = "black") 
# how many protected areas are there per county?
county_PAs %>%
  group_by(COUNTY_NAM) %>% # for all protected areas, group by the county name column 
  count() # count the number of rows by group
# that's a bit annoying we have the geometry info- what if we don't need that anymore?
county_PAs %>%
  st_drop_geometry() %>% # get rid of special geometry features
  group_by(COUNTY_NAM) %>%
  count()
# much simpler! 

#~#~# Exercise 4 #~#~#
## find all roads in Santa Cruz county that are over 10km long
# (hint: 1. perform an intersection, 2. filter, 3. count)

# plot the result

#~#~# #~#~#

# ---- SAMPLING TOOLS ---- # 
#let's create 100 random points within the largest Santa Cruz protected area for vegetation sampling
SC_park<- county_PAs %>%
  filter(COUNTY_NAM == "Santa Cruz") %>%
  slice_max(., order_by = area_km2) %>% # this is a helpful function that finds the largest (max) value based on 
                    # the "order_by" column in our data
  vect(.) # make it a vector again (terra object)                                    
plet(SC_park) # plot it

#and let's set a random seed so everyone in the workshop will have points in the same place
set.seed(181) 
random_points <- spatSample(SC_park, size=100, method="random")

#what does this look like?
ggplot() +
  geom_spatvector(data = SC_park, color = "darkgreen", size=1.5) +
  geom_spatvector(data=random_points, color = "black", size=2)+
  ggtitle("100 Random Points in Big Basin State Park")

#let's try again with 100 regular points
regular_points <- spatSample(SC_park, size=100, method="regular")

#what does this look like?
ggplot() +
  geom_spatvector(data = SC_park, color = "darkgreen", size=1.5) +
  geom_spatvector(data=regular_points, color = "black", size=2)+
  ggtitle("100 Regular Points in Big Basin State Park")

#then we will create a 1 km2 grid (1 km x 1 km) over the PA
#first we need to create a template and add values
template <- rast(SC_park, resolution = c(1000,1000))
values(template) <- 1:ncell(template)

#then transform the raster template to polygons
bb_1km2_grid <- as.polygons(template)
#as an alternative, see st_make_grid() in package sf

#let's check it out -looks good!
plot(bb_1km2_grid)

#let's clip this grid to the boundary of Pico Bonito if we aren't interested in the area outside
bb_1km2_grid_isect <- crop(bb_1km2_grid, SC_park)

#how can we see what this looks like?
ggplot() +
  geom_spatvector(data=bb_1km2_grid_isect, fill=NA, color = "black", lwd=1)+
  geom_spatvector(data = SC_park, color = "darkgreen", fill=NA, lwd=.75) +
  #use 'expression' and 'paste' to create superscripts, subscripts etc.
  labs(title=expression(paste("16 km" ^{2}," Grid in Pico Bonito NP")))

#What if we want to put in two camera traps at random locations within each grid?

random_points <- spatSample(bb_1km2_grid_isect, size=2, method="random")

#what does it look like?
ggplot() +
  geom_spatvector(data=bb_1km2_grid_isect, fill=NA, color = "darkblue", size=1)+
  geom_spatvector(data = SC_park, color = "darkgreen", fill=NA, size=0.75) +
  geom_spatvector(data=random_points, color = "purple", size=2)+
  labs(title=expression(paste("16 km" ^{2}," Hexagon Grid in Pico Bonito NP")))

#oh no. what happened? we only have two points for the whole entire grid
#we need to explicitly tell spatSample to sample within each of the cells within the grid

#let's see how many grids we have
dim(bb_1km2_grid_isect)   

#let's try again, supplying a vector such that it knows to sample 2 points
#in each of the grid cells 
random_points <- spatSample (bb_1km2_grid_isect, size=rep(2,nrow(bb_1km2_grid_isect)), method="random")

ggplot() +
  geom_spatvector(data=bb_1km2_grid_isect, fill=NA, color = "darkblue", size=1)+
  geom_spatvector(data = SC_park, fill=NA, color = "darkgreen", size=0.75) +
  geom_spatvector(data=random_points, color = "purple", size=2)+
  ggtitle("Two Random Pts Per Grid Cell in Big Basin State Park")

#let's save your these points to a .csv
  # first let's check that metadata
random_points_sf<- st_as_sf(random_points)
head(random_points_sf)
  # we don't have any info here on the county, the park, nothing! Let's add some in so we know what's going on...
colnames(random_points_sf)[1]<- "grid" # this changes the first column name of the sf object
random_points_sf # see?

# now let's add some county info
random_points_sf$county<- "Santa Cruz"
# let's add the name of our park
random_points_sf$park<- "Big Basin"
# and let's create a unique identifier for each point
random_points_sf$id<- seq(1, nrow(random_points_sf), by = 1)

# save it
random_points_df <- as.data.frame(vect(random_points_sf), geom="XY") #remember, geom=XY retains the coordinates
write.csv(random_points_df, "output/randompoints_bigbasin.csv", row.names=F)

#check it!
head(read.csv("output/randompoints_bigbasin.csv"))

#~#~# Exercise 5 #~#~#
## randomly sample one point per 4km x 4km grid cell in the largest park in Santa Clara county
# (hint: 1. find the park, 2. create a template, 3. create a grid, 4. sample point)
# plot the result
# add county name and a sequential id
# save the result as a csv

#~#~# #~#~#

### POINT VECTORS ###
# Let's say we have a camera grid or a point dataframe. How can we use other spatial data to understand how the features
# are distributed across the landscape? 

# now let's say that our randomly selected pair of points within the 1km x 1km grid in Big Basin State Park are now camera locations
camlocs_vec <- random_points
# how can we get the pairwise distance between all sets of points?
dist_matrix <- distance(camlocs_vec, unit="km", pairs=T, symmetrical=T)
# let's create a buffer of 100 m around the camera trap locations
cam_500m_buffer <- buffer(camlocs_vec, width = 500)

#did it work? let's see by making a map
#ensure that you plot buffers first so that the points can go over them
ggplot() +
  geom_spatvector(data=cam_500m_buffer, fill="red", color = "black")+
  geom_spatvector(data = camlocs_vec) +
  ggtitle("Map of Camera Trap Locations")

#we can also create a convex hull polygon around the camera trap locations
cam_convexhull <- convHull(camlocs_vec) 

#let's plot!
ggplot() +
  geom_spatvector(data=cam_convexhull, fill="white", color = "blue", size=2)+
  geom_spatvector(data=cam_500m_buffer, fill="red", color = "black")+
  geom_spatvector(data = camlocs_vec) +
  ggtitle("Map of Camera Trap Locations")

#how would we then get area of that convex hull polygon?
(area <- expanse(cam_convexhull, unit="km"))

# what if we wanted to know how close each point was to a road?
# we can also compute distance from each point to the nearest road segment
dist_to_road <- distance(camlocs_vec, clipped_roads)
camlocs_vec$dist_to_road_km <- apply(dist_to_road, 1, min)/1000
# Inspect 
hist(camlocs_vec$dist_to_road_km)

##### ---- LET'S GET INTO MORE RASTER DATA ---- ####

lc<- rast("data/lesson02/landcover.tif")
slope<- rast("data/lesson02/slope.tif")

#how can we get an overview of the imported raster?
#what does this tell us?
slope

#this is great, but can we get more stats beyond min/max?
#how can we get, say, quartiles of the data?
#turns out it's the same for any vector or data frame column in R
summary(slope)    #WARNING MESSAGE IS OK

#if you want it to use ALL the values in the dataset, use
summary(values(slope)) #wrap our summary function with values() from the terra package
#not much of a difference
#we may notice larger changes w bigger rasters

#here is a relatively fast, simple means of plotting a raster
plot(slope)

#what is the coordinate system? 
crs(slope, describe = T)
#it's UTM, 32610

#let's add roads on top
#add=T adds the roads to the existing plot 
plot(clipped_roads, add = T, lwd = 5)


#### ---- USING CROP AND MASK TOOLS ---- ####

# so what's going on with our "lc" raster?
plot(lc)
plot(slope, add = T)
#ok, so there is a lot of extra raster that we don't want to work with
#let's crop it to make raster processing go a bit faster
lc_crop <- crop(lc, slope) # this is our square vector from before

#Quick comparison between Cropping and Masking 
#Cropping: Removing rows and/or columns to reduce the raster extent
#Masking: Setting pixels outside of an area of interest to NA (similar to "SetNull" in ArcMap)

#lets try a mask on the landcover layer using the slope's spatial extent
slope_extent<- ext(slope) %>% vect(.) 

lc_mask <- mask(lc, slope_extent)
plot(lc_mask)
plot(slope_extent, add=T, lwd=5)
#the only weird thing is that the extent stays the same as the original landcover layer!
plot(lc_crop)
# now that's zoomed in

#masking can be useful when making maps or setting boundaries


#what if we wanted to plot in ggplot? - use geom_spatraster!
ggplot() +
  geom_spatraster(data = lc_crop) +
  #the below is a color bar that is colorblind-friendly
  scale_fill_viridis_c() 

#we can make a histogram within ggplot too
#can help you determine if you have wonky values
#first convert to a data frame
#values outside of an expected range can be considered suspect
slope_df <- as.data.frame(slope, xy=TRUE)

#plot it!
ggplot() +
  geom_histogram(data = slope_df, aes(slope_df[,3]), bins=40) 


#### ---- MAKING A RASTER STACK ---- ####

#what happens when we try to make a raster stack of vegetation and elevation?
stack <- c(slope, lc)
#ERROR ab different extents!

#let's check out the extents of each
ext(slope)
ext(lc)

# wait but didn't we crop one before? let's try that now...
stack<- c(slope, lc_crop)

# still not working! 
ext(slope)
ext(lc_crop)
res(slope)
res(lc)

#the extents are slightly different here, and the resolutions are VERY different
#the extents could be different due to pixels having a different lower left origin, for instance
# we also need to match their resolutions- since the landcover layer has a near 300m resolution,
# we need to upsample our slope layer to match it

#we will need to realign extents here through resample()
slope_match <- resample(slope, lc_crop, method="bilinear")
#make sure method aligns with data type (i.e. categorical vs. continuous) see ?resample

stack <- c(lc_crop, slope_match)
#yay, it works now!


#### ---- USING RECLASSIFY ---- ####
# so what is our landcover layer?
hist(lc_crop)

# okay that looks weird- turns out this data is organized into specific categories of landcover data types
# let's say that we'd like to categorize them into a simpler designation guide...
# we need to build a reclassification matrix and then use reclassify()

#let's set up those reclassification values
# this is a key for our landcover data 
iclus_key<- data.frame("code" = seq(0, 18, by = 1),
                       "group" = c(rep("water", 3),
                                   "protected",
                                   rep("production", 5),
                                   rep("developed", 10)),
                       "class_name" = c("natural water", "reservoir", "wetland", 
                                        "conservation", "timber", "grazing",
                                        "pasture", "cropland", "mining", "parks_golf",
                                        "exurban_low", "exurban_high", "suburban", 
                                        "urban_low", "urban_high", "commercial", 
                                        "industrial", "institutional", "transportation"))
iclus_key <- iclus_key %>% # now we will assign a "simple" classification designating if a category should be "natural" (1) or "anthropogenic" (2)
  mutate(landcover_simp = ifelse(class_name %in% c("cropland", "mining",
                                                   "urban_low", "urban_high", 
                                                   "commercial", 
                                                   "industrial", "transportation"), 2,
                                 ifelse(class_name %in% c("natural water", "resorvoir"), NA, 1))
  )


#now let's make it a matrix with a certain number of columns, and that we are filling by row
reclass_matrix <- as.matrix(iclus_key[, c("code", "landcover_simp")])
#now let's reclassify those values!
lc_reclass <- classify(lc_crop, reclass_matrix)

#let's see what it looks like now!
plot(lc_reclass)

#### ---- BUILDING DISTANCE LAYERS ---- ####

#let's move on to getting distances from roads 
#first, let's crop roads to Hwange extent
roads_sc <- crop(roads_utm, slope_extent)

#let's plot the roads
plot(roads_sc)

#for distance to linear features (roads), let's use distance()
#first, we create an empty raster of a certain resolution & extent such that we can *eventually* store our distances there
#We did something very similar creating a raster from scratch in part 2 but now we specify
#resolution by 'res=' 
# in this case, I'm setting the ext and res to our already altered "slope" layer so that they all match
raster_extent <-  rast(ext(slope_match), res=res(slope_match), crs="EPSG:32610")

#now we'll use distance() to calculate the distance between the different geometries
distroad_raster <- distance(raster_extent, roads_sc)

#we're done! let's plot the output
plot(distroad_raster)
plot(roads_sc, col="grey", lwd=2, add=T)

# sweet!

#### ---- NEIGHBORHOOD STATISTICS ---- ####

#quick foray into neighborhood statistics
#let's take the mean slope using a neighborhood of 15 x 15 cells 
#we are using 15 cells here to show how the values are "smoothed out" visually
slope_focal <- focal(slope_match, w=15, #number of pixels 
                    fun=mean, na.rm=TRUE)
#let's plot them together and compare
slope_stack <- c(slope_match, slope_focal)
plot(slope_stack) # see how it's a super smoothed out version?

#now we are able to make a raster stack of all 3 rasters! 
stack <- c(slope_match, lc_reclass, distroad_raster)

#what does the stack look like?
stack

#names are super weird... let's assign names
names(stack) <- c("slope", "landcover", "dist_road")
stack


#### ---- USING EXTRACT ---- ####

#cool. now we will use extract to extract values for each of our points we put in a santa cruz park
#from each of our three raster layers

# what if we wanted to know how much tree cover we can expect around each camera location?
random_points$tree_cover <- extract(tree_cover, random_points)[,2]
# now let's see precent tree cover per camera location
hist(random_points$tree_cover)

# what if we wanted to do the whole stack at once?
values <- extract(stack, random_points, df=T)

#what does this look like?
head(values) # so we've got our camera ID, then our three layers

#let's write this to .csv!
write.csv(values, "output/extracted_raster_values.csv", row.names=F)

#how can we save a single raster layer?
#set the GeoTIFF tag for NoDataValue to -9999
writeRaster(stack$slope, "output/slope.tif", filetype="GTiff", overwrite=T, NAflag=-9999)

#how can we save a raster stack?
writeRaster(stack, "output/raster_stack.tif", filetype="GTiff", overwrite=T, NAflag=-9999)

#THEN, in order to re-import the stack and use the individual raster layers, you can do the below
stack_import<- rast("output/raster_stack.tif")
stack_import
plot(stack_import)

#if we wish to subset elevation only
landcover <- subset(stack_import,subset=2)
plot(landcover)

# maybe the treecover and slope around just one point doesn't give us enough information... let's take our buffered camera locations, and get 
# average tree cover and slope per 500m buffer around our cameras. Since we have a whole polygon (of 500m diameter)
# we're intersecting with our raster, we need to apply a function, or "fun". This could also be max or min
cam_500m_buffer$tree_cover<- extract(tree_cover, cam_500m_buffer, fun = mean, na.rm = TRUE)[,2]

#let's plot!
ggplot() +
  geom_spatvector(data=cam_500m_buffer, aes(fill= tree_cover)) +
  viridis::scale_fill_viridis()+
  geom_spatvector(data = camlocs_vec) +
  ggtitle("Map of Camera Trap Locations")

# and if we look at this on a satellite map using plet, we should see the points in the middle indeed have high tree cover...
mapview(cam_500m_buffer)


#~#~# Exercise 6 #~#~#

# determine the maximum slope value within 500m of every camera location

# plot the result using histograms and either plet/mapview 

#~#~# #~#~#





