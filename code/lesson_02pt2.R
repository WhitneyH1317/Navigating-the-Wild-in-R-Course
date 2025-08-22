
# ---- DAY 2: SLIGHTLY MORE ADVANCED SPATIAL OPERATIONS IN R ----
rm(list = ls()) # clear out any existing data taking up room in your working environment

#install.packages(c("terra", "mapview", "dplyr", "sf", "ggplot2", "tidyterra")) # install these packages if you don't already have them
library(terra)
library(mapview)
library(ggplot2)
library(tidyterra)
library(sf)

# optional configuration for mapview:
mapviewOptions(basemaps = c("Esri.WorldImagery"))

# let's read in some data from last time
county_PAs<- st_read("output/county_PAs.geojson")
clipped_roads<- vect("output/CA_roads_clipped.geojson")

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
tree_cover<- rast("data/lesson02/treecover.tif")

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
#ERROR different extents!

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
#first, let's crop roads to slope extent
roads_sc <- crop(clipped_roads, slope_extent)

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
  ggtitle("Map of Camera Trap Locations ~ forest cover")

# and if we look at this on a satellite map using plet, we should see the points in the middle indeed have high tree cover...
mapview(cam_500m_buffer)


#~#~# Exercise 6 #~#~#

# determine the maximum slope value within 500m of every camera location

# plot the result using histograms and either plet/mapview 

#~#~# #~#~#

