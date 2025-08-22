

#### Lesson 02 Exercise Solutions ####

#~#~# Now you try #~#~#
### Exercise 1 ###
# pick 5 points on GoogleMaps- a place in your hometown, your study site, or somewhere you want to visit, and create a dataframe
tiburon<- data.frame(lat = c(37.8821, 37.9579, 37.8890, 37.89064420878854, 37.82852697800907), 
                     long = c(-122.4793, -122.6150, -122.6108, -122.47158107507612, -122.49769338375461))

# assign an attribute name to each point
tiburon$place<- c("downtown", "branson", "mt. tam", "hippie tree", "the headlands")

# assign a CRS (hint- what projection does GoogleMaps automatically use?)
tiburon_sf<- st_as_sf(tiburon, coords = c("long", "lat"), crs = 4326)

# plot the points
mapview(tiburon_sf)

# now filter to a specific point, and plot that one
branson<- tiburon_sf %>% filter(place == "branson")
mapview(branson)

#~#~# #~#~#

### Exercise 2 ###

# take our "areas" layer, and create an "areas_4326" object by transforming "areas" to the same crs projection as "roads"
areas_4326<- st_transform(areas, crs(roads))

# take our "p" layer, and do the same. Name it p_4326
p_4326<- st_transform(p, crs(roads))

# check all projections are equal
crs(p_4326)== crs(areas_4326)

# now clip the "roads" layer using the "p_4326" to crop instead of "p"
clipped_roads_4326 <- crop(roads, p_4326) # remember, p is our square in the middle of California

# plot the roads, areas_4326, and p_4326 layers all at once using mapview()
mapview(clipped_roads_4326) + mapview(clipped_areas) + mapview(p_4326)
mapview(clipped_roads_4326) + mapview(p_4326)

#~#~# #~#~#


#~#~# Exercise 3 #~#~#

# use commands above, such as "names()", "ext()", etc. to understand what our 'counties' layer is
names(counties)
ext(counties)
# plot it with clipped_roads and clipped_areas
mapview(clipped_roads) + mapview(clipped_areas) + mapview(counties)
# make sure it's the same crs as the above two layers (if needed, create a new object: counties_utm)
crs(clipped_roads) == crs(clipped_roads)
counties_utm<- project(counties, crs(clipped_areas))

## two possible ways... 
# 1
# create a new column in 'counties' that has the area of each county
counties_utm$area<- expanse(counties_utm, unit = "km")

# 2
## alternatively, can do...
counties_utm_sf<- st_as_sf(counties_utm)
counties_utm_sf$area<- st_area(counties_utm_sf)/1e6 #to convert meters directly to km squared

#~#~# #~#~#


#~#~# Exercise 4 #~#~#
## find all roads in Santa Cruz county that are over 10km long
# (hint: 1. perform an intersection, 2. filter, 3. count)
county_roads<- st_intersection(st_as_sf(clipped_roads), counties_utm)

perim(vect(county_roads))/1000 # divide by 1000 so it goes from meters to km
long_roads<- county_roads %>%
  filter(length > 10)
nrow(long_roads)

# plot the result
mapview(long_roads, zcol = "COUNTY_NAM")

#~#~# #~#~#
