

#### Lesson 02 Exercise Solutions ####


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


# plot the result

#~#~# #~#~#
