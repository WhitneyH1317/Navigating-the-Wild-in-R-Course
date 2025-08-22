

#### Lesson 02 pt 2 Exercise Solutions ####

#~#~# Exercise 5 #~#~#
## randomly sample one point per 4km x 4km grid cell in the largest park in Santa Clara county
# (hint: 1. find the park, 2. create a template, 3. create a grid, 4. sample point)
SClara_park<- county_PAs %>%
  filter(COUNTY_NAM == "Santa Clara") %>%
  slice_max(., order_by = area_km2) %>% # this is a helpful function that finds the largest (max) value based on 
  # the "order_by" column in our data
  vect(.) # make it a vector again (terra object)                                    
plet(SClara_park) # p

# reate a 4 km x 4 km over the PA
#first we need to create a template and add values
template <- rast(SClara_park, resolution = c(4000,4000))
values(template) <- 1:ncell(template)

#then transform the raster template to polygons
sc_1km2_grid <- as.polygons(template)
# crop it to the park
sc_1km2_grid_isect <- crop(sc_1km2_grid, SClara_park)
# generate random points
random_points_santaclara <- spatSample(sc_1km2_grid_isect, size=rep(1,nrow(sc_1km2_grid_isect)), method="random")

# plot the result
plet(random_points_santaclara)

# add county name and a sequential id
random_points_santaclara$county<- "Santa Clara"
random_points_santaclara$id<- seq(from = 1, to = nrow(random_points_santaclara), by = 1)

# save the result as a csv
# save it
random_points_santaclara_df <- as.data.frame(random_points_santaclara, geom="XY") #remember, geom=XY retains the coordinates
write.csv(random_points_santaclara_df, "output/randompoints_santaclara.csv", row.names=F)

#~#~# #~#~#


#~#~# Exercise 6 #~#~#

# determine the maximum slope value within 500m of every camera location
cam_500m_buffer$slope<- extract(slope, cam_500m_buffer, fun = max, na.rm = TRUE)[,2] # notice the "max" function here instead of "mean"

# plot the result using histograms and either plet/mapview/ggplot
ggplot() +
  geom_spatvector(data=cam_500m_buffer, aes(fill= slope)) +
  viridis::scale_fill_viridis()+
  geom_spatvector(data = camlocs_vec) +
  ggtitle("Map of Camera Trap Locations ~ slope")

mapview(cam_500m_buffer, zcol = "slope")
#~#~# #~#~#
