
##### Lesson 04 Solutions #####

# ~ # ~ # ~ # Exercise 1 # ~ # ~ # ~ #
# investigate your spatial data
mapview(roads)
head(roads)

# make the raster base 
raster_extent <-  rast(ext(stack[[1]]), res=res(stack[[1]]), crs=crs(stack[[1]]))

# transform the roads layer to match the crs of your raster stack
roads<- st_transform(roads, crs(stack[[1]]))

#now we'll use distance() to calculate the distance between the different geometries
distroad_raster <- distance(raster_extent, roads)

# and we can add it to our existing stack like this
stack<- c(stack, distroad_raster)

# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 


# ~ # ~ # ~ # Exercise 2 # ~ # ~ # ~ #
# make your "deer" dataframe include data JUST from deer collared in 2021
deer<- deer %>%
  filter(year(timestamp) == 2021) 

# how many individual deer have data in 2021? 
deer %>%
  distinct(ID) %>%
  nrow()

# can you make a plot of start/end times for just the 2021 deer?
deer %>%
  ggplot(., aes(x = timestamp, y = ID, color = ID)) +
  geom_point() 

# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 


# ~ # ~ # ~ # Exercise 3 # ~ # ~ # ~ #
# try making a temporary dataframe (no need to store it) with a column containing 3-hour sampled data, and compare number of fixes
smpld<- tracks %>%
  nest(track = -"ID") %>% # what this looks like is we "nest" our track data based on animal ID
  arrange(ID) %>%
  mutate( 
    smpl = map(track, ~ track_resample(., rate = minutes(180), tolerance = minutes(5), .keep_all = T))
  )

# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 

# ~ # ~ # ~ # Exercise 4 # ~ # ~ # ~ #
# make a plot like the one above, but looking at individual turning angles
steps %>%
  ggplot(., aes(x = ta_, fill = ID)) +
  geom_density(alpha = 0.4) +
  labs(x = "Turning Angle (radians)", y = "Density") +
  theme_minimal() +
  ggtitle("Turning Angle Distributions by Individual")

# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 


# ~ # ~ # ~ # Exercise 5 # ~ # ~ # ~ #
# investigate, either thru a plot or a summary statistic, the difference in movement behavior (step length or turning angle)
  # based on an individual's fawn status and their age
# hint: remember our age_simp column?

# plot turning angle (can replace "ta_" with "sl_"
steps_mort %>%
  group_by(age_simp, fawn_status) %>%
  ggplot(., aes(x = ta_)) +
  geom_density(aes(fill = age_simp), alpha = 0.4) +
  facet_wrap(~fawn_status) +
  labs(x = "Step Length", y = "Density") +
  theme_minimal() +
  ggtitle("Turning Angles based on Fawn Status and Age")

# or summary statistic
steps_mort %>%
  group_by(age_simp, fawn_status) %>%
  summarize(mean_step = mean(sl_)) %>%
  arrange(mean_step)

# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 

