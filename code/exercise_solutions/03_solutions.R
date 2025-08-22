

## LESSON 03 EXERCISE SOLUTIONS ##


### Exercise 1 ###
# remember anything from yesterday?? Hopefully!

# take your "cam_locs" object it and plot it using the function "plet"
# (hint: check the str() of your data first...)
str(cam_locs)
cam_locs_vect<- vect(cam_locs)
plet(cam_locs_vect, 
     alpha = 0.8,
     col = "black",
     tiles="Esri.WorldImagery") # some extra features- feel free to change the color, the alpha value, or tiles

#~#~# #~#~#

### Exercise2 ###
# what rows (from 1 to 10 of the dataframe) contain bear observations?
grepl("bear", raw_cam_data$keywords[1:10]) 

# what rows (from 1 to 10 of the dataframe) do NOT contain bear observations? 
!grepl("bear", raw_cam_data$keywords[1:10]) 

#~#~# #~#~#

### Exercise 3 ###
# create a small dataframe that only contains observations of "panther" 
# hint: you'll need to filter by TWO restrictions; use the "&"
panthers<- raw_cam_data %>% 
  filter(grepl("panther", keywords) & month(datetime) == 7) %>% 
  arrange(camID, datetime)

# now create a small dataframe that only contains observations of "deer" that
# do NOT occur in June
deer_not_june<- raw_cam_data %>%
  filter(grepl("deer", keywords) & month(datetime) != 6)

# test to make sure you have no rows that have a datetime occurring in june using "distinct"
deer_not_june %>%
  distinct(month(datetime))

#~#~# #~#~##~#~# 


### Exercise 4 ###
# what unique "keys" do we have in our dataset? 
detections %>%
  distinct(key)

# alternatively
unique(detections$key)

# filter our dataframe to only include rows with key = "sp" (species) so we can 
# start to analyze our detection observations
detections<- detections %>%
  filter(key == "sp")

######## ############


### Exercise 5 ###

# Try this pipeline. It has 4 bugs. Can you find and fix them?
species_counts_broken <- detections %>%
  filter(specie != "human") %>%      # typo in 'species'
  mutate(year = year(timestamp))   # the month "timestamp" doesn't exist, it's "datetime"
filter(year == 2022) %>%              # we only have data from 2015!
  rename(camera = camid)          # the "old" column is camID, not camid

species_counts_fixed <- detections %>% # exercise answer
  filter(species != "human") %>%
  mutate(year = year(datetime)) %>%
  rename(camera = camID)     

species_counts_fixed 


### Exercise 6 ###
# create a dataset of species-specific counts by month
# hint: use "mutate" to create a new column that designates the month 
# hint #2: you can use "group_by" on multiple columns! s

species_counts_by_month<- detections %>%
  mutate(month = month(datetime)) %>%
  group_by(species, month) %>% 
  count() 
species_counts_by_month

#################


#### Exercise 7 ####

# create a spatial plot of top cameras with prey observations that are less than the 
# mean number of prey observations per camera
# what is the range of predator observations? 
prey_freq<- detections_locs %>%
  filter(trophic_group == "prey") %>%
  count(camID) %>%
  ungroup() %>% # notice here: we ungroup before we apply the next set of functions
  rename(prey_observations = n) 
summary(prey_freq$prey_observations)
# let's make a spatial heatmap of all cameras that have at least 65 observations of a predator

# we'll plot all cameras, but color code them by # of predator observations
prey_freq %>%
  filter(prey_observations < 149) %>%
  left_join(., cam_locs, by = "camID") %>%
  st_as_sf(.) %>%
  ggplot(.) +
  geom_sf(aes(color = prey_observations), size = 3) +
  scale_color_viridis_c(option = "inferno", name = "Prey Observations") +
  theme_minimal() +
  labs(title = "Prey Occurrence at Camera Locations",
       subtitle = "Top locations by number of observations < mean",
       caption = "Data: Camera trap detections") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right"
  ) + 
  theme(aspect.ratio = 0.6)  # or a taller ratio


# save the plot
ggsave("output/low_prey_sites.jpg", plot = last_plot(), width = 8, height = 10, dpi = 500)

###################





