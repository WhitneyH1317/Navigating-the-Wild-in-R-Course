

# ---- DAY 4: ANIMAL MOVEMENT DATA  ----
rm(list = ls()) # clear out any existing data taking up room in your working environment
#install.packages(c("units", "rnaturlaearth", "move2", "devtools", "amt")

# load libraries
library(terra)
library(mapview)
library(amt)
library(dplyr)
library(ggplot2)
library(lubridate)
library(sf)

# read in data
load("data/lesson04/texas_move.rda") #.rda files are a nice way to save multiple datatypes at once
    # let's look at our "Environment" and see what's there- you should see 2 items

# 1) a movement dataframe
head(deer)
# 2) a roads layer
mapview(roads)
  # these are roads across 4 different properties

# now let's load one more dataset
stack<- rast("data/lesson04/texas_stack.tif")
# and take a look
stack

# ~ # ~ # ~ # Exercise 1 # ~ # ~ # ~ #
# investigate your spatial data

# make your roads layer into a dist-to-roads raster!

# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 

# let's look at our movement data again, and figure out some basic attributes
deer %>%
  distinct(ID) %>%
  count() # looks like we've got 39 animals
deer %>%
  group_by(ID) %>% # here we can look at the start/end times per animal
  summarize(start = min(timestamp),
            end = max(timestamp))
    # and maybe plot it for easier visualization
deer %>%
  ggplot(., aes(x = timestamp, y = ID, color = ID)) +
  geom_point() # looks like most deer have data for 2021, and a handful for 2023
    # so let's focus on data from 2021
deer<- deer %>%
  filter(year(timestamp) == 2021) 

# ~ # ~ # ~ # Exercise 2 # ~ # ~ # ~ #
# how many individual deer have data in 2021?

# can you make a plot of start/end times for just the 2021 deer?

# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 

# let's get into the movement data! Easy way to do that is make a movement track
# using the "amt" package
?make_track
  # looks like we need a dataframe/tbl, then to designate an x, y, and timestamp column, as well as a spatial projection
tracks<- deer %>%
  make_track(x, y, timestamp, ID,
             crs = 26914, all_cols = TRUE) %>%
  arrange(ID, t_) # order the dataset by ID and time

str(tracks) # see it's a new object? and it looks like we're renamed our columns
# but what if we want data per individual? If we look the time between fixes, for instance...
summarize_sampling_rate(tracks, time_unit = "hour") # wow that look a bit crazy... and what if it varies by individual?
summarize_sampling_rate_many(tracks, c("ID"), time_unit = "hour") # okay so per deer, it look a little better, but pretty messy still
    # let's plot it to see it better!
tracks %>%
  group_by(ID) %>% # make sure to apply this function only to each individual
  mutate(prev_t_ = lag(t_), # assign each row the previous time
         dt_hr = as.numeric(t_ - prev_t_)/60) %>% # delta-time in hours
  ggplot(aes(x = dt_hr, fill = ID)) +
  geom_density(alpha = 0.4) +
  labs(x = "Delta Time (hours)", y = "Density") +
  theme_minimal() +
  xlim(c(0, 1)) # we've got some extra big max values, so let's just filter those out
  # so it looks like move of the locations occur at 30 minutes or less...

# now to organize our data, we're going to create a list column
smpld<- tracks %>%
  nest(track = -"ID") %>% # what this looks like is we "nest" our track data based on animal ID
  arrange(ID) %>%
  mutate( # then to each row, which has a track object, we "resample" our track so we only select 1 point per 30 minutes, with a 5 minute buffer
    smpl_30min = map(track, ~ track_resample(., rate = minutes(30), tolerance = minutes(5), .keep_all = T))
  )
# let's take a look at that object
smpld # see how there's a column for track, then one for smpl? 
smpld %>% # what if we wanted to upsample our data, for a fix every hour?
  mutate( # then to each row, which has a track object, we "resample" our track so we only select 1 point per 30 minutes, with a 5 minute buffer
    smpl_1hr = map(track, ~ track_resample(., rate = minutes(60), tolerance = minutes(5))),
    fixes_30min = unlist(map(smpl_30min, ~ nrow(.))),
    fixes_1hr = unlist(map(smpl_1hr, ~ nrow(.)))
  ) # see how there are fewer fixes if we downsample our data?

# ~ # ~ # ~ # Exercise 3 # ~ # ~ # ~ #
# try making a new column with 3-hour sampled data, and compare number of fixes

# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 

# now let's create an object with regularized movement data for every 3 hours
smpld<- tracks %>%
  nest(track = -"ID") %>% # what this looks like is we "nest" our track data based on animal ID
  arrange(ID) %>%
  mutate( 
    smpl = map(track, ~ track_resample(., rate = minutes(180), tolerance = minutes(5), .keep_all = T))
  )

# and what if we want to investigate step by step movement? We can make something called a "steps_by_burst" object
?steps_by_burst
  # this function takes our track_xy object, which we have just created into a regularly sampled track (e.g. smpld)
  # and then creates "steps", so that every two gps points become one step. This gives us information on how quickly animals 
  # are moving, and which animals move more when etc. 
steps<- smpld %>%
  mutate(
    steps = map(smpl, ~ steps_by_burst(.))
  ) %>%
  dplyr::select(ID, steps) %>%
  unnest(cols = steps)
# let's investigate this object
steps
  # as you can see, we have an ID, we have two sets of coordinates (x1/y1, and x2/y2) which are the start-end points per step,
  # we have the step length (sl_), the turning angle (ta_), the start and end times (t1_ and t2_, respectively), and the delta time (which should be 3 hours for all steps)
# let's investigate the burst column. What does this mean?
steps %>%
  group_by(ID) %>%
  count(burst_) # it looks like we've got 3 bursts for individual, but the number of fixes per burst varies quite a bit... what's going on here?

steps %>% 
  group_by(ID, burst_) %>%
  summarize(start = min(t1_), end = max(t1_))
# so this appears to be showing us how if there are somewhat big gaps in the data, like over 6 hours when our fix rate is just 3 hours, our "steps_by_burst"
    # function will separate our movement data into bursts 

# let's investigate our step lengths and turning angles
steps %>%
  ggplot(., aes(x = sl_, fill = ID)) +
  geom_density(alpha = 0.4) +
  labs(x = "Step Length (m)", y = "Density") +
  theme_minimal() +
  ggtitle("Step Length Distributions by Individual")

# ~ # ~ # ~ # Exercise 4 # ~ # ~ # ~ #
# make a plot like the one above, but looking at individual turning angles
# steps %>%
#   ggplot(., aes(x = ta_, fill = ID)) +
#   geom_density(alpha = 0.4) +
#   labs(x = "Turning Angle (radians)", y = "Density") +
#   theme_minimal() +
#   ggtitle("Turning Angle Distributions by Individual")

# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 

# how about speeds per individual?
steps %>%
  mutate(time_of_day = ifelse( (hour(t2_) > 19 | hour(t2_) < 6), "night", "day")) %>%
  group_by(ID, time_of_day) %>%
  summarize(speed = mean(sl_)/180) %>%
  ggplot(., aes(x = speed)) +
  geom_histogram() +
  facet_wrap(~time_of_day) +
  labs(x = "Speed (meters/minute)", y = "Density") +
  theme_minimal() +
  ggtitle("Individual Speeds Day vs. Night")

# ~ # ~ # ~ # Exercise 5 # ~ # ~ # ~ #
# make a plot like the one above, but looking at individual turning angles at day vs. night

# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 

# what if we wanted to explore a bit more about our individuals?
(ind_info <- deer %>%
  distinct(ID, .keep_all = T) %>%
  dplyr::select(-c(x, y, timestamp)))
    # here we take a row per individual to get information on their sex, age, body length, and rumpfat
    # and removed extraneous columns
# we can join this data back to our movement data such as
steps_info<- steps %>%
  left_join(., ind_info, by = c("ID"))
steps_info

# so what if we wanted to plot movement data based on animal age? or size? 
steps_info %>%
  mutate(time_of_day = ifelse( (hour(t2_) > 19 | hour(t2_) < 6), "night", "day")) %>%
  group_by(ID, time_of_day, EstAge) %>%
  summarize(speed = mean(sl_)/180) %>%
  ggplot(., aes(x = speed)) +
  geom_density(aes(fill = EstAge)) +
  facet_wrap(~time_of_day) +
  labs(x = "Speed (meters/minute)", y = "Density") +
  theme_minimal() +
  ggtitle("Individual Speeds Day vs. Night")
    # hmmm the daytime plot looks a little odd, what's going on here?
    # maybe we don't have enough data per age class- let's simplify that information

steps_info<- steps_info %>%
  mutate(age_simp = ifelse(EstAge >= 6, "old", "young"))
    # now we've made a simplified column that tells us if the deer is old or young
  # let's plot again
steps_info %>%
  mutate(time_of_day = ifelse( (hour(t2_) > 19 | hour(t2_) < 6), "night", "day")) %>%
  group_by(ID, time_of_day, age_simp) %>%
  summarize(speed = mean(sl_)/180) %>%
  ggplot(., aes(x = speed)) +
  geom_density(aes(fill = age_simp)) +
  facet_wrap(~time_of_day) +
  labs(x = "Speed (meters/minute)", y = "Density") +
  theme_minimal() +
  ggtitle("Individual Speeds Day vs. Night")
    # well that's interesting- looks like older deer move much less on average... could this be a sample size issue?

steps_info %>%
  group_by(age_simp) %>%
  count() # looks like similar number of steps

steps_info %>%
  distinct(ID, .keep_all = T) %>%
  group_by(age_simp) %>%
  count() # similar number of individuals. So maybe it's a behavioral trend?

# luckily, in this system we have very cool fawn mortality data, so we know when these females' fawns died (or not) 
# alongside their movement data. Let's investigate some movement behavior by incorporating that information
load("data/lesson04/fawn_data.rda")
head(fawn_data)
  # looks like we have a birth date, mortality date, and an ID per individual
  # but there are multiple individuals represented over multiple years... let's try to join this data
  # to our movement data, so we know what steps occur before a fawn is born, while it's at heel, and 
  # after it's died (if it dies)
steps_mort<- steps_info %>%
  left_join(., fawn_data, by = c("ID"))
# looks like we got a lot of warnings- can you see why?
steps_mort # if you investigate it closely, it looks like each row is duplicated twice...
        # how can we fix that?

# there are a couple ways:
# 1) filter for the year
steps_mort_filt<- steps_mort %>%
  filter(year(t1_) == FawnYear)
  # good news is it's much smaller than the original object (check your Environment!) 
  # but bad news is we've also managed to cut quite a few steps... what's another way to do this?

# first off, we know our data is ONLY in 2021, so let's cut some of the fawn_data we don't need
fawn_data_filt<- fawn_data %>%
  filter(FawnYear == 2021) 
# now let's join again...
steps_mort<- steps_info %>%
  left_join(., fawn_data_filt, by = c("ID"))
  # much better, no warnings!

# and now we can use the birth and mortality columns to create a new column, with "fawn status"
steps_mort <- steps_mort %>%
  mutate(
    fawn_status = case_when(t1_ < BirthDate ~ "prefawn",
                            t1_ >= BirthDate & t1_ < MortDate ~ "fawn",
                            t1_ >= MortDate ~ "fawn passed")
  )
# let's investigate our steps relative to fawn status
steps_mort %>%
  group_by(fawn_status) %>%
  count() 
  # looks like quite a few NA's where we don't have data. Let's remove those steps

steps_mort<- steps_mort %>% filter(!is.na(fawn_status))

# let's investgiate movement behavior relative to fawn status
steps_mort %>%
  mutate(time_of_day = ifelse( (hour(t2_) > 19 | hour(t2_) < 6), "night", "day")) %>%
  group_by(ID, time_of_day, fawn_status) %>%
  summarize(speed = mean(sl_)/180) %>%
  ggplot(., aes(x = speed)) +
  geom_density(aes(fill = fawn_status), alpha = 0.4) +
  facet_wrap(~time_of_day) +
  labs(x = "Speed (meters/minute)", y = "Density") +
  theme_minimal() +
  ggtitle("Individual Speeds Day vs. Night")
  # what are you seeing here? 
  
# ~ # ~ # ~ # Exercise 6 # ~ # ~ # ~ #
# investigate, either thru a plot or a summary statistic, 
# the difference in movement behavior (step length or turning angle)
# based on an individual's fawn status and their body length


# ~ # ~ # ~ # ~ # ~ # ~ # ~ #  ~ # ~ # 


# what if we wanted to plot one individual's trajectory?
steps %>% 
  filter(ID == "D20_001") %>% # select teh first animal
  st_as_sf(., coords = c("x1_", "y1_"), crs = 26914) %>%
  mapview(., col.regions = c("purple"))
# and what if we wanted to plot 100 random points per individual
steps %>% 
  st_as_sf(., coords = c("x1_", "y1_"), crs = 26914) %>%
  mapview(., zcol = "ID")
  
# but that's not super helpful... so let's get into homeranges





########## MOVEBANK ############
require(units)
require(ggplot2)
require(move2)
require(rnaturalearth)

movebank_store_credentials("whitneyhansen17") # Replace with your username
lion_studies<- movebank_download_study_info(x="African Lion") # let's look for any data on lions...

# looking for lion studies out of the Kalahari... 
(ids<- lion_studies %>%
  filter(grepl("Kalahari", name) & grepl("African lion", name)) %>%
  distinct(id)) # these are the two id's we use to get actual data from movebank
movebank_retrieve(entity_type = "study_attribute", study_id = ids$id[2], sensor_type_id
                  = "gps")$short_name
 
# now download the data
lion_data<- movebank_download_study(
  ids$id[2],
  attributes = NULL,
  sensor_type_id = "gps",
  remove_movebank_outliers = TRUE,
  'license-md5'='1bbe1c69895a236d4d0c386a17aa21c6')
str(lion_data)
# so what does this data actually look like?
ggplot() +
  geom_sf(data = ne_countries(country = "Botswana", returnclass = "sf")) +
  theme_linedraw() +
  geom_sf(data = lion_data) +
  geom_sf(data = mt_track_lines(lion_data), aes(color = `individual_local_identifier`)) +
  coord_sf(
    crs = crs(lion_data),
    xlim = c(22.373, 23.958),
    ylim = c(-21.908, -20.840)
  )
# very nice!

# what's some basic information from this move2 object
range(mt_time(lion_data))              # Get timestamp range
unique(mt_track_id(lion_data))       # Get track IDs
mt_track_data(lion_data)        # Get track-level metadata (e.g., sex, name)

# let's figure out some basic attributes of our movement data
par(mfrow = c(1, 3))
hist(mt_distance(lion_data)) # step lengths
hist(mt_azimuth(lion_data)) # turning angles
hist(mt_time_lags(lion_data)) # step resolution
# looks like there are some huge steps in there- over 35km! And we have a very confusing set of timelags...

# looks like quite a mess of movement data- how can we regularize it? 
lion_lines<- lion_data %>%
  mutate_track_data(name = individual_local_identifier) %>%
  mutate(time_lag = mt_time_lags(.),
         dist = mt_distance(.)) %>%
  filter(!(time_lag > set_units(180, "min") | is.na(time_lag) | dist > set_units(20000, "m"))) %>%
  mt_track_lines()

# visualize
ggplot() +
  geom_sf(data = ne_countries(country = "Botswana", returnclass = "sf")) +
  theme_linedraw() +
  geom_sf(data = lion_lines, aes(color = `individual_local_identifier`)) +
  coord_sf(
    crs = crs(lion_data),
    xlim = c(22.373, 23.958),
    ylim = c(-21.908, -20.840)
  )

mapview(lion_lines, zcol = "individual_local_identifier")

