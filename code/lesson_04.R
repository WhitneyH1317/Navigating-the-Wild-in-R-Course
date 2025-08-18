

# ---- DAY 4: ANIMAL MOVEMENT DATA  ----
rm(list = ls()) # clear out any existing data taking up room in your working environment
#install.packages(c("units", "rnaturlaearth", "move2", "devtools")

# load libraries
library(terra)
library(mapview)
library(amt)
require(units)
require(dplyr)
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
# 


