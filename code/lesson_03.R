

# ---- DAY 3: BASIC DATA CLEANING OPERATIONS IN R ----
rm(list = ls()) # clear out any existing data taking up room in your working environment

# install.packages(c("stringr", "viridis"))

# load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(viridis)
library(sf)
library(terra)
library(stringr)

# let's work with some detection data
# we're going to use a new type of data object called an ".rda" object- .rda objects save all sorts of stuff into an R working environment,
# including datasets, spatial data, and even functions. It's a useful way to save multiple objects into one file that you can load at once. 
# let's try it here:
load("data/lesson03/florida_detections.rda")
# now look in your Environment- what do you see?

cam_locs # this dataset gives us camera trap locations
raw_cam_data # and this is a dataset of camera detections- something that you would get directly out of tagging software, like adobe bridge, where every sighting is associated with "keywords"
  # so how can we extract useful information from these two datasets to figure out what our camera trap data has to tell us?


### Exercise 1 ###
# remember anything from yesterday?? Hopefully!

# take your "cam_locs" object it and plot it using the function "plet"
# (hint: check the str() of your data first...)

#~#~# #~#~#

# let's try to wrangle our camera trap data so we can figure out what's going on

# we're going to use a coding language called "tidyverse", which allows us to manipulate
# our data in a nested, consecutive structure. It works a bit like this:

# let's say we want to create a new column that contains a sequential number for every camera entry. We could do...
raw_cam_data<- raw_cam_data[order(raw_cam_data$camID, raw_cam_data$datetime),]
raw_cam_data$row_id <- seq(from = 1, to = nrow(raw_cam_data), by = 1)
head(raw_cam_data) # now you see the new row_id column, and the order of observations, yes?

# let's remove that column really quick
raw_cam_data$row_id<- NULL
# and reorder the camera data by datetime alone so we can see the difference later...
raw_cam_data<- raw_cam_data[order(raw_cam_data$datetime),]
head(raw_cam_data) # see it now? 


# so let's do those two things but using tidyverse:

test_A01<- raw_cam_data %>% # this is called a "pipe":  is a tool in R that allows for chaining together 
                      # multiple operations in a sequential and readable manner.
  arrange(camID, datetime) %>% # this function organizes our dataframe so it's in order by 1) camera then 2) datetime
  mutate(row_id =  seq(from = 1, to = nrow(.), by = 1)) %>% # "mutate" means to mutate your dataframe- in this case, adding a new column called "row_id"
                                  # and we can use "nrow(.)" because the "." means 'the dataframe we're currently operating on
  filter(camID == "A01") # "filter" is another useful one: it means 'only select rows where column x (in this case, camID) matches some statement'
        # in this case, where camID equals "A01"

nrow(raw_cam_data)
nrow(test_A01) # see how many fewer rows there are in this smaller subset, thanks to the filter statement?
head(test_A01)                # and how we have a new column ?

# IMPORTANTLY: what if we had run all those pipes without STORING them? 
raw_cam_data %>% 
  arrange(camID, datetime) %>% 
  mutate(row_id =  seq(from = 1, to = nrow(.), by = 1)) %>% 
  filter(camID == "A02")
# now they're just printed out in the console, but there's nothing in our Environment that 
# has stored data from camera A02. The "testA01<- " part of our code above is what stores the
# newly updated data into an object that we can use in future code. Make sense? 


# what are other useful ways to filter?
    # we can filter by selecting rows that occur at a certain time using the lubridate package
year(raw_cam_data$datetime) # see how this extracts the year from the datetime column? 
  # what function could you use to extract the month from the datetime column I wonder...

test_august<- raw_cam_data %>% # see how we're storing our data here?
  arrange(camID, datetime) %>% 
  mutate(row_id = seq(from = 1, to = nrow(.), by = 1)) %>% 
  filter(month(datetime) == 8) # here we use a similar function, "month", to filter rows where the month is August

nrow(raw_cam_data)
nrow(test_august) # see how many fewer rows there are in this smaller subset, thanks to the filter statement?
head(test_august)                # and how we have a new column ?

# importantly for field collected data, we can use a set of functions that manipulate character strings to work with complex columns,
  # something important when you have convoluted notes from behavioral observations, or maybe camera trap data!
  # For camera trap data, it can look something like our column "keywords"
# these functions can find character strings (e.g. words) or symbols within long row entries
# for instance...

raw_cam_data$keywords[1:10] # these two rows have a bunch of text in them
str_detect(raw_cam_data$keywords[1:10], "deer")
grepl("deer", raw_cam_data$keywords[1:10]) 
    # these function are saying, "does row 1-10 of raw_cam_data's keywords column 
      # contain the character string 'x' ?" It's either true or false.

# what if you wanted all NON-deer observations? Easy! Add an "!"
grepl("deer", raw_cam_data$keywords[1:10]) 
!grepl("deer", raw_cam_data$keywords[1:10]) # see how it's flipped?

# The "!" is a very useful addition to your coding language- you can use it
# to find all "non-NA" values such as ' !is.na(df$column)' or to filter out 
# observations that are NOT in a certain month and are NOT of a certain species (for instance).
# We can even try to find all values that 'do not equal' 10 by using, 'df$column != x'

### Exercise2 ###
# what rows (from 1 to 10 of the dataframe) contain bear observations?

# what rows (from 1 to 10 of the dataframe) do NOT contain bear observations? 

#~#~# #~#~#

# So let's filter our dataset to just "deer" observations using tidyverse
all_deer_obs<- raw_cam_data %>% 
  filter(grepl("deer", keywords)) %>% # see how in tidyverse, we don't need to rename the column
  # with reference to the dataframe? Instead of df$column, it's just 'column'
  arrange(camID, datetime)
head(all_deer_obs)

# what if we had multiple filtering observations? Maybe we want bear observations that happen
# in a specific month, or we want to determine all predator observations. 
bears_in_july<- raw_cam_data %>% 
  filter(grepl("bear", keywords) & month(datetime) == 7) %>% 
      # here we use the "&" to say we want rule 1 to be true, AND we want rule 2 to be true
  arrange(camID, datetime)
head(bears_in_july)
# we can use the function 'unique' or 'distinct' to make sure our 'filter' did what we meant:
bears_in_july %>%
  distinct(month(datetime)) # here, we are asking: what distinct months are represented in our datetime column?

# let's say we want to determine all predator observations. In that case, we want to 
# figure out what rows have a "bear" OR have a "panther" observations, as both of these
# species are predators:
top_predators<- raw_cam_data %>% 
  filter(grepl("bear", keywords) | grepl("panther", keywords)) %>% 
  # here we use the "|" to say we want rule 1 to be true, OR we want rule 2 to be true
  arrange(camID, datetime)
head(top_predators) # and we can already see a bear and a panther represented here
# awesome!


### Exercise 3 ###
# create a small dataframe that only contains observations of "panther" 
# hint: you'll need to filter by TWO restrictions; use the "&"

# now create a small dataframe that only contains observations of "deer" that
# do NOT occur in June

# test to make sure you have no rows that have a datetime occurring in june using "distinct"

#~#~# #~#~##~#~# 


# okay, now I don't know about you, but working with this keywords column is getting
# slightly cumbersome. Let's make it into a proper set of columns that we can use to
# get a handle on our detection data:

detections<- raw_cam_data %>% # first we're creating a new object to store our data in
  separate_rows(keywords, sep = ",") %>% # and this useful function will createt a new column
          # based on the values in this row, separating the columns by a comma
   # We'll create two new columns that specify what the "key" is, and what the "value" is 
  mutate(
    key = ifelse(str_detect(keywords, "="), 
                 str_extract(keywords, "^[^=]+"), "tag"),
    value = ifelse(str_detect(keywords, "="),
                   str_extract(keywords, "(?<=\\=).*"), keywords)
  ) 
head(detections)

# now what if you want to know what's going on in your piping, or you have an error?
# The best way to figure it out is to decompose the pipes, such as...

# okay, what's my data look like:
head(raw_cam_data)

# what does the first piping do:
raw_cam_data %>% 
  separate_rows(keywords, sep = ",") %>%
  head(.) # this is just like when we do head(dataframe), but it's in a piping operation!

# what does the 2nd piping do: 
raw_cam_data %>% 
  separate_rows(keywords, sep = ",") %>% 
  mutate(
    key = ifelse(str_detect(keywords, "="), 
                 str_extract(keywords, "^[^=]+"), "tag"),
    value = ifelse(str_detect(keywords, "="),
                   str_extract(keywords, "(?<=\\=).*"), keywords)
  ) %>%
  head(.) 

# a bit clearer now?

# our "keys" are the information that we've collected from each photo, and the "value"
# is what the key says- so you can see the "tag", the "sp" (species), the "grid", etc.

### Exercise 4 ###
# what unique "keys" do we have in our dataset? 


# filter our dataframe to only include rows with key = "sp" (species) so we can 
# start to analyze our detection observations


######## ############

# now that we know we're just looking at a dataframe of observations of species, 
# at a specific camera, at a specific time, we don't really need our keywords column
# anymore, or the "key" column. So let's remove those:
detections<- detections %>%
  select(-c(key, keywords)) # the way you do this is using the "select" function
      # which tells us what columns we want or don't want. When we use the "-" (minus),
      # is means we DON'T want those columns. Alternatively, we could have done:
detections %>%
  select(c(datetime, camID, value))

# now our remaining column is called "value" which made sense when we had a "keyword" and then an associated term,
# but currently it's a bit weird. Let's rename it to "species"
detections<- detections %>%
  rename(species = value) # this is saying, let's rename our column to "new_name" from "old_name"
head(detections) # see?

#### Exercise 5 ######
# Try this pipeline. It has 4 bugs. Can you find and fix them?
species_counts_broken <- detections %>%
  filter(specie != "human") %>%      # typo in 'species'
  mutate(year = year(timestamp))   # the month "timestamp" doesn't exist, it's "datetime"
  filter(year == 2022) %>%              # we only have data from 2015!
  rename(camera = camid)          # the "old" column is camID, not camid
  
# species_counts_fixed <- ?? 
  
####################


# now that we just have a dataframe of species, let's see what species we even have:
detections %>%
  distinct(species)

# quite a variety! How many observations per species do we have?
species_counts<- detections %>%
  group_by(species) %>% # "group_by" is a super useful function: it groups our dataframe
   # by the distinct values in a designated column, then we can apply functions across 
   # the dataframe by the distinct column values
  count() # in this case, let's count how many rows (observations) there are per species
species_counts

# let's plot this:
species_counts %>%
  ggplot(., aes(x = species, y = n, fill = species)) +
  geom_bar(stat = "identity")

# that's a simple bar plot, but let's make it prettier: 
# we'll add a title, and turn the x-axis text a bit so it's easier to read
species_counts %>%
  ggplot(., aes(x = species, y = n, fill = species)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Number of Observations by Species") +
  xlab("Species") +
  ylab("Number of Observations") 

# what if we want to explore species detetions by month?

### Exercise 6 ###
# create a dataset of species-specific counts by month
# hint: use "mutate" to create a new column that designates the month 
# hint #2: you can use "group_by" on multiple columns! s


#################

# here's some basic ggplot code you can use for exploratory plots
species_counts_by_month %>%
  ggplot(., aes(x = species, y = n, fill = species)) + # designating the x axis, y axis, and color scheme
  geom_bar(stat = "identity") + # telling ggplot instead of counting rows itself, use the y-column 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + # adjust the x-axis text
  facet_wrap(~month) + # now we'll create a new panel by month
  ggtitle("Number of Observations by Species") + # set the title
  xlab("Species") + # set the  x-axis
  ylab("Number of Observations")# set the y-axis

# but we're in a data course: how can we make this plot look real nice? 

# first, instead of "6, 7, 8", let's rename our months so it looks nicer: 
species_counts_by_month<- species_counts_by_month %>%
  mutate( # first, we're using "mutate" to say "we are mutating our dataframe by creating a new column"
    month = factor( # now we're using the case_when function, which says: 
              # 'in the case that the column month equals x, change it to y
      case_when(month == 6  ~ "June", 
                month == 7 ~ "July",
                month == 8 ~ "August"), # note that for this function, you need to cover all possible
                              # values of the column; if we had 12 months of the year, we couldn't only change
                              # three of the months and leave the rest "as they were"
      levels = c("June", "July", "August")) 
    # now we're using the function "factor" to make sure R knows that we have a specific order:
    # it should go June, July, then August. If we did NOT use factor or set levels,
    # then ggplot would simply plot things in alphabetical order
              
    ) 

plot1<- species_counts_by_month %>% # now, we are storing this into an object- can be useful when you're creating a figure with multiple figures in it
  ggplot(aes(x = reorder(species, -n), y = n, fill = species)) + #let's sort our species from 
                # most observed to least observed (if you left out the '-n', it'd be from least to most observed)
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.2) + # let's set the width and outline color of our bars
  scale_fill_viridis_d(option = "plasma", guide = "none") +  # clean color palette (using the viridis library)
  facet_wrap(~month, ncol = 3, scales = "free_y") + # we want to set our facets so that they are horizontally stacked
          # hint: if you use "nrow", instead of "ncol", we'd have 3 vertically stacked plots
  labs( # a cleaner way of setting titles
    title = "Monthly Observation Counts by Species",
    x = "Species",
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 12) + # making sure our text is all at least 12 size font
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9), # tilting x-axis text
    strip.text = element_text(face = "bold"), # making the facet labels "bold"
    panel.grid.major.x = element_blank(), # the "element_blank" gives the background a cleaner view
    panel.grid.minor = element_blank()
  ) 

plot1 # now just call the stored object to view

# wow, so pretty! Let's save it
ggsave(
  filename = "species_counts_by_month.jpg", 
  plot = plot1,                        # you can also use "last_plot()" 
  path = "output",                            # Optional: folder path
  width = 10,                                # Width in inches
  height = 6,                                # Height in inches
  dpi = 300                                  # High resolution
)

# We can see from this plot that there are a lot of species that barely have any observations.
# So let's look at the most active species
species_counts <- detections %>%
  filter(species != "human") %>%
  count(species, sort = TRUE) # here's another way, instead of using "group_by" and "n", 
      # to count how many observations there are per species, then sort the data by "n"
species_counts

# To get the top 5, let's say, most observed species, we can use this fun function:
species_counts %>%
  slice_max(n, n = 5) # this "slices" our column 'n' to select the 5 highest counts (e.g. 5 top maximums)

species_counts %>%
  slice_min(n, n = 5) # slice min works the same way, but the bottom 5...

# so how can we put all this together? Let's get counts of our species,
  # select the top 5, then make a stunning plot
detections %>%
  filter(species != "human") %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  ggplot(aes(x = reorder(species, -n), y = n, fill = species)) + 
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.2) + 
  scale_fill_viridis_d(option = "plasma", guide = "none") + 
  labs( 
    title = "Top 5 Most Observed Species",
    x = "Species", 
    y = "Number of Observations"
  ) +
  theme_minimal(base_size = 12) + # making sure our text is all at least 12 size font
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9), # tilting x-axis text
    strip.text = element_text(face = "bold"), # making the facet labels "bold"
    panel.grid.major.x = element_blank(), # the "element_blank" gives the background a cleaner view
    panel.grid.minor = element_blank()
  ) 

# what if we want to look specifically at trophic groups?
# Add a new classification column to your detections dataset
detections <- detections %>%
  mutate(trophic_group = case_when(
    species %in% c("bear", "panther", "bobcat", "coyote",
                   "otter", "alligator") ~ "predator",
    species != "human" ~ "prey",  # everything else is prey
    TRUE ~ NA_character_          # leave humans as NA
  ))

# Check output
detections %>%
  count(trophic_group)

# and plot it! 
detections %>%
  filter(!is.na(trophic_group)) %>%
  count(trophic_group, species) %>%
  ggplot(aes(x = reorder(species, -n), y = n, fill = trophic_group)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", linewidth = 0.2) +
    # here's another useful coloring function: we can designate the colors we want
    # to fill our bars with using "scale_fill_manual" (for a lineplot, you could use "scale_color_manual"),
    # and we set the colors using "values = c("color1", "color2")
  scale_fill_manual(values = c("darkblue", "orange")) +
  facet_wrap(~trophic_group, scales = "free_x") +
  theme_minimal(base_size = 12) +
  labs(title = "Predator vs. Prey Observations",
       x = "Species",
       y = "Number of Observations",
       fill = "Trophic Group") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9), # tilting x-axis text
    strip.text = element_text(face = "bold"), # making the facet labels "bold"
    panel.grid.major.x = element_blank(), # the "element_blank" gives the background a cleaner view
    panel.grid.minor = element_blank()
  ) 

# we did all that spatial work yesterday, maybe we want to plot where animals are occurring in space!
# But where are the cameras located?

# let's look at our "cam_locs" dataframe:
head(cam_locs)
# how can we join the spatial information in our "cam_locs" dataframe to our species observations? 

# simple: use a "join"!
# our detections data has a "camID", as does our cam_locs data
detections_locs<- detections %>%
  # here we are saying, "let's join our data 'detections' (designated here as a '.') to 
  # our data cam_locs (joining to the "left") using the column "camID" as a key to match
  # rows to each other
  left_join(., cam_locs, by = "camID")

## notice here: yesterday with did "spatial joins" via extract; we could put data together by figuring out where
# our data occurred in space relative to each other. Here, we are joining based on keyed information- in this dataset, 
# the camera ID number. Even though there is spatial data involved, this is NOT a spatial join

head(detections_locs) # now our camera points have a geometry column! And a trail column!


# let's find the 10 cameras with the most predator observations
top_10<- detections_locs %>%
  group_by(trophic_group) %>%
  count(camID) %>%
  rename(predator_observations = n) %>%
  slice_max(predator_observations, n = 10) 
top_10 # wait a second, there's still 30 cameras but we want top 10... what happened?

# well, let's break down the pipe: we grouped by 'trophic_group',
# then we counted the number of observations by camera ID. Then we renamed the column,
# and we got the top 10 observations of predator observations... by group?


top_10 # notice that in the object it says "tibble: 30x3" then it says 
# "Groups: trophic_group [3]", because there are 3 groups.
# But we don't want to "slice_max" to find the top 10 most visited cameras by trophic_group!
# How do we fix that?

top_10<- detections_locs %>%
  group_by(trophic_group) %>%
  count(camID) %>%
  ungroup() %>% # notice here: we ungroup before we apply the next set of functions
  rename(predator_observations = n) %>%
  slice_max(predator_observations, n = 10) 
top_10 # and there's no Groups that you can see!


# what is the range of predator observations? 
pred_freq<- detections_locs %>%
  filter(trophic_group == "predator") %>%
  count(camID) %>%
  arrange(camID) %>%
  ungroup() %>% # notice here: we ungroup before we apply the next set of functions
  rename(predator_observations = n) 
summary(pred_freq$predator_observations)
# let's make a spatial heatmap of all cameras that have at least 65 observations of a predator

# we'll plot all cameras, but color code them by # of predator observations
pred_freq %>%
  filter(predator_observations > 23) %>%
  left_join(., cam_locs, by = "camID") %>%
  st_as_sf(.) %>%
  ggplot(.) +
  geom_sf(aes(color = predator_observations), size = 3) +
  scale_color_viridis_c(option = "inferno", name = "Predator Observations") +
  theme_minimal() +
  labs(title = "Predator Occurrence at Camera Locations",
       subtitle = "Top locations by number of observations > mean",
       caption = "Data: Camera trap detections") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.position = "right"
  ) + 
  theme(aspect.ratio = 0.6)  # or a taller ratio

# let's save it!
ggsave("output/top_predator_sites.jpg", plot = last_plot(), width = 8, height = 10, dpi = 500)


#### Exercise 7 ####

# create a spatial plot of top cameras with prey observations that are less than the 
# mean number of prey observations per camera

# save the plot

###################


