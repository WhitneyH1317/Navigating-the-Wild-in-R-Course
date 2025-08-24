# Navigating-the-Wild-in-R-Course

Materials and lesson plans for course in data processing, wrangling, and visualization for wildlife ecologists, taught by Dr. K Whitney Hansen and Dr. Johannes DeGroeve. 
Hosted by Dr. Emily Bennitt at University of Botswana's Okavango Research Institute

Lesson 01 can be found [here](https://rprogramming-7718dc.gitlab.io/modules/data_management/data_management.html). 

Part of the material for Lesson 01 has been adapted from Dr. Simona Piccardi's data organizational workshop materials (https://github.com/picardis).

Lessons 04 and 05 were adapted from Dr. Brian Smith's movement data workshop materials (https://github.com/bsmity13).


* www: [Navigating-the-Wild-in-R](https://WhitneyH1317.github.io/Navigating-the-Wild-in-R-Course/) 
* repository: [Navigating-the-Wild-in-R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course) 

# Directory Structure 

The repository contains the following files and directories:

```
├── Navigating-the-Wild-in-R-Course.Rproj
├── README.md
├── additional_materials
│   ├── 01_sim_data.R
│   ├── 01_sim_habitat.R
│   ├── 02_sim_data.R
│   ├── 04_pt2_lecture.html
│   ├── 04_refs.bib
│   ├── HSF_exercise.html
│   └── ICTWS 2025 Workshop.pptx
├── code
│   ├── lesson_02.R
│   ├── lesson_02pt2.R
│   ├── lesson_03.R
│   ├── lesson_03pt2.R
│   ├── lesson_04.R
│   ├── lesson_04pt2.R
│   ├── lesson_05.R
│   └── lesson_05pt2.R
│   ├── exercise_solutions
│   │   ├── 02_solutions.R
│   │   ├── 02_pt2_solutions.R
│   │   ├── 03_solutions.R
│   │   ├── 04_solutions.R
│   │   ├── 04_pt2_solutions.R
│   │   └── 05_HSF_solution.R
│   ├── fun
│   │   ├── bcrw.R
│   │   └── reclass_landuse.R
├── data
│   ├── lesson02
│   │   ├── CA_counties.geojson
│   │   ├── CA_counties.qmd
│   │   ├── CA_protected_areas.geojson
│   │   ├── CA_protected_areas.gpkg
│   │   ├── CA_protected_areas.qmd
│   │   ├── CA_roads.geojson
│   │   ├── CA_roads.qmd
│   │   ├── areas_valid.gpkg
│   │   ├── landcover.tif
│   │   ├── slope.tif
│   │   └── treecover.tif
│   ├── lesson03
│   │   ├── florida_detections.rda
│   │   └── florida_stack.tif
│   ├── lesson04
│   │   ├── fawn_data.rda
│   │   ├── texas_move.rda
│   │   └── texas_stack.tif
│   └── lesson05
│       ├── habitat.tif
│       ├── sim_gps_hsf.csv
│       └── sim_gps_ssf.csv
├── index.html
├── output
│   ├── CA_protected_areas_clipped.geojson
│   ├── CA_roads_clipped.geojson
│   ├── county_PAs.geojson
│   ├── extracted_raster_values.csv
│   ├── low_prey_sites.jpg
│   ├── mypoints.geojson
│   ├── myraster.tif
│   ├── myshapefile.geojson
│   ├── randompoints_bigbasin.csv
│   ├── randompoints_santaclara.csv
│   ├── raster_stack.tif
│   ├── slope.tif
│   ├── species_counts_by_month.jpg
│   ├── species_detections.rda
│   └── top_predator_sites.jpg
├── repo_tree.txt

```

## code-directory 

lessons are stored in the code-directory. 

| File(s)/Directories | Description |
|:--------------------|:------------|
| **[bcrw.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/fun/bcrw.R)** | function for Lesson 04 |
| **[reclass_landuse.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/fun/reclass_landuse.R)** | function for Lesson 05 |
| **[lesson_02.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_02.R)** | Lesson file: basic spatial operations (basic spatial data types, vector operations |
| **[lesson_02pt2.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_02pt2.R)** | Lesson file: basic spatial operations part 2 (sampling points, raster wrangling) |
| **[lesson_03.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_03.R)** | Lesson file: using tidyverse and wrangling detection data |
| **[lesson_03pt2.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_03pt2.R)** | Lesson file: investigating species activity patterns |
| **[lesson_04.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_04.R)** | Lesson file: processing movement data |
| **[lesson_04pt2.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_04pt2.R)** | Lesson file: understanding and plotting homeranges |
| **[lesson_05.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_05.R)** | Lesson file: testing habitat selection functions using simulated data |
| **[lesson_05pt2.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_05pt2.R)** | Lesson file: testing step selection functions using simulated data |


## data-directory  

Raw and processed datasets are stored in the data-directory. Datasets are described in more detail in the above scripts. 

| File(s)/Directories | Description |
|:---------------------|:------------|
| **[CA_counties.geojson](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson02/CA_counties.geojson)** | Lesson 2 data: County designation for California |
| **[CA_counties.qmd](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson02/CA_counties.qmd)** | Lesson 2 data: qmd |
| **[CA_protected_areas.geojson](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson02/CA_protected_areas.geojson)** | Lesson 2 data: Protected area designation for California (geojson version) |
| **[CA_protected_areas.gpkg](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson02/CA_protected_areas.gpkg)** | Lesson 2 data: Protected area designation for California (gpkg version) |
| **[CA_protected_areas.qmd](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson02/CA_protected_areas.qmd)** | Lesson 2 data: qmd |
| **[CA_roads.geojson](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson02/CA_roads.geojson)** | Lesson 2 data: Roads in California |
| **[CA_roads.qmd](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson02/CA_roads.qmd)** | Lesson 2 data: qmd |
| **[landcover.tif](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson02/landcover.tif)** | Lesson 2 data: Raster layer designating landcover types in California |
| **[slope.tif](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson02/slope.tif)** | Lesson 2 data: Raster layer designating slope in California |
| **[treecover.tif](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson02/treecover.tif)** | Lesson 2 data: Raster layer designating tree and shrub cover in California |
| **[florida_detections.rda](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson03/florida_detections.rda)** | Lesson 3 data: RDA file combining camera observations dataframe and camera locations spatial object for Florida camera trap study |
| **[florida_stack.tif](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson03/florida_stack.tif)** | Lesson 3 data: Stack of raster layers combining landcover types in Florida study site |
| **[fawn_data.rda](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson04/fawn_data.rda)** | Lesson 4 data: RDA file combining deer movement data from South Texas and a spatial features of roads across study sites |
| **[texas_move.rda](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson04/texas_move.rda)** | Lesson 4 data: RDA file of fawn mortality data |
| **[texas_stack.tif](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson04/texas_stack.tif)** | Lesson 4 data: Stack of raster layers combining landcover types in Texas study site |
| **[habitat.tif](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson05/habitat.tif)** | Lesson 05 data: simulated habitat data |
| **[sim_gps_hsf.csv](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson05/sim_gps_hsf.csv)** | Lesson 05 data: simulated movement data for HSF |
| **[sim_gps_ssf.csv](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson05/sim_gps_ssf.csv)** | Lesson 05 data: simulated movement data for SSF |



