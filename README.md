# Navigating-the-Wild-in-R-Course

Materials and lesson plans for course in data processing, wrangling, and visualization for wildlife ecologists, taught by Dr. K Whitney Hansen and Dr. Johannes DeGroeve. 
Hosted by Dr. Emily Bennitt at University of Botswana's Okavango Research Institute

Lesson 01 was adapted from Dr. Simona Piccardi's data organizational workshop materials (https://github.com/picardis).

Lessons 04 and 05 were adapted from Dr. Brian Smith's movement data workshop materials (https://github.com/bsmity13).


* www: [Navigating-the-Wild-in-R](https://WhitneyH1317.github.io/Navigating-the-Wild-in-R-Course/) 
* repository: [Navigating-the-Wild-in-R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course) 

# Directory Structure 

The repository contains the following files and directories:

```
. 
├── Navigating-the-Wild-in-R-Course.Rproj
├── README.md
├── code
│   ├── fun
│   │   └── bcrw.R
│   ├── lesson_02.R
│   ├── lesson_03.R
│   ├── lesson_04.R
│   └── lesson_04_pt2
│       ├── 04_refs.bib
│       ├── 04a_lecture.html
│       ├── 04b_walkthrough.R
│       ├── 04c_exercise.Rmd
│       ├── 04c_exercise.html
│       ├── 04d_solution.R
│       ├── figs
│       └── pop_vs_ind_h.R
├── data
│   ├── lesson01
│   │   └── datasetname.csv
│   ├── lesson02
│   │   ├── CA_counties.geojson
│   │   ├── CA_counties.qmd
│   │   ├── CA_protected_areas.geojson
│   │   ├── CA_protected_areas.gpkg
│   │   ├── CA_protected_areas.qmd
│   │   ├── CA_roads.geojson
│   │   ├── CA_roads.qmd
│   │   ├── datasetname.csv
│   │   ├── landcover.tif
│   │   ├── slope.tif
│   │   └── treecover.tif
│   ├── lesson03
│   │   ├── florida_detections.rda
│   │   └── florida_stack.tif
│   └── lesson04
│       ├── fawn_data.rda
│       ├── texas_move.rda
│       └── texas_stack.tif
├── index.html
├── output
│   ├── CA_protected_areas_clipped.geojson
│   ├── CA_roads_clipped.geojson
│   ├── extracted_raster_values.csv
│   ├── mypoints.geojson
│   ├── myraster.tif
│   ├── myshapefile.geojson
│   ├── randompoints_bigbasin.csv
│   ├── raster_stack.tif
│   ├── slope.tif
│   ├── species_counts_by_month.jpg
│   └── top_predator_sites.jpg
└── repo_tree.txt
```

## code-directory 

lessons are stored in the code-directory. 

| File(s)/Directories | Description |
|:--------------------|:------------|
| **[bcrw.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/fun/bcrw.R)** | Function for Lesson 04 |
| **[lesson_02.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_02.R)** | Lesson file: basic spatial operations in R |
| **[lesson_03.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_03.R)** | Lesson file: using tidyverse and wrangling detection data in R |
| **[lesson_04.R](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/code/lesson_04.R)** | Lesson file: Processing movement data in R |

## data-directory  

Raw and processed datasets are stored in the data-directory. Datasets are described in more detail in the above scripts. 

| File(s)/Directories | Description |
|:---------------------|:------------|
| **[datasetname.csv](https://github.com/WhitneyH1317/Navigating-the-Wild-in-R-Course/blob/main/data/lesson01/datasetname.csv)** | Lesson 1 placeholder data |
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



