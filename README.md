# local-reef-pressures
A global mapping analysis of 6 local reef potential pressure layers

Results from this analysis are available as a scientific article still under revision, but accessible as a pre-print at:
https://www.biorxiv.org/content/10.1101/2021.04.03.438313v1


## 1. List of the contents of this repository

### `BCU Report cards/`

Contains the individual report cards (pdf), one for each of the 83 Bioclimatic Units (BCUs).

### `data-raw/` 

Contains the original data layers.

`data-raw/read_layers.R` reads the original data layers, performs the spatial processing and saves the results in the `data/allreefs` layer. You do not need to run this script if you are only interested in the final results or if you want to extract values for new sites.

### `data/`

Contains the final GIS layers of the 6 local reef pressures and the global climatic scores of Beyer et al. (2018), in three different formats: ESRI Shapefile (`.shp`, to open with ArcGIS), `.GPKG` (to open with QGis) and `.RData` (with R).

`key.xlsx` contains correspondence between field names of the RData and GPKG files, on one hand, the the ESRI file, on the other hand

#### `data/report-cards/`:

Contains data to produce the individual report cards, one for each of the 83 Bioclimatic Units (BCUs).

### `R/`

Contains some scripts to generate the report cards and extract the data from the data layers giving point coordinates. In particular:

- `extract_values.R`. Function to extract pressure values for a set of coordinates provided by the user. This is useful if you want to know the values of the six pressures for new sites.

### `plots/`
Empty folder, it is used to store working files to produce the final plots of the article. 

### `analysis/`

Contains all the scripts needed to reproduce the results and the figures of the article.

`Analysis.R`. Code to perform all the analysis of the article and to draw Figure 3 (Individual threats: regional comparisons), Figure 4 (Density distribution of pressure percentiles in BCUs vs non-BCUs), Figure S7 (Comparison of frequency of occurrence of top pressures between regions), Figure S8 (Pressure intensity when top-ranked) and Figure S9 (Comparison of frequency of occurrence of top pressures in BCUs vs non-BCUs) of the article

`Analysis BCU medians.R`. Code to plot Figure S10 (Regional comparisons of median pressure of BCU) and Figure 5 (median pressure for each BCU) of the article

`Plot Figure 6.R`. Not used in the article

`Plot Maps.R`. Code to plot Figure 1 (map of top pressures), Figure 2 (map of cumulative impact score) and Figures S1 to S6 (map of individual pressures) of the article

`Sensitivity_buffer_size.R`. Code for sensitivity analysis of the buffer size of the coastal development layer

`analysis/report-cards/` contains the code to produce the report cards, which are saved in ``BCU Report cards/`.


## 2. How to use the content of this repository

### Creating the final GIS layers from the original layers

Run `data-raw/read_layers.R` to create the final GIS layers in `data/` from the six original layers stored in `data-raw/`. Beware that treatment of the raster layers (coastal development and tourism) takes a long time to run.

### Reproducing the Figures and the results appearing in the paper
#### Figure 1
Run `analysis/Plot Maps.R` (first part)

#### Figure 2
Run `analysis/Plot Maps.R` (second part)

#### Figure 3
Run `analysis/Analysis.R`

#### Figure 4
Run `analysis/Analysis.R`

#### Figure 5
Run `analysis/Analysis BCU medians.R`

#### Supplementary Figure 1 to 6
Run `analysis/Plot Maps.R` (second part)

#### Supplementary Figure 7
Run `analysis/Analysis.R`

#### Supplementary Figure 8
Run `analysis/Analysis.R`

#### Supplementary Figure 9 
Run `analysis/Analysis.R`

#### Supplementary Figure 10
Run `analysis/Analysis BCU medians.R`

#### Supplementary Figure 11
This Figure was made with QGis and annotated in Power POint, there is no R code to produce it

#### Supplementary Figure 12


### Getting the list of BCUs with their top and second-top pressure
Run `analysis/Analysis BCU medians.R`.

### Extracting pressure values for a given set of sites
