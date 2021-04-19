# local-reef-pressures
A global mapping analysis of 6 local reef potential pressure layers

Results from this analysis are available as a scientific article still under revision, but accessible as a pre-print at:
https://www.biorxiv.org/content/10.1101/2021.04.03.438313v1



Guide to the contents of this repository


Folder: BCU Report cards
Contains the individual report cards (pdf), one for each of the 83 Bioclimatic Units (BCUs).


Folder: data
Contains the GIS layers of the 6 local reef pressures and the global climatic scores of Beyer et al. (2018), in three different formats: ESRI Shapefile (.shp, to open with ArcGIS), GPKG (to open with QGis) and RData (with R).
key.xlsx contains correspondence between field names of the RData and GPKG files, on one hand, the the ESRI file, on the other hand
Folder report cards: contains data to produce the individual report cards, one for each of the 83 Bioclimatic Units (BCUs).


Folder: helper-scrips
Conytains some scripts to generate the report cards and extract the data from the data layers giving point coordinates


Folder: plots
Contains working files to produce the final plots of the article. The figures appearing in the article are combination of these plots. 


Folder: scripts
Contains all the scripts needed to reproduce the results and the figures of the article.

Analysis.R. Code to perform all the analysis of the article and to draw Figure 5 (barplot and dotplot of pressures in each BCU) and most Supplementary Figures

Plot Figure 3 & 4.R. Code to plot Figure 3 (Regional comparisons) and Figure 4 (comparison BCUs vs non-BCUs) of the article

Plot Figure 6.R. Not used in the article

Plot Maps.R. Code to plot Figure 1 (map of top pressures) and Figure 2 (map of cumulative impact score) of the article

Sensitivity_buffer_size.R. Code for sensitivity analysis of the buffer size of the coastal development layer
