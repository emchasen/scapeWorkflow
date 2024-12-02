# scapeWorkflow
lays out the workflow for creating new regions in the grazeScape/smartScape tools

1. Download SSURGO data 
1.1. scripts/downloadSoil.R 
1.2. clean soil data
2. to create rasters
2.1. first make shapefile: MakeSoilShape_helperScript.R (on GitHub)
2.2. Create rasters from shape file 
2.2.1. categoricSoilRasterCreation.R
2.2.2. numericSoilRasterCreation.R
2.2.3. To create the DEM, slope and LS rasters
2.2.3.1. first use the DEM shapefile from DNR
2.2.3.2. follow DEM_slope_LS_rasterCreation.R (GitHub)
3. To make the models:
3.1. join the snapplus simulation batch output files with the csvs created in cleanSSURGO_snapplus.R (one file per county that is run in the simulation)
then modelCreation.proj in snapplus grazescape alias

