#-----------------------------------------------------------------------#
# Script to format ERA5-Land climate data as required by Phenofit model #
#-----------------------------------------------------------------------#

# author : V. Van der Meersch
# date : 16/02/2022

wd <- "~/projects/inverse_calibration/data_compilation/format"


## Packages needed

library(ncdf4)
library(future)
library(future.apply)
library(dplyr)
library(raster)
library(data.table)


## Functions needed

source(paste0(wd, 'functions/phenofit_formatting.R')) # just to format variables for Phenofit, v2 with raster format (faster)
source(paste0(wd, 'functions/phenofit_processing_and_formatting.R')) # to calculate supplementary variables and format them for Phenofit, v2 with raster format (faster)
source(paste0(wd, 'functions/phenofit_compute_PET2.R')) # to calculate evapotranspiration, v2 with different parallel architecture
source(paste0(wd, 'functions/phenofit_get_altitude.R')) # to get altitude
source(paste0(wd, 'functions/phenofit_get_WHC.R')) # to get WHC


## Settings

rawdata_folder <- "D:/climate/ERA5-Land/raw/"
processeddata_folder <- "D:/climate/ERA5-Land/phenofit_format/"


## Compile daily data from hourly data
phenofit_formatting(1969:2000, "2m_dewpoint_temperature", 'mean', rawdata_folder, processeddata_folder, ncores=5)
gc()

phenofit_formatting(1969:2000, "2m_temperature", "min", rawdata_folder, processeddata_folder, ncores=5)
gc()

phenofit_formatting(1969:2000, "2m_temperature", "max", rawdata_folder, processeddata_folder, ncores=5)
gc()

phenofit_formatting(1969:2000, "total_precipitation", "sum", rawdata_folder, processeddata_folder, ncores=4)
gc()

phenofit_formatting(1969:2000, "surface_solar_radiation_downwards", "sum", rawdata_folder, processeddata_folder, ncores=5) #used only for ETP calculation
gc()

phenofit_processing_and_formatting(1969:2000, "wind", "mean", rawdata_folder, processeddata_folder, ncores=5) #used only for ETP calculation
gc()

phenofit_processing_and_formatting2(2014:2016, "relative_humidity", "min", rawdata_folder, processeddata_folder, ncores=3) #used only for ETP calculation
gc()

phenofit_processing_and_formatting2(2014:2016, "relative_humidity", "max", rawdata_folder, processeddata_folder, ncores=3) #used only for ETP calculation
gc()


## Get altitude
tmin_file <- paste0(processeddata_folder, "ERA5LAND_", "tmn", "_", 1973, "_dly.fit")
tmin <- fread(tmin_file, showProgress=F)
loc_needed <- data.frame(tmin[,1], tmin[,2])
names(loc_needed) <- c("lat", "lon")
phenofit_get_altitude(loc = loc_needed, rawdata_folder, processeddata_folder)
rm(tmin, loc_needed)
gc()


## Compute evapotranspiration
phenofit_compute_PET(1969:2000, processeddata_folder, method = "PenmanMonteith", ncores=2, transform_NA_and_zero=T)






