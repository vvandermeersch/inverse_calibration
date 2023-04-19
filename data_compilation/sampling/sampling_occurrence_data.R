
#----------------------------------------#
# Script to sample presence/absence data #
#----------------------------------------#

# Example with F. sylvatica

wd <- "~/projects/inverse_calibration/data_compilation/sampling"


# Packages needed
library(future)
library(future.apply)
options(future.globals.maxSize= 1000*1024^2)
library(data.table)
library(dplyr)
library(stringr)
library(raster)
library(abind)
library(dismo, include.only = 'biovars')
# setDTthreads(4)

# Package for reporting progress updates
library(progressr)
handlers(global = TRUE)
handlers("txtprogressbar")

# Functions needed
source(paste0(wd, 'functions/compute_biovars.R')) # to compute bioclim variables
source(paste0(wd, 'functions/process_occurrence.R')) # extract and assemble occurrence data from multiple databases
source(paste0(wd, 'functions/generate_subsets.R')) # generate pres/abs combination

# Settings 
processeddata_folder <- "D:/climate/ERA5-Land/phenofit_format/transformed/"
rawdata_folder <- "D:/climate/ERA5-Land/raw/"
bioclimdata_folder <- "D:/climate/ERA5-Land/bioclim_format/"
speciesdata_folder <- "D:/species/"



# 1. Compute bioclim variables... save in bioclimdata_folder
compute_biovars(1970:2000, processeddata_folder, bioclimdata_folder, ncores=3)
gc()

# Mean of 31 years of bioclim data
biovars_all <- list()
for(i in as.character(1970:2000)){
  load(paste0(bioclimdata_folder, "biovars_", i, ".Rdata"))
  biovars_all <- append(biovars_all, list(biovars))
}
rm(biovars)
biovars_30y <- abind(biovars_all, along=3)
biovars_30y <- apply(biovars_30y, c(1,2), mean)
biovars_30y <- as.data.frame(biovars_30y)



# 2. Process occurrence data

# load data
EVM <- read_sf(paste0(speciesdata_folder,"EuroVegMap/evm_bonusspecies.shp"))
AFE <- read_sf(paste0(speciesdata_folder,"AFE/species/all_bonusspecies.gpkg")) %>% dplyr::filter(status == 6)
EUForest <- read.csv(paste0(speciesdata_folder,"EUForest/EuForestspecies.csv")) 
ERA5land <- raster(paste0(rawdata_folder,"2m_dewpoint_temperature_1969_01.nc"))

# create rasters
GBIF <- fread(paste0(speciesdata_folder,"GBIF/Fagus_sylvatica_20012022/0112486-210914110416597.csv"), sep="\t", dec=".", quote = "",
                             header = TRUE, fill = TRUE, stringsAsFactors = FALSE)
fagussylvatica_occ_rs <- process_occurrence("Fagus sylvatica", EUForest, GBIF, EVM, AFE, ERA5land)
save(fagussylvatica_occ_rs, file = "D:/species/processed/fagus_sylvatica/fagus_sylvatica_occ_rs.Rdata") # one can save file to prevent future computations



# 3. Sampling presence and absence 

# Create several combinations 
N <- 10 # number of combinations
npres <- 1000 # number of presence points
nabs <- 1000 # number of absence points
nclusters <- 10 # number of clusters for presence data

output_folder <- "D:/species/processed/fagus_sylvatica"

generate_subsets(N, npres, nabs, nclusters, quercuspubescens_occ_rs, output_folder)

