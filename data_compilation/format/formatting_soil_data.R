
#----------------------------------------------------------#
# Script to format soil data as required by PHENOFIT model #
#----------------------------------------------------------#

### Settings ###

rawdata_folder <- "D:/soil"

# ERA5_Land grid (0.1deg)
ERA5land_grid <- raster("D:/climate/ERA5-Land/raw/2m_dewpoint_temperature_1969_01.nc")

# 7 soil depths : 0, 5, 15, 30, 60, 100, and 200 cm depth
dep <- c(0, 5, 15, 30, 60, 100, 200)
# 6 layers ?
thick <- c(5, 10, 15, 30, 40, 100)

# Folders
EU_SHD_folder <- file.path(rawdata_folder, "EU_SoilHydroGrids_1km/raw") # folder where EU-SoilHydroGrids data are stored
SoilGrids_folder <- file.path(rawdata_folder, "SoilGrids250m") # folder where SoilGrids data are stored



### Load data ###

# Load field capacity data
filenames <- list.files(EU_SHD_folder, pattern="^FC", full.names=TRUE)
fc_stack <- raster::stack(filenames) %>% crop(ERA5land_grid)

# Load wilting point data
filenames <- list.files(EU_SHD_folder, pattern="^WP", full.names=TRUE)
wp_stack <- raster::stack(filenames) %>% crop(ERA5land_grid)

# Load volumetric percentage of coarse fragments (>2 mm)
filenames <- list.files(SoilGrids_folder, pattern="^CRFVOL", full.names=TRUE)
crf_stack <- raster::stack(filenames) %>% crop(ERA5land_grid)



### Compile data ###

# Change resolution, from 250m/1km to 0.1deg
fc_stack <- resample(fc_stack, ERA5land_grid, method="bilinear")
wp_stack <- resample(wp_stack, ERA5land_grid, method="bilinear")
crf_stack <- resample(crf_stack, ERA5land_grid, method="bilinear")
depth <- resample(depth, ERA5land_grid, method="bilinear")

# From one depth layer to layered weights
depth_weights <- depth
depth_weights[!is.na(depth_weights)] <- thick[1]
names(depth_weights) <- "layer1"
for (i in 2:6){
  r <- depth
  r[] <- NA
  r[depth>=dep[(i+1)]] <- thick[i]
  r[depth<dep[(i+1)]] <- depth[depth<dep[(i+1)]] - dep[i]
  r[r<0] <- 0
  names(r) <- paste0("layer", i)
  depth_weights <- stack(depth_weights, r)
}

# 7 depths => 6 layer values (mean two by two), only for SoilGrids v1 (2017)
fc_layer <- mean(fc_stack[[1]],fc_stack[[2]], na.rm=T)
names(fc_layer) <- "layer1"
for (i in 3:7) {
  r <- mean(fc_stack[[i-1]],fc_stack[[i]], na.rm=T)
  names(r) <- paste0("layer", i-1)
  fc_layer <-stack(fc_layer,r)
}
wp_layer <- mean(wp_stack[[1]],wp_stack[[2]], na.rm=T)
names(wp_layer) <- "layer1"
for (i in 3:7) {
  r <- mean(wp_stack[[i-1]],wp_stack[[i]], na.rm=T)
  names(r) <- paste0("layer", i-1)
  wp_layer <-stack(wp_layer,r)
}

# PHENOFIT need a Water Holding Capacity (mm)
dif <- (fc_layer - wp_layer)/100 
whc <- sum(dif*depth_weights*(100-crf_layer)/100)
whc <- <- whc *10 # convert to mm

