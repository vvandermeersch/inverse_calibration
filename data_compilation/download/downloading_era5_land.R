library(reticulate)

setwd("D:/climate/ERA5_Land/raw") # folder where files will be downloaded

# Python config
use_python("C:/Users/vandermeersch/AppData/Local/Programs/Python/Python310/python.exe", required=T)
cdsapi <- import("cdsapi")
source_python("C:/Users/vandermeersch/Documents/CEFE/projects/inverse_calibration/climate_compilation/download/ERA5_land_download.py") # custom script


# select the variable; name must be a valid ERA5 CDS API name
var <- '10m_u_component_of_wind'

# For valid keywords, see Table 2 of:
# https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation#ERA5Land:datadocumentation-parameterlistingParameterlistings

# Select years
years <- as.character(1969:2000)

# Maximum number of per-user requests that access the online CDS data is 2
# plan(multisession, workers = 2) ?!
# But Python from reticulate cannot run in parallel because of external pointers !
# We can launch manually 2 R sessions, with these two subsets:
# years_1 <- years[1:(length(years)%/%2)]
# years_2 <- years[(length(years)%/%2+1):length(years)]

#Run script
era5_land(years = years, var = var)



