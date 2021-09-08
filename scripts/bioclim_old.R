#### Extract ANUSPLIN bioclimatic variables 2km2 resolution #####

# Marie-Helene Brice
# June 13th 2018

# Bioclimatic raster data were obtained directly from McKinney. 
# This script extract climate for all Qc forest plots
# Return the mean of the bioclimatic variables for the year of the plot survey as well as over the last 5 and 10 years before the year of the plot survey.

### PACKAGES ####

require(raster)
require(sp)
require(sf)
require(reshape2)
require(dplyr)
require(data.table)
require(zoo)

### DATA ####

### xy coordinates

plot_xy <- st_read("data/plot_xy_nov2019.gpkg")

plot_xy_id <- cbind.data.frame(ID_PE = plot_xy$ID_PE, st_coordinates(plot_xy))

plot_xy <- st_as_sf(plot_xy) %>% 
  st_transform(4269) %>% # change projection to match climate raster
  as("Spatial") # require sp object to use raster::extract


tree_data <- readRDS("data/tree_data_nov2019.RDS")


# Open climate raster files in each folder (year), 
# create a list of climate files,
# stack them,
# then create a list of rasterstacks
# path_bio <- paste0("raw_data/ANUSPLIN_climate grid_Qc_1960-2013/")
# year_folder <- list.files(path_bio)
# climate_files <- list.files(paste(path_bio, year_folder[1],sep = ""))
# climate_files <- climate_files[-c(20:55)] # remove monthly variables
# 
# bioclim_list <- list()
# bioclim_list_60_13 <- list()
# 
# for(year in year_folder) {
#   for(file in climate_files){
#     bname <- strsplit(file, ".", fixed=TRUE)[[1]][1]
#     tmp <- raster(paste(path_bio, year, "/", file, sep = ""))
#     proj4string(tmp) <- newproj
#     bioclim_list[[bname]] <- tmp
#   }
#   yname <- paste("bioclim", year, sep = "_")
#   bioclim_list_60_13[[yname]] <- stack(bioclim_list)
# }

#### Save the climate rasterstack for each year in a folder

# mapply(writeRaster, bioclim_list_60_13[40:54], 
      # paste0("raw_data/ANUSPLIN_rasterstacks/", names(bioclim_list_60_13)[40:54]), overwrite=T)

#### BIOCLIM data extraction ####

# Read climate rasterstack and create a list
path_bio <- "raw_data/ANUSPLIN_rasterstacks/"

climate_files <- list.files(path_bio, pattern=".grd")
bioclim_list <- list()
for(file in climate_files){
  bname <- strsplit(file, ".", fixed=TRUE)[[1]][1]
  tmp <- stack(paste(path_bio, file, sep = ""))
  bioclim_list[[bname]] <- tmp
}

# Set variable and layer names
ynames <- names(bioclim_list)
varnames <- names(bioclim_list$bioclim_1960)

# Change background values to NAs
na_raster <- bioclim_list$bioclim_1960$bio_01
na_raster[na_raster[]>100] <- NA

id <- which(is.na(values(na_raster)))

for(el in ynames) {
  bioclim_list[[el]][id] <- NA 
}


### Extract climate values at plot location from the raster stack ####
bioclim_pts <- list()
for(y in ynames) {
  tmp <- raster::extract(bioclim_list[[y]], plot_xy)
  bioclim_pts[[y]] <- cbind(plot_xy_id, tmp)
}



### Correct crazy values ####

# get lat-lon of NA values (from plots that are located at the margin of the climate raster)
crazy_plots <- subset(bioclim_pts[[1]], is.na(bio_01), select = ID_PE)

crazy_plots_xy <- plot_xy@coords[plot_xy$ID_PE %in% crazy_plots$ID_PE,]

# extract cell number on the raster
crazy_cell <- raster::extract(na_raster, crazy_plots_xy, cellnumbers = TRUE)[,1L]

# for loop to find the nearest adjacent cells with non-NA values

nbc <- length(crazy_cell)

new_cells <- list()

for (i in 1:nbc) {
  print(i)
  nr <- 3; l <- 1
  while (l) {
    mid <- floor(nr/2) + 1
    cat('order =', mid, '\n')
    mat <- matrix(1, nr, nr)
    mat[mid, mid] <- 0
    tmp_cells <- adjacent(na_raster, crazy_cell[i], directions = mat)[,2L]
    tmp_val <- values(na_raster)[tmp_cells]
    if (!all(is.na(tmp_val))) {
      new_cells[[i]] <- tmp_cells[!is.na(tmp_val)]
      l <- 0
    } else nr <- nr + 2
  }
}

# Replace the NA values by the mean value of adjacent cells
bioclim_corr <- bioclim_pts

for (y in ynames) {
  
  # extract climate values around focal cells and compute the mean
  new_vals <- lapply(new_cells, FUN = function(x) apply(extract(bioclim_list[[y]], x), 2, mean))
  # replace NAs by new mean values
  bioclim_corr[[y]][bioclim_corr[[y]]$ID_PE %in% crazy_plots$ID_PE,-c(1:3)] <- do.call(rbind, new_vals)
}


#
bioclim_corr <- rbindlist(bioclim_corr, idcol = "year")
bioclim_corr$year <- tstrsplit(bioclim_corr$year, "bioclim_", keep=2L)



#### Correction for temperature variables ####

# # All temperature variables must be divided by 10 
T.var <- c("bio_01", "bio_02", "bio_05", "bio_06", "bio_07", "bio_08", "bio_09", "bio_10", "bio_11")

# # Temperature seasonality (bio_04) must be divided by 100
Tseason <- "bio_04"

bioclim_corr <- bioclim_corr %>% 
  mutate_at(T.var, funs(./10)) %>%
  mutate_at(Tseason, funs(./100))


saveRDS(bioclim_corr, "data/bioclim_corrected.RDS")

# Reshape to long data frame
bioclim_corr <- as.data.table(bioclim_corr)
bioclim_long <- melt(bioclim_corr, id.vars = c("year", "ID_PE"), 
                    measure.vars = varnames, variable.name = "bioclim_var", value.name = "clim")

### Rolling average  ####
# for 5 years before and 10 years before

bioclim_roll <- bioclim_long[,list(year = year,
                                  clim = clim,
                                  clim_mean5 = rollmean(clim, k = 5, na.rm = TRUE, align = "right", fill = NA), 
                                  clim_mean10 = rollmean(clim, k = 10, na.rm = TRUE, align = "right", fill = NA)),
                            by = list(bioclim_var, ID_PE)]


bioclim_roll <- bioclim_roll[, year:=as.integer(year)]

saveRDS(bioclim_roll, "data/bioclim_rollmean.RDS")


bioclim_roll2 <- tree_data %>% 
  select(ID_PE, ID_PE_MES, plot_id, year_measured) %>%
  distinct() %>%
  # create dummy_year with max=2013, because bioclim var stops in 2013 while inventories stop in 2016 
  # so inventories after 2013 takes the climate value or mean of 2013
  mutate(dummy_year = ifelse(year_measured>2013,2013,year_measured)) %>%
  left_join(bioclim_roll, by = c("ID_PE"="ID_PE", "dummy_year" = "year")) %>%
  select(-dummy_year)
  


# Create and save bioclim matrices for punctual, mean of 5 preceding years and mean of 10 preceding years
bioclim1_mat <- dcast(bioclim_roll2, plot_id + year_measured ~ bioclim_var, value.var = "clim", mean)

bioclim1_mat <- bioclim_roll2 %>% 
  select(-clim_mean5, - clim_mean10) %>%
  tidyr::spread(key = bioclim_var, value = clim)

bioclim5_mat <- bioclim_roll2 %>% 
  select(-clim, - clim_mean10) %>%
  tidyr::spread(key = bioclim_var, value = clim_mean5)

bioclim10_mat <- bioclim_roll2 %>% 
  select(-clim, - clim_mean5) %>%
  tidyr::spread(key = bioclim_var, value = clim_mean10)


### SAVE

saveRDS(bioclim1_mat, "data/bioclim1_mat.RDS")
saveRDS(bioclim5_mat, "data/bioclim5_mat.RDS")
saveRDS(bioclim10_mat, "data/bioclim10_mat.RDS")


