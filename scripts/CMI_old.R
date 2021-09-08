#### Extract Climate Moisture Index 2km2 resolution #####

# Marie-Helene Brice
# November 2018

# CMI raster data were obtained from Natural Resources Canada: ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids
# This script extract CMI for all Qc forest plots
# Return the mean of the CMI for the year of the plot survey as well as over the last 5 and 10 years before the year of the plot survey. Combine it with the other bioclimatic variables

### PACKAGES ####

require(raster)
require(sf)
require(zoo)
require(reshape2)
require(dplyr)

# Point coordinates

plot_xy <- st_read("data/plot_xy32198_oct2020.gpkg")

plot_xy_id <- cbind.data.frame(ID_PE = plot_xy$ID_PE, st_coordinates(plot_xy))

plot_xy <- plot_xy %>% 
  st_transform(st_crs(x)) #%>% # change projection to match climate raster
  as("Spatial")  # require sp object to use raster::extract

bbx <- plot_xy@bbox


### GET DATA ####

dir.create("raw_data/clim/")

#ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids
basurl <- "ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids/zipfiles60/"
# one among "bio", "cmi", "mint", "maxt", "pcp", "sg"
info <- c("sg", "cmi")
# either "_300arcsec.zip" or "_60arcsec.zip"
# 300 = 10 km2. 60 = 2 km2.
end <- "_300arcsec.zip"

ls_cmi <- list()
# year available: from 1960 to 2018
for(i in info) {
  path_info <- paste0("raw_data/clim/", i, "/")
  dir.create(path_info)
  for (year in c(1960:2018)) {
    
    zout <- paste0(path_info, i, year, ".zip")
    
    download.file(
      paste0(basurl, i, year, end),
      destfile = path_info,
      method = "wget")
    
    unzip(paste0(path_info, i, year), exdir = "raw_data/clim/")
    unlink(zout)
    
    ra <- raster(paste0("raw_data/clim/", year, "/cmi60_sum.asc"))
    
    ls_cmi[[paste0(info, year)]]  <- extract(x = crop(ra, bbx, snap = "out"), y = plot_xy)
    
    unlink(paste0("raw_data/clim/", year), recursive=TRUE) 
  }
}



oldls <- readRDS("data/cmi_ls.RDS")
oldls <- do.call("cbind", oldls)

rownames(cmi_xy) <- plot_xy$ID_PE
colnames(cmi_xy) <- 2016:2018 # 1960:2015


### Correct crazy values ####

# get lat-lon of NA values (from plots that are located at the margin of the climate raster)
na_plots <- names(which(is.na(cmi_xy[,3])))

na_plots_xy <- plot_xy@coords[plot_xy$ID_PE %in% na_plots,]

# extract cell number on the raster
ra <- crop(ra, bbx, snap = "out")
na_cell <- extract(ra, na_plots_xy, cellnumbers = TRUE)[,1L]

# for loop to find the nearest adjacent cells with non-NA values

nbc <- length(na_cell)

new_cells <- list()

for (i in 1:nbc) {
  print(i)
  nr <- 3; l <- 1
  while (l) {
    mid <- floor(nr/2) + 1
    cat('order =', mid, '\n')
    mat <- matrix(1, nr, nr)
    mat[mid, mid] <- 0
    tmp_cells <- adjacent(ra, na_cell[i], directions = mat)[,2L]
    tmp_val <- values(ra)[tmp_cells]
    if (!all(is.na(tmp_val))) {
      new_cells[[i]] <- tmp_cells[!is.na(tmp_val)]
      l <- 0
    } else nr <- nr + 2
  }
}

# Replace the NA values by the mean value of adjacent cells
na_ls <- list()

for (year in 1960:2018) {
  
  # get July max temperature 
  ra <- crop(raster(paste0("raw_data/moistureIndex/", year, "/cmi60_sum.asc")), bbx, snap = "out")
  
  # extract climate values around focal cells and compute the mean
  new_vals <- lapply(new_cells, FUN = function(x) mean(ra[x]))

  na_ls[[year]] <- do.call(rbind, new_vals)
}



cmi_xy[which(is.na(cmi_xy[,1])),] <- do.call(cbind, na_ls)

#### ROLLMEAN ####

cmi_mean10 <- t(apply(cmi_xy, 1, function(x) rollmean(x, k = 10, align = "right")))
cmi_mean5 <- t(apply(cmi_xy, 1, function(x) rollmean(x, k = 5, align = "right")))

x=tibble::rownames_to_column(as.data.frame(cmi_xy), var = "ID_PE")

cmi_ls <- list(cmi1 = cmi_xy, cmi5 = cmi_mean5, cmi10 = cmi_mean10)

cmi_ls <- lapply(cmi_ls, function(x) tibble::rownames_to_column(as.data.frame(x), var = "ID_PE"))

saveRDS(cmi_ls, "data/cmi_ls.RDS")

### Combine with the other bioclimatic variables ####

bioclim1 <- readRDS("data/bioclim1_mat.RDS")
bioclim5 <- readRDS("data/bioclim5_mat.RDS")
bioclim10 <- readRDS("data/bioclim10_mat.RDS")
bioclim_all <- readRDS("data/bioclim_corrected.RDS")


cmi_long <- lapply(cmi_ls, function(x) melt(x, id.var = "ID_PE", value.name = "cmi"))

colnames(cmi_long$cmi1) <- c("ID_PE", "year_measured", "cmi")
colnames(cmi_long$cmi5) <- c("ID_PE", "year_measured", "cmi")
colnames(cmi_long$cmi10) <- c("ID_PE", "year_measured", "cmi")


cmi_long$cmi1$year_measured <- as.numeric(as.character(cmi_long$cmi1$year_measured))
cmi_long$cmi5$year_measured <- as.numeric(as.character(cmi_long$cmi5$year_measured))
cmi_long$cmi10$year_measured <- as.numeric(as.character(cmi_long$cmi10$year_measured))

bioclim1 <- bioclim1 %>%  
  mutate(dummy_year = ifelse(year_measured>2015,2015,year_measured)) %>%
  left_join(cmi_long$cmi1, by = c("ID_PE", "dummy_year" = "year_measured")) %>%
  dplyr::select(-dummy_year)

bioclim5 <- bioclim5 %>%  
  mutate(dummy_year = ifelse(year_measured>2015,2015,year_measured)) %>%
  left_join(cmi_long$cmi5, by = c("ID_PE", "dummy_year" = "year_measured")) %>%
  dplyr::select(-dummy_year)

bioclim10 <- bioclim10 %>%  
  mutate(dummy_year = ifelse(year_measured>2015,2015,year_measured)) %>%
  left_join(cmi_long$cmi10, by = c("ID_PE", "dummy_year" = "year_measured")) %>%
  dplyr::select(-dummy_year)

bioclim_all <- bioclim_all %>% 
  left_join(cmi_long$cmi1, by = c("ID_PE", "year" = "year_measured"))


saveRDS(bioclim1, "data/bioclim1_mat.RDS")
saveRDS(bioclim5, "data/bioclim5_mat.RDS")
saveRDS(bioclim10, "data/bioclim10_mat.RDS")

saveRDS(bioclim_all, "data/bioclim_corrected.RDS")


