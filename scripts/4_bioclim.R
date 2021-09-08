#### Extract bioclimatic variables 2km2 resolution #####


# bioclimatic raster data were obtained from Natural Resources Canada: ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids
# This script extract bioclimatic for all Qc forest plots
# Return the mean of the bioclimatic for the year of the plot survey as well as over the last 10 years before the year of the plot survey.

### PACKAGES ####

require(raster)
require(sf)
require(zoo)
require(reshape2)
require(dplyr)


### DATA ####

### xy coordinates

plot_xy <- st_read("data/plot_xy32198_oct2020.gpkg")


plot_xy <- plot_xy %>% 
  st_transform("+proj=longlat +datum=WGS84 +no_defs") 



### Function to download raster of climate data for North America ####

# Modified from https://github.com/inSileco/inSilecoDataRetrieval/blob/master/R/get_climate_nam_grids.R

retrieveClimateData <- function(years = 1900:2018,
                                info =  c("bio", "cmi", "mint", "maxt", "pcp", "sg"), res = 300,
                                path = "raw_data/clim", geom) {
  
  stopifnot(res %in% c(60, 300))
  ls_clim <- list()
  
  dir.create(path, showWarnings = FALSE)
  
  #
  basurl <- "ftp://ftp.nrcan.gc.ca/pub/outgoing/NAM_grids/zipfiles"
  #
  info <- match.arg(info)
  beg <- paste0(basurl, res, "/")
  end <- paste0("_", res, "arcsec.zip")
  # year available: from 1900 to 2018
  for (year in years) {
    tmp <- tempfile(fileext = ".zip")
    print(tmp)
    print(paste0(beg, info, year, end))
    
    # Download
    curl::curl_download(paste0(beg, info, year, end), tmp)
    unzip(tmp, exdir = path)
    unlink(tmp)
    
    # extract data
    ls_tmp <- extract_climate_data(path = path, info = info, 
                                                          year = year, geom = geom)
    
    # Save intermediate results just in case it crashes
    saveRDS(ls_tmp, paste0(path, "/", info, year, "_", res, ".rds"))
    ls_clim[[paste0(info, year)]] <- ls_tmp
    unlink(paste0(path, "/", year), recursive = TRUE)
  }
  invisible(NULL)
  
  # Save final results
  saveRDS(ls_clim, paste0(path, "/", info, "_", res, ".rds"))

}

### Function to extract data from raster to multipoints ####

# Modified from https://github.com/inSileco/inSilecoDataRetrieval/blob/master/R/get_climate_nam_grids.R

extract_climate_data <- function(path, info, year, geom, pattern = "\\.asc$|\\.tif$") {
  nm_fo <- paste0(path, "/", year)
  fls <- list.files(nm_fo, pattern = pattern, full.names = TRUE)
  
  out <- lapply(lapply(fls, raster), 
                function(x) extract(crop(x, y =  geom), y =  geom))
  
  names(out) <- paste0(gsub(list.files(nm_fo, pattern = pattern), pat = pattern, rep = ""), "_", year)

  out
}


retrieveClimateData(years = 1960:2018, info = "sg", res = 60, geom = plot_xy, path = "raw_data/clim")

retrieveClimateData(years = 1960:2018, info = "cmi", res = 60, geom = plot_xy, path = "raw_data/clim")

### Format bioclimatic variables ####

sg_60 <- readRDS("raw_data/clim/sg_60.rds")

cmi_60 <- readRDS("raw_data/clim/cmi_60.rds")

# Find NA values (from plots that are located at the margin of the climate raster)

na_sg <- which(is.na(sg_60$sg1960$sg60_01_1960))
na_cmi <- which(is.na(cmi_60$cmi1960$cmi60_sum_1960))

na_sg_xy <- plot_xy[na_sg,]
na_cmi_xy <- plot_xy[na_cmi,]

# Replace NAs with nearest neighbor

xy_unassign_sg <- st_join(na_sg_xy, plot_xy[-na_sg,], join = st_nearest_feature)
xy_unassign_cmi <- st_join(na_cmi_xy, plot_xy[-na_cmi,], join = st_nearest_feature)


unassign_sg <- unlist(lapply(xy_unassign_sg$ID_PE.y, function(x) which(plot_xy$ID_PE %in% x)))

unassign_cmi <- unlist(lapply(xy_unassign_cmi$ID_PE.y, function(x) which(plot_xy$ID_PE %in% x)))
  
sg_60 <- rapply(sg_60, function(x) replace(x, na_sg, x[unassign_sg]), how = 'list')

cmi_60 <- rapply(cmi_60, function(x) replace(x, na_cmi, x[unassign_cmi]), how = 'list')

### From list to df ####

names(cmi_60) <- 1960:2018

cmi_60 <- rapply(cmi_60, as.data.frame, how = "list")

cmi_60 <- lapply(cmi_60, function(y){lapply(y, function(x) {rownames(x) <- plot_xy$ID_PE; x})})

cmi_60 <- do.call(rbind, lapply(cmi_60, data.frame))
colnames(cmi_60) <- paste0("cmi_", c(1:12, 'sum'))
cmi_60 <- tibble::rownames_to_column(cmi_60)

cmi_60 <- cmi_60 %>%
  tidyr::separate(rowname, c("year", "ID_PE"), "\\.")


names(sg_60) <- 1960:2018

sg_60 <- rapply(sg_60, as.data.frame, how = "list")

sg_60 <- lapply(sg_60, function(y){lapply(y, function(x) {rownames(x) <- plot_xy$ID_PE; x})})

sg_60 <- do.call(rbind, lapply(sg_60, data.frame))
colnames(sg_60) <- paste0("sg_", 1:16)
sg_60 <- tibble::rownames_to_column(sg_60)

sg_60 <- sg_60 %>%
  tidyr::separate(rowname, c("year", "ID_PE"), "\\.")

bioclim_ally <- left_join(sg_60, cmi_60, by = c("ID_PE", "year"))

saveRDS(bioclim_ally, "data/bioclim_ally_oct2020.RDS")

#### ROLLMEAN ####

cmi_long <- melt(as.data.table(cmi_60), id.vars = c("year", "ID_PE"), 
                     measure.vars = paste0("cmi_", c(1:12, 'sum')), 
                                           variable.name = "bioclim_var", 
                                           value.name = "clim")

sg_long <- melt(as.data.table(sg_60), id.vars = c("year", "ID_PE"), 
                 measure.vars = paste0("sg_", 1:16), 
                 variable.name = "bioclim_var", 
                 value.name = "clim")


### Rolling average  ####
#  10 years before

cmi_roll <- cmi_long[,list(year = year,
                           clim = clim,
                           clim_mean10 = rollmean(clim, k = 10, na.rm = TRUE, 
                                                  align = "right", fill = NA)),
                     by = list(bioclim_var, ID_PE)]

sg_roll <- sg_long[,list(year = year,
                           clim = clim,
                           clim_mean10 = rollmean(clim, k = 10, na.rm = TRUE, 
                                                  align = "right", fill = NA)),
                     by = list(bioclim_var, ID_PE)]


env_data <-readRDS("data/env_data_oct2020.RDS")


cmi_roll$year <- as.numeric(as.character(cmi_roll$year))
sg_roll$year <- as.numeric(as.character(sg_roll$year))

cmi_roll2 <- env_data %>% 
  select(ID_PE, ID_PE_MES, year_measured) %>%
  # create dummy_year with max=2013, because bioclim var stops in 2018 while inventories stop in 2019 
  # so inventories after 2018 takes the climate value or mean of 2018
  mutate(dummy_year = ifelse(year_measured>2018,2018,year_measured)) %>%
  left_join(cmi_roll, by = c("ID_PE"="ID_PE", "dummy_year" = "year"))

sg_roll2 <- env_data %>% 
  select(ID_PE, ID_PE_MES, year_measured) %>%
  # create dummy_year with max=2013, because bioclim var stops in 2018 while inventories stop in 2019 
  # so inventories after 2018 takes the climate value or mean of 2018
  mutate(dummy_year = ifelse(year_measured>2018,2018,year_measured)) %>%
  left_join(sg_roll, by = c("ID_PE"="ID_PE", "dummy_year" = "year"))


cmi_mat <- dcast(as.data.table(cmi_roll2), ID_PE +ID_PE_MES + year_measured ~ bioclim_var, value.var = "clim_mean10")

sg_mat <- dcast(as.data.table(sg_roll2), ID_PE +ID_PE_MES + year_measured ~ bioclim_var, value.var = "clim_mean10")

bioclim_mat <- left_join(sg_mat, cmi_mat, by= c("ID_PE", "ID_PE_MES", "year_measured"))

saveRDS(bioclim_mat, "data/bioclim10_mat_oct2020.RDS")

