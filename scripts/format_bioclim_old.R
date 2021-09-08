### Climate data ####
library(tibble)
library(dplyr)
library(sf)

### Functions ####
replaceNA_df <- function(df1, df2) {
  nas <- which(is.na(df1), arr.ind = T)
  df1[nas] <- df2[nas]
  df1
}

nearestVal <- function(df1, pts) {
  nas <- which(is.na(df1), arr.ind = T)
  
  if(nrow(nas)>0) {
    plot_nas <- pts[nas[,1],]
    plot_all <- pts %>% filter(!(ID_PE %in% unique(rownames(nas))))
    
    nearpts <- unlist(nngeo::st_nn(plot_nas, plot_all))
    newID <- as.character(plot_all[nearpts,]$ID_PE)
    
    for(i in 1:nrow(nas)) {
      ll <- which(rownames(df1) == newID[i])
      df1[nas[i,1], nas[i,2]] <- df1[ll ,nas[i,2]]
    }
  }
  df1
}

### DATA ####
plot_xy <- st_read("data/plot_xy32198_nov2019.gpkg")

plot_xy_id <- cbind.data.frame(ID_PE = plot_xy$ID_PE, st_coordinates(plot_xy))


env_data <- readRDS("data/env_data_nov2019.RDS")

bio_files <- list.files("raw_data/output", pattern = "bio", full.names = TRUE)
cmi_files <- list.files("raw_data/output", pattern = "cmi", full.names = TRUE)
sg_files <- list.files("raw_data/output", pattern = "sg", full.names = TRUE)


### BIO ####

## bio, res = 60
bio60_files <- bio_files[-grep("300", bio_files)]

bio60 <- lapply(bio60_files, readRDS)

names(bio60) <- 1950:2018
bio60 <- lapply(bio60, "rownames<-", plot_xy_id$ID_PE)
bio60 <- lapply(bio60, "colnames<-", 
                paste0("bio_", formatC(1:19, width=2, flag="0")))

## bio, res = 300
bio300_files <- bio_files[grep("300", bio_files)]

bio300 <- lapply(bio300_files, readRDS)

names(bio300) <- 1950:2015
bio300 <- lapply(bio300, "rownames<-", plot_xy_id$ID_PE)
bio300 <- lapply(bio300, "colnames<-", 
                 paste0("bio_", formatC(1:19, width=2, flag="0")))


#### Replace missing info in res60 by res300 ####

for(y in as.character(1950:2015)) {
  bio60[[y]] <- replaceNA_df(bio60[[y]], bio300[[y]])
}


### Replace missing info with nearest neighbour ####

for(y in as.character(1950:2018)) {
  bio60[[y]] <- nearestVal(bio60[[y]], pts = plot_xy)
}
lapply(bio60, function(x) length(which(is.na(x))))


### List to data frame ####

bio60df <- do.call(rbind, bio60)

bio60df <- bio60df %>% 
  rownames_to_column("rowname") %>% 
  tidyr::separate(col = "rowname", into = c("year", "ID_PE"), sep = "\\.")

# Correction for temperature variables ####

# # All temperature variables must be divided by 10 
T.var <- c("bio_01", "bio_02", "bio_05", "bio_06", "bio_07", "bio_08", "bio_09", "bio_10", "bio_11")

# # Temperature seasonality (bio_04) must be divided by 100
Tseason <- "bio_04"

bio60df <- bio60df %>% 
  mutate_at(T.var, ~./10) %>%
  mutate_at(Tseason, ~./100)



### CMI #####
## cmi, res = 60
cmi60_files <- cmi_files[-grep("300", cmi_files)]

cmi60 <- lapply(cmi60_files, readRDS)

names(cmi60) <- 1950:2018
cmi60 <- lapply(cmi60, "rownames<-", plot_xy_id$ID_PE)
cmi60 <- lapply(cmi60, "colnames<-", 
                paste0("cmi_", c(formatC(1:12, width=2, flag="0"),"sum")))

## cmi, res = 300
cmi300_files <- cmi_files[grep("300", cmi_files)]

cmi300 <- lapply(cmi300_files, readRDS)

names(cmi300) <- 1950:2015
cmi300 <- lapply(cmi300, "rownames<-", plot_xy_id$ID_PE)
cmi300 <- lapply(cmi300, "colnames<-", 
                 paste0("cmi_", c(formatC(1:12, width=2, flag="0"),"sum")))


#### Replace missing info in res60 by res300 ####

# CMI
for(y in as.character(1950:2015)) {
  cmi60[[y]] <- replaceNA_df(cmi60[[y]], cmi300[[y]])
}

lapply(cmi60, function(x) length(which(is.na(x)))) # only some NAs in 2018

### Replace missing info with nearest neighbour ####

cmi60[["2018"]] <- nearestVal(cmi60[["2018"]], pts = plot_xy)

lapply(cmi60, function(x) length(which(is.na(x))))

### List to data frame ####

cmi60df <- do.call(rbind, cmi60)

cmi60df <- cmi60df %>% 
  rownames_to_column("rowname") %>% 
  tidyr::separate(col = "rowname", into = c("year", "ID_PE"), sep = "\\.")


### SG ####

## sg, res = 60
sg60_files <- sg_files[-grep("300", sg_files)]

sg60 <- lapply(sg60_files, readRDS)

names(sg60) <- 1950:2018
sg60 <- lapply(sg60, "rownames<-", plot_xy_id$ID_PE)
sg60 <- lapply(sg60, "colnames<-", 
                paste0("sg_", formatC(1:16, width=2, flag="0")))

## sg, res = 300
sg300_files <- sg_files[grep("300", sg_files)]

sg300 <- lapply(sg300_files, readRDS)

names(sg300) <- 1950:2015
sg300 <- lapply(sg300, "rownames<-", plot_xy_id$ID_PE)
sg300 <- lapply(sg300, "colnames<-", 
                 paste0("sg_", formatC(1:16, width=2, flag="0")))


#### Replace missing info in res60 by res300 ####

for(y in as.character(1950:2015)) {
  sg60[[y]] <- replaceNA_df(sg60[[y]], sg300[[y]])
}
lapply(sg60, function(x) length(which(is.na(x))))

### Replace missing info with nearest neighbour ####

sg60[["2018"]] <- nearestVal(sg60[["2018"]], pts = plot_xy)
lapply(sg60, function(x) length(which(is.na(x))))


### List to data frame ####

sg60df <- do.call(rbind, sg60)

sg60df <- sg60df %>% 
  rownames_to_column("rowname") %>% 
  tidyr::separate(col = "rowname", into = c("year", "ID_PE"), sep = "\\.")


### Combine and save ####

bioclim_ally <- bio60df %>% 
  left_join(cmi60df, by = c("year", "ID_PE")) %>% 
  left_join(sg60df, by = c("year", "ID_PE"))
saveRDS(bioclim_ally, "data/bioclim_ally_nov2019.RDS")


#### ROLLMEAN on 10 years ####
rollfun <- function(x) rollmean(x, k = 10, align = "right", fill = NA)
bioclim_roll <- bioclim_ally %>% group_by(ID_PE) %>% arrange(year) %>%
  mutate_if(is.numeric, ~rollfun(.))

bioclim_roll <- na.omit(bioclim_roll)

saveRDS(bioclim_roll, "data/bioclim_roll10_ally_nov2019.RDS")

### Select surveyed years with env data ###
plot_year <- env_data[,c("ID_PE", "year_measured")]
bioclim_roll$year <- as.numeric(bioclim_roll$year)

bioclim_roll10 <- bioclim_roll %>% 
  right_join(plot_year, by = c("ID_PE", "year" = "year_measured"))

saveRDS(bioclim_roll10, "data/bioclim_roll10_nov2019.RDS")
