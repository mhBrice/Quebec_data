### PACKAGES ####
library(rgdal)
library(sf)
library(dplyr)
library(reshape2)
library(data.table)

### DATA ####
pep_tree <- readRDS("data/tree_data_nov2019.RDS")
pep_xy <- st_read("data/plot_xy32198_nov2019.gpkg")
pep_mes <- st_read("raw_data/PEP_GDB/PEP.gdb", layer = "PLACETTE_MES")


### READ GDB LAYERS ####

# Check the list of layers in the gdb
ogrListLayers("raw_data/PET2_GDB/PET2.gdb")
ogrListLayers("raw_data/PET3_GDB/PET3.gdb")
ogrListLayers("raw_data/PET4_GDB/PET4.gdb")
ogrListLayers("raw_data/PET5_GDB/PET5.gdb")

# Plot coordinates
pet2_xy <- st_read("raw_data/PET2_GDB/PET2.gdb", layer = "PLACETTE")
pet3_xy <- st_read("raw_data/PET3_GDB/PET3.gdb", layer = "PLACETTE")
pet4_xy <- st_read("raw_data/PET4_GDB/PET4.gdb", layer = "PLACETTE")
pet5_xy <- st_read("raw_data/PET5_GDB/PET5.gdb", layer = "PLACETTE")


# DHP + cote DENDRO_ARBRES
pet2_tree <- st_read("raw_data/PET2_GDB/PET2.gdb", layer = "DENDRO_TIGES")
pet3_tree <- st_read("raw_data/PET3_GDB/PET3.gdb", layer = "DENDRO_TIGES")
pet4_tree <- st_read("raw_data/PET4_GDB/PET4.gdb", layer = "DENDRO_ARBRES")
pet5_tree <- st_read("raw_data/PET5_GDB/PET5.gdb", layer = "DENDRO_ARBRES")

# List of all inventories

pet_xy <- list(pet2_xy = pet2_xy, 
               pet3_xy = pet3_xy, 
               pet4_xy = pet4_xy, 
               pet5_xy = pet5_xy)
pet_tree <- list(pet2_tree = pet2_tree, 
                 pet3_tree = pet3_tree, 
                 pet4_tree = pet4_tree, 
                 pet5_tree = pet5_tree)

# Species code
sps_code <- read.csv2("raw_data/ref_spCode.csv")

### DATA CLEANING ####

# Keep only plot of 400m2 
table(pet2_xy$DIMENSION)
table(pet3_xy$DIMENSION)
table(pet4_xy$DIMENSION)
table(pet5_xy$DIMENSION)

dim <- c("04", "09", "10", "11", "20", "30", "40")

# Keep only necessary columns 

col_xy <- c("NO_PRG", "ID_PE", "LATITUDE", "LONGITUDE", "DATE_SOND")
col_tree <- c("ID_PE", "ESSENCE", "CL_DHP", "NB_TIGE", "TIGE_HA", "ST_HA")

for(i in 1:4) {
  pet_xy[[i]] <- pet_xy[[i]] %>% 
    filter(DIMENSION %in% dim) %>% 
    filter(TYPE_PE != "PEP")
  
  pet_tree[[i]] <- pet_tree[[i]] %>% 
    filter(ID_PE %in% pet_xy[[i]]$ID_PE)
  
  pet_xy[[i]] <- pet_xy[[i]] %>% 
    select(col_xy)
  
  pet_tree[[i]] <- pet_tree[[i]][, colnames(pet_tree[[i]]) %in% col_tree]

}


### Summarize information by species for each ID_PE

for(i in 1:4) {
  pet_tree[[i]] <- as.data.table(pet_tree[[i]])
  pet_tree[[i]] <- pet_tree[[i]][, list(TIGE_HA = sum(TIGE_HA), 
                                        ST_HA = sum(ST_HA)), 
                                 by = list(ID_PE, ESSENCE)]
}



### JOIN EACH INVENTORY WITH XY ####

for(i in 1:4) {
  # Change species code
  pet_tree[[i]]$sp_code <- sps_code$spCode[match(pet_tree[[i]]$ESSENCE, sps_code$qc_code)]

  pet_tree[[i]] <- pet_tree[[i]] %>% left_join(pet_xy[[i]], by = "ID_PE") 
  
  pet_tree[[i]] <- st_as_sf(pet_tree[[i]], coords = c("LATITUDE", "LONGITUDE"))
}


# plot(filter(pet_tree, NO_PRG==2)["ESSENCE"])

#format pep ####

pep_mes <- pep_mes %>% 
  filter(ID_PE %in% pep_xy$ID_PE) %>%
  mutate(year_measured = as.integer(format(DATE_SOND, format="%Y"))) %>%
  select(ID_PE, ID_PE_MES, VERSION, year_measured) %>% 
  mutate(VERSION = case_when(grepl("1er", VERSION) ~ 2,
                             grepl("2e", VERSION) ~ 2,
                             grepl("3e", VERSION) ~ 3,
                             grepl("4e", VERSION) ~ 4,
                             grepl("5e", VERSION) ~ 5))

pep_mes <- pep_mes %>% left_join(pep_xy, by = "ID_PE")

pep_mes <- st_as_sf(pep_mes, crs = 32198)
pep_mes_all <- pep_mes

pep_mes <- split(pep_mes, pep_mes$VERSION)

### BY INVENTORY ####

### NEIGHBORHOOD COMPOSITION ####

# NEAREST NEIGHBOR IN BUFFER ####
# Get the 100 nearest neighbors from PET in a 10km buffer of each PEP, by inventory

neigh <- list()
for(i in 1:4) {
  neigh[[i]] <- nngeo::st_nn(pep_mes[[i]], pet_tree[[i]], k = 100, maxdist = 10000)
}

MySpecies <- levels(pet_tree$pet2_tree$sp_code)

neigh_nb_comp <- neigh_fr_comp <- neigh_st_comp <- list()

# Get composition in the nearest neighbors

for(i in 1:4) {
  
  # Neighbors for all PEP by inventory
  neigh_i <- neigh[[i]]
  
  # Create empty df
  neigh_nb_df <- data.frame(matrix(ncol = length(MySpecies), 
                                nrow = length(neigh_i), 
                                dimnames = list(NULL, MySpecies)))
  
  neigh_st_df  <- neigh_fr_df <- neigh_nb_df
  #neigh_tmp <- list()
  for(l in 1:length(neigh_i)) {
    ll <- neigh_i[[l]]
    if(length(ll) > 0) {
      # Frequence
      nr <- nrow(pet_tree[[i]][ll,])
      neigh_fr <- aggregate(TIGE_HA ~ sp_code, 
                            data = pet_tree[[i]][ll,], 
                            FUN = function(x) length(x)/nr, drop = FALSE)[,2]
      neigh_fr[is.na(neigh_fr)] <- 0
      neigh_fr_df[l,] <- neigh_fr
      
      # TIGE_HA
      neigh_nb <- aggregate(TIGE_HA ~ sp_code, 
                                  data = pet_tree[[i]][ll,], 
                                  FUN = mean, drop = FALSE)[,2]
      neigh_nb[is.na(neigh_nb)] <- 0
      neigh_nb_df[l,] <- neigh_nb
      
      # ST_HA
      neigh_st <- aggregate(ST_HA ~ sp_code, 
                             data = pet_tree[[i]][ll,], 
                             FUN = mean, drop = FALSE)[,2]
      neigh_st[is.na(neigh_st)] <- 0
      neigh_st_df[l,] <- neigh_st
      
    } else {
      neigh_nb_df[l,] <- rep(NA, length(MySpecies))
      neigh_st_df[l,] <- rep(NA, length(MySpecies))
      neigh_fr_df[l,] <- rep(NA, length(MySpecies))
    }
    
  }
  neigh_nb_comp[[i]] <- neigh_nb_df
  neigh_st_comp[[i]] <- neigh_st_df
  neigh_fr_comp[[i]] <- neigh_fr_df
  
}


# NEAREST NEIGHBOR IN BUFFER SCALED BY DISTANCE ####

# Get the 100 nearest neighbors from PET in a 10km buffer of each PEP, by inventory

kernel <- function(d, alpha = 0.1) {
  exp(-alpha * abs(d))
}

neighd <- list()
for(i in 1:4) {
  neighd[[i]] <- nngeo::st_nn(pep_mes[[i]], pet_tree[[i]], k = 100, maxdist = 20000, returnDist = TRUE)
}

MySpecies <- levels(pet_tree$pet2_tree$sp_code)
neigh_std_comp <- list()

# Get composition in the nearest neighbors

for(i in 1:4) {
  
  # Neighbors for all PEP by inventory
  neigh_i <- neighd[[i]]$nn
  d_i <- neighd[[i]]$dist
  
  # Create empty df
  neigh_df <- data.frame(matrix(ncol = length(MySpecies), 
                                nrow = length(neigh_i), 
                                dimnames = list(NULL, MySpecies)))
  
  #neigh_tmp <- list()
  for(l in 1:length(neigh_i)) {
    ll <- neigh_i[[l]]
    # negative exponential kernel on distance in meters
    dd <- kernel(d = d_i[l,]/1000) 
    dd <- na.omit(dd)
   
     if(length(ll) > 0) {
      pet_tmp <- pet_tree[[i]][ll,] %>% mutate(ST_HA = ST_HA*dd)
      neigh_tmp <- aggregate(ST_HA ~ sp_code, 
                             data = pet_tmp, 
                             FUN = mean, 
                             drop = FALSE)[,2]
      neigh_tmp[is.na(neigh_tmp)] <- 0
      
      neigh_df[l,] <- neigh_tmp
    } else {
      neigh_df[l,] <- rep(NA, length(MySpecies))
    }
    
  }
  neigh_std_comp[[i]] <- neigh_df
  
}

for(i in 1:4) {
  neigh_std_comp[[i]] <- cbind.data.frame(pep_mes[[i]], neigh_std_comp[[i]])
  neigh_st_comp[[i]] <- cbind.data.frame(pep_mes[[i]], neigh_st_comp[[i]])
  neigh_fr_comp[[i]] <- cbind.data.frame(pep_mes[[i]], neigh_fr_comp[[i]])
  neigh_nb_comp[[i]] <- cbind.data.frame(pep_mes[[i]], neigh_nb_comp[[i]])
}



neigh_comp <- list(neigh_STdist_comp = do.call(rbind, neigh_std_comp),
     neigh_ST_comp = do.call(rbind, neigh_st_comp),
     neigh_NB_comp = do.call(rbind, neigh_nb_comp),
     neigh_FR_comp = do.call(rbind, neigh_fr_comp))

saveRDS(neigh_comp, "data/neigh_comp.RDS")


### ALL INVENTORIES TOGETHER ####

pet_tree_all <- do.call(rbind, pet_tree)
pet_xy_all <- do.call(rbind, pet_xy)


neighd <- nngeo::st_nn(pep_xy, pet_xy_all, k = 100, maxdist = 20000, returnDist = TRUE)

neigh_i <- neighd$nn
d_i <- neighd$dist

# Create empty df
neigh_df <- data.frame(matrix(ncol = length(MySpecies), 
                              nrow = length(neigh_i), 
                              dimnames = list(NULL, MySpecies)))

#neigh_tmp <- list()
for(l in 1:length(neigh_i)) {
  ll <- neigh_i[[l]]
  id <- pet_xy_all$ID_PE[ll]
  # negative exponential kernel on distance in meters
  dd <- kernel(d = d_i[l,]/1000) 
  dd <- na.omit(dd)
  
  id_dd <- cbind.data.frame(ID_PE = id, dd)
  
  if(length(ll) > 0) {
    
    pet_tmp <- subset(pet_tree_all, ID_PE %in% id) 
   
    pet_tmp$dd <- id_dd$dd[match(pet_tmp$ID_PE, id_dd$ID_PE)]
      
    pet_tmp$ST_HA <- pet_tmp$ST_HA * pet_tmp$dd
    
    neigh_tmp <- aggregate(ST_HA ~ sp_code, 
                           data = pet_tmp, 
                           FUN = mean, 
                           drop = FALSE)[,2]
    neigh_tmp[is.na(neigh_tmp)] <- 0
    
    neigh_df[l,] <- neigh_tmp
  } else {
    neigh_df[l,] <- rep(NA, length(MySpecies))
  }
  
}
neigh_std_comp <- neigh_df

neigh_std_comp <- cbind.data.frame(pep_xy, neigh_std_comp)


saveRDS(neigh_std_comp, "data/neigh_std_comp.RDS")
