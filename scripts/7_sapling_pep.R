### Formatting PEP tree data from Quebec ####

# The geopackage containing the PEP data (placette-échantillon permanente) is available online at
# https://www.donneesquebec.ca/recherche/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui

# database downloaded on February, 1rst 2023

### PACKAGES ####
library(sf)
library(dplyr)
library(plyr)

### READ DATA ####

# PEP coordinates
pep_xy <- st_read("pep_xy32198_fev2023.gpkg")

# PEP env data
env_data <- readRDS("data/env_data_fev2023.RDS") %>%
  select(id_pe, id_pe_mes, year_measured)


# Check the list of layers in PEP.gpkg
st_layers("raw_data/PEP_GPKG/PEP.gpkg")

# DHP + cote DENDRO_ARBRES
sap_mes <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "dendro_gaules")
seed_mes <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "station_semis")

# Species code
sps_code <- read.csv2("raw_data/ref_spCode.csv")

# Keep same PEP as in tree pep
sap_mes <- sap_mes %>%
  filter(id_pe %in% env_data$id_pe)

seed_mes <- seed_mes %>%
  filter(id_pe %in% env_data$id_pe)

### Change species code
sap_mes$sp_code <- sps_code$spCode[match(sap_mes$essence, sps_code$qc_code)]
seed_mes$sp_code <- sps_code$spCode[match(seed_mes$essence, sps_code$qc_code)]

# NOTE: NA in sp_code columns = not a tree species

length(which(!(env_data$id_pe_mes %in% unique(sap_mes$id_pe_mes))))
# 3969 PE_MES are missing from sap_mes

# add missing pe_mes
sap_mes <- sap_mes %>% 
  right_join(env_data, by = c("id_pe", "id_pe_mes")) %>% 
  select(id_pe, id_pe_mes, year_measured, sp_code, cl_dhp, nb_tige)
sap_mes[which(is.na(sap_mes$sp_code)), "nb_tige"] <- 0
sap_mes[which(is.na(sap_mes$sp_code)), "cl_dhp"] <- NA

length(which(!(env_data$id_pe_mes %in% unique(seed_mes$id_pe_mes))))
# 26783 PE_MES are missing from seed_mes
# add missing pe_mes
seed_mes <- seed_mes %>% 
  right_join(env_data, by = c("id_pe", "id_pe_mes")) %>% 
  select(id_pe, id_pe_mes, micro_pe, year_measured, sp_code, cl_ht_semi)

seed_mes[which(is.na(seed_mes$sp_code)), "cl_ht_semi"] <- NA

# NOTE - on ne sait pas qui sont les nouvelles recrues d'un inventaire à l'aute...

saveRDS(sap_mes, "data/sap_data_fev2023.RDS")
saveRDS(seed_mes, "data/seed_data_fev2023.RDS")
