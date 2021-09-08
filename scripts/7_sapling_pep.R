### Formatting PEP tree data from Quebec ####

# The geodatabase containing the PEP tree data (placette-échantillon permanente) is available online at https://www.donneesquebec.ca/recherche/fr/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui

# download.file("ftp://transfert.mffp.gouv.qc.ca/Public/Diffusion/DonneeGratuite/Foret/DONNEES_FOR_ECO_SUD/Placettes_permanentes/PEP_GDB.zip", destfile = "raw_data/PEP.zip")

# unzip("raw_data/PEP.zip", exdir = "raw_data")
# database downloaded on May the 24th 2018

### PACKAGES ####
library(rgdal)
library(sf)
library(dplyr)
library(plyr)


### READ GDB LAYERS ####

# Check the list of layers in the gdb
ogrListLayers("raw_data/PEP_GDB/PEP.gdb")

# Plot coordinates
plot_xy <- st_read("raw_data/PEP_GDB/PEP.gdb", layer = "PLACETTE")

# Plot date of measurements
plot_mes <- st_read("raw_data/PEP_GDB/PEP.gdb", layer = "PLACETTE_MES")

# DHP + cote DENDRO_ARBRES
sap_mes <- st_read("raw_data/PEP_GDB/PEP.gdb", layer = "DENDRO_GAULES")


# Species code
sps_code <- read.csv2("raw_data/ref_spCode.csv")

# Remove abandonned plots (STATUT_MES %in% c(AB, RE, RL, DE, NT, SR))

table(plot_mes$STATUT_MES)
ID_PE_aband <- plot_mes %>% filter(!is.na(STATUT_MES))

plot_mes <- plot_mes %>%
  filter(!(ID_PE %in% ID_PE_aband$ID_PE)) %>%
  mutate(year_measured = as.integer(format(DATE_SOND, format="%Y"))) %>%
  dplyr::select(ID_PE, ID_PE_MES, year_measured) 

plot_xy <- plot_xy %>%
  filter(ID_PE %in% plot_mes$ID_PE) %>%
  dplyr::select(ID_PE, SHAPE) 

sap_mes <- sap_mes %>%
  filter(ID_PE %in% plot_mes$ID_PE)

### Change species code

sap_mes$sp_code <- sps_code$spCode[match(sap_mes$ESSENCE, sps_code$qc_code)]

# Remove NA in sp_code columns (not a tree species)

sap_mes <- sap_mes %>% 
  filter(!is.na(sp_code)) %>%
  arrange(ID_PE_MES)

length(which(!(plot_mes$ID_PE_MES %in% unique(sap_mes$ID_PE_MES))))
# 4028 PE_MES are missing from tree_data...
missing_ID <- plot_mes %>% filter(!(ID_PE_MES %in% sap_mes$ID_PE_MES))


# NOTE - weird certaine PEP_MES sans saplings dans sap_mes mais reporté dans tree_mes
# Reponse à moi-meme = pas de gaule dans la sous-placette

sap_mes <- sap_mes %>% 
  right_join(plot_mes, by = c("ID_PE", "ID_PE_MES")) %>% 
  select(ID_PE, ID_PE_MES, year_measured, sp_code, CL_DHP, NB_TIGE)

sap_mes[which(is.na(sap_mes$sp_code)), "NB_TIGE"] <- 0

# NOTE - on ne sait pas qui sont les nouvelles recrues d'un inventaire à l'aute...

saveRDS(sap_mes, "data/sap_data_oct2020.RDS")
