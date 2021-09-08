#### Getting non climatic environmental variables from Quebec forest inventory data ####

# Marie-Helene Brice
# June 13th 2018

# The geodatabase containing the PEP tree data (placette-échantillon permanente) is available online at https://www.donneesquebec.ca/recherche/fr/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui

# Environmental variables of interest:
# Soil variables (humus type, humus ph, pierrosity, drainage)
# Disturbances (logging, burning, windfall, insect outbreak)
# Forest age


### PACKAGES ####

require(rgdal)
require(rgeos)
require(data.table)
require(sf)
require(lwgeom)
require(dplyr)

## DATA ####

### Formatted species data with xy coordinates

tree_data <- readRDS("data/tree_data_oct2020.RDS")

# check the list of layers in the gdb

ogrListLayers("raw_data/PEP_GDB/PEP.gdb")

# Layer containing soil variables (humus, texture, ph)
pep_sol <- st_read("raw_data/PEP_GDB/PEP.gdb", layer = "STATION_SOL")

# Layer containing disturbance variables and drainage
## Field data
pep_pe <- st_read("raw_data/PEP_GDB/PEP.gdb", layer = "STATION_PE")

## photo-interpretation (better)
pep_ori <- st_read("raw_data/PEP_GDB/PEP.gdb", layer = "PEE_ORI_SOND")



# Layer containing age of selected trees in all PE MES
pep_arb <- st_read("raw_data/PEP_GDB/PEP.gdb", layer = "DENDRO_ARBRES_ETUDES")

# For soil type, take the measurements for each PEP MES (humus type, organic matter depth, sometimes vary through year mainly because of disturbances)
# For analysis, we could decide to take only the last measurements or the mean (for quantitative variable)

### SELECT VARIABLES ####

# % of complete cases
apply(pep_sol, 2, function(x) length(which(complete.cases(x))) / nrow(pep_sol) * 100)

apply(pep_pe, 2, function(x) length(which(complete.cases(x))) / nrow(pep_pe) * 100)

apply(pep_ori, 2, function(x) length(which(complete.cases(x))) / nrow(pep_ori) * 100)

apply(pep_arb, 2, function(x) length(which(complete.cases(x))) / nrow(pep_arb) * 100)


# SOIL VARIABLES
pep_sol <- pep_sol %>%
  select(ID_PE, ID_PE_MES, TYPEHUMUS, EPMATORG, PH_HUMUS, PH_HORIZB, POURCPIERR)

# DISTURBANCE VARIABLES
# field data
pep_pe <- pep_pe %>%
  select(ID_PE, ID_PE_MES, ORIGINE, PERTURB, CL_AGE,
         CL_DRAI, ALTITUDE, VERSANT, PC_PENT, EXPOSITION, DEP_SUR, NATURE_DEP)
# photo-interpretation data
pep_ori <- pep_ori %>%
  select(ID_PE, ID_PE_MES, ORIGINE, AN_ORIGINE, PERTURB, AN_PERTURB, CL_AGE,
         CL_DRAI, DEP_SUR) %>%
  rename_with(~paste0(., "_ori"), ORIGINE:DEP_SUR)


# DISTURBANCE VARIABLES
pep_arb <- pep_arb %>%
  select(ID_PE, ID_PE_MES, ID_ARBRE, ID_ARB_MES, AGE)

# XY

pep_xy <- st_read("data/plot_xy32198_oct2020.gpkg")

### JOIN VARIBALES ####

env_data <- tree_data %>%
  ungroup() %>%
  select(ID_PE, ID_PE_MES, year_measured) %>%
  distinct() %>%
  left_join(pep_sol, by = c("ID_PE", "ID_PE_MES")) %>%
  left_join(pep_pe, by = c("ID_PE", "ID_PE_MES")) %>%
  left_join(pep_ori, by = c("ID_PE", "ID_PE_MES"))

age_data <- tree_data %>%
  select(ID_PE, ID_PE_MES, ID_ARBRE, ID_ARB_MES) %>%
  left_join(pep_arb, by = c("ID_PE", "ID_PE_MES", "ID_ARBRE", "ID_ARB_MES"))

### RECLASSIFY VARIABLES ####

table(pep_pe$ORIGINE) # 3986 plots with coupe total
# ORIGINE => Les perturbations naturelles et les interventions anthropiques d’origine qui ont éliminé plus de 75 % de la surface terrière du peuplement précédent
table(pep_ori$ORIGINE)

## DRAINAGE
env_data <- env_data %>%
  mutate(CL_DRAI2 = case_when(CL_DRAI %in% 0 ~ "excessif",
                              CL_DRAI %in% c(10:14) ~ "rapide",
                              CL_DRAI == 16 ~ "complexe",
                              CL_DRAI %in% c(20:24) ~ "bon",
                              CL_DRAI %in% c(30:34) ~ "modere",
                              CL_DRAI %in% c(40:44) ~ "imparfait",
                              CL_DRAI %in% c(50:54) ~ "mauvais",
                              CL_DRAI %in% c(60:64) ~ "tres_mauvais")) %>%
  mutate(CL_DRAI_ori2 = case_when(CL_DRAI_ori %in% c(0, "00") ~ "excessif",
                                  CL_DRAI_ori %in% c(10:14) ~ "rapide",
                                  CL_DRAI_ori == 16 ~ "complexe",
                                  CL_DRAI_ori %in% c(20:24) ~ "bon",
                                  CL_DRAI_ori %in% c(30:34) ~ "modere",
                                  CL_DRAI_ori %in% c(40:44) ~ "imparfait",
                                  CL_DRAI_ori %in% c(50:54) ~ "mauvais",
                                  CL_DRAI_ori %in% c(60:64) ~ "tres_mauvais"))
env_data$CL_DRAI2 <- as.factor(env_data$CL_DRAI2)
env_data$CL_DRAI_ori2 <- as.factor(env_data$CL_DRAI_ori2)


## PERTURBATION D'ORIGINE
env_data <- env_data %>%
  mutate(ORIGINE_ori2 = case_when(ORIGINE_ori == "BR" ~ "burn",
                                  ORIGINE_ori %in% c("CBA", "CBT", "CDV", "CPH", "CPR", "CPT", "CRB", "CRS", "CS", "CT", "ETR", "RPS") ~ "logging",
                                  ORIGINE_ori %in% c("CHT","DT") ~ "windfall", # DT = dépérissement
                                  ORIGINE_ori %in% c("P", "PLN", "PLR", "ENS") ~ "plantation",
                                  ORIGINE_ori == "ES" ~ "severe_outbreak",
                                  ORIGINE_ori == "FR" ~ "wasteland"))
env_data$ORIGINE_ori2 <- as.factor(env_data$ORIGINE_ori2)

## PERTURBATION PARTIELLE
env_data <- env_data %>%
  mutate(PERTURB_ori2 = case_when(PERTURB_ori == "BRP" ~ "partial_burn",
                                  PERTURB_ori %in% c("CA", "CAM", "CB","CD","CDL","CE", "CEA", "CIP", "CJ", "CJG", "CJP", "CJT", "CP", 'CPC', "CPF", "CPI", "CPM", "CPS", "CPX", "CTR", "DEG", "DLD", "DRM", "EC", "ECE", "EPC", "ESI", "PCP") ~
                                "partial_logging",
                                PERTURB_ori == "EL" ~ "light_outbreak",
                                PERTURB_ori %in% c("CHP", "VEP", "DP") ~ "partial_windfall", # DP = dépérissement
                                PERTURB_ori %in% c("ENR", "RR",  "RRG") ~ "partial_plantation")) 
env_data$PERTURB_ori2 <- as.factor(env_data$PERTURB_ori2)

## AGE
age_data <- age_data %>%
  ungroup() %>%
  group_by(ID_PE_MES) %>%
  dplyr::summarise(age_mean = mean(as.integer(AGE), na.rm = TRUE)) %>%
  tidyr::replace_na(list(age_mean = NA))

env_data <- env_data %>% left_join(age_data, by = "ID_PE_MES")

# Order and select last soil measures

env_data <- env_data %>% 
  select(ID_PE:year_measured, 
         ORIGINE_ori:AN_PERTURB_ori, ORIGINE_ori2, PERTURB_ori2,
         ORIGINE:PERTURB, 
         age_mean, CL_AGE,
         TYPEHUMUS:POURCPIERR, CL_DRAI, CL_DRAI2, CL_DRAI_ori2, ALTITUDE:NATURE_DEP) 

env_data <- env_data %>% 
  group_by(ID_PE) %>%
  arrange(year_measured, .by_group = TRUE) %>% 
  mutate_at(vars(TYPEHUMUS:NATURE_DEP), last) %>%
  ungroup()

### COMPLETE MISSING ALTITUDE ####
alt_miss <- unique(env_data$ID_PE[which(is.na(env_data$ALTITUDE))])

alt_xy <- pep_xy %>% 
  filter(ID_PE %in% alt_miss) %>%
  st_transform(crs = 4326) %>%
  st_coordinates()

# all in the same tile...

alt_ras <- raster::getData("SRTM", lon = alt_xy[1,1], lat = alt_xy[1,2])

alt <- raster::extract(alt_ras, alt_xy)

for(i in 1:length(alt_miss)) {
  env_data$ALTITUDE[which(env_data$ID_PE %in% alt_miss[i])] = alt[i]
}

  

### SAVE ####

saveRDS(env_data, "data/env_data_oct2020.RDS")

