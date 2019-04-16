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
require(sp)
require(data.table)
require(sf)
require(lwgeom)
require(dplyr)

## DATA ####

### Formatted species data with xy coordinates

tree_data <- readRDS("data/tree_data_may2018.RDS")

# check the list of layers in the gdb

ogrListLayers("data_raw/Pep/PEP.gdb")

# Layer containing soil variables (humus, texture, ph)
pep_sol <- st_read("data_raw/Pep/PEP.gdb", layer = "STATION_SOL")

# Layer containing disturbance variables and drainage
pep_pe <- st_read("data_raw/Pep/PEP.gdb", layer = "STATION_PE")

# Layer containing age of selected trees in all PE MES
pep_arb <- st_read("data_raw/Pep/PEP.gdb", layer = "DENDRO_ARBRES_ETUDES")

# For soil type, take the measurements for each PEP MES (humus type, organic matter depth, sometimes vary through year mainly because of disturbances)
# For analysis, we could decide to take only the last measurements or the mean (for quantitative variable)

### SELECT VARIABLES ####

# % of complete cases
apply(pep_sol, 2, function(x) length(which(complete.cases(x))) / nrow(pep_sol) * 100)

apply(pep_pe, 2, function(x) length(which(complete.cases(x))) / nrow(pep_pe) * 100)

apply(pep_arb, 2, function(x) length(which(complete.cases(x))) / nrow(pep_arb) * 100)


# SOIL VARIABLES
pep_sol <- pep_sol %>%
  select(ID_PE, ID_PE_MES, TYPEHUMUS, EPMATORG, PH_HUMUS, POURCPIERR)

# DISTURBANCE VARIABLES
pep_pe <- pep_pe %>%
  select(ID_PE, ID_PE_MES, CL_DRAI, ORIGINE, PERTURB)

# DISTURBANCE VARIABLES
pep_arb <- pep_arb %>%
  select(ID_PE, ID_PE_MES, ID_ARBRE, ID_ARB_MES, AGE)

### JOIN VARIBALES ####

env_data <- tree_data %>%
  select(ID_PE, ID_PE_MES, plot_id, year_measured) %>%
  distinct() %>%
  left_join(pep_sol, by = c("ID_PE", "ID_PE_MES")) %>%
  left_join(pep_pe, by = c("ID_PE", "ID_PE_MES"))

age_data <- tree_data %>%
  select(ID_PE, ID_PE_MES, ID_ARBRE, ID_ARB_MES) %>%
  left_join(pep_arb, by = c("ID_PE", "ID_PE_MES", "ID_ARBRE", "ID_ARB_MES"))

### RECLASSIFY VARIABLES ####

table(pep_pe$ORIGINE) # 3766 plots with coupe total
# ignore FR (friche)
# ORIGINE => Les perturbations naturelles et les interventions anthropiques d’origine qui ont éliminé plus de 75 % de la surface terrière du peuplement précédent


## DRAINAGE
env_data <- env_data %>%
  mutate(CL_DRAI2 = case_when(CL_DRAI %in% 0 ~ "excessif",
                              CL_DRAI %in% c(10:14) ~ "rapide",
                              CL_DRAI == 16 ~ "complexe",
                              CL_DRAI %in% c(20:24) ~ "bon",
                              CL_DRAI %in% c(30:34) ~ "modere",
                              CL_DRAI %in% c(40:44) ~ "imparfait",
                              CL_DRAI %in% c(50:54) ~ "mauvais",
                              CL_DRAI %in% c(60:64) ~ "tres_mauvais"))
env_data$CL_DRAI2 <- as.factor(env_data$CL_DRAI2)

## PERTURBATION D'ORIGINE
env_data <- env_data %>%
  mutate(ORIGINE2 = case_when(ORIGINE == "BR" ~ "burn",
                              ORIGINE %in% c("CBA", "CBT", "CPR", "CT") ~ "logging",
                              ORIGINE %in% c("CHT","DT") ~ "windfall", # DT = dépérissement
                              ORIGINE %in% c("P", "PLN", "PLR", "ENS") ~ "plantation",
                              ORIGINE == "ES" ~ "severe_outbreak"))
env_data$ORIGINE2 <- as.factor(env_data$ORIGINE2)

## PERTURBATION PARTIELLE
env_data <- env_data %>%
  mutate(PERTURB2 = case_when(PERTURB == "BRP" ~ "partial_burn",
                              PERTURB %in% c("CAM","CB","CD","CDL","CE","CJ","CP","DLD","EPC") ~
                                "partial_logging",
                              PERTURB == "EL" ~ "light_outbreak",
                              PERTURB %in% c("CHP", "VEP", "DP") ~ "partial_windfall")) # DP = dépérissement
env_data$PERTURB2 <- as.factor(env_data$PERTURB2)

## AGE
age_data <- age_data %>%
  group_by(ID_PE_MES) %>%
  summarise(age_mean = mean(as.integer(AGE), na.rm = T)) %>%
  tidyr::replace_na(list(age_mean=NA))

env_data <- env_data %>% left_join(age_data, by = "ID_PE_MES")

### SAVE ####

saveRDS(env_data, "data/env_data_may2018.RDS")

