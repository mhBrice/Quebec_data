#### Getting non climatic environmental variables from Quebec forest inventory data ####

# Marie-Helene Brice
# June 13th 2018

# The geodatabase containing the PEP tree data (placette-échantillon permanente) is available online at https://www.donneesquebec.ca/recherche/fr/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui

# Environmental variables of interest:
# Soil variables (humus type, humus ph, pierrosity, drainage)
# Disturbances (logging, burning, windfall, insect outbreak)
# Forest age


### PACKAGES ####

require(sf)
require(dplyr)
require(tidyr)
require(data.table)



## DATA ####

### Formatted species data with xy coordinates

tree_data <- readRDS("data/tree_data_fev2023.RDS")

# check the list of layers in the gpkg

st_layers("raw_data/PEP_GPKG/PEP.gpkg")

# Layer containing soil variables (humus, texture, ph)
pep_sol <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "station_sol")

# Layer containing disturbance variables and drainage
## Field data
pep_pe <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "station_pe")

## photo-interpretation (better for disturbances)
pep_ori <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "pee_ori_sond")

# Layer containing age of selected trees in all PE MES
pep_arb <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "dendro_arbres_etudes")

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
  select(id_pe, id_pe_mes, typehumus, epmatorg, ph_humus, ph_horizb, pourcpierr)

# DISTURBANCE VARIABLES
# field data
pep_pe <- pep_pe %>%
  select(id_pe, id_pe_mes, origine, perturb, cl_age,
         cl_drai, altitude, versant, pc_pent, exposition, dep_sur, nature_dep)
# photo-interpretation data
pep_ori <- pep_ori %>%
  select(id_pe, id_pe_mes, origine, an_origine, perturb, an_perturb, cl_age,
         cl_drai, dep_sur) %>%
  rename_with(~paste0(., "_ori"), origine:dep_sur)


# DISTURBANCE VARIABLES
pep_arb <- pep_arb %>%
  select(id_pe, id_pe_mes, id_arbre, id_arb_mes, age)

# XY

pep_xy <- st_read("data/pep_xy32198_fev2023.gpkg")

### JOIN VARIBALES ####

env_data <- tree_data %>%
  ungroup() %>%
  select(id_pe, id_pe_mes, year_measured) %>%
  distinct() %>%
  left_join(pep_sol, by = c("id_pe", "id_pe_mes")) %>%
  left_join(pep_pe, by = c("id_pe", "id_pe_mes")) %>%
  left_join(pep_ori, by = c("id_pe", "id_pe_mes"))

age_data <- tree_data %>%
  select(id_pe, id_pe_mes, id_arbre, id_arb_mes) %>%
  left_join(pep_arb, by = c("id_pe", "id_pe_mes", "id_arbre", "id_arb_mes"))

### RECLASSIFY VARIABLES ####

table(pep_pe$origine) # 4096 plots with coupe total
table(pep_ori$origine_ori) # 4341 plots with coupe total
# ORIGINE => perturbations naturelles et interventions anthropiques d’origine 
# qui ont éliminé plus de 75 % de la surface terrière du peuplement précédent

## DRAINAGE
env_data <- env_data %>%
  mutate(cl_drai2 = case_when(cl_drai %in% 0 ~ "excessif",
                              cl_drai %in% c(10:14) ~ "rapide",
                              cl_drai == 16 ~ "complexe",
                              cl_drai %in% c(20:24) ~ "bon",
                              cl_drai %in% c(30:34) ~ "modere",
                              cl_drai %in% c(40:44) ~ "imparfait",
                              cl_drai %in% c(50:54) ~ "mauvais",
                              cl_drai %in% c(60:64) ~ "tres_mauvais")) %>%
  mutate(cl_drai_ori2 = case_when(cl_drai_ori %in% c(0, "00") ~ "excessif",
                                  cl_drai_ori %in% c(10:14) ~ "rapide",
                                  cl_drai_ori == 16 ~ "complexe",
                                  cl_drai_ori %in% c(20:24) ~ "bon",
                                  cl_drai_ori %in% c(30:34) ~ "modere",
                                  cl_drai_ori %in% c(40:44) ~ "imparfait",
                                  cl_drai_ori %in% c(50:54) ~ "mauvais",
                                  cl_drai_ori %in% c(60:64) ~ "tres_mauvais"))
env_data$cl_drai2 <- as.factor(env_data$cl_drai2)
env_data$cl_drai_ori2 <- as.factor(env_data$cl_drai_ori2)


## PERTURBATION D'ORIGINE
logging <- c(
  "CBA", "CBT", "CDV", "CEF",
  "CPH", "CPR", "CPT", "CRB", "CRS",
  "CS", "CT", "ETR", "RPS"
)
env_data <- env_data %>%
  mutate(origine_ori2 = case_when(origine_ori == "BR" ~ "burn",
                                  origine_ori %in% logging ~ "logging",
                                  origine_ori %in% c("CHT","DT") ~ "windfall", # DT = dépérissement
                                  origine_ori %in% c("P", "PLN", "PLR", "PRR", "ENS", "REA") ~ "plantation",
                                  origine_ori == "ES" ~ "severe_outbreak",
                                  origine_ori == "FR" ~ "wasteland"))
env_data$origine_ori2 <- as.factor(env_data$origine_ori2)

## PERTURBATION PARTIELLE
partial_logging <- c(
  "CA", "CAM", "CB", "CD", "CDL", "CE", "CEA", "CIP",
  "CJ", "CJG", "CJP", "CJT",
  "CP", "CPC", "CPF", "CPI", "CPM", "CPS", "CPX", "CTR",
  "DEG", "DLD", "DRM", "EC", "ECE", "EPC", "ESI", "PCP"
)
env_data <- env_data %>%
  mutate(perturb_ori2 = case_when(perturb_ori == "BRP" ~ "partial_burn",
                                  perturb_ori %in% partial_logging ~ "partial_logging",
                                perturb_ori == "EL" ~ "light_outbreak",
                                perturb_ori %in% c("CHP", "VEP", "DP") ~ "partial_windfall", # DP = dépérissement
                                perturb_ori %in% c("ENR", "RR",  "RRG") ~ "partial_plantation")) 
env_data$perturb_ori2 <- as.factor(env_data$perturb_ori2)

## AGE
age_data <- age_data %>%
  ungroup() %>%
  group_by(id_pe_mes) %>%
  summarise(age_mean = mean(as.integer(age), na.rm = TRUE)) %>%
  replace_na(list(age_mean = NA))

env_data <- env_data %>% 
  left_join(age_data, by = "id_pe_mes")

# Order and select last soil measures

env_data <- env_data %>% 
  select(id_pe:year_measured, 
         origine_ori:an_perturb_ori, origine_ori2, perturb_ori2,
         origine:perturb, 
         age_mean, cl_age,
         typehumus:pourcpierr, 
         cl_drai, cl_drai2, cl_drai_ori2, 
         altitude:nature_dep) 

env_data <- env_data %>% 
  group_by(id_pe) %>%
  arrange(year_measured, .by_group = TRUE) %>% 
  mutate_at(vars(typehumus:nature_dep), last) %>%
  ungroup()

### SAVE ####

saveRDS(env_data, "data/env_data_fev2023.RDS")

