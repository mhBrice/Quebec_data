### Formatting PEP tree data from Quebec ####

### DOMNLOAD DATA ####

# The geopackage containing the PEP data (placette-échantillon permanente) is available online at
# https://www.donneesquebec.ca/recherche/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui

download.file("https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/DONNEES_FOR_ECO_SUD/Placettes_permanentes/PEP_GPKG.zip",
  destfile = "raw_data/PEP.zip"
)
# database downloaded on February, 1rst 2023

unzip("raw_data/PEP.zip", exdir = "raw_data")

# Documentation

download.file("https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/DONNEES_FOR_ECO_SUD/Placettes_permanentes/1-Documentation/DICTIONNAIRE_PLACETTE.xlsx",
  destfile = "raw_data/DICTIONNAIRE_PLACETTE.xlsx"
)

download.file("https://diffusion.mffp.gouv.qc.ca/Diffusion/DonneeGratuite/Foret/DONNEES_FOR_ECO_SUD/Placettes_permanentes/1-Documentation/LISEZ-MOI_pep.pdf",
  destfile = "raw_data/LISEZ-MOI_pep.pdf"
)

# Steps for data cleaning
# remove trees which dhp < 90mm and gaules
# remove some states (25 (intru), 44, 45, 46 (dead recruit), 34, 35, 36 (dead forgotten))
# renumber resurected trees (some recruits were given the IDs from dead trees)
# remove second death measures
# Correct tree ids
# Manage renumbered trees : ETAT==29 and 50,52,54,55,56
# Simplify state to alive, dead, unknown
# Check for realistic growth rate

### PACKAGES ####
library(sf)
library(dplyr)


### READ GDB LAYERS ####

# Check the list of layers in PEP.gpkg
st_layers("raw_data/PEP_GPKG/PEP.gpkg")

# PEP coordinates
pep_xy <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "placette")

# PEP date of measurements
pep_mes <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "placette_mes")

# tree DHP + status
tree_mes <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "dendro_arbres")


# Disturbances

## photo-interpretation (better)
pep_ori <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "pee_ori_sond")

## Field data
pep_pe <- st_read("raw_data/PEP_GPKG/PEP.gpkg", layer = "station_pe")

# Species code
sps_code <- read.csv2("raw_data/ref_spCode.csv")

### DATA CLEANING ####

# Format date

pep_mes <- pep_mes %>%
  mutate(year_measured = as.integer(format(date_sond, format = "%Y")))

### Change species code

tree_mes$sp_code <- sps_code$spCode[match(tree_mes$essence, sps_code$qc_code)]


# Keep only necessary columns
table(pep_mes$statut_mes)

# Remove abandoned, relocalized, destroyed, etc. PEP (statut_mes %in% c(AB, RE, RL, DE, NT, SR))

id_pe_aband <- pep_mes %>%
  filter(!is.na(statut_mes))

pep_mes <- pep_mes %>%
  filter(!(id_pe %in% id_pe_aband$id_pe)) %>%
  select(id_pe, id_pe_mes, year_measured)

pep_xy <- pep_xy %>%
  filter(id_pe %in% pep_mes$id_pe) %>%
  select(id_pe, geom)

tree_mes <- tree_mes %>%
  filter(id_pe %in% pep_mes$id_pe) %>%
  select(-c(in_ess_nc, in_1410:prio_recol, stade_degr, haut_esti:vmb_ha))



length(which(!(pep_mes$id_pe_mes %in% unique(tree_mes$id_pe_mes))))
# 534 PE_MES are missing from tree_data...
missing_ID <- pep_mes %>%
  filter(!(id_pe_mes %in% tree_mes$id_pe_mes))
pert <- pep_ori %>%
  filter(id_pe %in% missing_ID$id_pe)
pert2 <- pert %>%
  group_by(id_pe) %>%
  filter(all(is.na(origine)))
length(unique(pert2$id_pe))

# all, but 27 ID_PE, were highly disturbed hence no tree
# Remove the 27 ID_PE for which we cannot explain the absence of tree

# Merge with plot info

tree_data0 <- pep_mes %>%
  filter(!(id_pe %in% pert2$id_pe)) %>%
  left_join(tree_mes, by = c("id_pe", "id_pe_mes"), multiple = "all")

added_id <- tree_data0 %>% filter(!(id_pe_mes %in% tree_mes$id_pe_mes))

tree_data0$etat[tree_data0$id_pe_mes %in% added_id$id_pe_mes] <- "AllDead"


### REMOVE ALL GAULES & DHP < 90mm ####
# but not dhp = 0 or NA because can be dead trees

tree_data <- tree_data0 %>%
  filter(!etat %in% c("GV", "GM", "GA")) %>%
  filter(dhp > 90 | dhp == 0 | is.na(dhp))

### REMOVE SOME STATES ####
# Remove etat == 25 (intru), 34, 35, 36 (dead forgotten)
rm <- tree_data$id_arbre[tree_data$etat %in% c(25, 34, 35, 36)]
tree_data <- tree_data %>%
  filter(!(id_arbre %in% rm))

### RESURRECTED SPECIES
table(tree_data$etat)
dead_state <- c(14:17, 21:24, 26, 44:46, 54:56, 68:98)
live_state <- c(10, 12, 30, 32, 40, 42, 50, 52, 58)

dead_id <- tree_data$id_arbre[tree_data$etat %in% dead_state]
dead <- tree_data %>%
  filter(id_arbre %in% dead_id) %>%
  group_by(id_arbre) %>%
  arrange(year_measured) %>%
  slice(-(1:min(which(etat %in% dead_state))))
# remove all rows before first dead state by tree id


### correct renumbering mistakes


### RESURRECTION
# then look if there are still living state in dead2
resurrected <- dead %>% subset(etat %in% live_state)
length(unique(resurrected$id_arbre))
# 411 resurrections = mainly renumbering mistakes because disturbances

# Only correct those that are new recruits (caused by disturbances)
# Remove the others
resur_recru <- resurrected %>%
  group_by(id_arbre) %>%
  filter(any(etat == 40))
resur_rm <- resurrected %>%
  filter(!(id_arbre %in% resur_recru$id_arbre))

tree_data <- tree_data %>%
  filter(!(id_arbre %in% resur_rm$id_arbre))

tree_data$no_arbre <- as.numeric(tree_data$no_arbre)
for (id in unique(resur_recru$id_arbre)) { # loop over resurrected trees
  tmp <- subset(tree_data, id_arbre == id)
  tmp_p <- subset(tree_data, id_pe == unique(tmp$id_pe))
  d <- which(tmp$etat %in% dead_state) # lines where dead
  l <- which(tmp$etat %in% live_state) # lines where alive
  if (any(min(d) < l)) { # if alive after death
    l_resur <- l[min(which(l > min(d)))]:nrow(tmp)
    # change tree no_arbre for the last no_arbre + 1 in this plot
    tmp$no_arbre[l_resur] <- (max(tmp_p$no_arbre, na.rm = TRUE) + 1)
    # create unique tree id
    tmp$id_arbre[l_resur] <- paste0(
      tmp$id_pe[l_resur],
      formatC(tmp$no_arbre[l_resur], width = 3, format = "d", flag = "0")
    )
  }
  tree_data$no_arbre[which(tree_data$id_arbre == id)] <- tmp$no_arbre
  tree_data$id_arbre[which(tree_data$id_arbre == id)] <- tmp$id_arbre
}



#### 2 x DEAD - Remove tree once it's dead
dead2 <- dead %>%
  filter(!(id_arbre %in% resurrected$id_arbre))

tree_data <- tree_data %>%
  filter(!(id_arb_mes %in% dead2$id_arb_mes))


### CORRECTIONS OF TREE IDS #####
tree_data1 <- tree_data

### Are all tree_id associated with only one species?

tree_id_verif <- table(tree_data$id_arbre, tree_data$sp_code)
tree_id_verif[tree_id_verif > 0] <- 1
length(which(rowSums(tree_id_verif) > 1)) / nrow(tree_id_verif)
# 6630 ->  0.9% d'erreur

dupli_tree_id <- unique(tree_data$id_arbre)[which(rowSums(tree_id_verif) > 1)]
dupli_plot_id <- subset(tree_data, id_arbre %in% dupli_tree_id)
# NOPE - some tree ids are associated with more than one species;
# most seem likely identification mistakes but some are from a bad renumbering

### take the last species identification (remove NAs)
last_sp_code <- tree_data %>%
  filter(id_arbre %in% dupli_tree_id, !is.na(sp_code)) %>%
  slice_max(order_by = year_measured, by = id_arbre) %>%
  select(id_arbre, sp_code)

for (id in last_sp_code$id_arbre) {
  tree_data$sp_code[which(tree_data$id_arbre == id)] <- last_sp_code$sp_code[last_sp_code$id_arbre == id]
}

### Remove trees with missing info ####

miss_info <- tree_data[with(tree_data, which(is.na(sp_code) & is.na(etat) & is.na(dhp))), ]
table(miss_info$id_pe)
# All 3 id_arbre with missing info in ID_PE == "8709601201"
# Remove id_arbre with missing info
tree_data <- tree_data %>%
  filter(!(id_arbre %in% miss_info$id_arbre))

### Add sp_code to id_arb_mes that have NAs (either dead or 29) ####
na_sp_code <- subset(tree_data, is.na(sp_code))


tree_data <- tree_data %>%
  group_by(id_arbre) %>%
  mutate(sp_code = first(sp_code)) %>%
  ungroup()

tree_data <- tree_data[-with(tree_data, which(is.na(sp_code) & etat != "AllDead")), ]


# remaining NAs are species that were dead the first time they were inventoried
tree_data1 <- tree_data

#### Manage etat == 29  ####
# 29 = Arbre non identifiable ou arbres soudés

# which PEP MES contains etat == 29
pepmes29 <- unique(subset(tree_data, etat == 29)$id_pe_mes)

tree_data29 <- tree_data[which(tree_data$id_pe_mes %in% pepmes29), ]

# which PEP MES contains etat=29 but not etat=5x
pepmes29_5x <- unique(tree_data29[(tree_data29$etat %in% c(50, 52, 54, 55, 56)), ]$id_pe_mes)

tree_data29_no5x <- subset(tree_data29, !(id_pe_mes %in% pepmes29_5x))

# which PEP MES has been disturbed
disturb_pepmes <- subset(pep_ori, complete.cases(origine) | complete.cases(perturb))$id_pe_mes

# 1. if etat==29 & !=5X & there was a disturbance in the PEP MES, change etat to 24
# sometimes only 1 etat=29 in the disturbed PEP MES, but many other dead trees (etat=24,23)
# it makes sense to consider them as 24 as well

tree_29_disturb <- subset(tree_data29_no5x, id_pe_mes %in% disturb_pepmes & etat == 29)

tree_data$etat[tree_data$id_arb_mes %in% tree_29_disturb$id_arb_mes] <- 24

# 2. Add a new state variable
# state == "unknown" for all tree measures with etat==29 in PEP MES that does not contain any etat == 5X and were not disturbed
# in growth model -> remove tree measures with unknown state and if there is only one tree measurement, we'll remove the tree_id completely
# in mortality model -> remove tree measures with unknown state and if there is only one tree measurement, we'll remove the tree_id completely

tree_data$state <- NA

tree_29_nodisturb <- subset(tree_data29_no5x, !(id_pe_mes %in% disturb_pepmes) & etat == 29)

# some PEP MES still have numerous 29 (1 pep mes have more than 10) ... unreported disturbance?
which(table(tree_29_nodisturb$id_pe_mes) > 10)

# it makes sense to keep them and consider them as 24
treeidmes29 <- subset(tree_29_nodisturb, id_pe_mes %in% names(which(table(tree_29_nodisturb$id_pe_mes) > 10)))
tree_data$etat[tree_data$id_arb_mes %in% treeidmes29$id_arb_mes] <- 24

# refresh
tree_29_nodisturb <- tree_29_nodisturb[-which(tree_29_nodisturb$id_arb_mes %in% treeidmes29$id_arb_mes), ]

# Add state == unknown
tree_data$state[which(tree_data$id_arb_mes %in% tree_29_nodisturb$id_arb_mes)] <- "unknown"

# 3. For the rest of ETAT==29,5X assign the state unknown to the tree measure

tree_data$state[which(tree_data$etat == 29)] <- "unknown"
tree_data$state[which(tree_data$etat %in% c(50, 52))] <- "alive"
tree_data$state[which(tree_data$etat %in% c(54, 55, 56))] <- "dead"


#### RECLASSIFY STATES ####

#### etat == 26 -> state == "harvested"
tree_data$state[which(tree_data$etat == 26)] <- "harvested"

#### etat %in% live_state -> state == "alive"
live_state

tree_data$state[tree_data$etat %in% live_state] <- "alive"

#### etat %in% c(14:24, 44:46, 54:56) -> state == "dead"
tree_data$state[tree_data$etat %in% c(14:24, 44:46, 54:56) & is.na(tree_data$state)] <- "dead"

#### etat == na -> state == "unknown"
tree_data$state[is.na(tree_data$etat)] <- "unknown"
# maybe we should remove the id_pe_mes with unknown states from analysis

tree_data$state[tree_data$etat == "AllDead"] <- "dead"

### REALISTIC GROWTH ####

# Add an indicator in DHP_NC for trees which growth doesn't make sense (negative or huge)p=

growth <- tree_data %>%
  filter(state == "alive") %>%
  group_by(id_arbre) %>%
  arrange(year_measured) %>%
  mutate(year1 = lag(year_measured, 1L), year2 = year_measured) %>%
  mutate(dhp1 = lag(dhp, 1L), dhp2 = dhp)
growth <- growth %>%
  mutate(time_interv = year2 - year1) %>%
  mutate(growth = (dhp2 - dhp1) / time_interv) %>%
  filter(!is.na(year1)) %>%
  select(id_pe, id_pe_mes, year1, year2, time_interv, id_arbre, id_arb_mes, sp_code, dhp1, dhp2, dhp_nc, growth)

# Growth distribution
quant_growth <- aggregate(growth ~ sp_code,
  data = growth,
  FUN = function(x) quantile(x, c(0, .1, .5, .9, .95, .99, .999, 1))
)

# We will add an indicator for negative growth and growth over 1 sd over the species 99.9th percentile
quant99_growth <- aggregate(growth ~ sp_code, data = growth, FUN = function(x) quantile(x, .999))
sd_growth <- aggregate(growth ~ sp_code, data = growth, FUN = sd)

max_growth <- cbind.data.frame(sp_code = quant99_growth$sp_code, maxG = quant99_growth$growth + sd_growth$growth)

# Extremely high growth
levels(growth$dhp_nc) <- c(unique(growth$dhp_nc), "extremeG", "negativeG")
for (sp in max_growth$sp_code) {
  growth$dhp_nc[which(growth$sp_code == sp & growth$growth > max_growth$maxG[max_growth$sp_code == sp])] <- "extremeG"
}
# Negative growth
growth$dhp_nc[growth$growth < 0] <- "negativeG"

levels(tree_data$dhp_nc) <- levels(growth$dhp_nc)

tree_data$dhp_nc[tree_data$id_arb_mes %in% subset(growth, dhp_nc == "extremeG")$id_arb_mes] <- "extremeG"
tree_data$dhp_nc[tree_data$id_arb_mes %in% subset(growth, dhp_nc == "negativeG")$id_arb_mes] <- "negativeG"


#### Check if some PE MES were deleted by mistake

pe_mes_deleted <- setdiff(unique(tree_data0$id_pe_mes), unique(tree_data$id_pe_mes))
# 529 PE MES were deleted because empty (no living mature trees)

pe_deleted <- tree_data0 %>%
  filter(id_pe_mes %in% pe_mes_deleted) %>%
  mutate(state = "AllDead") %>%
  distinct(id_pe, id_pe_mes, year_measured, state)


### Add missing PE MES

tree_data <- tree_data %>%
  bind_rows(pe_deleted) %>%
  arrange(id_pe, no_arbre)


setdiff(unique(tree_data0$id_pe_mes), unique(tree_data$id_pe_mes))


#### Join xy ####
tree_data <- left_join(tree_data, pep_xy, by = "id_pe")

growth <- left_join(growth, pep_xy, by = "id_pe") %>% arrange(id_pe)

pep_xy <- pep_xy %>% filter(id_pe %in% unique(tree_data$id_pe))

#### Reshape - site x species matrix ####
library(tidyr)

sp_mat <- tree_data %>%
  filter(state == "alive") %>%
  pivot_wider(
    id_cols = c(id_pe, id_pe_mes, year_measured),
    names_from = sp_code,
    names_sort = TRUE,
    values_from = dhp,
    values_fn = length,
    values_fill = 0
  )


add_rm <- tree_data %>%
  ungroup() %>%
  subset(!(id_pe_mes %in% sp_mat$id_pe_mes)) %>%
  distinct(id_pe, id_pe_mes, year_measured)

sp_mat <- sp_mat %>%
  bind_rows(add_rm) %>%
  arrange(id_pe, id_pe_mes, year_measured)
sp_mat[is.na(sp_mat)] <- 0

# remove NA when computing basal area (to keep all stems of a species that where measured)

sp_BA <- tree_data %>%
  filter(state == "alive") %>%
  pivot_wider(
    id_cols = c(id_pe, id_pe_mes, year_measured),
    names_from = sp_code,
    names_sort = TRUE,
    values_from = dhp,
    values_fn = function(x) sum(pi * (x / (2 * 1000))^2, na.rm = T) * (10000 / 399.7312),
    values_fill = 0
  )


add_rm <- tree_data %>%
  ungroup() %>%
  subset(!(id_pe_mes %in% sp_BA$id_pe_mes)) %>%
  distinct(id_pe, id_pe_mes, year_measured)

sp_BA <- sp_BA %>%
  bind_rows(add_rm) %>%
  arrange(id_pe, id_pe_mes, year_measured)
sp_BA[is.na(sp_BA)] <- 0


#### SAVE ####

saveRDS(tree_data, "data/tree_data_fev2023.RDS")

saveRDS(growth, "data/growth_data_fev2023.RDS")

saveRDS(sp_mat, "data/sp_mat_abun_fev2023.RDS")
saveRDS(sp_BA, "data/sp_mat_ba_fev2023.RDS")

st_write(pep_xy, "data/pep_xy32198_fev2023.gpkg", layer_options = "OVERWRITE = yes")
