### Formatting PEP tree data from Quebec ####

# The geodatabase containing the PEP tree data (placette-échantillon permanente) is available online at https://www.donneesquebec.ca/recherche/fr/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui

# download.file("ftp://transfert.mffp.gouv.qc.ca/Public/Diffusion/DonneeGratuite/Foret/DONNEES_FOR_ECO_SUD/Placettes_permanentes/PEP_GDB.zip", destfile = "raw_data/PEP.zip")

# unzip("raw_data/PEP.zip", exdir = "raw_data")
# database downloaded on May the 24th 2018

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
tree_mes <- st_read("raw_data/PEP_GDB/PEP.gdb", layer = "DENDRO_ARBRES")

which(duplicated(plot_xy$ID_PE))

# Disturbances
pep_pe <- st_read("raw_data/PEP_GDB/PEP.gdb", layer = "STATION_PE")

# Species code
sps_code <- read.csv2("raw_data/ref_spCode.csv")

### DATA CLEANING ####

# Keep only necessary columns 
# Remove abandonned plots (STATUT_MES %in% c(AB, RE, RL, DE, NT, SR))

table(plot_mes$STATUT_MES)
ID_PE_aband <- plot_mes %>% filter(!is.na(STATUT_MES))

plot_mes <- plot_mes %>%
  filter(!(ID_PE %in% ID_PE_aband$ID_PE)) %>%
  mutate(year_measured = as.integer(format(DATE_SOND, format="%Y"))) %>%
  dplyr::select(ID_PE, ID_PE_MES, year_measured) 

plot_xy <- plot_xy %>%
  filter(ID_PE %in% plot_mes1$ID_PE) %>%
  dplyr::select(ID_PE, SHAPE) 

tree_mes <- tree_mes %>%
  filter(ID_PE %in% plot_mes1$ID_PE) %>%
  dplyr::select(-c(NO_MES, IN_ESS_NC, IN_1410, DEFOL_MIN, 
                   CL_QUAL:PRIO_RECOL, STADE_DEGR:VMB_HA))
### Change species code

tree_mes$sp_code <- sps_code$spCode[match(tree_mes$ESSENCE, sps_code$qc_code)]


length(which(!(plot_mes$ID_PE_MES %in% unique(tree_mes$ID_PE_MES))))
# 534 PE_MES are missing from tree_data...
missing_ID <- plot_mes %>% filter(!(ID_PE_MES %in% tree_mes$ID_PE_MES))
pert <- pep_pe %>% filter(ID_PE %in% missing_ID$ID_PE)
pert2 <- pert %>% group_by(ID_PE) %>% filter(all(is.na(ORIGINE)))
length(unique(pert2$ID_PE))
# all, but 11 ID_PE, were highly disturbed hence no tree
# Remove the 11 ID_PE for which we cannot explain the absence of tree

# Merge with plot info

tree_data0 <- plot_mes %>% filter(!(ID_PE %in% pert2$ID_PE)) %>%
  left_join(tree_mes, by = c("ID_PE", "ID_PE_MES"))

added_ID <- tree_data0 %>% filter(!(ID_PE_MES %in% tree_mes$ID_PE_MES))

levels(tree_data0$ETAT) <- c(levels(tree_data0$ETAT),"AllDead")
tree_data0$ETAT[tree_data0$ID_PE_MES %in% added_ID$ID_PE_MES] <- "AllDead"

# Recode plot id, tree id
tree_data <- tree_data0 %>%
  mutate(plot_id = as.integer(as.factor(ID_PE))) %>%
  mutate(tree_id = as.integer(ID_ARBRE)) %>%
  mutate(NO_ARBRE = as.integer(as.character(NO_ARBRE))) %>%
  arrange(plot_id, tree_id) 

### REMOVE ALL GAULES & DHP < 90mm ####
#but not dhp = 0 or NA because can be dead trees

tree_data <- tree_data %>% filter(!ETAT %in% c("GV","GM","GA")) %>%
  filter(DHP > 90 | DHP == 0 | is.na(DHP))
  
### REMOVE SOME STATES ####
# Remove etat == 25 (intru), 44, 45, 46 (dead recruit), 34, 35, 36 (dead forgotten)
rm <- tree_data$tree_id[tree_data$ETAT %in% c(25,44,45,46,34,35,36)]
tree_data <- tree_data %>% 
  filter(!(tree_id %in% rm)) 

### RESURECTED SPECIES

dead_state <- c(14,16,23,24,26,44,46,54,55,56)
live_state <- c(10,12,30,32,40,42,50,52)

dead_id <- tree_data$tree_id[tree_data$ETAT %in% dead_state]
dead <- tree_data %>% 
  filter(tree_id %in% dead_id) %>%
  group_by(tree_id) %>%
  arrange(year_measured) %>%
  slice(-(1:min(which(ETAT %in% dead_state)))) # remove all rows before dead state by tree id


### correct renumbering mistakes


### RESURECTION 
# then look if there are still living state in dead2
resurected <- dead %>% subset(ETAT %in% live_state)
length(unique(resurected$ID_ARBRE))
#384 resurections = mainly renumbering mistakes because disturbances

# Only correct those that are new recruits (caused by disturbances)
# Remove the others
resur_recru <- resurected %>% group_by(ID_ARBRE) %>% filter(any(ETAT==40))
resur_rm <- resurected %>% filter(!(ID_ARBRE %in% resur_recru$ID_ARBRE))
# resur_many <- resurected %>% filter(ID_PE_MES %in% names(which(table(resurected$ID_PE_MES)>3)))
# match(resur_many$ID_PE, resur_recru$ID_PE)
# match(resur_recru$ID_PE, resur_many$ID_PE)

tree_data <- tree_data %>% filter(!(ID_ARBRE %in% resur_rm$ID_ARBRE))

for(id in unique(resur_recru$tree_id)) { # loop over resurected trees
  tmp <- subset(tree_data, tree_id==id)
  tmp_p <- subset(tree_data, plot_id==unique(tmp$plot_id))
  d <- which(tmp$ETAT %in% dead_state) # lines where dead
  l <- which(tmp$ETAT %in% live_state) # lines where alive
  if(any(min(d)<l)) { # if alive after death
    # change tree no for the last no + 1 in this plot
    tmp$NO_ARBRE[l[min(which(l>min(d)))]:nrow(tmp)] <- (max(tmp_p$NO_ARBRE, na.rm = TRUE)+1) 
  }
  tree_data$NO_ARBRE[which(tree_data$tree_id==id)] <- tmp$NO_ARBRE
}


#### 2 x DEAD - Remove tree once it's dead
dead2 <- dead %>% filter(!(tree_id %in% resurected$tree_id))

tree_data <- tree_data %>% filter(!(ID_ARB_MES %in% dead2$ID_ARB_MES))

#### Create unique tree ids again
fac <- with(tree_data, paste(tree_data$plot_id, tree_data$NO_ARBRE))
tree_data <- within(tree_data, tree_id <- match(fac, unique(fac)))


### CORRECTIONS OF TREE IDS #####


### Are all tree_id associated with only one species? 

tree_id_verif <- table(tree_data$tree_id, tree_data$sp_code)
tree_id_verif[tree_id_verif>0] <-1
length(which(rowSums(tree_id_verif)>1))/nrow(tree_id_verif)
# 6318 ->  0.9% d'erreur

dupli_tree_id <- unique(tree_data$tree_id)[which(rowSums(tree_id_verif)>1)]
dupli_plot_id <- subset(tree_data, tree_id %in% dupli_tree_id) 
### NOPE - some tree ids are associated with more than one species; most seem like identification mistakes but some are from a bad renumbering

### take the last species identification (remove NAs first)
last_sp_code <- tree_data[tree_data$tree_id %in% dupli_tree_id,] %>%
  group_by(tree_id) %>% 
  filter(!is.na(sp_code)) %>%
  top_n(1, year_measured) %>%
  select(tree_id, sp_code)

for(id in last_sp_code$tree_id) {
  tree_data$sp_code[which(tree_data$tree_id == id)] <- last_sp_code$sp_code[last_sp_code$tree_id==id]
}

### Remove trees with missing info #### 
# is.na(sp_code) & is.na(ETAT) & is.na(DHP)

miss_info <- tree_data[with(tree_data, which(is.na(sp_code) & is.na(ETAT) & is.na(DHP))),]
table(miss_info$ID_PE)
# Remove ID_PE == 7209501402 -> problem with the data
tree_data <- tree_data %>% filter(!(ID_PE %in% "7209501402"))
tree_data <- tree_data %>% filter(!(tree_id %in% miss_info$tree_id))

tree_data2 = tree_data
### Add sp_code to ID_ARB_MES that have NAs (either dead or 29) ####
na_sp_code <- subset(tree_data, is.na(sp_code))

new_sp_code <- subset(tree_data, tree_id %in% na_sp_code$tree_id) %>% 
  group_by(tree_id) %>% slice(1L) #%>% filter(!is.na(sp_code))

tree_data <- tree_data %>% group_by(tree_id) %>%
  dplyr::mutate(sp_code2 = dplyr::first(sp_code)) 

tree_data <- tree_data[-with(tree_data, which(is.na(sp_code2) & ETAT != "AllDead")),]

tree_data2=tree_data
# remaining NAs are species that were dead the first time they were inventoried


#### Manage ETAT == 29  ####
#(non-identifiable or forked trees)

# which PEP MES contains ETAT=29
pepmes29 <- unique(subset(tree_data, ETAT == 29)$ID_PE_MES)

tree_data29 <- tree_data[which(tree_data$ID_PE_MES %in% pepmes29),]

# which PEP MES contains ETAT=29 but not ETAT=5x
pepmes29_5x <- unique(tree_data29[(tree_data29$ETAT %in% c(50,52,54,55,56)),]$ID_PE_MES)

tree_data29_no5x <- subset(tree_data29, !(ID_PE_MES %in% pepmes29_5x))

# which PEP MES has been disturbed
disturb_pepmes <- subset(pep_pe, complete.cases(ORIGINE) | complete.cases(PERTURB))$ID_PE_MES

# 1. if ETAT=29 and !=5X and there was a disturbance in the PEP MES change ETAT to 24
# sometimes only 1 ETAT=29 in the disturbed PEP MES, but many other dead trees (ETAT=24,23)
# it makes sense to consider them as 24 as well

tree_29_disturb <- subset(tree_data29_no5x, ID_PE_MES %in% disturb_pepmes & ETAT == 29)

tree_data$ETAT[which(tree_data$ID_ARB_MES %in% tree_29_disturb$ID_ARB_MES)] <- 24
  
# 2. Add a new state variable and put as "unknown" all tree measures with ETAT==29 from PEP MES that does not contain any ETAT == 5X and were not disturbed
# in growth model -> we'll remove the tree measure with unknown state and if there is only one tree measurement, we'll remove the tree_id completely
# in mortality model -> we'll remove the tree measure with unknown state and if there is only one tree measurement, we'll remove the tree_id completely... not sure yet. 
tree_data$state <- NA

tree_29_nodisturb <- subset(tree_data29_no5x, !(ID_PE_MES %in% disturb_pepmes) & ETAT == 29)

# some PEP MES still have numerous 29 (1 pep mes have more than 10) ... unreported disturbance? 
which(table(tree_29_nodisturb$ID_PE_MES)>10)

# it makes sense to keep them and consider them as 24
treeidmes29 <- subset(tree_29_nodisturb, ID_PE_MES %in% names(which(table(tree_29_nodisturb$ID_PE_MES)>10)))
tree_data$ETAT[tree_data$ID_ARB_MES %in% treeidmes29$ID_ARB_MES] <- 24

# refresh
tree_29_nodisturb <- tree_29_nodisturb[-which(tree_29_nodisturb$ID_ARB_MES %in% treeidmes29$ID_ARB_MES),]

# Add state == unknown
tree_data$state[which(tree_data$ID_ARB_MES %in% tree_29_nodisturb$ID_ARB_MES)] <- "unknown"

# if ETAT==29 and ETAT==54,55,56 (renumbered and dead) in the plot -> try to match
# tree29 <- tree_data[which(tree_data$ETAT==29),]
# tree54 <- tree_data[which(tree_data$ETAT %in% c(54,55,56)),]
# 
# tree2954 <- subset(tree_data, ID_PE_MES %in% tree29$ID_PE_MES & ID_PE_MES %in% tree54$ID_PE_MES)


# for(p in unique(tree2954$ID_PE_MES)) {
#   tmp <- subset(tree2954, ID_PE_MES==p)
#   
#   tmp29 <- tmp[tmp$ETAT == 29,]
#   tmp5x <- tmp[tmp$ETAT %in% c(54,55,56),]
#   
#   species <- intersect(unique(tmp5x$sp_code),unique(tmp29$sp_code))
#   
#   # if no species in common
#   if(length(species)==0) { tmp$state[tmp$ID_ARB_MES %in% c(tmp_sp29$ID_ARB_MES,tmp_sp5x$ID_ARB_MES)] <- "unknown"
#   } else {
#     for(sp in species) {
#       tmp_sp29 <- tmp29[tmp29$sp_code==sp,]
#       tmp_sp5x <- tmp5x[tmp5x$sp_code==sp,]
#       
#       l29 <- length(tmp_sp29$tree_id)
#       l5x <- length(tmp_sp5x$tree_id)
#       n <- min(l29, l5x)
#       
#       # match id
#       for(i in 1:n) { 
#         tree_data$tree_id[which(tree_data$tree_id %in% tmp_sp5x$tree_id[i])] <- tmp_sp29$tree_id[i]
#         tree_data <- tree_data[-which(tree_data$ID_ARB_MES %in% tmp_sp29$ID_ARB_MES[i]),]  
#         # DHP non comparable
#         tree_data$DHP_NC[which(tree_data$ID_ARB_MES %in% tmp_sp5x$ID_ARB_MES[i])] <- "NC"
#         }
#       
#       # unknown for the rest
#       if(n < l29) { tree_data$state[which(tree_data$ID_ARB_MES %in% tmp_sp29$ID_ARB_MES[(n+1):l29])] <- "unknown" }
#       if(n < l5x) { tree_data$state[which(tree_data$ID_ARB_MES %in% tmp_sp5x$ID_ARB_MES[(n+1):l5x])] <- "unknown" }
#     }
#   }
# }


# 3. For the rest of ETAT==29,5X assign the state unknown to the tree measure

tree_data$state[which(tree_data$ETAT == 29)] <- "unknown"
tree_data$state[which(tree_data$ETAT %in% c(50,52))] <- "alive"
tree_data$state[which(tree_data$ETAT %in% c(54,55,56))] <- "dead"




#### RECLASSIFY STATES ####

#### ETAT == 26 -> state == "harvested"
tree_data$state[which(tree_data$ETAT == 26)] <- "harvested"

#### ETAT == 10,12,30,32,40,42 -> state == "alive"

tree_data$state[which(tree_data$ETAT %in% c(10,12,30,32,40,42))] <- "alive"

#### ETAT == 14,15,16,23,24,54,55,56 -> state == "dead"
tree_data$state[which(tree_data$ETAT %in% c(14,15,16,23,24,54,55,56) & is.na(tree_data$state))] <- "dead"

#### ETAT == NA -> state == "unknown"
tree_data$state[which(is.na(tree_data$ETAT))] <- "unknown" 
# maybe we should remove the ID_PE_MES with unknown states from analysis

tree_data$state[which(tree_data$ETAT == "AllDead")] <- "dead" 

#### ETAT == 23,24 -> state == "mechanical death"
#tree_data$state[which(tree_data$ETAT %in% c(23,24))] <- "mechanical"

#### ETAT == 14,16,54,56 -> state == "stress and biotic death death"
#tree_data$state[which(tree_data$ETAT %in% c(14,15,16,54,55,56))] <- "stress"

### REALISTIC GROWTH ####

# Add an indicator in DHP_NC for trees which growth doesn't make sense (negative or huge)p= 

growth <- tree_data %>% 
  filter(state == "alive") %>%
  group_by(tree_id) %>%
  arrange(year_measured) %>%
  mutate(year1 = lag(year_measured, 1L), year2 = year_measured) %>%
  mutate(DHP1 = lag(DHP, 1L), DHP2 = DHP) %>%
  mutate(time_interv = year2 - year1) %>%
  mutate(year_measured = NULL) %>%
  mutate(growth = (DHP2 - DHP1)/time_interv) %>%
  filter(!is.na(year1)) %>%
  select(ID_PE,ID_PE_MES, plot_id, year1, year2, time_interv, tree_id, ID_ARB_MES, sp_code, DHP1, DHP2, DHP_NC, growth)

# Growth distribution
quant_growth <- aggregate(growth ~ sp_code, data = growth, 
                          FUN = function(x) quantile(x, c(0, .1, .5, .9, .95, .99, .999, 1)))

# We will add an indicator for negative growth and growth over 1 sd over the species 99.9th percentile
quant99_growth <- aggregate(growth ~ sp_code, data = growth, FUN = function(x) quantile(x, .999))
sd_growth <- aggregate(growth ~ sp_code, data = growth, FUN = sd)

max_growth <- cbind.data.frame(sp_code = quant99_growth$sp_code, maxG = quant99_growth$growth + sd_growth$growth)

# Extremely high growth
levels(growth$DHP_NC) <-  c(levels(growth$DHP_NC), "extremeG", "negativeG")
for(sp in max_growth$sp_code) {
  growth$DHP_NC[which(growth$sp_code == sp & growth$growth > max_growth$maxG[max_growth$sp_code==sp])] <- "extremeG"
}
# Negative growth
growth$DHP_NC[growth$growth < 0] <- "negativeG"

levels(tree_data$DHP_NC) <-  levels(growth$DHP_NC)

tree_data$DHP_NC[tree_data$ID_ARB_MES %in% subset(growth, DHP_NC=="extremeG")$ID_ARB_MES] <- "extremeG"
tree_data$DHP_NC[tree_data$ID_ARB_MES %in% subset(growth, DHP_NC=="negativeG")$ID_ARB_MES] <- "negativeG"


#### Check if some PE MES were deleted by mistake

plot_mes_deleted <- setdiff(unique(tree_data0$ID_PE_MES), unique(tree_data$ID_PE_MES))
# 545 PE MES were deleted because empty (no living mature trees)
# except ID_PE == 7209501402 -> problem with the data

plot_mes_deleted <- plot_mes_deleted[-grep("7209501402", plot_mes_deleted)]
     
plot_deleted <- tree_data0 %>% filter(ID_PE_MES %in% plot_mes_deleted) %>%
  mutate(state = "AllDead") %>%
  distinct(ID_PE, ID_PE_MES, year_measured, state)


### Add missing PE MES

tree_data <- tree_data %>% 
  bind_rows(plot_deleted) %>%
  arrange(ID_PE, NO_ARBRE) %>% 
  group_by(ID_PE) %>%
  dplyr::mutate(plot_id = dplyr::first(plot_id))


setdiff(unique(tree_data0$ID_PE_MES), unique(tree_data$ID_PE_MES)) 


#### Join xy ####

tree_data <- left_join(tree_data, plot_xy, by = "ID_PE")

growth <- left_join(growth, plot_xy, by = "ID_PE") %>% arrange(plot_id)

plot_xy <- right_join(plot_xy, unique(tree_data[,c("ID_PE", "plot_id")]), by = "ID_PE")
  
#### Reshape - site x species matrix ####

sp_mat <- reshape2::dcast(tree_data, ID_PE + ID_PE_MES + plot_id + year_measured ~ sp_code, 
               subset = plyr::.(state == "alive"), 
               fun.aggregate = length,
               fill = 0, 
               value.var = "DHP")

add_rm <- subset(tree_data, !(ID_PE_MES %in% sp_mat$ID_PE_MES)) %>% 
  distinct(ID_PE, ID_PE_MES, plot_id, year_measured)

sp_mat <- sp_mat %>% bind_rows(add_rm) %>%
  arrange(ID_PE, ID_PE_MES, year_measured)
sp_mat[is.na(sp_mat)] <- 0

# remove NA when computing basal area (to keep all stems of a species that where measured)
sp_BA <- reshape2::dcast(tree_data, ID_PE + ID_PE_MES + plot_id + year_measured ~ sp_code, 
              fun.aggregate = function(x) sum(pi*(x/(2 * 1000))^2, na.rm = T)*(10000/399.7312),
              subset = plyr::.(state == "alive"), 
              fill = 0, 
              value.var = "DHP")

add_rm <- subset(tree_data, !(ID_PE_MES %in% sp_BA$ID_PE_MES)) %>% 
  distinct(ID_PE, ID_PE_MES, plot_id, year_measured)

sp_BA <- sp_BA %>% bind_rows(add_rm) %>%
  arrange(ID_PE, ID_PE_MES, year_measured)
sp_BA[is.na(sp_BA)] <- 0

# tree_data$BA <- pi * (tree_data$DHP/(2 * 1000))^2
# sp_BA2 <-dcast(tree_data, plot_id + year_measured ~ sp_code, 
#               fun.aggregate = function(x) sum(x)*(10000/399.7312),
#               subset = .(state == "alive"), 
#               fill = 0,
#               value.var = "BA")

#### SAVE ####

saveRDS(tree_data, "data/tree_data_nov2019.RDS")

saveRDS(growth, "data/growth_data_nov2019.RDS")

saveRDS(sp_mat, "data/sp_mat_abun_nov2019.RDS")
saveRDS(sp_BA, "data/sp_mat_ba_nov2019.RDS")

st_write(plot_xy, "data/plot_xy32198_nov2019.gpkg", layer_options = "OVERWRITE=yes")

