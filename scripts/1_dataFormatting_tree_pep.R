### Formatting PEP tree data from Quebec ####

# The geodatabase containing the PEP tree data (placette-échantillon permanente) is available online at https://www.donneesquebec.ca/recherche/fr/dataset/placettes-echantillons-permanentes-1970-a-aujourd-hui

# download.file("ftp://ftp.mffp.gouv.qc.ca/Public/DGIF_Diffusion/Foret/DONNEES_FOR_ECO_SUD/Placettes_permanentes/PEP_GDB.zip", destfile = "raw_data/PEP.zip")
# unzip("raw_data/PEP.zip", exdir = path_data)
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
ogrListLayers("raw_data/Pep/PEP.gdb")

# Plot coordinates
plot_xy <- st_read("raw_data/Pep/PEP.gdb", layer = "PLACETTE")

# Plot date of measurements
plot_mes1 <- st_read("raw_data/Pep/PEP.gdb", layer = "PLACETTE_MES")

# DHP + cote DENDRO_ARBRES
tree_mes <- st_read("raw_data/Pep/PEP.gdb", layer = "DENDRO_ARBRES")

which(duplicated(plot_xy$ID_PE))

# Disturbances
pep_pe <- st_read("raw_data/Pep/PEP.gdb", layer = "STATION_PE")

# Species code
sps_code <- read.csv2("raw_data/ref_spCode.csv")

### DATA CLEANING ####

# Keep only necessary columns

plot_xy <- plot_xy %>%
  dplyr::select(ID_PE, SHAPE) 

plot_mes <- plot_mes %>%
  mutate(year_measured = as.integer(format(DATE_SOND, format="%Y"))) %>%
  dplyr::select(ID_PE, ID_PE_MES, year_measured) 

tree_mes <- tree_mes %>%
  dplyr::select(-c(NO_MES, IN_ESS_NC, IN_1410, DEFOL_MIN, CL_QUAL:PRIO_RECOL, STADE_DEGR:VMB_HA)) 


length(which(!(plot_mes$ID_PE_MES %in% unique(tree_mes$ID_PE_MES))))
# 775 PE_MES are missing from tree_data... most of which were abandonned or relocalised


### Change species code

tree_mes$sp_code <- sps_code$spCode[match(tree_mes$ESSENCE, sps_code$qc_code)]


# Merge with plot info

tree_data <- plot_mes %>% 
  #full_join(plot_xy, by = "ID_PE") %>%
  right_join(tree_mes, by = c("ID_PE", "ID_PE_MES"))

# Recode plot id, tree id
tree_data <- tree_data %>%
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

tree_data <- tree_data %>% 
  group_by(tree_id) %>% 
  filter(!any(ETAT %in% c(25,44,45,46,34,35,36))) 

### RESURECTED SPECIES

dead_state <- c(14,16,23,24,26,44,46,54,55,56)
live_state <- c(10,12,30,32,40,42,50,52)

dead <- tree_data %>% 
  group_by(tree_id) %>%
  filter(any(ETAT %in% dead_state)) %>%
  arrange(year_measured) %>%
  slice(-(1:min(which(ETAT %in% dead_state)))) # remove all rows before dead state by tree id


### correct renumbering mistakes


### RESURECTION 
# then look if there are still living state in dead2
resurected <- dead %>% subset(ETAT %in% live_state, tree_id)


for(id in unique(resurected$tree_id)) { # loop over resurected trees
  tmp <- subset(tree_data, tree_id==id)
  tmp_p <- subset(tree_data, plot_id==unique(tmp$plot_id))
  d <- which(tmp$ETAT %in% dead_state) # lines where dead
  l <- which(tmp$ETAT %in% live_state) # lines where alive
  if(any(min(d)<l)) { # if alive after death
    # change tree no for the last no + 1 in this plot
    tmp$NO_ARBRE[l[min(which(l>min(d)))]:nrow(tmp)] <- (max(tmp_p$NO_ARBRE)+1) 
  }
  tree_data$NO_ARBRE[tree_data$tree_id==id] <- tmp$NO_ARBRE

}

#### Create unique tree ids again
fac <- with(tree_data, paste(tree_data$plot_id, tree_data$NO_ARBRE))
tree_data <- within(tree_data, tree_id <- match(fac, unique(fac)))

#### 2 x DEAD

dead <- tree_data %>% 
  group_by(tree_id) %>%
  filter(any(ETAT %in% dead_state)) %>%
  arrange(year_measured) %>%
  slice(-(1:min(which(ETAT %in% dead_state)))) # remove all rows before dead state by tree id

dead2x_id <- which(tree_data$ID_ARB_MES %in% dead$ID_ARB_MES)

tree_data <- (tree_data[-dead2x_id,])


### CORRECTIONS OF TREE IDS #####

### Are all tree_id associated with only one species? 

tree_id_verif <- tree_data %>%
  dplyr::select(tree_id, sp_code) %>%
  filter(!is.na(sp_code)) %>%
  group_by(tree_id) %>%
  summarise(freq = length(unique(sp_code)))

length(which(tree_id_verif$freq > 1))
# 6417 -> 6417/695146 = 0.9% d'erreur

dupli_tree_id <- subset(tree_id_verif, freq > 1)$tree_id 
dupli_plot_id <- subset(tree_data, tree_id %in% dupli_tree_id) 
### NOPE - some tree ids are associated with more than one species; most seem like identification mistakes but some are from a bad renumbering

### take the last species identification (remove NAs first)
last_sp_code <- tree_data[tree_data$tree_id %in% dupli_tree_id,] %>%
  group_by(tree_id) %>% 
  filter(!is.na(sp_code)) %>%
  top_n(1, year_measured) %>%
  select(sp_code)

for(id in last_sp_code$tree_id) {
  tree_data$sp_code[which(tree_data$tree_id == id)] <- last_sp_code$sp_code[last_sp_code$tree_id==id]
}

### Remove trees with missing info #### 
# is.na(sp_code) & is.na(ETAT) & is.na(DHP)

tree_data <- tree_data[-with(tree_data, which(is.na(sp_code) & is.na(ETAT) & is.na(DHP))),]

### Add sp_code to ID_ARB_MES that have NAs (either dead or 29) ####
na_sp_code <- subset(tree_data, is.na(sp_code))

new_sp_code <- subset(tree_data, tree_id %in% na_sp_code$tree_id) %>% 
  group_by(tree_id) %>% slice(1L)


for(id in new_sp_code$tree_id) {
  tree_data$sp_code[tree_data$tree_id == id & is.na(tree_data$sp_code)] <- new_sp_code$sp_code[new_sp_code$tree_id == id]
} 

# remaining NAs are species that were dead the first time they were inventoried


#### Manage ETAT == 29  ####
#(renumbered trees because non-identifiable or forked)

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
tree29 <- tree_data[which(tree_data$ETAT==29),]
tree54 <- tree_data[which(tree_data$ETAT %in% c(54,55,56)),]

tree2954 <- subset(tree_data, ID_PE_MES %in% tree29$ID_PE_MES & ID_PE_MES %in% tree54$ID_PE_MES)

for(p in unique(tree2954$ID_PE_MES)) {
  tmp <- subset(tree2954, ID_PE_MES==p)
  
  tmp29 <- tmp[tmp$ETAT == 29,]
  tmp5x <- tmp[tmp$ETAT %in% c(54,55,56),]
  
  species <- intersect(unique(tmp5x$sp_code),unique(tmp29$sp_code))
  
  # if no species in common
  if(length(species)==0) { tmp$state[tmp$ID_ARB_MES %in% c(tmp_sp29$ID_ARB_MES,tmp_sp5x$ID_ARB_MES)] <- "unknown"
  } else {
    for(sp in species) {
      tmp_sp29 <- tmp29[tmp29$sp_code==sp,]
      tmp_sp5x <- tmp5x[tmp5x$sp_code==sp,]
      
      l29 <- length(tmp_sp29$tree_id)
      l5x <- length(tmp_sp5x$tree_id)
      n <- min(l29, l5x)
      
      # match id
      for(i in 1:n) { 
        tree_data$tree_id[which(tree_data$tree_id %in% tmp_sp5x$tree_id[i])] <- tmp_sp29$tree_id[i]
        tree_data <- tree_data[-which(tree_data$ID_ARB_MES %in% tmp_sp29$ID_ARB_MES[i]),]  
        # DHP non comparable
        tree_data$DHP_NC[which(tree_data$ID_ARB_MES %in% tmp_sp5x$ID_ARB_MES[i])] <- "NC"
        }
      
      # unknown for the rest
      if(n < l29) { tree_data$state[which(tree_data$ID_ARB_MES %in% tmp_sp29$ID_ARB_MES[(n+1):l29])] <- "unknown" }
      if(n < l5x) { tree_data$state[which(tree_data$ID_ARB_MES %in% tmp_sp5x$ID_ARB_MES[(n+1):l5x])] <- "unknown" }
    }
  }
}


# 3. For the rest of ETAT==29,5X assign the state unknown to the tree measure

tree_data$state[which(tree_data$ETAT == 29)] <- "unknown"
tree_data$state[which(tree_data$ETAT %in% c(50,52))] <- "unknown"
tree_data$state[which(tree_data$ETAT %in% c(54,55,56) & is.na(tree_data$DHP_NC))] <- "unknown"




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

plot_mes_deleted <- setdiff(unique(tree_mes$ID_PE_MES), unique(tree_data$ID_PE_MES))
# 875 PE MES were deleted because empty (no living mature trees)


### Add missing PE MES
plot_mes_deleted <- data.frame(ID_PE_MES = plot_mes_deleted)

tree_data <- plot_mes %>% # to get the year of sampling
  right_join(plot_mes_deleted, by = "ID_PE_MES") %>%
  mutate(state = "dead") %>%
  bind_rows(tree_data) %>% 
  group_by(ID_PE) %>% 
  mutate(plot_id = ifelse(is.na(plot_id), unique(plot_id[complete.cases(plot_id)]), plot_id)) %>%
  filter(!is.na(plot_id)) %>%
  arrange(plot_id, tree_id)

setdiff(unique(tree_mes$ID_PE_MES), unique(tree_data$ID_PE_MES)) 
# still 105 PE MES missing but all the MES in the PE never contained living mature trees

#### Join xy ####

tree_data <- left_join(tree_data, plot_xy, by = "ID_PE")

growth <- left_join(growth, plot_xy, by = "ID_PE") %>% arrange(plot_id)

plot_xy <- right_join(plot_xy, unique(tree_data[,c("ID_PE", "plot_id")]), by = "ID_PE")
  
#### Reshape - site x species matrix ####

sp_mat <- dcast(tree_data, ID_PE + ID_PE_MES + plot_id + year_measured ~ sp_code, 
               #subset = .(state == "alive"), 
               fun.aggregate = length,
               fill = 0, 
               value.var = "DHP")
sp_mat[,"NA"] <- NULL

sp_BA <- dcast(tree_data, ID_PE + ID_PE_MES + plot_id + year_measured ~ sp_code, 
              fun.aggregate = function(x) sum(pi*(x/(2 * 1000))^2)*(10000/399.7312),
              #subset = .(state == "alive"), 
              fill = 0, 
              value.var = "DHP")
sp_BA[,"NA"] <- NULL

# tree_data$BA <- pi * (tree_data$DHP/(2 * 1000))^2
# sp_BA2 <-dcast(tree_data, plot_id + year_measured ~ sp_code, 
#               fun.aggregate = function(x) sum(x)*(10000/399.7312),
#               subset = .(state == "alive"), 
#               fill = 0,
#               value.var = "BA")

#### SAVE ####

saveRDS(tree_data, "data/tree_data_may2018.RDS")

saveRDS(growth, "data/growth_data_may2018.RDS")

saveRDS(sp_mat, "data/sp_mat_abun_fev2019.RDS")
saveRDS(sp_BA, "data/sp_mat_ba_fev2019.RDS")

st_write(plot_xy, "data/plot_xy32198_may2018.gpkg", layer_options = "OVERWRITE=yes")



### ASSIGN PLOTS TO ECOREGION ####

ecoregion <- st_read("data/ecoregion_simple.gpkg", quiet = T)
ecoregion <- st_transform(ecoregion, st_crs(plot_xy))

#### Assigning each sample to the region where it belongs ####

xy_assign_reg <- st_intersection(plot_xy, ecoregion) %>% st_set_geometry(NULL) %>% distinct()

# Some plots are missing because they fall just outside the ecoregion polygon area (some are to far north, some are close to the boundary)
# I'll assign them to the nearest neighboor region


xy_unassign <- plot_xy %>% filter(!(plot_id %in% xy_assign_reg$plot_id)) 


xy_unassign <- st_join(xy_unassign, ecoregion, join = st_nn)  %>% st_set_geometry(NULL)

xy_assign_reg <- bind_rows(xy_assign_reg, xy_unassign) %>% arrange(plot_id) 

# some plot were assigned to 2  sous domaine (were at the intersection of 2), just remove 1 one them doesn't matter which
xy_assign_reg <- xy_assign_reg[-which(duplicated(xy_assign_reg$plot_id)),]

# Creating final ecoregion df

ecoreg_df <- xy_assign_reg %>% select(-AREA, -PERIMETER) %>%
  rename(ecoreg11 = SOUS_DOM, ecoreg10 = SOUS_DOM10, ecoreg6 =SOUS_DOM6, ecoreg3 = SOUS_DOM3) %>%
  mutate(ecoreg6 = factor(ecoreg6, labels = rev(c("Spruce-moss",
                                                  "Balsam fir-white birch",
                                                  "Balsam fir-yellow birch",
                                                  "Sugar maple-yellow birch",
                                                  "Sugar maple-basswood",
                                                  "Sugar maple-bitternut hickory"))))




saveRDS(ecoreg_df, "data/ecoreg_df.RDS")

