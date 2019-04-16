### FIRE & INSECT DATA ####

# Marie-Helene Brice
# February 2018

# This script extract disturbance data (fire and insect outbreak) for all Qc forest plots

# Base nationale de données sur les feux de forêt du Canada (BNDFFC)
# http://cwfis.cfs.nrcan.gc.ca/datamart/download/nfdbpoly?token=c74594b75b0157c6ca2590e27976ff90

# Insectes
# http://mffp.gouv.qc.ca/le-ministere/acces-aux-donnees-gratuites/

# Interventions forestières
# https://www.donneesquebec.ca/recherche/fr/dataset/recolte-et-reboisement

# Perturbations autres
# https://www.donneesquebec.ca/recherche/fr/dataset/epidemies-chablis-et-verglas

### PACKAGES ####

require(rgdal)
require(rgeos)
require(sp)
require(raster)
require(mapview)
require(data.table)
require(sf)
require(lwgeom)
require(dplyr)


### DATA ####

#### Coordinate data ####

xy32198 <- st_read("data/plot_xy32198_may2018.gpkg")


### Fire data ####

fire <- st_read("raw_data/feux/NFDB_poly/", "NFDB_poly_20171106")


### Insect data ####

arpenteuse <- st_read("raw_data/insectes/Arpenteuse/Arpenteuse_donnees_1991_2012/",
                      "Arpenteuse_donnees_1991_2012")

livree <- st_read("raw_data/insectes/Livree/",
                  "Livree_1985_2010")

tordeuse1 <- st_read("raw_data/insectes/TBE/TBE_Donnees_1992-2006/",
                     "TBE_1992_2006")

tordeuse2 <- st_read("raw_data/insectes/TBE/TBE_Donnees_2007-2013/",
                     "TBE_2007_2013")


#### Disturbances ####

# check the list of layers in the gdb
subset(ogrDrivers(), grepl("GDB", name))

ogrListLayers("raw_data/pertu/PERTU_AUTRE_PROV.gdb")
pertu_autre <- st_read("raw_data/pertu/PERTU_AUTRE_PROV.gdb",
                       layer="PERTU_AUTRE_PROV")

ogrListLayers("raw_data/pertu/INTERV_FORES_PROV.gdb")
interv_fores <-st_read("raw_data/pertu/INTERV_FORES_PROV.gdb",
                       layer="INTERV_FORES_PROV")


#### GEOMETRY VALIDATION ####

which(!st_is_valid(fire))
fire <- st_make_valid(fire)
which(!st_is(fire,  "MULTIPOLYGON"))
fire <- st_cast(fire, "MULTIPOLYGON")


which(!st_is_valid(arpenteuse))
arpenteuse <- st_make_valid(arpenteuse)

which(!st_is_valid(livree))
livree <- st_make_valid(livree)

which(!st_is(livree,  "MULTIPOLYGON"))
livree <- st_cast(livree, "MULTIPOLYGON") # cast GEOMETRYCOLLECTIONS to MULTIPOLYGON
livree <- livree[which(st_is(livree,  "MULTIPOLYGON")),] # keep only multipolygon

which(!st_is_valid(tordeuse1))
tordeuse1 <- st_make_valid(tordeuse1)

which(!st_is_valid(tordeuse2))
tordeuse2 <- st_make_valid(tordeuse2)

which(!st_is(tordeuse2,  "MULTIPOLYGON"))
mapview(tordeuse2[which(!st_is(tordeuse2,  "MULTIPOLYGON")),])
tordeuse2 <- st_cast(tordeuse2, "MULTIPOLYGON") # cast GEOMETRYCOLLECTIONS to MULTIPOLYGON


which(!st_is_valid(pertu_autre))
pertu_autre <- st_make_valid(pertu_autre)
which(!st_is(pertu_autre,  "MULTIPOLYGON"))

which(!st_is_valid(interv_fores))
interv_fores <- st_make_valid(interv_fores)
which(!st_is(interv_fores,  "MULTIPOLYGON"))

### SAVE VALID GPKG ####

st_write(fire, "raw_data/feux/NFDB_poly_20171106_valid.gpkg",
         driver="GPKG")

st_write(arpenteuse, "raw_data/insectes/insect_valid/Arpenteuse_donnees_1991_2012_valid.gpkg",
         driver="GPKG")

st_write(livree, "raw_data/insectes/insect_valid/Livree_1985_2010_valid.gpkg",
         driver="GPKG")

st_write(tordeuse1, "raw_data/insectes/insect_valid/TBE_1992_2006_valid.gpkg",
         driver="GPKG")

st_write(tordeuse2, "raw_data/insectes/insect_valid/TBE_2007_2013_valid.gpkg",
         driver="GPKG")

st_write(pertu_autre, "raw_data/pertu/pertu_autre_valid.gpkp",
         driver = "GPKG")

st_write(interv_fores, "raw_data/pertu/interv_fores_valid.gpkg",
         driver = "GPKG")




### READ VALID FILES ####

fire <- st_read("raw_data/feux/NFDB_poly_20171106_valid.gpkg")

arpenteuse <- st_read("raw_data/insectes/insect_valid/Arpenteuse_donnees_1991_2012_valid.gpkg")

livree <- st_read("raw_data/insectes/insect_valid/Livree_1985_2010_valid.gpkg")

tordeuse1 <- st_read("raw_data/insectes/insect_valid/TBE_1992_2006_valid.gpkg")

tordeuse2 <- st_read("raw_data/insectes/insect_valid/TBE_2007_2013_valid.gpkg")

pertu_autre <- st_read("raw_data/pertu/pertu_autre_valid.gpkg")

interv_fores <- st_read("raw_data/pertu/interv_fores_valid.gpkg")



### PROJECTION ####

xy_fire <- st_transform(xy32198, crs = st_crs(fire))

arpenteuse <- st_transform(arpenteuse, crs = st_crs(xy32198))

livree <- st_transform(livree, crs = st_crs(xy32198))

tordeuse1 <- st_transform(tordeuse1, crs = st_crs(xy32198))

tordeuse2 <- st_transform(tordeuse2, crs = st_crs(xy32198))

pertu_autre <- st_transform(pertu_autre, crs = st_crs(xy32198))

interv_fores <- st_transform(interv_fores, crs = st_crs(xy32198))

### INTERSECTION ####

# Fire

fire_crop <- st_intersection(fire, st_as_sfc(st_bbox(xy_fire)))

plot_fire <- st_intersection(xy_fire, fire_crop) %>% st_set_geometry(NULL)


# Insects

plot_arpen <- st_intersection(xy32198, arpenteuse) %>% st_set_geometry(NULL)

plot_livree <- st_intersection(xy32198, livree) %>% st_set_geometry(NULL)

plot_tordeuse1 <- st_intersection(xy32198, tordeuse1) %>% st_set_geometry(NULL)

plot_tordeuse2 <- st_intersection(xy32198, tordeuse2) %>% st_set_geometry(NULL)


# Disturbances

plot_pertu <- st_intersection(xy32198, pertu_autre) %>% st_set_geometry(NULL)

plot_interv <- st_intersection(xy32198, interv_fores) %>% st_set_geometry(NULL)


# Warning message:
#   attribute variables are assumed to be spatially constant throughout all geometries
# It's ok, they are spatially constant.

### FORMATTING DATA FRAME WITH ALL DISTURBANCES ####

### Insects ####

plot_arpen$insect <- "arpenteuse"
plot_livree$insect <- "livree"
plot_tordeuse1$insect <- "tordeuse"
plot_tordeuse2$insect <- "tordeuse"

# Correct colnames to match data frame

colnames(plot_livree) <- colnames(plot_tordeuse1)
colnames(plot_arpen) <- colnames(plot_tordeuse2)

# Matching class

plot_livree$ANNEE <- as.integer(as.character(plot_livree$ANNEE))

plot_arpen$ANNEE <- as.integer(as.character(plot_arpen$ANNEE))

plot_insect <- bind_rows(plot_arpen, plot_livree, plot_tordeuse1, plot_tordeuse2)

# reordering by plot_id and renumbering rows
plot_insect <- plot_insect %>% arrange(plot_id)


### Disturbances ####


plot_pertu <- plot_pertu %>%
  select(ID_PE, plot_id, ORIGINE, AN_ORIGINE, PERTURB, AN_PERTURB) %>%
  arrange(plot_id) %>%
  mutate(disturbance = "pertu_autre")

plot_pertu$AN_ORIGINE <- as.integer(as.character(plot_pertu$AN_ORIGINE))
plot_pertu$AN_PERTURB <- as.integer(as.character(plot_pertu$AN_PERTURB))

plot_interv <- plot_interv %>%
  select(ID_PE, plot_id, ORIGINE, AN_ORIGINE, PERTURB, AN_PERTURB, starts_with("REB")) %>%
  arrange(plot_id) %>%
  mutate(disturbance = "interv_fores")

plot_interv$AN_ORIGINE <- as.integer(as.character(plot_interv$AN_ORIGINE))
plot_interv$AN_PERTURB <- as.integer(as.character(plot_interv$AN_PERTURB))


plot_pertu_interv <- bind_rows(plot_pertu, plot_interv)

plot_pertu_interv <- plot_pertu_interv %>%
  arrange(plot_id)

# fire

plot_fire <- plot_fire %>%
  dplyr::select(ID_PE, plot_id, FIRE_ID, YEAR, DECADE, SIZE_HA, CAUSE) %>%
  arrange(plot_id)


# everything together

colnames(plot_fire) <- c("ID_PE", "plot_id", "FIRE_ID",
                         "FIRE_YEAR", "FIRE_DECADE", "FIRE_HA", "FIRE_CAUSE")

colnames(plot_insect) <- c("ID_PE", "plot_id", "INSECT_ANNEE", "INSECT_HA",
                           "INSECT_NIVEAU", "INSECT")
plot_insect <- plot_insect[,c("ID_PE", "plot_id", "INSECT", "INSECT_ANNEE", "INSECT_HA", "INSECT_NIVEAU")]



plot_disturb <- xy32198 %>%
  full_join(plot_fire, by = c("ID_PE", "plot_id")) %>%
  full_join(plot_insect, by = c("ID_PE", "plot_id")) %>%
  full_join(plot_pertu_interv, by = c("ID_PE", "plot_id")) %>%
  arrange(plot_id) %>% st_set_geometry(NULL)

saveRDS(plot_disturb, "data/plot_disturb.RDS")

# YEAH!
