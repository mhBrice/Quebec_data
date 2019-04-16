### Spatial data formatting

# packages
require(rgdal)
require(rgeos)
require(sf)
require(dplyr)
require(nngeo)

#### Bioclimatic domains ####

# Shapefile obtain from https://bib.umontreal.ca/guides/donnees-statistiques-geospatiales/donnees-geospatiales

# Other online resource: https://www.donneesquebec.ca/recherche/fr/dataset/systeme-hierarchique-de-classification-ecologique-du-territoire

ecoregion <- st_read("raw_data/zone_bioclimatique_QC/mrnf_sd05073g_poly.shp")


# Drop unuse variables

ecoregion <- ecoregion %>% select(SOUS_DOM11 = SOUS_DOM) 


# Reclassify ecoregion - 11, 10, 6, or 3
ecoregion <- ecoregion %>% 
  mutate(SOUS_DOM6 = recode_factor(SOUS_DOM11,
                                   "1" = "Sugar maple-bitternut hickory",
                                   "2est" = "Sugar maple-basswood",   
                                   "2ouest" = "Sugar maple-basswood",
                                   "3est" = "Sugar maple-yellow birch",  
                                   "3ouest" = "Sugar maple-yellow birch",
                                   "4est" = "Balsam fir-yellow birch",
                                   "4ouest" = "Balsam fir-yellow birch",
                                   "5est" = "Balsam fir-white birch",
                                   "5ouest" = "Balsam fir-white birch", 
                                   "6est" = "Spruce-moss",
                                   "6ouest" = "Spruce-moss"))


ecoregion <- ecoregion %>% 
  mutate(SOUS_DOM3 = case_when(SOUS_DOM11 %in% c("1","2est","2ouest","3est", "3ouest") ~ "Hardwood", 
                               SOUS_DOM11 %in% c("4est", "4ouest") ~ "Mixed",
                               SOUS_DOM11 %in% c("5est", "5ouest", "6est", "6ouest") ~ "Boreal"))


ecoregion <- st_transform(ecoregion, 32198)

# st_write(ecoregion, "data/ecoregion.gpkg", driver = "GPKG", layer_options= "OVERWRITE=YES")

# Simplify ecoregion polygon for faster plotting
ecoregion_simple <- st_simplify(ecoregion, dTolerance = 500, preserveTopology = TRUE)
ecoregion_simple <- st_transform(ecoregion_simple, 4269)

st_write(ecoregion_simple, "data/ecoregion_simple.gpkg", driver = "GPKG", 
         layer_options = "OVERWRITE=YES")


### ASSIGN PLOTS TO ECOREGION ####

#### Assigning each sample to the region where it belongs ####
plot_xy <- st_read("data/plot_xy32198_may2018.gpkg")

xy_assign_reg <- st_intersection(plot_xy, ecoregion) %>% 
  distinct()

mapview::mapview(xy_assign_reg, zcol="SOUS_DOM6")

# Some plots are missing because they fall just outside the ecoregion polygon area (some are to far north, some are close to the boundary)
# I'll assign them to the nearest neighboor region


xy_unassign <- plot_xy %>% 
  filter(!(plot_id %in% xy_assign_reg$plot_id)) 


xy_unassign <- st_join(xy_unassign, ecoregion, join = st_nearest_feature)
  
mapview::mapview(xy_unassign, zcol="SOUS_DOM6")

xy_assign_reg <- bind_rows(xy_assign_reg, xy_unassign) %>% 
  arrange(plot_id) 

ecoreg_df <- xy_assign_reg %>% 
  st_set_geometry(NULL) %>% 
  distinct() %>% 
  rename(ecoreg11 = SOUS_DOM11, ecoreg6 = SOUS_DOM6, ecoreg3 = SOUS_DOM3)

saveRDS(ecoreg_df, "data/ecoreg_df.RDS")


### QUEBEC BOUNDARY ####

# Create a polygone of the region without subdivision
bound_CAN <- raster::getData(country='CAN', level=1, path= "data/")
bound_Qc <- bound_CAN[bound_CAN@data$NAME_1 == "Québec",]

bound_Qc <- st_as_sf(bound_Qc)
mapview(bound_Qc)

st_write(bound_Qc, "data/bound_Qc.gpkg")
