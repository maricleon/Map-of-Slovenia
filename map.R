library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("osmdata")
library("ggspatial")


#ploting world map and selecting only slovenia map and also regions
world <- ne_countries(scale = "medium", returnclass = "sf")
slovenia <- world[world$iso_a2 == "SI", ]
nuts <- ne_states(country = "slovenia", returnclass = "sf")
pomurje <- nuts[nuts$region == "Pomurska", ]
podravje <- nuts[nuts$region == "Podravska", ]
gorenjska <- nuts[nuts$region == "Gorenjska", ]

##########################################################################
#points of sampling sites

points_ms <- data.frame(
  lon = c(16.0407281,16.1514182,16.1920924,16.4722014,16.4619217,16.1669138,16.1951581,16.2639272,16.2217072),
  lat = c(46.7475416,46.6708534,46.6599058,46.5417949,46.5279347,46.638443,46.6260992,46.6447512,46.6304834),
  name = c("MS1","MS2","MS3","MS4","MS5","MS6","MS7","MS8","MS9")
  )

points_mb <- data.frame(
  lon = c(15.6610541,15.6156159,15.6480357,15.7258191,15.7163642,15.7315992,15.7654403,15.8842153,15.7591614),
  lat = c(46.5563618,46.5663517,46.5691935,46.5860295,46.5099613,46.5050092,46.479896,46.405152,46.4299319),
  name = c("MB1","MB2","MB3","MB4","MB5","MB6","MB7","MB8","MB9")
)

points_kr <- data.frame(
  lon = c(14.359424,14.3435683,14.1205,14.312372,14.3425083,14.3932647,14.4048089),
  lat = c(46.2737319,46.288084,46.38811,46.2611024,46.2253443,46.2102414,46.2020098),
  name = c("KR1","KR2","KR3","KR4","KR5","KR6","KR7")
)

###############################################################################
#organising data for river ploting

#read shp file, where all the data about river is stored
#data for rivers is available on: https://podatki.gov.si/dataset/hidrografija

rivers <- st_read("./DRSV_HIDRO5_LIN_PV/HIDRO5_LIN_PV_TIPTV1_2.shp")

#filter from all rivers in prekmurje to just Ledava river
river_prekmurje <- rivers[rivers$IME == "Ledava", ]
# Check the CRS of pomurje and river_prekmurje
crs_pomurje <- st_crs(pomurje)
crs_river <- st_crs(river_prekmurje)
# If they are not the same, transform river_prekmurje to match pomurje
if (crs_pomurje != crs_river) {
  river_prekmurje <- st_transform(river_prekmurje, crs_pomurje)
}
#crop river, to be limited only on map area
river_prekmurje_clipped <- st_intersection(river_prekmurje, pomurje)

#read shp file, where all the data about river is stored
river_podravje <- rivers[rivers$IME == "Drava", ]
# Check the CRS of podravje and river_podravje
crs_podravje <- st_crs(podravje)
crs_river <- st_crs(river_podravje)
# If they are not the same, transform river_podravje to match podravje
if (crs_podravje != crs_river) {
  river_podravje <- st_transform(river_podravje, crs_podravje)
}
#crop river, to be limited only on map area
river_podravje_clipped <- st_intersection(river_podravje, podravje)

#read shp file, where all the data about river is stored
river_gorenjska <- rivers[rivers$IME %in% c("Sava", "Sava Dolinka"), ]
# Check the CRS of podravje and river_podravje
crs_gorenjska <- st_crs(gorenjska)
crs_river <- st_crs(river_gorenjska)
# If they are not the same, transform river_podravje to match podravje
if (crs_gorenjska != crs_river) {
  river_gorenjska <- st_transform(river_gorenjska, crs_gorenjska)
}
#crop river, to be limited only on map area
river_gorenjska_clipped <- st_intersection(river_gorenjska, gorenjska)


#####################################################################################

#map for slovenia and 3 rivers

ggplot() +
  geom_sf(data = nuts, aes(fill = nuts_regions), fill = "grey", alpha = 0.4, color = NA) +
  scale_fill_manual(values = "black") +
  geom_point(data = points_ms, aes(x = lon, y = lat), color = "black", size = 1.7) +
  geom_point(data = points_mb, aes(x = lon, y = lat), color = "black", size = 1.7) +
  geom_point(data = points_kr, aes(x = lon, y = lat), color = "black", size = 1.7) +
  geom_sf(data = river_prekmurje_clipped, color = "blue") +
  geom_sf(data = river_podravje, color = "blue") +
  geom_sf(data = river_gorenjska, color = "blue") +
  annotation_scale(location = "bl", width_hint = 0.3) +  # Adds a scale bar in bottom-left
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering()) + # Adds north arrow
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA)
  )



####Pomurje
ggplot() +
  geom_sf(data = pomurje, fill = "gray", color = NA, alpha = 0.4) +
  geom_point(data = points_ms, aes(x = lon, y = lat), color = "black", size = 3) + # Add coordinate points
  geom_text(data = points_ms, aes(x = lon, y = lat, label = name), vjust = -0.4, hjust = -0.2, size = 3) + # Add labels for points
  geom_sf(data = river_prekmurje_clipped, color = "cornflowerblue") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA)
    
  )

#####Podravje
ggplot() +
  geom_sf(data = podravje, fill = "gray", color = NA, alpha = 0.4) +
  geom_point(data = points_mb, aes(x = lon, y = lat), color = "black", size = 3) + # Add coordinate points
  geom_text(data = points_mb, aes(x = lon, y = lat, label = name), vjust = -0.4, hjust = -0.2, size = 3) + # Add labels for points
  geom_sf(data = river_podravje_clipped, color = "cornflowerblue", size = 5) +  # Add rivers
    theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA)
    
  )
#####Gorenjska
ggplot() +
  geom_sf(data = gorenjska, fill = "gray", color = NA, alpha = 0.4) +
  geom_point(data = points_kr, aes(x = lon, y = lat), color = "black", size = 2) + # Add coordinate points
  geom_text(data = points_kr, aes(x = lon, y = lat, label = name), vjust = -0.4, hjust = -0.2, size = 3) +
  geom_sf(data = river_gorenjska_clipped, color = "cornflowerblue") +  # Add rivers
  # Add labels for points
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "transparent", color = NA)
    
  )
