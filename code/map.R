library(rnaturalearth)
library(sf)
world <- ne_countries(scale = "small", returnclass = "sf")

# Point map
marine_taxa_table_sf = aus_fish_map %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

ggplot() +
  geom_sf(data = world, fill = "gray80",color=NA) +
  geom_sf(data = marine_taxa_table_sf, alpha = 0.5,shape=21, linewidth=3,color = "gray20",aes(size = n)) +
  theme_bw()
  
  