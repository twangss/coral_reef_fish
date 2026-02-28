library(tidyverse)
library(jsonlite) #WoRMS rest service access
library(httr) #WoRMS rest service access
library(worrms) # worms r package

aus_fish = readRDS("data/finalfullreeffish.rds") %>%
  filter(temperate == 0) %>%
  dplyr::select(scientific_name) %>% distinct()

aus_fish_map = read.csv(aus_fish_path) %>%
  filter(temperate == 0) %>%
  dplyr::select(latitude, longitude, scientific_name, site) %>%
  distinct() %>%
  group_by(latitude, longitude) %>%
  summarise(n=n())


# obis database
obis_path = "/Users/twangs/Documents/Github/LPI/OBIS_data/obis_species_fishbaseid_v3.csv"
obis_data = read.csv(obis_path)


fish_join = aus_fish %>% left_join(obis_data, by= c("scientific_name" = "scientificname"))

# for the ones that were not joined let's get their Aphia ID from WoRMS (the taxonomic register)
# https://www.marinespecies.org/rest/, check this out to manually test some species names
# the Aphia ID will be necessary to join to fishbase (which itself is not good at taxonomy)
for (i in which(is.na(fish_join$fishbase_id))){
  print(i)
  Binomial_tmp = fish_join$scientific_name[i]
  encoded_name <- URLencode(Binomial_tmp, reserved = TRUE)
  url=sprintf("https://www.marinespecies.org/rest/AphiaIDByName/%s?marine_only=false",encoded_name)
  tryCatch({ # skip over errors in rest service
    aphiaid_tmp <- fromJSON(url)
    
    # Get the accepted AphiaID (this will return the accepted ID even if input was a synonym)
    accepted_url <- sprintf("https://www.marinespecies.org/rest/AphiaRecordByAphiaID/%s", aphiaid_tmp)
    record <- fromJSON(accepted_url)
    
    accepted_aphiaid <- record$valid_AphiaID  # This gives you the accepted AphiaID
    
    fish_join$aphiaid[i] = accepted_aphiaid
    fish_join$fishbase_id[i] = wm_external(accepted_aphiaid, type = "fishbase")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) # skip over errors in rest service
  
  rm(aphiaid_tmp,Binomial_tmp)
}

# after running this, seems like 30% species that got missed in the 1st pass, now have their Aphia ID in the 2nd pass
# I would maybe edit some names since there are typos, or manually fill in their Aphia IDs
# For example, no periods should be allowed ("spp." is not good)  
# ideally, I would run the previous chunk just once and save out the data (similar to how I previously saeved out and read in obis_species_fishbaseid_v3.csv)

library(rfishbase)
fishbase_tbl = fb_tbl("species") 
# this pulls MANY attributes of ALL species in fishbase, maybe select the few attributes you're interested in as an arugment? use help tool
fish_join_fishbase = fish_join %>% left_join(fishbase_tbl,by=c("fishbase_id"="SpecCode"))

# rfishbase also has other functions to get more attributes
trophic = estimate() # this gets the trophic level of species
lengths = length_length()
fish_join_fishbase_TL = fish_join_fishbase %>% 
  left_join(trophic, by=c("fishbase_id"="SpecCode"))

write.csv(fish_join_fishbase_TL, "data/fish_Fishbase_attr.csv")


# Join to FAO Fishing Area for later random effects
sf_use_s2(F)
FAO = st_read("data/Shapefile/FAO_AREAS_ERASE/FAO_AREAS_ERASE.shp") %>%
  filter(F_LEVEL == "MAJOR") %>%
  dplyr::select(F_NAME) %>% 
  st_make_valid() 

fish_join_fishbase_TL_FAO = fish_join_fishbase_TL %>% 
  st_as_sf(coords = c("longitude", "latitude"),remove=F) %>%
  st_set_crs(4326) %>% st_join(FAO,join = st_nearest_feature) %>%
  st_drop_geometry()
sf_use_s2(T)


fish_join_fishbase_TL_FAO %>% 
  dplyr::select(scientific_name,Troph,F_NAME) %>%
  distinct() %>%
  ggplot()+
  geom_histogram(aes(x=Troph)) + 
  labs(x = "Trophic levels") + theme_bw() +
  facet_wrap(~F_NAME)

fish_join_fishbase_TL_FAO %>% 
  dplyr::select(scientific_name,Length,F_NAME) %>%
  distinct() %>%
  ggplot()+
  geom_histogram(aes(x=Length)) + 
  labs(x = "Length (cm)") + theme_bw() +
  facet_wrap(~F_NAME)

fish_join_fishbase_TL_FAO %>% 
  dplyr::select(scientific_name, family) %>%
  distinct() %>%
  count(family, name = "n") %>%  # This creates one row per family with count
  arrange(n) %>%
  mutate(family = factor(family, levels = family)) %>%  # Now levels are correct
  ggplot() +
  geom_col(aes(x = n, y = family)) +  # Use geom_col since you have counts
  labs(x = "species count") + 
  theme_bw()

fish_join_fishbase_TL_FAO %>% 
  dplyr::select(scientific_name, Importance) %>%
  distinct() %>%
  count(Importance, name = "n") %>%  # This creates one row per family with count
  arrange(n) %>%
  mutate(Importance = factor(Importance, levels = Importance)) %>%  # Now levels are correct
  ggplot() +
  geom_col(aes(x = n, y = Importance)) +  # Use geom_col since you have counts
  labs(x = "species count") + 
  theme_bw()

