library(tidyverse)

# fishe species attributes
fishbase = read.csv( "data/fish_Fishbase_attr.csv") %>%
  select(scientific_name, Troph, Length, family, Importance) 

# poplation data
reef_fish = readRDS("data/finalfullreeffish.rds")

# example to calculate LPI for commercial species
commercial_reef = fishbase %>%
  filter(Importance == "commercial") %>%
  # inner join to get ONLY commercial fishes
  inner_join(reef_fish, by = "scientific_name") %>%
  # actually lets just only calculate LPI for several fish
  filter(scientific_name %in% c("Coris picta","Aluterus scriptus"))

### Run the LPI for commercial fishes
# set up the data for LPI package
LPD2022_public_txt = commercial_reef %>%
  mutate(ID = paste(scientific_name, site, transect.spc, sep=""),
         Binomial = gsub(" ", "_", scientific_name),
         year = year) %>%
  # ID has to be a number in LPI function
  group_by(ID) %>% mutate(ID = cur_group_id()) %>% ungroup() %>%
  # this is to get the mean of the density for each population (ID) in each year, as LPI needs one value per population per year
  # some sources sampled fish multiple days within a year
  group_by(ID, year) %>%
  mutate(popvalue = mean(density,na.rm=T)) %>% distinct() %>%
  select(Binomial,ID,year,popvalue)%>%
  # filter out any time series that only had 1 data point, as LPI needs at least 2 data points to calculate a trend
  group_by(ID) %>% filter(n() > 1) %>%
  # this filters out any time series that had all 0 counts, see metadata on why we filled in 0 counts for some species
  #LPI doesnt like all 0 time series
  filter(max(popvalue, na.rm = TRUE) > 0)

lpi_txt_path = paste("lpi_temp/","LPD2022_public_pop_tmp",".txt",sep="")
write.table(LPD2022_public_txt,lpi_txt_path,sep="\t",row.names=FALSE)

lpi_infile = data.frame(FileName = lpi_txt_path, Group = 1, Weighting = 1)
lpi_infile_path = paste("lpi_temp/","LPD2022_public_infile_tmp",".txt",sep="")
write.table(lpi_infile,lpi_infile_path,sep="\t",row.names=FALSE)

# Calculate LPI
lpi_unwt_mean_tmp <- rlpi::LPIMain("lpi_temp/LPD2022_public_infile_tmp.txt", 
                                   use_weightings = 0, VERBOSE=T,save_plots=0,plot_lpi=1,REF_YEAR=2000,PLOT_MAX = 2025,
                                   GAM_GLOBAL_FLAG=0)



