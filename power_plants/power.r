## Read in the data

power <- read.csv("july_generator2017.csv", header=T)
View(power)
power <- power %>%
  filter(grepl("Grant", Plant.Name))
View(power)
rm(power)

##Load The Tidyverse
##library(tidyverse)
power <- read.csv("july_generator2017.csv", header=T)
View(power)
## Use select to select all columns.  Use mutate to add a joint lat long column (and another in case I get it backward).  Write filter to pull out plants we care about
power <- power %>%
  select(Entity.ID:Longitude) %>%
  mutate(location = paste(Longitude, Latitude, sep=", ")) %>%
  mutate(locationrev = paste(Latitude, Longitude, sep=", ")) %>%  
  filter((Plant.ID == 6705 & Generator.ID == 4) | 
         (Plant.ID == 983 & (Generator.ID == 1 | Generator.ID == 2 | Generator.ID == 3)) | 
         (Plant.ID == 6113 & (Generator.ID == 3 | Generator.ID == 5)) |
         (Plant.ID == 994 & (Generator.ID == "ST2" | Generator.ID == "ST3")) | 
         (Plant.ID == 6018 & Generator.ID == 2) |
         (Plant.ID == 1374 & Generator.ID == 1) |  
         (Plant.ID == 1378 & Generator.ID == 3) |
         (Plant.ID == 6031 & Generator.ID == 2) |
         (Plant.ID == 2876 & (Generator.ID == 1 | Generator.ID == 2 | Generator.ID == 3 | Generator.ID == 4 | Generator.ID == 5)) |
         (Plant.ID == 6019 & Generator.ID == "ST1") | 
         (Plant.ID == 6094 & Generator.ID == 1) |
         (Plant.ID == 10641) |
         ##10641 says it has two units, 1 and 2.  Neither shows up, but a GEN1 does
         (Plant.ID == 8226 & Generator.ID == 1) |
         (Plant.ID == 3122 & (Generator.ID == 1 | Generator.ID == 2 | Generator.ID == 3)) |
         (Plant.ID == 3136 & (Generator.ID == 1 | Generator.ID == 2)) |
         (Plant.ID == 3149 & (Generator.ID == 1 | Generator.ID == 2)) |
         (Plant.ID == 10151 ) |
         ##10151 says it has 1A and 1B.  Neither shows up, but a GEN1 does. 
         (Plant.ID == 3944 & (Generator.ID == 1 | Generator.ID == 2 | Generator.ID == 3)) |
         (Plant.ID == 6004 & (Generator.ID == 1 | Generator.ID == 2)) 
         )
   
View(power)

##Explort the CSV of our files for upload to carto
power %>%
  write.csv("md.csv", col.names=TRUE)
rm(power)
