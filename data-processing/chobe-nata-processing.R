# Chobe
# ******

# load data
chobe.source <- read.csv("/Users/smit1678/github/botswana-data/chobe/chobe_field-data_20180622.csv", header = T)

# number of valid buildings by village
chobe.valid <- filter(chobe.source, building != "yes" | building != "")
chobe.valid.village <- chobe.valid %>%
  group_by(addr.village) %>%
  summarise (valid_buildings = n())

# number of residential buildings by village
chobe.valid.residential <- filter(chobe.valid, building == "apartments" | building == "residential" | building == "commercial;residential" & building.roof != "" & wall == "yes")
chobe.valid.residential.village <- chobe.valid.residential %>%
  group_by(addr.village) %>%
  summarise (residential = n())

# number of sprayable by village 
# filter by building materials and roof material
# exclude canvas, metal, metal*, plastic 
chobe.valid.sprayable <- filter(chobe.valid.residential, building.material != "canvas" & building.material != "metal" & building.material != "plastic" & building.material != "tent")
chobe.valid.sprayable.village <- chobe.valid.sprayable %>%
  group_by(addr.village) %>%
  summarise (sprayable = n())

# number of sprayable by building_material by village
chobe.valid.sprayable <- filter(chobe.valid.residential, building.material != "canvas" & building.material != "metal" & building.material != "plastic" & building.material != "tent")
chobe.valid.sprayable.village.material <- chobe.valid.sprayable %>%
  group_by(addr.village,building.material) %>%
  summarise (sprayable = n())
write.csv(chobe.valid.sprayable.village.material, "github/botswana-data/chobe.valid.sprayable.village.material.csv", row.names=FALSE, na="")

# number of sprayable rooms by village
# first need to create avg number of rooms per sprayable building
# using total building rooms as sprayable
chobe.valid.sprayable.rooms <- filter(chobe.valid.sprayable, building.rooms != "")
chobe.valid.sprayable.rooms$building.rooms <- as.numeric(as.character(chobe.valid.sprayable.rooms$building.rooms))
#chobe.valid.sprayable.rooms$building.sleeping_spaces <- as.numeric(as.character(chobe.valid.sprayable.rooms$building.sleeping_spaces))
#chobe.valid.sprayable.rooms <- filter(chobe.valid.sprayable.rooms, building.sleeping_spaces < 21)
mean(chobe.valid.sprayable.rooms$building.rooms)

# sprayable average by village
chobe.valid.sprayable.rooms.avg <- chobe.valid.sprayable.rooms %>%
  group_by(addr.village) %>%
  summarise (avg_sprayable_rooms = mean(building.rooms))

# then apply that proportion to buildings without interior data
# start with chobe.valid.sprayable
chobe.valid.sprayable$building.rooms_updated <- as.numeric(as.character(chobe.valid.sprayable$building.rooms))
chobe.valid.sprayable$building.rooms_updated[chobe.valid.sprayable$residential_details=="no" | chobe.valid.sprayable$building.rooms==""] <- 2.38

# aggregate number of sprayable rooms by village 
#chobe.valid.sprayable.updated.rooms <- filter(chobe.valid.sprayable, building.sleeping_spaces_updated < 21)
chobe.valid.sprayable.updated.rooms.village <- chobe.valid.sprayable %>%
  group_by(addr.village) %>%
  summarise (sprayable_rooms = sum(building.rooms_updated))

# percentage of painted rooms
# need to generate the average 
chobe.valid.painted.rooms <- filter(chobe.valid.sprayable, building.rooms_painted != "")
chobe.valid.painted.rooms <- filter(chobe.valid.painted.rooms, building.rooms_painted < 1482)
mean(chobe.valid.painted.rooms$building.rooms_painted)

# average by village
chobe.valid.painted.rooms.avg <- chobe.valid.painted.rooms %>%
  group_by(addr.village) %>%
  summarise (avg_painted_rooms = mean(building.rooms_painted))

# apply average to buildings without painted room data
chobe.valid.sprayable$building.rooms_painted_updated <- as.numeric(as.character(chobe.valid.sprayable$building.rooms_painted))
chobe.valid.sprayable$building.rooms_painted_updated[chobe.valid.sprayable$residential_details=="no" | chobe.valid.sprayable$building.rooms_painted==""] <- 1.54

#chobe.valid.sprayable$nonpainted_rooms <- chobe.valid.sprayable$building.sleeping_spaces_updated - chobe.valid.sprayable$building.rooms_painted_updated

chobe.valid.painted.updated.rooms <- filter(chobe.valid.sprayable, building.rooms_painted_updated < 1482)
chobe.valid.painted.updated.rooms.village <- chobe.valid.painted.updated.rooms %>%
  group_by(addr.village) %>%
  summarise (painted_rooms = sum(building.rooms_painted_updated))


# merge things together
chobe.building.sprayable.merged <- Reduce(function(x, y) merge(x, y, all=TRUE), list(chobe.valid.village, 
                                                                                    chobe.valid.residential.village,
                                                                                    chobe.valid.sprayable.village,
                                                                                    chobe.valid.sprayable.rooms.avg,
                                                                                    chobe.valid.sprayable.updated.rooms.village,
                                                                                    chobe.valid.painted.rooms.avg,
                                                                                    chobe.valid.painted.updated.rooms.village))
write.csv(chobe.building.sprayable.merged, "github/botswana-data/chobe.building.sprayable.merged.csv", row.names=FALSE, na="")

# 
# **************************************
# Nata

nata.points <- read.csv("/Users/smit1678/github/botswana-data/nata/20180622_nata_buildings_merged_points.csv", header = T)
nata.polygons <- read.csv("/Users/smit1678/github/botswana-data/nata/20180622_nata_buildings_merged_polygons.csv", header = T)
nata.points.selected <- select(nata.points, building, addr.village, building.material, building.roof, residential_details, wall, building.rooms_painted, building.sleeping_spaces, building.rooms, building.population)
nata.polygons.selected <- select(nata.polygons, building, addr.village, building.material, building.roof, residential_details, wall, building.rooms_painted, building.sleeping_spaces, building.rooms, building.population)
#combines points and polygons together
nata.source <- rbind(nata.polygons.selected, nata.points.selected)

# number of valid buildings by village
nata.valid <- filter(nata.source, building != "yes" | building != "" | addr.village != "Motopi")
nata.valid.village <- nata.valid %>%
  group_by(addr.village) %>%
  summarise (valid_buildings = n())

# number of residential buildings by village
nata.valid.residential <- filter(nata.valid, building == "apartments" | building == "residential" | building == "commercial;residential" & building.roof != "" & wall == "yes")
nata.valid.residential.village <- nata.valid.residential %>%
  group_by(addr.village) %>%
  summarise (residential = n())

# number of sprayable by village 
# filter by building materials and roof material
# exclude canvas, metal, metal*, plastic 
nata.valid.sprayable <- filter(nata.valid.residential, building.material != "canvas" & building.material != "metal" & building.material != "plastic" & building.material != "tent")
nata.valid.sprayable.village <- nata.valid.sprayable %>%
  group_by(addr.village) %>%
  summarise (sprayable = n())

# number of sprayable by building_material by village
nata.valid.sprayable <- filter(nata.valid.residential, building.material != "canvas" & building.material != "metal" & building.material != "plastic" & building.material != "tent")
nata.valid.sprayable.village.material <- nata.valid.sprayable %>%
  group_by(addr.village,building.material) %>%
  summarise (sprayable = n())
write.csv(nata.valid.sprayable.village.material, "github/botswana-data/nata.valid.sprayable.village.material.csv", row.names=FALSE, na="")

# number of sprayable rooms by village
# first need to create avg number of rooms per sprayable building
nata.valid.sprayable.rooms <- filter(nata.valid.sprayable, building.rooms != "")
nata.valid.sprayable.rooms$building.rooms <- as.numeric(as.character(nata.valid.sprayable.rooms$building.rooms))
#nata.valid.sprayable.rooms$building.sleeping_spaces <- as.numeric(as.character(nata.valid.sprayable.rooms$building.sleeping_spaces))
#nata.valid.sprayable.rooms <- filter(nata.valid.sprayable.rooms, building.rooms < 21)
mean(nata.valid.sprayable.rooms$building.rooms)

# sprayable average by village
nata.valid.sprayable.rooms.avg <- nata.valid.sprayable.rooms %>%
  group_by(addr.village) %>%
  summarise (avg_sprayable_rooms = mean(building.rooms))

# then apply that proportion to buildings without interior data
# start with chobe.valid.sprayable
nata.valid.sprayable$building.rooms_updated <- nata.valid.sprayable$building.rooms
nata.valid.sprayable$building.rooms_updated[nata.valid.sprayable$residential_details=="no" | nata.valid.sprayable$building.rooms==""] <- 1.92

# aggregate number of sprayable rooms by village 
nata.valid.sprayable.updated.rooms <- filter(nata.valid.sprayable, building.rooms_updated != "")
nata.valid.sprayable.updated.rooms.village <- nata.valid.sprayable.updated.rooms %>%
  group_by(addr.village) %>%
  summarise (sprayable_rooms = sum(building.rooms_updated))

# percentage of painted rooms
# need to generate the average 
nata.valid.painted.rooms <- filter(nata.valid.sprayable, building.rooms_painted != "")
nata.valid.painted.rooms <- filter(nata.valid.painted.rooms, building.rooms_painted < 1482)
mean(nata.valid.painted.rooms$building.rooms_painted)
# 0.92 painted rooms avg

# average by village
nata.valid.painted.rooms.avg <- nata.valid.painted.rooms %>%
  group_by(addr.village) %>%
  summarise (avg_painted_rooms = mean(building.rooms_painted))

# apply average to buildings without painted room data
nata.valid.sprayable$building.rooms_painted_updated <- as.numeric(as.character(nata.valid.sprayable$building.rooms_painted))
nata.valid.sprayable$building.rooms_painted_updated[nata.valid.sprayable$residential_details=="no" | nata.valid.sprayable$building.rooms_painted==""] <- 0.92

nata.valid.painted.updated.rooms <- filter(nata.valid.sprayable, building.rooms_painted_updated < 1482)
nata.valid.painted.updated.rooms.village <- nata.valid.painted.updated.rooms %>%
  group_by(addr.village) %>%
  summarise (painted_rooms = sum(building.rooms_painted_updated))


# merge things together
nata.building.sprayable.merged <- Reduce(function(x, y) merge(x, y, all=TRUE), list(nata.valid.village, 
                                                                                    nata.valid.residential.village,
                                                                                    nata.valid.sprayable.village,
                                                                                    nata.valid.sprayable.rooms.avg,
                                                                                    nata.valid.sprayable.updated.rooms.village,
                                                                                    nata.valid.painted.rooms.avg,
                                                                                    nata.valid.painted.updated.rooms.village))
write.csv(nata.building.sprayable.merged, "github/botswana-data/nata.building.sprayable.merged.csv", row.names=FALSE, na="")
 
# 
# ***********
# 
# combine nata and chobe

# percent residential
# chobe.valid
nata.valid.combine <- select(nata.valid, addr.village, building.rooms, building.rooms_painted)
chobe.valid.combine <- select(chobe.valid, addr.village, building.rooms, building.rooms_painted)
chobe.nata.valid.combined <- rbind(nata.valid.combine, chobe.valid.combine)
chobe.nata.valid.combined.village <- chobe.nata.valid.combined %>%
  group_by(addr.village) %>%
  summarise (valid = n())
range(chobe.nata.valid.combined.village$valid, na.rm = TRUE)
count(chobe.nata.valid.combined)

# residential 
# nata.valid.residential
nata.valid.residential.combine <- select(nata.valid.residential, addr.village, building.rooms, building.rooms_painted)
chobe.valid.residential.combine <- select(chobe.valid.residential, addr.village, building.rooms, building.rooms_painted)
chobe.nata.residential.combined <- rbind(nata.valid.residential.combine, chobe.valid.residential.combine)
chobe.nata.residential.combined.village <- chobe.nata.residential.combined %>%
  group_by(addr.village) %>%
  summarise (residential = n())
range(chobe.nata.residential.combined.village$residential, na.rm = TRUE)
count(chobe.nata.residential.combined)
# range = 5 4718
# total = 19721

# percent sprayable 
# nata.valid.sprayable
nata.valid.sprayable.combine <- select(nata.valid.sprayable, addr.village, building.rooms, building.rooms_painted)
chobe.valid.sprayable.combine <- select(chobe.valid.sprayable, addr.village, building.rooms, building.rooms_painted)
chobe.nata.sprayable.combined <- rbind(nata.valid.sprayable.combine, chobe.valid.sprayable.combine)
chobe.nata.sprayable.combined.village <- chobe.nata.sprayable.combined %>%
  group_by(addr.village) %>%
  summarise (sprayable = n())
range(chobe.nata.sprayable.combined.village$sprayable, na.rm = TRUE)
count(chobe.nata.sprayable.combined)

# avg sprayable and painted 
nata.valid.sprayable.rooms.combine <- select(nata.valid.sprayable.rooms, addr.village, building.rooms, building.rooms_painted)
chobe.valid.sprayable.rooms.combine <- select(chobe.valid.sprayable.rooms, addr.village, building.rooms, building.rooms_painted)
chobe.nata.combined <- rbind(nata.valid.sprayable.rooms.combine, chobe.valid.sprayable.rooms.combine)
chobe.nata.combined <- filter(chobe.nata.combined, building.rooms != "" & building.rooms_painted != "")
mean(chobe.nata.combined$building.rooms)
mean(chobe.nata.combined$building.rooms_painted)
