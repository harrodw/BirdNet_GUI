# ################################################################################
# Title: Making a BirdNET species list
# BirdNet R workflow, Script 2 of 4
# Author: Will Harrod
# Date Created: 2026-05-01
# Credit to  the species list creator: 
# https://github.com/conormcmahon/birdnet_species_list_processing/blob/main/all_species_metadata.csv
################################################################################

################################################################################
# 1) Preparation ###############################################################
################################################################################

# Clear environments
rm(list = ls())

# Install packages 
# install.packages("tidyverse")

# Load packages
library(tidyverse)
library(birdnetR)
library(usethis)

# Link to GIRT
use_git_config(user.name = "harrodw", user.email = "will.harrod99@gmail.com")

# Talk with GIT
usethis::use_git()

# Read in the total list of species
all_species <- read.csv("C:/NCSU/Data/BirdNET_Info/all_species_metadata.csv")

# View the structure
glimpse(all_species)

# How many classes?
all_species %>% 
  distinct(class)

################################################################################
# 2) Decide on possible species ################################################
################################################################################

# 2.1) Anthropogenic sounds ----------------------------------------------------
all_anthro <- all_species %>% 
  filter(is.na(class)) %>% 
  distinct(class, order, family, genus, species, common_name, full_code) %>% 
  mutate(across(.cols = c(class, order, family, genus, species), 
                .fns = ~ as.character("Anthropogenic"))) %>% 
  mutate() %>% 
  mutate(
         class = case_when(common_name== "Dog" ~ "Mammalia",
                           TRUE ~ class),
         order = case_when(common_name == "Dog" ~ "Carnivora",
                           TRUE ~ order),
         family = case_when(common_name == "Dog" ~ "Canidae",
                           TRUE ~ family),
         genus = case_when(common_name == "Dog" ~ "Canus",
                           TRUE ~ genus),
         species = case_when(common_name == "Dog" ~ "lupis",
                           TRUE ~ species))  
all_anthro
all_anthro$full_code

# List of possible anthropogenic sounds
psb_anthro_codes <- c(
  "Dog_Dog", 
  "Engine_Engine", 
  # "Environmental_Environmental", 
  # "Fireworks_Fireworks", 
  "Gun_Gun", 
  "Human non-vocal_Human non-vocal", 
  "Human vocal_Human vocal", 
  # "Human whistle_Human whistle", 
  # "Noise_Noise", 
  "Power tools_Power tools" 
  # "Siren_Siren"
)

# filter to those values
psb_anthro <- all_anthro %>% 
  filter(full_code %in% psb_anthro_codes)
slice_head(psb_anthro, n = Inf)

# 2.2) Mammals -----------------------------------------------------------------

# View all the mammals?
all_mammals <- all_species %>% 
  filter(class == "Mammalia") %>% 
  distinct(class, order, family, genus, species, common_name, full_code) 
all_mammals
all_mammals$full_code

# Define the possible mammals 
psb_mammal_codes <- c(
  # "Tamias striatus_Eastern Chipmunk" ,
  # "Sciurus carolinensis_Eastern Gray Squirrel",
  # "Odocoileus virginianus_White-tailed Deer",
  # "Canis latrans_Coyote"
)

# Filter the mammals to that list
psb_mammals <- all_mammals %>% 
  filter(full_code %in% psb_mammal_codes) %>% 
  arrange(class, order, family, genus, species)
# View
slice_head(psb_mammals, n = length(psb_mammal_codes))

# 2.3) Anurans -----------------------------------------------------------------

# How many amphibians?
all_species %>% 
  filter(class == "Amphibia") %>% 
  distinct(class, order, family, genus, species, common_name, full_code) %>% 
  nrow()

# View all of the amphibians
all_frogs <- all_species %>% 
  filter(class == "Amphibia") %>% 
  distinct(class, order, family, genus, species, common_name, full_code) %>% 
  arrange(full_code) 
all_frogs
all_frogs$full_code

# List of possible frogs
psb_frog_codes <- c(
  # "Acris crepitans_Northern Cricket Frog",
  "Acris gryllus_Southern Cricket Frog",
  # "Anaxyrus americanus_American Toad", 
  "Anaxyrus fowleri_Fowler's Toad",
  "Anaxyrus quercicus_Oak Toad",
  "Anaxyrus terrestris_Southern Toad",
  # "Dryophytes andersonii_Pine Barrens Treefrog",
  "Dryophytes chrysoscelis_Cope's Gray Treefrog",
  "Dryophytes cinereus_Green Treefrog",
  "Dryophytes femoralis_Pine Woods Treefrog",
  # "Dryophytes gratiosus_Barking Treefrog",
  "Dryophytes squirellus_Squirrel Treefrog",               
  "Dryophytes versicolor_Gray Treefrog",
  "Gastrophryne carolinensis_Eastern Narrow-mouthed Toad",
  "Lithobates catesbeianus_American Bullfrog", 
  "Lithobates clamitans_Green Frog", 
  "Lithobates palustris_Pickerel Frog", 
  # "Lithobates sylvaticus_Wood Frog", 
  "Pseudacris brimleyi_Brimley's Chorus Frog", 
  "Pseudacris crucifer_Spring Peeper", 
  # "Pseudacris nigrita_Southern Chorus Frog", 
  "Pseudacris feriarum_Upland Chorus Frog",
  "Pseudacris ocularis_Little Grass Frog"
  # "Pseudacris ornata_Ornate Chorus Frog"
)

# Filter the data frame to only those frogs
psb_frogs <- all_frogs %>% 
  filter(full_code %in% psb_frog_codes) %>% 
  arrange(class, order, family, genus, species)

# View
slice_head(psb_frogs, n = Inf)

# 2.4) Birds -------------------------------------------------------------------

# How many birds?
all_birds <- all_species %>% 
  filter(class == "Aves") %>% 
  distinct(class, order, family, genus, species, common_name, full_code) 
nrow(all_birds)

# Coords for timbermill
lat <- 36.159651
long <- -76.598058

# define a week (April 22nd)
week <- 20

# load the meta model
meta_model <- birdnet_model_meta("v2.4")

# What bird species are possible?
# Predict species occurrence during the average week of the audio recording
pred_birds <- predict_species_at_location_and_time(meta_model,
                                                         latitude = lat,
                                                         longitude = long,
                                                          week = week) %>%
  arrange(-confidence)

# View
pred_birds
pred_birds$label

# List of possible birds 
psb_bird_codes <- taxonomic_bird_list <- c(
    "Colinus virginianus_Northern Bobwhite",
    "Meleagris gallopavo_Wild Turkey",
    # "Branta canadensis_Canada Goose",
    # "Aix sponsa_Wood Duck",
    # "Spatula discors_Blue-winged Teal",
    # "Anas platyrhynchos_Mallard",
    # "Anas rubripes_American Black Duck",
    # "Podilymbus podiceps_Pied-billed Grebe",
    # "Columba livia_Rock Pigeon",
    # "Streptopelia decaocto_Eurasian Collared-Dove",
    "Zenaida macroura_Mourning Dove",
    "Coccyzus americanus_Yellow-billed Cuckoo",
    "Antrostomus carolinensis_Chuck-will's-widow",
    "Antrostomus vociferus_Eastern Whip-poor-will",
    # "Chaetura pelagica_Chimney Swift",
    "Archilochus colubris_Ruby-throated Hummingbird",
    # "Rallus crepitans_Clapper Rail",
    # "Fulica americana_American Coot",
    # "Haematopus palliatus_American Oystercatcher",
    # "Pluvialis squatarola_Black-bellied Plover",
    "Charadrius vociferus_Killdeer",
    # "Charadrius semipalmatus_Semipalmated Plover",
    # "Numenius phaeopus_Whimbrel",
    # "Arenaria interpres_Ruddy Turnstone",
    # "Calidris alba_Sanderling",
    # "Calidris alpina_Dunlin",
    # "Calidris pusilla_Semipalmated Sandpiper",
    # "Calidris minutilla_Least Sandpiper",
    # "Limnodromus griseus_Short-billed Dowitcher",
    # "Actitis macularius_Spotted Sandpiper",
    # "Tringa solitaria_Solitary Sandpiper",
    # "Tringa semipalmata_Willet",
    # "Tringa melanoleuca_Greater Yellowlegs",
    # "Tringa flavipes_Lesser Yellowlegs",
    # "Rynchops niger_Black Skimmer",
    # "Leucophaeus atricilla_Laughing Gull",
    # "Larus delawarensis_Ring-billed Gull",
    # "Larus argentatus_Herring Gull",
    # "Larus marinus_Great Black-backed Gull",
    # "Sternula antillarum_Least Tern",
    # "Sterna hirundo_Common Tern",
    # "Sterna forsteri_Forster's Tern",
    # "Thalasseus maximus_Royal Tern",
    # "Thalasseus sandvicensis_Sandwich Tern",
    # "Gavia immer_Common Loon",
    # "Mycteria americana_Wood Stork",
    # "Anhinga anhinga_Anhinga",
    # "Nannopterum auritum_Double-crested Cormorant",
    # "Pelecanus occidentalis_Brown Pelican",
    # "Ardea herodias_Great Blue Heron",
    # "Ardea alba_Great Egret",
    # "Egretta thula_Snowy Egret",
    # "Egretta caerulea_Little Blue Heron",
    # "Egretta tricolor_Tricolored Heron",
    # "Bubulcus ibis_Cattle Egret",
    # "Butorides virescens_Green Heron",
    # "Eudocimus albus_White Ibis",
    # "Plegadis falcinellus_Glossy Ibis",
    # "Coragyps atratus_Black Vulture",
    # "Cathartes aura_Turkey Vulture",
    # "Pandion haliaetus_Osprey",
    # "Haliaeetus leucocephalus_Bald Eagle",
    # "Accipiter cooperii_Cooper's Hawk",
    # "Buteo lineatus_Red-shouldered Hawk",
    # "Buteo jamaicensis_Red-tailed Hawk",
    "Strix varia_Barred Owl",
    # "Megaceryle alcyon_Belted Kingfisher",
    "Melanerpes erythrocephalus_Red-headed Woodpecker",
    "Melanerpes carolinus_Red-bellied Woodpecker",
    "Dryobates pubescens_Downy Woodpecker",
    "Dryobates villosus_Hairy Woodpecker",
    "Colaptes auratus_Northern Flicker",
    "Dryocopus pileatus_Pileated Woodpecker",
    # "Falco sparverius_American Kestrel",
    "Contopus virens_Eastern Wood-Pewee",
    "Empidonax virescens_Acadian Flycatcher",
    "Sayornis phoebe_Eastern Phoebe",
    "Myiarchus crinitus_Great Crested Flycatcher",
    "Tyrannus tyrannus_Eastern Kingbird",
    "Vireo griseus_White-eyed Vireo",
    "Vireo flavifrons_Yellow-throated Vireo",
    # "Vireo solitarius_Blue-headed Vireo",
    "Vireo olivaceus_Red-eyed Vireo",
    "Cyanocitta cristata_Blue Jay",
    "Corvus brachyrhynchos_American Crow",
    "Corvus ossifragus_Fish Crow",
    # "Poecile atricapillus_Black-capped Chickadee",
    "Poecile carolinensis_Carolina Chickadee",
    "Baeolophus bicolor_Tufted Titmouse",
    # "Progne subis_Purple Martin",
    # "Tachycineta bicolor_Tree Swallow",
    # "Stelgidopteryx serripennis_Northern Rough-winged Swallow",
    # "Hirundo rustica_Barn Swallow",
    # "Corthylio calendula_Ruby-crowned Kinglet",
    # "Sitta canadensis_Red-breasted Nuthatch",
    "Sitta carolinensis_White-breasted Nuthatch",
    "Sitta pusilla_Brown-headed Nuthatch",
    "Polioptila caerulea_Blue-gray Gnatcatcher",
    "Troglodytes aedon_House Wren",
    "Thryothorus ludovicianus_Carolina Wren",
    # "Sturnus vulgaris_European Starling",
    "Dumetella carolinensis_Gray Catbird",
    "Toxostoma rufum_Brown Thrasher",
    "Mimus polyglottos_Northern Mockingbird",
    "Sialia sialis_Eastern Bluebird",
    # "Catharus ustulatus_Swainson's Thrush",
    # "Catharus guttatus_Hermit Thrush",
    "Hylocichla mustelina_Wood Thrush",
    "Turdus migratorius_American Robin",
    # "Bombycilla cedrorum_Cedar Waxwing",
    # "Passer domesticus_House Sparrow",
    "Haemorhous mexicanus_House Finch",
    "Spinus tristis_American Goldfinch",
    "Spizella passerina_Chipping Sparrow",
    "Spizella pusilla_Field Sparrow",
    "Passerculus sandwichensis_Savannah Sparrow",
    # "Ammospiza maritima_Seaside Sparrow",
    "Melospiza melodia_Song Sparrow",
    "Melospiza georgiana_Swamp Sparrow",
    "Zonotrichia albicollis_White-throated Sparrow",
    "Pipilo erythrophthalmus_Eastern Towhee",
    "Icteria virens_Yellow-breasted Chat",
    # "Dolichonyx oryzivorus_Bobolink",
    "Sturnella magna_Eastern Meadowlark",
    "Icterus spurius_Orchard Oriole",
    # "Icterus galbula_Baltimore Oriole",
    "Agelaius phoeniceus_Red-winged Blackbird",
    "Molothrus ater_Brown-headed Cowbird",
    # "Quiscalus quiscula_Common Grackle",
    # "Quiscalus major_Boat-tailed Grackle",
    "Seiurus aurocapilla_Ovenbird",
    # "Helmitheros vermivorum_Worm-eating Warbler",
    "Parkesia motacilla_Louisiana Waterthrush",
    # "Parkesia noveboracensis_Northern Waterthrush",
    "Mniotilta varia_Black-and-white Warbler",
    "Protonotaria citrea_Prothonotary Warbler",
    "Limnothlypis swainsonii_Swainson's Warbler",
    "Geothlypis trichas_Common Yellowthroat",
    "Setophaga citrina_Hooded Warbler",
    # "Setophaga ruticilla_American Redstart",
    # "Setophaga tigrina_Cape May Warbler",
    "Setophaga americana_Northern Parula",
    # "Setophaga magnolia_Magnolia Warbler",
    # "Setophaga petechia_Yellow Warbler",
    # "Setophaga pensylvanica_Chestnut-sided Warbler",
    # "Setophaga caerulescens_Black-throated Blue Warbler",
    # "Setophaga palmarum_Palm Warbler",
    "Setophaga pinus_Pine Warbler",
    # "Setophaga coronata_Yellow-rumped Warbler",
    "Setophaga dominica_Yellow-throated Warbler",
    "Setophaga discolor_Prairie Warbler",
    # "Setophaga virens_Black-throated Green Warbler",
    # "Setophaga striata_Blackpoll Warbler",
    "Piranga rubra_Summer Tanager",
    "Piranga olivacea_Scarlet Tanager",
    "Cardinalis cardinalis_Northern Cardinal",
    # "Pheucticus ludovicianus_Rose-breasted Grosbeak",
    "Passerina caerulea_Blue Grosbeak",
    "Passerina cyanea_Indigo Bunting"
    # "Passerina ciris_Painted Bunting"
  )
  
# Filter the full list 
psb_birds <- all_birds %>% 
  filter(full_code %in% psb_bird_codes) %>% 
  arrange(class, order, family, genus, species)

# View
slice_head(psb_birds, n = Inf)

# How many birds?
nrow(psb_birds)

################################################################################
# 3) Save the data #############################################################
################################################################################

# Define a working directory
wd <- "C:/NCSU/Disertation_Code/BirdNet_GUI"
setwd(wd)

# Combine the lists
psb_species <- bind_rows(psb_birds, psb_frogs, psb_mammals, psb_anthro)
# View
glimpse(psb_species)
psb_species %>% 
  select(-full_code) %>% 
  slice_head(, n = Inf)

# Save the full list
write.csv(psb_species, "Data/possible_species.csv", row.names = FALSE)

# Save the full codes
writeLines(psb_species$full_code, "Data/possible_species.txt")
