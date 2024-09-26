#Prepare workspace
rm(list = ls())
setwd("/Volumes/Seagate Exp/Full PLS Data 2.1")
library(sf)
library(dplyr)

#Load and merge datasets downloaded from https://paleon.geography.wisc.edu/doku.php/data_and_products:settlement_vegetation
IL <- read.csv("ndilpls_v2.1.csv")
IL <- IL[,c(2, 3, 34, 44, 54, 64)]
IN <- read.csv("ndinpls_v2.1.csv")
IN <- IN[,c(2, 3, 34, 44, 54, 64)]
Marlin <- read.csv("Marlin_IL_v2.0.csv")
Marlin <- Marlin[,c(2, 3, 34, 44, 54, 64)]
data <- rbind(IL, IN, Marlin)

data.3 <- unique(data[,3])
data.4 <- unique(data[,4])
data.5 <- unique(data[,5])
data.6 <- unique(data[,6])
forest_taxa <- unique(c(data.3, data.4, data.5, data.6))
not_forest <- c("Oak", "Hickory", "No tree", "Water", "Wet", NA, "88888", "No data", "Unknown tree")
forest_taxa <- forest_taxa[!forest_taxa %in% not_forest]
undef_taxa <- c("Water", "Wet", "88888", "No data", "Unknown tree")
for (i in 1:nrow(data)){
  if ("No tree" %in% data[i,3]){
    data$Ecosystem[i] <- "Prairie"
  }
  else if (apply(data[i,3:6], 1, function(row) any(row %in% forest_taxa))){
    data$Ecosystem[i] <- "Forest"
  }
  else if (apply(data[i,3:6], 1, function(row) any(row %in% undef_taxa))){
    data$Ecosystem[i] <- "Undefined"
  }
  else if (is.na(data[i, 3]) == T && is.na(data[i, 4]) == T && is.na(data[i, 5]) == T && is.na(data[i, 6]) == T){
    data$Ecosystem[i] <- "Undefined"
  }
  else{
    data$Ecosystem[i] <- "Savanna"
  }
}

data <- subset(data, Ecosystem != "Undefined")
save(data, file = "Full_ecosystem_data.RData")


#Plot the ecosystem data
setwd("/Volumes/Seagate Exp/Full PLS Data 2.1")
load("Full_ecosystem_data.RData")

states <- sf::st_as_sf(maps::map('state', region = c('illinois', 'indiana'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:4326')

# Make into sf object to convert CRS
data_sf <- sf::st_as_sf(data, coords = c('x', 'y'))
# Add current CRS
sf::st_crs(data_sf) <- 'EPSG:3175'
# Convert to new CRS
data_sf <- sf::st_transform(data_sf, crs = 'EPSG:4326')

#Just plot without management areas first
data_sf |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(color = Ecosystem), shape = '.', alpha = 0.7) +
  ggplot2::scale_color_manual(values = c('Prairie' = '#bb5566', 'Savanna' = '#ddaa34', 'Forest' = '#002a53')) +
  ggplot2::labs(color = 'Ecosystem State') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Ecosystem distributions')


#Load shapefiles of management areas
setwd("/Volumes/Seagate Exp/Management Areas")

IL_Forest1 <- st_read("IL_Forest1_Aggregate.shp")
IL_Forest1 <- sf::st_transform(IL_Forest1, crs = 'EPSG:4326')
IL_Forest1 <- st_union(IL_Forest1)

IL_Mixed1 <- st_read("IL_Mixed1_Aggregate.shp")
IL_Mixed1 <- sf::st_transform(IL_Mixed1, crs = 'EPSG:4326')
#IL_Mixed1 <- st_union(IL_Mixed1)

IL_Prairie1 <- st_read("IL_Prairie1_Aggregate.shp")
IL_Prairie1 <- sf::st_transform(IL_Prairie1, crs = 'EPSG:4326')
IL_Prairie1 <- st_union(IL_Prairie1)

IL_Prairie2 <- st_read("IL_Prairie2_Aggregate.shp")
IL_Prairie2 <- sf::st_transform(IL_Prairie2, crs = 'EPSG:4326')
IL_Prairie2 <- st_union(IL_Prairie2)

IL_Prairie3 <- st_read("IL_Prairie3_Aggregate.shp")
IL_Prairie3 <- sf::st_transform(IL_Prairie3, crs = 'EPSG:4326')
IL_Prairie3 <- st_union(IL_Prairie3)

IL_River1 <- st_read("IL_River1_Aggregate.shp")
IL_River1 <- sf::st_transform(IL_River1, crs = 'EPSG:4326')
#IL_River1 <- st_union(IL_River1)

IL_River2 <- st_read("IL_River2_Aggregate.shp")
IL_River2 <- sf::st_transform(IL_River2, crs = 'EPSG:4326')
#IL_River2 <- st_union(IL_River2)

IL_Shawnee <- st_read("IL_Shawnee_Aggregate.shp")
IL_Shawnee <- sf::st_transform(IL_Shawnee, crs = 'EPSG:4326')
#IL_Shawnee <- st_union(IL_Shawnee)

IL_Small1 <- st_read("IL_Small1_Aggregate.shp")
IL_Small1 <- sf::st_transform(IL_Small1, crs = 'EPSG:4326')
#IL_Small1 <- st_union(IL_Small1)

IL_Small2 <- st_read("IL_Small2_Aggregate.shp")
IL_Small2 <- sf::st_transform(IL_Small2, crs = 'EPSG:4326')
IL_Small2 <- st_union(IL_Small2)

IL_Small3 <- st_read("IL_Small3_Aggregate.shp")
IL_Small3 <- sf::st_transform(IL_Small3, crs = 'EPSG:4326')
IL_Small3 <- st_union(IL_Small3)

IL_StudyArea <- st_read("IL_StudyArea_Aggregate.shp")
IL_StudyArea <- sf::st_transform(IL_StudyArea, crs = 'EPSG:4326')
#IL_StudyArea <- st_union(IL_StudyArea)

IN_Dunes <- st_read("IN_Dunes_Aggregate.shp")
IN_Dunes <- sf::st_transform(IN_Dunes, crs = 'EPSG:4326')
#IN_Dunes <- st_union(IN_Dunes)

IN_Forest1 <- st_read("IN_Forest1_Aggregate.shp")
IN_Forest1 <- sf::st_transform(IN_Forest1, crs = 'EPSG:4326')
IN_Forest1 <- st_union(IN_Forest1)

IN_Forest2 <- st_read("IN_Forest2_Aggregate.shp")
IN_Forest2 <- sf::st_transform(IN_Forest2, crs = 'EPSG:4326')
#IN_Forest2 <- st_union(IN_Forest2)

IN_Forest3 <- st_read("IN_Forest3_Aggregate.shp")
IN_Forest3 <- sf::st_transform(IN_Forest3, crs = 'EPSG:4326')
IN_Forest3 <- st_union(IN_Forest3)

IN_Forest4 <- st_read("IN_Forest4_Aggregate.shp")
IN_Forest4 <- sf::st_transform(IN_Forest4, crs = 'EPSG:4326')
IN_Forest4 <- st_union(IN_Forest4)

IN_HoosierNorth <- st_read("IN_HoosierNorth_Aggregate.shp")
IN_HoosierNorth <- sf::st_transform(IN_HoosierNorth, crs = 'EPSG:4326')
#IN_HoosierNorth <- st_union(IN_HoosierNorth)

IN_HoosierSouth <- st_read("IN_HoosierSouth_Aggregate.shp")
IN_HoosierSouth <- sf::st_transform(IN_HoosierSouth, crs = 'EPSG:4326')
IN_HoosierSouth <- st_union(IN_HoosierSouth)

IN_Indianapolis <- st_read("IN_Indianapolis_Aggregate.shp")
IN_Indianapolis <- sf::st_transform(IN_Indianapolis, crs = 'EPSG:4326')
IN_Indianapolis <- st_union(IN_Indianapolis)

IN_NE <- st_read("NE_IN_Townships.shp")
IN_NE <- sf::st_transform(IN_NE, crs = 'EPSG:4326')
IN_NE <- st_union(IN_NE)

IN_Prairie1 <- st_read("IN_Prairie1_Aggregate.shp")
IN_Prairie1 <- sf::st_transform(IN_Prairie1, crs = 'EPSG:4326')
IN_Prairie1 <- st_union(IN_Prairie1)


#Plot all management areas
data_sf |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(color = Ecosystem), shape = '.', alpha = 0.7) +
  ggplot2::scale_color_manual(values = c('Prairie' = '#bb5566', 'Savanna' = '#ddaa34', 'Forest' = '#002a53')) +
  ggplot2::labs(color = 'Ecosystem State') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Forest1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Mixed1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Prairie1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Prairie2, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Prairie3, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_River1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_River2, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Shawnee, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Small1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Small2, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Small3, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_StudyArea, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest2, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest3, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest4, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_HoosierNorth, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_HoosierSouth, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Indianapolis, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_NE, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Prairie1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Dunes, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Ecosystem distributions')

#Plot management areas coded by OOS/IS
data_sf |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(color = Ecosystem), shape = '.', alpha = 0.7) +
  ggplot2::scale_color_manual(values = c('Prairie' = '#bb5566', 'Savanna' = '#ddaa34', 'Forest' = '#002a53')) +
  ggplot2::labs(color = 'Ecosystem State') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Forest1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Mixed1, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Prairie1, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Prairie2, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Prairie3, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_River1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_River2, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Shawnee, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Small1, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Small2, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Small3, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_StudyArea, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest1, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest2, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest3, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest4, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_HoosierNorth, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_HoosierSouth, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Indianapolis, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_NE, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Prairie1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Dunes, color = 'red', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Ecosystem distributions')



#Plot Simplified Taxon-Lvel
data_tx <- rbind(IL, IN, Marlin)
data_tx<- data_tx[, c("L3_tree1", "L3_tree2", "L3_tree3", "L3_tree4", "x", "y")]
taxa <- c("Oak", "Hickory", "Beech", "Maple", "No tree")
data_tx <- data_tx %>%
  dplyr::mutate(L3_tree1 = dplyr::if_else(L3_tree1 %in% taxa, L3_tree1, NA_character_))
for (i in 1:nrow(data_tx)){
  if (is.na(data_tx[i,1]) == T){
    data_tx$L3_tree1[i] <- data_tx$L3_tree2[i]
  }}
data_tx <- data_tx %>%
  dplyr::mutate(L3_tree1 = dplyr::if_else(L3_tree1 %in% taxa, L3_tree1, NA_character_))
for (i in 1:nrow(data_tx)){
  if (is.na(data_tx[i,1]) == T){
    data_tx$L3_tree1[i] <- data_tx$L3_tree3[i]
  }}
data_tx <- data_tx %>%
  dplyr::mutate(L3_tree1 = dplyr::if_else(L3_tree1 %in% taxa, L3_tree1, NA_character_))
for (i in 1:nrow(data_tx)){
  if (is.na(data_tx[i,1]) == T){
    data_tx$L3_tree1[i] <- data_tx$L3_tree4[i]
  }}
data_tx <- data_tx %>%
  dplyr::mutate(L3_tree1 = dplyr::if_else(L3_tree1 %in% taxa, L3_tree1, NA_character_))
data_tx <- data_tx %>%
  dplyr::filter(!is.na(L3_tree1))

data_tx <- sf::st_as_sf(data_tx, coords = c('x', 'y'))
# Add current CRS
sf::st_crs(data_tx) <- 'EPSG:3175'
# Convert to new CRS
data_tx <- sf::st_transform(data_tx, crs = 'EPSG:4326')
data_tx$L3_tree1 <- factor(data_tx$L3_tree1, levels=c("No tree", "Oak", "Hickory", "Beech", "Maple"))

#Plot Without Management Areas
data_tx |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(color = L3_tree1), shape = '.', alpha = 0.7) +
  ggplot2::scale_color_manual(values = c('No tree' = '#bb5566', 'Oak' = '#ddaa34', 'Hickory' = '#ecd08f', 'Beech' = '#4c7cac', 'Maple' = '#005f5f')) +
  ggplot2::labs(color = 'Taxon') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Taxon distributions')

#Plot With All Management Areas
data_tx |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(color = L3_tree1), shape = '.', alpha = 0.7) +
  ggplot2::scale_color_manual(values = c('No tree' = '#bb5566', 'Oak' = '#ddaa34', 'Hickory' = '#ecd08f', 'Beech' = '#4c7cac', 'Maple' = '#005f5f')) +
  ggplot2::labs(color = 'Taxon') +
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 16, size = 7))) +
  ggplot2::theme_void() +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Forest1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Mixed1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Prairie1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Prairie2, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Prairie3, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_River1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_River2, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Shawnee, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Small1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Small2, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_Small3, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IL_StudyArea, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest2, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest3, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Forest4, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_HoosierNorth, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_HoosierSouth, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Indianapolis, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_NE, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Prairie1, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::geom_sf(data = IN_Dunes, color = 'blue', fill = NA, linewidth = 1) +
  ggplot2::coord_sf(crs = 'EPSG:4326') +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = 'bold', hjust = 0.5),
                 legend.title = ggplot2::element_text(size = 12),
                 legend.text = ggplot2::element_text(size = 12)) +
  ggplot2::ggtitle('Taxon distributions')

