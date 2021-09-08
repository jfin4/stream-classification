options(max.print=1000)
rm(list =ls())
today <- format(Sys.Date(), "%Y%m%d")

get_upstream_comids <- function(comid) {
    fnode <- stream_net$FromNode[which(stream_net$ComID == comid)]
    stream_net$ComID[which(stream_net$ToNode == fnode)]
}

build_network <- function(comid, network = NULL) {
    up_comids <- get_upstream_comids(comid)
    network <- c(comid, up_comids)
    while (length(up_comids) > 0) {
        up_comids <- unique(unlist(lapply(up_comids, get_upstream_comids)))
        up_comids <- up_comids[!(up_comids %in% network)]
        network <- c(network, up_comids)
    }
    return(network)
}

get_impact <- function(comid) {
    network <- networks[[match(comid, names(networks))]]
    network_lu <- lu_table[match(network, lu_table$comid), ]
    sum(network_lu$human, na.rm = T) / sum(network_lu$total_area, na.rm = T)
}

data_dir <- "H:\\gabilan-turbidity-tmdl\\data\\"
cowardin_xwalk <- read.csv(paste0(data_dir, "cowardin-xwalk.csv"))
stream_net <- read.csv(paste0(data_dir, "stream-network.csv"))

lu_table <- read.csv(paste0(data_dir, "lu-table.csv"))
lu_table <- lu_table[-match("OBJECTID", names(lu_table))]
names(lu_table) <- c("comid", "11 Open Water", "21 Developed Open Space",
    "22 Developed, Low Intensity", "23 Developed, Medium Intensity",
    "24 Developed, High Intensity", "31 Barren (Rock/Sand/Clay)",
    "41 Deciduous Forest", "42 Evergreen Forest", "43 Mixed Forest",
    "52 Shrub/Scrub", "71 Grassland/Herbaceous", "81 Pasture/Hay",
    "82 Cultivated Crops", "90 Woody Wetlands",
    "95 Emergent Herbaceous Wetlands")
human <- c("21 Developed Open Space", "22 Developed, Low Intensity", 
           "23 Developed, Medium Intensity", "24 Developed, High Intensity",
           "81 Pasture/Hay", "82 Cultivated Crops")
lu_table$total_area <- apply(lu_table[-1], 1, sum)
lu_table$human_area <- apply(lu_table[human], 1, sum)

mon_stats <- read.csv(paste0(data_dir, "mon-stations-edits.csv"))
mon_stats <- mon_stats[!is.na(mon_stats$COMID), ]

comids <- unique(mon_stats$COMID)
networks <- lapply(comids, build_network)
names(networks) <- comids
mon_stats$impact <- sapply(mon_stats$COMID, get_impact)
mon_stats$stream_order <- stream_net$StreamOrde[match(mon_stats$COMID, stream_net$ComID)]
mon_stations <- merge(mon_stats, cowardin_xwalk, by = "ATTRIBUTE")
# write.csv(mon_stations, paste0(data_dir, "mon-stations-classified-", today, ".csv"))
head(mon_stations)

# create networks only for monitored streams (for map)
mon_stations <- mon_stations[!is.na(mon_stations$stream_order), ]
mon_stations$order <- NA
mon_stations$order[mon_stations$stream_order <= 3] <- "head"
mon_stations$order[mon_stations$stream_order > 3] <- "medium"
mon_stations$class <- paste(mon_stations$order, mon_stations$slope, 
                            mon_stations$substrate, sep = "-")

mon_comids <- mon_stations$COMID[mon_stations$class == "head-low-unconsolidated"  
                        | mon_stations$class == "head-low-vegetated"
                        | mon_stations$class == "medium-low-unconsolidated"]
mon_networks <- lapply(mon_comids, build_network)
mon_networks <- unique(unlist(mon_networks))
mon_networks <- data.frame(comid = mon_networks, monitored = 1)
# write.csv(mon_networks, paste0(data_dir, "mon-networks-", today, ".csv"))
