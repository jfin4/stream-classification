###############################################################################
#                               Curate CEDEN Data
#                          Gabilan Creek Turbidity TMDL
#                                   11/27/2019
#                         john.inman@waterboards.ca.gov
###############################################################################
options(max.print=1000)
rm(list =ls())
today <- format(Sys.Date(), "%Y%m%d")

# Pull in raw CEDEN download
data_dir <- 'H:\\gabilan-turbidity-tmdl\\data\\'
data <- # downloaded from CEDEN on 5/16/2019
    read.csv(paste0(data_dir, 'ceden-turbidity-raw.csv'))

# remove non-creeks, etc.
site_desc <- read.csv(paste0(data_dir, 'removed-sites.csv'))
rem_sites <- site_desc$StationCode[site_desc$Remove == 1]
data <- data[is.na(match(data$StationCode, rem_sites)), ]

# remove NAs
data <- data[!is.na(data$Result), ]

# Remove rows with "Failed" QAQC flags
# rows with "failed" flags
failed <- which(substr(as.character(data$QACode), 1, 1) == "F")
# remove rows with "failed" flags 
data <- data[-failed, ]

# Remove duplicates
data$date <- # format date for unique id
    as.Date(data$SampleDate, format = "%m/%d/%Y")
data$sample_id <- # create unique id for each sampling event
    paste0(data$StationCode, "-", data$date)
ids <- # get list of unique ids
    unique(sort(data$sample_id))
dupe_ids <- # get list of duplicate ids
    which(table(data$sample_id) > 1)
dupes <- # df of dupes
    data[data$sample_id %in% names(dupe_ids), ]
dupes <- # sorted 
    dupes[order(dupes$sample_id), ]
uniques <- # df without dupes
    data[!(data$sample_id %in% dupes$sample_id), ]

ccamp <- # df of ccamp dupes
    dupes[dupes$Program == "Surface Water Ambient Monitoring Program", ]
lab_samples <- # dupes due to lab samples (we are keeping these)
    ccamp[ccamp$Meth == "EPA 180.1", ]
not_labs <- # dupes not due to lab samples (we are avering these)
    ccamp[!(ccamp$sample_id %in% lab_samples$sample_id), ]
not_labs_mean <- # df of uniqe sample ids
    not_labs[match(unique(not_labs$sample_id), not_labs$sample_id), ]
not_labs_mean$Result <- # replace with averaged Result
    tapply(not_labs$Result, not_labs$sample_id, mean)

other <- # df of non-ccamp dupes
    dupes[!(dupes$Program == "Surface Water Ambient Monitoring Program"), ]
other <-  # sorted
    other[order(other$sample_id), ]
other_mean <- # df of unique sample ids
    other[match(unique(other$sample_id), other$sample_id), ]
other_mean$Result <- # replace with averaged Result
    tapply(other$Result, other$sample_id, mean)

# Combine all
data <- rbind(uniques, lab_samples, not_labs_mean, other_mean) 
# remove tmp cols
data <- data[-match(c("date", "sample_id"), names(data))]

# Export table to shared drive and personal drive 
# write.csv(data, paste0(data_dir, "ceden-turbidity.csv"), row.names = F)

