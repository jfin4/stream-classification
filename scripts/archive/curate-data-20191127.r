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
data_dir <- 'R:\\RB3\\Shared\\TMDL_Wtrshd Assess\\TMDL_Projects\\Salinas River\\Turbidity_TMDL\\0 Work In Progress\\3 - data analysis\\CEDEN_R3\\'
data <- # downloaded from CEDEN on 5/16/2019
    read.csv(paste0(data_dir, 'ceden_data_20190516_Turbidity.csv'))
data <- # remove NAs
    data[!is.na(data$Result), ]

# Remove rows with "Failed" QAQC flags
failed <- # rows with "failed" flags
    which(substr(as.character(data$QACode), 1, 1) == "F")
data <- # remove rows with "failed" flags 
    data[-failed, ]

# Deal with duplicates
data$SampleDate_f <- # format date for unique id
    strftime(as.Date(data$SampleDate, format = "%m/%d/%Y"), format = "%Y%m%d")
data$CollectionTime_f <- # format time for unique id
    strftime(as.POSIXlt(data$CollectionTime, format = "%H:%M:%S"), format = "%H%M%S")
data$sample_id <- # create unique id for each sampling event
    paste0(data$StationCode, "-", data$SampleDate_f, data$CollectionTime_f)
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
data_out <- 
    rbind(uniques, lab_samples, not_labs_mean, other_mean) 
data_out <- 
    data_out[-match(c("SampleDate_f", "CollectionTime_f", "row", "sample_id"), names(data_out))]

# Export table to shared drive and personal drive 
write.csv(data_out, paste0(data_dir, "ceden_data_", today, "_Turbidity_curated.csv"), row.names = F)
out_dir <- 'H:\\gabilan-turbidity-tmdl\\tables\\'
write.csv(data_out, paste0(out_dir, "ceden_turbidity_", today, ".csv"), row.names = F)

