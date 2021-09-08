options(max.print=1000)
rm(list =ls())
today <- format(Sys.Date(), "%Y%m%d")

data_dir <- 'R:\\RB3\\Shared\\TMDL_Wtrshd Assess\\TMDL_Projects\\Salinas River\\Turbidity_TMDL\\0 Work In Progress\\3 - data analysis\\CEDEN_R3\\'
out_dir <- 'H:\\gabilan-turbidity-tmdl\\tables\\'
petes_data <- # downloaded from CEDEN on 5/16/2019
    read.csv(paste0(data_dir, 'ceden_data_20190516_Turbidity.csv'))
names(petes_data)
petes_data <- # keep only relevant fields
    petes_data[c("StationCode", "SampleDate", "CollectionTime", "Result", "QACode")]
petes_data <- # remove NAs
    petes_data[!is.na(petes_data$Result), ]
steves_data <- # downloaded from CEDEN on 9/4/2019
    read.csv(paste0(data_dir, 'ceden_ntu_r3_2019-07-26.csv'))
steves_data <- # keep only relevant fields
    steves_data[c("StationCode", "SampleDate", "CollectionTime", "Result", "QACode")]
steves_data <- # remove NAs
    steves_data[!is.na(steves_data$Result), ]

petes_data$SampleDate <- 
    strftime(as.Date(petes_data$SampleDate, format = "%m/%d/%Y"), format = "%Y%m%d")
petes_data$CollectionTime <- 
    strftime(as.POSIXlt(petes_data$CollectionTime, format = "%H:%M:%S"), format = "%H%M%S")
steves_data$SampleDate <- 
    strftime(as.Date(steves_data$SampleDate, format = "%Y-%m-%d"), format = "%Y%m%d")
steves_data$CollectionTime <- 
    strftime(as.POSIXlt(steves_data$CollectionTime, format = "%H:%M:%S"), format = "%H%M%S")

petes_data$sample_id <- paste0(petes_data$StationCode, "-", petes_data$SampleDate, petes_data$CollectionTime)
steves_data$sample_id <- paste0(steves_data$StationCode, "-", steves_data$SampleDate, steves_data$CollectionTime)
join <- merge(petes_data, steves_data, "sample_id", suffixes = c("_petes", "_steves"), all = TRUE)
not_in_both <- join[which(is.na(join$Result_petes) | is.na(join$Result_steves)), ]
# write.csv(not_in_both, paste0(out_dir, "data-comparison-", today, ".csv"))

