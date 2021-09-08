################################################################################
#                            Reproduce Summary Stats
#                          Gabilan Creek Turbidity TMDL
#                                   11/27/2019
#                         john.inman@waterboards.ca.gov
################################################################################

# Peter created some summary stats and graphics using uncurated data. Reproduce
# his summary stats table so he can regenerate his graphics with updated
# numbers

options(max.print=1000)
rm(list =ls())
today <- format(Sys.Date(), "%Y%m%d")

# Pull in curated CEDEN data
data_dir <- 'H:\\gabilan-turbidity-tmdl\\data\\'
data <- # downloaded from CEDEN on 5/16/2019
    read.csv(paste0(data_dir, 'ceden-turbidity.csv'))


% all data# {{{
date <- as.Date(data$SampleDate, format = "%m/%d/%Y")
month <- as.numeric(strftime(date, format = "%m"))
dry <- month > 4 & month < 10
datd <- data[dry, ]
datw <- data[!dry, ]

dats <- list(data, datd, datw)
names(dats) <- c("all", "dry", "wet")
sums <- lapply(1:length(dats), function(x) NA)
names(sums) <- names(dats)

for (i in 1:length(dats)) {
    dat <- dats[[i]]
    stacs <- 
        dats[[i]][, match("StationCode", names(dat))]
    results <- 
        dats[[i]][, match("Result", names(dat))]
    N <-
        tapply(stacs, stacs, length)
    StationCode <- 
        names(N)
    turb_median <- 
        tapply(results, stacs, median)
    turb_max <-
        tapply(results, stacs, max)
    turb_min <- 
        tapply(results, stacs, min)
    turb_quan1 <- 
        tapply(results, stacs, quantile, probs = 0.10)
    turb_quan25 <- 
        tapply(results, stacs, quantile, probs = 0.25)
    turb_quan5 <- 
        tapply(results, stacs, quantile, probs = 0.50)
    turb_quan75 <- 
        tapply(results, stacs, quantile, probs = 0.75)
    turb_quan9 <- 
        tapply(results, stacs, quantile, probs = 0.90)
    turb_mean <- 
        tapply(results, stacs, mean)
    N.gte.25 <-
        sapply(1:length(StationCode), function(i) sum(ifelse(results[stacs == StationCode[i]] >= 25, 1, 0)))
    pct.gte.25 <-
        N.gte.25 / N * 100
    N.gte.40 <-
        sapply(1:length(StationCode), function(i) sum(ifelse(results[stacs == StationCode[i]] >= 40, 1, 0)))
    pct.gte.40 <-
        N.gte.40 / N * 100
    sums[[i]] <- data.frame(StationCode, N, turb_median, turb_max, turb_min, turb_quan1, turb_quan25, turb_quan5, turb_quan75, turb_quan9, turb_mean, N.gte.25, pct.gte.25, N.gte.40, pct.gte.40)
}

# write.csv(sums$all, paste0(data_dir, "summary-stats-all-", today, ".csv"))
# write.csv(sums$dry, paste0(data_dir, "summary-stats-dry-", today, ".csv"))
# write.csv(sums$dry, paste0(data_dir, "summary-stats-wet-", today, ".csv"))
# }}}
% cmp data# {{{
data <- 
    data[data$ParentProject == "RWB3 Cooperative Monitoring Program", ]
data$StationCode <- factor(data$StationCode)

date <- as.Date(data$SampleDate, format = "%m/%d/%Y")
month <- as.numeric(strftime(date, format = "%m"))
dry <- month > 4 & month < 10
datd <- data[dry, ]
datw <- data[!dry, ]

dats <- list(data, datd, datw)
names(dats) <- c("all", "dry", "wet")
sums <- lapply(1:length(dats), function(x) NA)
names(sums) <- names(dats)

for (i in 1:length(dats)) {
    dat <- dats[[i]]
    stacs <- 
        dats[[i]][, match("StationCode", names(dat))]
    results <- 
        dats[[i]][, match("Result", names(dat))]
    N <-
        tapply(stacs, stacs, length)
    StationCode <- 
        names(N)
    turb_median <- 
        tapply(results, stacs, median)
    turb_max <-
        tapply(results, stacs, max)
    turb_min <- 
        tapply(results, stacs, min)
    turb_quan1 <- 
        tapply(results, stacs, quantile, probs = 0.10)
    turb_quan25 <- 
        tapply(results, stacs, quantile, probs = 0.25)
    turb_quan5 <- 
        tapply(results, stacs, quantile, probs = 0.50)
    turb_quan75 <- 
        tapply(results, stacs, quantile, probs = 0.75)
    turb_quan9 <- 
        tapply(results, stacs, quantile, probs = 0.90)
    turb_mean <- 
        tapply(results, stacs, mean)
    N.gte.25 <-
        sapply(1:length(StationCode), function(i) sum(ifelse(results[stacs == StationCode[i]] >= 25, 1, 0)))
    pct.gte.25 <-
        N.gte.25 / N * 100
    N.gte.40 <-
        sapply(1:length(StationCode), function(i) sum(ifelse(results[stacs == StationCode[i]] >= 40, 1, 0)))
    pct.gte.40 <-
        N.gte.40 / N * 100
    sums[[i]] <- data.frame(StationCode, N, turb_median, turb_max, turb_min, turb_quan1, turb_quan25, turb_quan5, turb_quan75, turb_quan9, turb_mean, N.gte.25, pct.gte.25, N.gte.40, pct.gte.40)
}

write.csv(sums$all, paste0(data_dir, "summary-stats-all-cmp-", today, ".csv"))
write.csv(sums$dry, paste0(data_dir, "summary-stats-dry-cmp-", today, ".csv"))
write.csv(sums$wet, paste0(data_dir, "summary-stats-wet-cmp-", today, ".csv"))
# }}}


