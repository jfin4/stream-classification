rm(list = ls())
wd <- "H:\\gabilan-turbidity-tmdl\\stream-classification\\"

mon_stations <- read.csv(paste(wd, "gis-stream-classification\\tables\\mon_stations.csv", sep = "\\"))
mon_stations <- mon_stations[c("StationCode", "COMID", "GNIS_NAME", "StreamOrde", "SLOPE", "Q0001E")]
names(mon_stations) <- c("station", "comid", "name", "so", "m", "q")
mon_stations <- mon_stations[is.na(mon_stations$m) == FALSE , ]
mon_stations[c("m_class", "q_class")] <- NA
m_breaks <- c(0, 0.005, 0.02, 0.04, 0.10, 1)
q_breaks <- c(0, 1, 10, 100, 1000)
lapply(1:(length(m_breaks) - 1), 
       function(x) {
            mon_stations$m_class[which(  mon_stations$m >= m_breaks[x] 
                                       & mon_stations$m <  m_breaks[x + 1])] <<- x
       })
lapply(1:(length(q_breaks) - 1), 
       function(x) {
            mon_stations$q_class[which(  mon_stations$q >= q_breaks[x] 
                                       & mon_stations$q <  q_breaks[x + 1])] <<- x
       })

mon_data <- read.csv(paste(wd, "gis-stream-classification\\tables\\ceden_ntu_r3_2019-07-26.csv", sep = "\\"))
mon_data <- mon_data[c("StationCode", "SampleDate", "Result")]
names(mon_data) <- c("station", "date", "result")
# mon_data <- # dry months
#     mon_data[which(as.numeric(substr(mon_data$date, 6, 7)) > 4 & as.numeric(substr(mon_data$date, 6, 7)) < 10), ]

sites <- c("309GAB", "309NAD", "309ALG", "309ALD", "309JON", "309TEH", "309TEM", "309TDW", "309OLD", "309ASB", "309MER", "309ESP", "309RTA")
site_data <- droplevels(mon_data[mon_data$station %in% sites, ])
q25 <- tapply(X = site_data$result, INDEX = site_data$station, FUN = quantile, 0.25)
q50 <- tapply(X = site_data$result, INDEX = site_data$station, FUN = quantile, 0.5)
q75 <- tapply(X = site_data$result, INDEX = site_data$station, FUN = quantile, 0.75)
n <- tapply(X = site_data$result, INDEX = site_data$station, FUN = length)
results_tab <- cbind(n, q25, q50, q75)
write.csv(results_tab, "results_tab.csv")

