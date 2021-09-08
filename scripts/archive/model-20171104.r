library(lme4)
rm(list = ls())

mon_data <- read.csv("H:\\Gabilan Turbidity TMDL\\data\\ceden_ntu_r3_2019-07-26.csv")
lc_by_huc <- read.csv("H:\\Gabilan Turbidity TMDL\\gis\\stream-classification\\tables\\lu_by_huc.csv")
mon_stations <- read.csv("H:\\Gabilan Turbidity TMDL\\gis\\stream-classification\\tables\\mon_stations.csv")

# NLCD Codes
# "11 Open Water"
# "21 Developed Open Space"
# "22 Developed, Low Intensity"
# "23 Developed, Medium Intensity"
# "24 Developed, High Intensity"
# "31 Barren (Rock/Sand/Clay)"
# "41 Deciduous Forest"
# "42 Evergreen Forest"
# "43 Mixed Forest"
# "52 Shrub/Scrub"
# "71 Grassland/Herbaceous"
# "81 Pasture/Hay"
# "82 Cultivated Crops"
# "90 Woody Wetlands"
# "95 Emergent Herbaceous Wetlands"

sq_m_to_acres <- 0.000247105
lc_by_huc$total_acres <- apply(lc_by_huc[, 3:ncol(lc_by_huc)], 1, sum) * sq_m_to_acres
lc_by_huc$human_acres <-  apply(lc_by_huc[, c(4:7, 14:15)], 1, sum) * sq_m_to_acres
lc_by_huc$human_perc <-  lc_by_huc$human_acres / lc_by_huc$total_acres
lc_by_huc$huc_10 <- substr(lc_by_huc$HUC_12, 1, 10)

lc_by_huc_10 <- aggregate(cbind(human_acres, total_acres) ~ huc_10, lc_by_huc, sum)
lc_by_huc_10$human_perc <-  lc_by_huc_10$human_acres / lc_by_huc_10$total_acres

mon_stations$huc_10 <- substr(mon_stations$HUC_12, 1, 10)

mon_data_trim <- mon_data[c("StationCode",  "SampleDate",  "Result")]
data_dry <- mon_data_trim[
  as.numeric(substr(mon_data_trim$SampleDate, 6, 7)) < 10 & 
  as.numeric(substr(mon_data_trim$SampleDate, 6, 7)) > 4, ]

data_mon_stations <-  merge(data_dry, mon_stations)[c(1:3, 8:9, 11)]
# data_lc <- merge(data_mon_stations, lc_by_huc)
data_lc <- merge(data_mon_stations, lc_by_huc_10)
data <- data_lc[c("Result", "Q0001E", "V0001E", "human_perc", "huc_10")]

mod_mm <- lmer(Result ~ Q0001E + human_perc + (1 | huc_10), data = data)
summary(mod_mm)

mod_lm <- lm(Result ~ Q0001E + human_perc, data = data)
summary(mod_lm)
max(data$human_perc)
intercept <- mod_lm$coefficients[1]
flow <- mod_lm$coefficients[2]
human <- mod_lm$coefficients[3]
intercept + flow * 10 + human * 60

head(data)
huc_10s <- rep(unique(data$huc_10), each = 10)
human_percs <- rep(seq(0, 100, length.out = 10), length = length(huc_10s))
Qs <- rep(1, length = length(huc_10s))
data_pred <- data.frame("huc_10" = huc_10s, "human_perc" = human_percs, "Q0001E" = Qs, stringsAsFactors = F)
pred_y <- predict(mod, newdata = data_pred)
pred_x <- data_pred$human_perc
plot(pred_y ~ pred_x, type = "l")
plot(sort(data$Result) ~ seq(1, nrow(data)))
