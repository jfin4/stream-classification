rm(list = ls())

data <- read.csv(file.path(
  "R:",
  "RB3",
  "Shared",
  "TMDL_Wtrshd Assess",
  "TMDL_Projects",
  "Salinas River",
  "Turbidity_TMDL",
  "0 Work In Progress",
  "3 - data analysis",
  "CEDEN_R3",
  "ceden_ntu_r3_2019-07-26.csv",
  fsep = "\\"
))
names(data)
head(data)

data <- data[c("StationCode", 
         "waterbody_type",
         "TargetLatitude",
         "TargetLongitude",
         "SampleDate", 
         "CollectionTime", 
         "Result", 
         "ResultsComments"
)]

data <- data[order(data$Result, decreasing = T), ]
months <- as.integer(substr(data$SampleDate, 6, 7))
dry_months <- months >= 5 & months <= 9
data_dry <- data[dry_months, ]
data_wet <- data[!dry_months, ]
quant25 <- quantile(data$Result, 0.25)
quant25_dry <- quantile(data_dry$Result, 0.25)
quant25_wet <- quantile(data_wet$Result, 0.25)

ylim = 5000
hist <- hist(log10(data$Result), 
             axes = F, 
             xlab = "NTU",
             ylim = c(0, ylim), 
             main = "")
axis(1, at = hist$breaks, labels = 10^hist$breaks)
axis(2)
text(x = median(hist$breaks), 
     y = ylim  * 1.10,
     xpd = TRUE,
     adj = c(0.5, 1), 
     labels = "Histogram of All Region 3 Turbidity Observations")
text(x = log(quant25),
     y = ylim * 0.95,
     xpd = TRUE,
     col = "red",
     adj = c(0.5, 0),
     labels = paste("25th Percentile = ", quant25))
lines(c(log(quant25), log(quant25)), c(0, ylim * 0.9), col = "red")

hist_dry <- hist(log10(data_dry$Result), 
             breaks = hist$breaks,
             ylim = c(0, ylim),
             axes = F, 
             xlab = "NTU",
             main = "",
             xpd = TRUE)
axis(1, at = hist_dry$breaks, labels = 10^hist_dry$breaks)
axis(2)
text(x = median(hist$breaks), 
     y = max(hist$counts) * 1.10, 
     xpd = TRUE,
     adj = c(0.5, 1), 
     labels = "Histogram of All Region 3 Turbidity Observations\nDry Months")
text(x = log(quant25_dry),
     y = ylim * 0.5,
     xpd = TRUE,
     col = "red",
     adj = c(0.5, 0),
     labels = paste("25th Percentile = ", quant25_dry))
lines(c(log(quant25_dry), log(quant25_dry)), c(0, ylim * 0.45), col = "red")


hist_wet <- hist(log10(data_wet$Result), 
             breaks = hist$breaks,
             ylim = c(0, max(hist$counts)),
             axes = F, 
             xlab = "NTU",
             main = "",
             xpd = TRUE)
axis(1, at = hist_wet$breaks, labels = 10^hist_wet$breaks)
axis(2)
text(x = median(hist$breaks), 
     y = max(hist$counts) * 1.10, 
     xpd = TRUE,
     adj = c(0.5, 1), 
     labels = "Histogram of All Region 3 Turbidity Observations\nWet Months")
text(x = log(quant25_wet),
     y = ylim * 0.5,
     xpd = TRUE,
     col = "red",
     adj = c(0.5, 0),
     labels = paste("25th Percentile = ", quant25_wet))
lines(c(log(quant25_wet), log(quant25_wet)), c(0, ylim * 0.45), col = "red")
