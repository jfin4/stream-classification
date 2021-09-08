options(max.print = 1000)
rm(list = ls())
library(ggplot2)

data <- 
    read.csv(paste0("H:\\gabilan-turbidity-tmdl\\data\\",
                    "ceden_data_20191202_Turbidity_curated.csv"))

data$month <- # dry months only
    as.numeric(strftime(as.Date(data$SampleDate, format = "%m/%d/%Y"), 
                        format = "%m"))
data <- 
    data[data$month > 4 & data$month < 10, ]

rest_codes <- # only restoration sites
    c("309GAB", "309NAD", "309ALG", "309ALD", "309JON", "309TEH", "309TEM", 
      "309TDW", "309OLD", "309ASB", "309MER", "309ESP", "309RTA")
data <- 
    data[which(!is.na(match(data$StationCode, rest_codes))), ]

data$date <- 
    as.Date(data$SampleDate, format = "%m/%d/%Y")
data$year <- 
    strftime(as.Date(data$SampleDate, format = "%m/%d/%Y"), format = "%Y")

breaks <- as.Date(c("2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01",
                    "2020-01-01"))
ggplot(data, aes(x = date, y = Result)) + geom_point() +
    facet_wrap( ~ StationCode, nrow = 7) + xlab("Sample Date") + ylab("NTU") +
    scale_x_date(breaks = breaks, date_labels = "%Y", 
                 date_minor_breaks = "1 year") +
    scale_y_continuous(limits = c(0, 3000))

# ggsave(paste0("H:\\gabilan-turbidity-tmdl\\graphics\\", 
#                 "turbidity-time-series-dry-6x9.png"),
#        width = 6, height = 9, units = "in")

data$date[data$Result == 3000]
data$Result[data$date == "2015-08-26"]
