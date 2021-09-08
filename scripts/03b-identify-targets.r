options(max.print=1000)
rm(list =ls())
today <- format(Sys.Date(), "%Y%m%d")

get_ref_target <- function(class, turb_data) {
    class_codes <- ref_stations$station_co[ref_stations$class == class]
    results <- turb_data$Result[turb_data$StationCode %in% class_codes]
    quantile(results, probs = 0.75)
}

get_gen_target <- function(class, turb_data) {
    class_codes <- mon_stations$station_co[mon_stations$class == class]
    results <- turb_data$Result[turb_data$StationCode %in% class_codes]
    quantile(results, probs = 0.25)
}

data_dir <- "H:\\gabilan-turbidity-tmdl\\data\\"
mon_stations <- 
    read.csv(paste0(data_dir, "mon-stations-classified.csv"))
mon_stations <- mon_stations[!is.na(mon_stations$stream_order), ]
mon_stations$order <- NA
# https://www.thoughtco.com/what-is-stream-order-1435354
mon_stations$order[mon_stations$stream_order <= 3] <- "head"
mon_stations$order[mon_stations$stream_order > 3] <- "medium"
mon_stations$class <- paste(mon_stations$order, mon_stations$slope, 
                            mon_stations$substrate, sep = "-")

# for appendix
# mon_out <- mon_stations[c("station_co", "station_na", "class")]
# mon_summ <- as.data.frame(table(mon_out$class))
# write.csv(mon_out, paste0(data_dir, "mon-stations-classed-", today, ".csv"), row.names = FALSE)
# write.csv(mon_summ, paste0(data_dir, "mon-stations-summ-", today, ".csv"), row.names = FALSE)

# import turbidity data and subset by season
turb_all <- read.csv(paste0(data_dir, "ceden-turbidity.csv"))
date <- as.Date(turb_all$SampleDate, format = "%m/%d/%Y")
month <- strftime(date, format = "%m")
month <- as.numeric(month)
is_dry <- month > 4 & month < 10
turb_dry <- turb_all[is_dry, ]
turb_wet <- turb_all[!is_dry, ]

rest_names <- c("309GAB", "309NAD", "309ALG", "309ALD", "309JON", "309TEH", 
                "309TEM", "309TDW", "309OLD", "309ASB", "309MER", "309ESP", 
                "309RTA")
rest_stations <- mon_stations[mon_stations$station_co %in% rest_names, ]
rest_classes <- names(table(rest_stations$class))
table(rest_stations$class)

p25_impact <- quantile(mon_stations$impact, probs = 0.25)
ref_stations <- mon_stations[mon_stations$impact <= p25_impact, ]

# # for report
# ref_summ <- as.data.frame(table(ref_stations$class))
# write.csv(ref_summ, paste0(data_dir, "ref-stations-summ-", today, ".csv"), row.names = FALSE)

ref_targets_all <- 
    sapply(rest_classes, get_ref_target, turb_all)
gen_targets_all <- 
    sapply(rest_classes, get_gen_target, turb_all)
ref_targets_wet <- 
    sapply(rest_classes, get_ref_target, turb_wet)
gen_targets_wet <- 
    sapply(rest_classes, get_gen_target, turb_wet)
ref_targets_dry <- 
    sapply(rest_classes, get_ref_target, turb_dry)
gen_targets_dry <- 
    sapply(rest_classes, get_gen_target, turb_dry)


out <- data.frame(least_imp = c(ref_targets_all, ref_targets_wet, 
                                ref_targets_dry), 
                  gen = c(gen_targets_all, gen_targets_wet, gen_targets_dry))
name_sub <- gsub(".75%", "", names(ref_targets_dry))
out$class <- rep(name_sub, 3)
out$season <- rep(c("all", "wet", "dry"), each = 3)
# write.csv(out, paste0(data_dir, "targets-", today, ".csv"), row.names = FALSE)

# all reference
# '305SSCAUC', '312ALA', '309NAC', '312WE1028', '308LILPAL', '309ARCBRC', '310COO', '308BGC', '309SED071', '308ROCBBQ', '308BSU', '309NAF', '308MWCAH1', '307SCCARR', '309CAW050', '312WE1035', '312CCC', '312SBC', '304WAD', '310LSL', '312RYCALR', '309PS0043', '309CAW009', '309LAS1MI', '308WER325', '308LIM', '308LSRASC', '310USL', '304GAZ', '304APS', '308PS0156', '312CUY', '309PS0116', '309CAW174', '314WE0779', '314SYC', '309CAW178', '304PS0194', '312CUT', '312SQRABP', '312PS0099', '308CAW055', '307CMN', '307PS0028', '307CE0094', '304WDCAH1', '310AGB', '304SCM', '307CMU', '309CAW116', '309SAP', '309CAW194', '314WE0796', '312SIV', '312CAW031', '304CAW016', '310SCP', '309SEC', '304CAW153', '310LPCBPC', '308BSR', '308WLO', '308LSU', '308GAR', '307CMRADC', '305UVCASC', '309ARSARC', '305BRI', '310ADU', '309CAW130', '314SYP', '312SIS'

# least impacted
# ref_stations$station_co[ref_stations$class == "head-low-unconsolidated"  
#                         | ref_stations$class == "head-low-vegetated"
#                         | ref_stations$class == "medium-low-unconsolidated"]

# '304CAW016', '312CUY', '309SAP', '312WE1035', '312CAV', '312SBC', '304WAD', '312RYCALR', '309SARANF', '310ADU', '314WE0779', '312SIV', '309CAW178', '305LGCACR', '309CAW116', '309CAW050', '312DAVDAV', '312SIS', '305SSCAUC', '312CUT', '305BRI', '309CAW174', '314WE0796', '310LSL', '310SCP', '307SCCARR', '310ADC', '314SYP', '304SVC', '309NAC', '309PS0116', '309CAW194', '312CCC', '304PS0194', '309PS0043', '308PS0308', '309CAW130', '314SYC'

# general
# mon_stations$station_co[mon_stations$class == "head-low-unconsolidated"  
#                         | mon_stations$class == "head-low-vegetated"
#                         | mon_stations$class == "medium-low-unconsolidated"]

# '304WAD', '310SYB', '310PCO', '309POT', 'GVWTC1', '311SLE', '309CAW178', '310SLD', '305HAR', '305STL', '309SOS', '309SBC', '309NOS', '305SJA', '305PRR', '305SJN', '310LVR', '310SBE', '310CLK', '310CLV', '310MNO', '313WE0899', '305SSCAUC', '315ROM', '315GBR', '315MIU', '305PES', 'GVWBL2', '315LCC', '309NAD', '315ABU', '315BEF', '310LSL', 'GVWLC2', 'GVWDV24', '305CAW097', '304WE1096', '304SVC', 'GVWMY2', '304PS0194', '304CAW016', 'GVWSP3', 'GVWDV25', '315DOS', 'NATLAS', '305OAK', 'GVWLV1', '310PRE', '305CHE', '314MCM', '304SOK', '310WRP', '305CCUSGS', '312RYCALR', '310DAM', '310SLU', '310DAL', '315RSB', '310UWR', '305ACR', '305LGCACR', '309UQA', '305LEA', '312CUT', '310SCP', '309TEH', '309SALDDM', '309OLD', '312SMA', '309TEM', '304SOQ', '309SBR', '310TWB', '315ATA', 'GVWAT1', '310VIA', '309NAC', '309USA', '309PS0043', '313SAE', '310SLB', '307CML', '310CER', '314WE0785', '307CARME36', '307CARME38', '310UCR', '307CARME366', '313SAI', '310OLD', '310SSC', '314SYN', '310CCC', '310MOR', '312SMI', '310CAW176', '307CMD', '310CAN', '309JON', '306CORNC31', '309BLA', '309ALU', '309ASB', '309ALG', '315LCR', '309GAB', 'GVWDV20', '306CARNE36', '309ALD', '306WAC', 'GABVET', 'GVWLC1', '309MER', '309AXX', '309AVR', '305WCS', '306CAR', '305PS0034', '305MUR', '312HUA', '312CUY', '317EST', '315SCC', '315TOR', '315ABH', 'GVWDV21', 'GVWDV22', '315GAN', '309ANT', '315SJH', '309TOP', '309SUN', '309SARANF', '305CAW049', '309CAW116', '305SBH', '309GRN', '317ESE', '315MYC', '315CAU', '305BRI', 'GVWMY1', '307SCCARR', '309LOK', '309PS0116', '309PSO', '309SAG', '309SAN', '315ATU', 'GVWSJ1', 'GVWAT2', '307TUL', '308PS0308', '312NIT', '310DAU', '309PS0072', '309SAP', '315CRP', '309CAW182', 'GVWGA2', '309SSP', '309SAC', '309PS0040', '310UPN', '310ADU', '309SALUBD', '312NIP', 'GVWSJ2', '309SAS', '310APN', '309DSA', '309SAT', '310PEN', '309CAW050', '309SET', '309CAW174', '309DAV', '310CPN', '310ADC', '309CAW194', 'CMWCP00', '309CAW130', '309QUI', '309CRR', '309UAL', '312ORI', '309SRITA34', '309QCW', '309-SRITA-36', '310BER', 'GVWCG1', '306CARNE32', '309CCD', 'GVWSP1', '315FRC', '314DDE', 'GVWAT3', '315FRCMVD', '312ORN', '315FRCMVU', '310CHD', '306CARNE33A', '315FMV', '306CARNE31', '310LBC', '312GVS', '309HRT', '312MAB', '310ARG', '315MIS', 'CMWSM01', '315ANN', '315SJC', '309RTA', '312GVT', '309QUA', '310TUR', '315YSI', '315SMC', '315SPC', 'CMWFK00', 'GVWGA1', '309ESP', '305MVR', '315MTC', '312ORC', '312WE1035', '314WE0779', '312DAVDAV', '314WE0796', '311SLN', '314SYP', '305TRE', '314SYL', '312CCC', '314SYI', '314SYC', '305PAC', '312CAV', '315RIN', '305CAW161', '312SIV', '312SIS', '312SBC'







