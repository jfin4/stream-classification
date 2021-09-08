options(max.print = 1000)# {{{
rm(list = ls())
today <- format(Sys.Date(), "%Y%m%d")
# }}}
# all data
project_dir <- "H:\\gabilan-turbidity-tmdl\\"# {{{

data_all <- read.csv(paste0(project_dir,
                            "data\\summary-stats-all-20191203.csv"))
data_dry <- read.csv(paste0(project_dir,
                            "data\\summary-stats-dry-20191203.csv"))
data_wet <- read.csv(paste0(project_dir,
                            "data\\summary-stats-wet-20191203.csv"))

rest_codes <- c("309GAB",                           "309NAD",
                "309ALG",                           "309ALD",
                "309JON",                           "309TEH",
                "309TEM",                           "309TDW",
                "309OLD",                           "309ASB",
                "309MER",                           "309ESP",
                "309RTA")
rest_names <- c("Gabilan Creek",                    "Natividad Creek",
                "Reclamation Canal/Alisal Creek",   "Reclamation Canal",
                "Reclamation Canal",                "Tembladero Slough",
                "Tembladero Slough",                "Tembladero Slough",
                "Old Salinas River Channel",        "Alisal Slough",
                "Merrill Ditch",                    "Espinosa Slogh",
                "Santa Rita Creek")
ref_codes <- c("305LCS",                                 "305CAN",
               "305CAR",                                 "305WSA",
               "305CHI",                                 "305COR",
               "305SJA",                                 "305SJN",
               "306WAC",                                 "310CCC",
               "310TWB",                                 "310PRE",
               "310SLB",                                 "310PIS",
               "312OFN",                                 "312ORI",
               "313SAI",                                 "314SYN",
               "314SYF",                                 "314SYL",
               "315DEV")
ref_names <- c("Lower Llagas Creek",                     "Lower Uvas Creek",
               "Lower Uvas Creek",                       "Watsonville Slough",
               "Lower Pajaro River",                     "Salsipuedes Creek",
               "San Juan Creek",                         "San Juan Creek",
               "Elkhorn Slough (Watsonville Creek)",     "Chorro Creek",
               "Chorro Creek",                           "San Luis Obispo Creek",
               "San Luis Obispo Creek",                  "Pismo Creek",
               "Oso Flaco Creek",                        "Orcutt Creek (at Highway 1)",
               "Lower San Antonio Creek",                "Santa Lucia Canyon-Santa Ynez River",
               "Santa Lucia Canyon-Santa Ynez River",    "Santa Miguelito Canyon-Santa Ynez River",
               "Dos Pueblos Canyon (Devereux Slough)")
# }}}
# exceedance table{{{
exceed_data <- data_all[match(rest_codes, data_all$StationCode), 
                        c("StationCode", "N", "pct.gte.25", "pct.gte.40")]
exceed_tab <- cbind(rest_names, exceed_data)
exceed_out <- paste0(project_dir, "data\\rest-site-exceedances.csv")
# write.csv(exceed_tab, exceed_out)

# iqr table
iqr_data <- data_all[match(rest_codes, data_all$StationCode), 
                     c("StationCode", "N", "turb_quan25", 
                       "turb_quan5", "turb_quan75")]
iqr <- iqr_data$turb_quan75 - iqr_data$turb_quan25
iqr_tab <- cbind(rest_names, iqr_data, iqr)
iqr_out <- paste0(project_dir, "data\\rest-site-iqr.csv")
# write.csv(iqr_tab, iqr_out)
# }}}
# seasonal iqr table{{{
seas_wet_data <- data_wet[match(rest_codes, data_wet$StationCode), 
                          c("StationCode", "N", "turb_quan25", 
                            "turb_quan5", "turb_quan75")]
seas_dry_data <- data_dry[match(rest_codes, data_dry$StationCode), 
                          c("StationCode", "N", "turb_quan25", 
                            "turb_quan5", "turb_quan75")]
seas_wet_iqr <- seas_wet_data$turb_quan75 - seas_wet_data$turb_quan25
seas_dry_iqr <- seas_dry_data$turb_quan75 - seas_dry_data$turb_quan25
seas_wet_tab <- cbind(rest_names, "season" = "wet", seas_wet_data, 
                      "iqr" = seas_wet_iqr, "order" = 1:length(rest_codes))
seas_dry_tab <- cbind(rest_names, "season" = "dry", seas_dry_data, 
                      "iqr" = seas_dry_iqr, "order" = 1:length(rest_codes))
seas_tab <- rbind(seas_dry_tab, seas_wet_tab)
seas_tab <- seas_tab[order(seas_tab$order), ]
seas_out <- paste0(project_dir, "data\\seasonal-iqr.csv")
write.csv(seas_tab, seas_out)
# }}}
# ref iqr table---all seasons{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
ref_iqr_data <- data_all[match(ref_codes, data_all$StationCode), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
ref_iqr <- ref_iqr_data$turb_quan75 - ref_iqr_data$turb_quan25
ref_iqr_tab <- cbind(ref_names, ref_iqr_data, ref_iqr)
ref_iqr_tab <- ref_iqr_tab[order(ref_iqr_tab$StationCode), ]
all <-  ref_iqr_tab
all_out <- paste0(project_dir, "data\\ref-site-iqr-all-", today, ".csv")
# write.csv(all, all_out)
# }}}
# ref iqr table---dry season{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
ref_iqr_data <- data_dry[match(ref_codes, data_dry$StationCode), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
ref_iqr <- ref_iqr_data$turb_quan75 - ref_iqr_data$turb_quan25
ref_iqr_tab <- cbind(ref_names, ref_iqr_data, ref_iqr)
ref_iqr_tab <- ref_iqr_tab[order(ref_iqr_tab$StationCode), ]
dry <- ref_iqr_tab
dry_out <- paste0(project_dir, "data\\ref-site-iqr-dry-", today, ".csv")
# write.csv(dry, dry_out)
# }}}
# ref iqr table---wet season{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
ref_iqr_data <- data_wet[match(ref_codes, data_wet$StationCode), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
ref_iqr <- ref_iqr_data$turb_quan75 - ref_iqr_data$turb_quan25
ref_iqr_tab <- cbind(ref_names, ref_iqr_data, ref_iqr)
ref_iqr_tab <- ref_iqr_tab[order(ref_iqr_tab$StationCode), ]
wet <- ref_iqr_tab
wet_out <- paste0(project_dir, "data\\ref-site-iqr-wet-", today, ".csv")
# write.csv(wet, wet_out)
# }}}
# only CMP data
project_dir <- "H:\\gabilan-turbidity-tmdl\\"# {{{

data_all <- read.csv(paste0(project_dir,
                            "data\\summary-stats-all-cmp-20200115.csv"))
data_dry <- read.csv(paste0(project_dir,
                            "data\\summary-stats-dry-cmp-20200115.csv"))
data_wet <- read.csv(paste0(project_dir,
                            "data\\summary-stats-wet-cmp-20200115.csv"))

rest_codes <- c("309GAB",                           "309NAD",
                "309ALG",                           "309ALD",
                "309JON",                           "309TEH",
                "309TEM",                           "309TDW",
                "309OLD",                           "309ASB",
                "309MER",                           "309ESP",
                "309RTA")
rest_names <- c("Gabilan Creek",                    "Natividad Creek",
                "Reclamation Canal/Alisal Creek",   "Reclamation Canal",
                "Reclamation Canal",                "Tembladero Slough",
                "Tembladero Slough",                "Tembladero Slough",
                "Old Salinas River Channel",        "Alisal Slough",
                "Merrill Ditch",                    "Espinosa Slogh",
                "Santa Rita Creek")
ref_codes <- c("305LCS",                                 "305CAN",
               "305CAR",                                 "305WSA",
               "305CHI",                                 "305COR",
               "305SJA",                                 "305SJN",
               "306WAC",                                 "310CCC",
               "310TWB",                                 "310PRE",
               "310SLB",                                 "310PIS",
               "312OFN",                                 "312ORI",
               "313SAI",                                 "314SYN",
               "314SYF",                                 "314SYL",
               "315DEV")
ref_names <- c("Lower Llagas Creek",                     "Lower Uvas Creek",
               "Lower Uvas Creek",                       "Watsonville Slough",
               "Lower Pajaro River",                     "Salsipuedes Creek",
               "San Juan Creek",                         "San Juan Creek",
               "Elkhorn Slough (Watsonville Creek)",     "Chorro Creek",
               "Chorro Creek",                           "San Luis Obispo Creek",
               "San Luis Obispo Creek",                  "Pismo Creek",
               "Oso Flaco Creek",                        "Orcutt Creek (at Highway 1)",
               "Lower San Antonio Creek",                "Santa Lucia Canyon-Santa Ynez River",
               "Santa Lucia Canyon-Santa Ynez River",    "Santa Miguelito Canyon-Santa Ynez River",
               "Dos Pueblos Canyon (Devereux Slough)")
# }}}
# ref iqr table---all seasons{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
ref_iqr_data <- data_all[match(ref_codes, data_all$StationCode), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
ref_iqr <- ref_iqr_data$turb_quan75 - ref_iqr_data$turb_quan25
ref_iqr_tab <- cbind(ref_names, ref_iqr_data, ref_iqr)
ref_iqr_tab <- ref_iqr_tab[order(ref_iqr_tab$StationCode), ]
all <-  ref_iqr_tab
all_out <- paste0(project_dir, "data\\ref-site-iqr-all-cmp-", today, ".csv")
write.csv(all, all_out)
# }}}
# ref iqr table---dry season{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
ref_iqr_data <- data_dry[match(ref_codes, data_dry$StationCode), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
ref_iqr <- ref_iqr_data$turb_quan75 - ref_iqr_data$turb_quan25
ref_iqr_tab <- cbind(ref_names, ref_iqr_data, ref_iqr)
ref_iqr_tab <- ref_iqr_tab[order(ref_iqr_tab$StationCode), ]
dry <- ref_iqr_tab
dry_out <- paste0(project_dir, "data\\ref-site-iqr-dry-cmp-", today, ".csv")
write.csv(dry, dry_out)
# }}}
# ref iqr table---wet season{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
ref_iqr_data <- data_wet[match(ref_codes, data_wet$StationCode), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
ref_iqr <- ref_iqr_data$turb_quan75 - ref_iqr_data$turb_quan25
ref_iqr_tab <- cbind(ref_names, ref_iqr_data, ref_iqr)
ref_iqr_tab <- ref_iqr_tab[order(ref_iqr_tab$StationCode), ]
wet <- ref_iqr_tab
wet_out <- paste0(project_dir, "data\\ref-site-iqr-wet-cmp-", today, ".csv")
write.csv(wet, wet_out)
# }}}
