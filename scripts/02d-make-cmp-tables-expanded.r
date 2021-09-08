options(max.print = 1000)# {{{
rm(list = ls())
today <- format(Sys.Date(), "%Y%m%d")
# }}}
# import data #{{{
project_dir <- "H:\\gabilan-turbidity-tmdl\\"

cmp_sites <- as.character(read.csv(paste0(project_dir, "data\\cmp-sites.csv"))[[1]])
exclude <- c("306MOR", "309BLA", "309CRR", "309GRN", "309QUI", "309SAC",
             "309SAG", "309SSP")

data_all <- read.csv(paste0(project_dir,
                            # "data\\summary-stats-all-cmp-", today, ".csv"))
                            "data\\summary-stats-all-cmp-20200116.csv"))
data_all <- data_all[data_all$StationCode %in% cmp_sites, ]
data_all <- data_all[!data_all$StationCode %in% exclude, ]

data_dry <- read.csv(paste0(project_dir,
                            # "data\\summary-stats-dry-cmp-", today, ".csv"))
                            "data\\summary-stats-dry-cmp-20200116.csv"))
data_dry <- data_dry[data_dry$StationCode %in% cmp_sites, ]
data_dry <- data_dry[!data_dry$StationCode %in% exclude, ]
data_dry <- na.omit(data_dry)

data_wet <- read.csv(paste0(project_dir,
                            # "data\\summary-stats-wet-cmp-", today, ".csv"))
                            "data\\summary-stats-wet-cmp-20200116.csv"))
data_wet <- data_wet[data_wet$StationCode %in% cmp_sites, ]
data_wet <- data_wet[!data_wet$StationCode %in% exclude, ]

# }}}
# restoration stations{{{
rest_codes <- c(
                "ReclamationCanal"             = "309ALD",
                "ReclamationCanal/AlisalCreek" = "309ALG",
                "AlisalSlough"                 = "309ASB",
                "EspinosaSlogh"                = "309ESP",
                "GabilanCreek"                 = "309GAB",
                "ReclamationCanal"             = "309JON",
                "MerrillDitch"                 = "309MER",
                "NatividadCreek"               = "309NAD",
                "OldSalinasRiverChannel"       = "309OLD",
                "SantaRitaCreek"               = "309RTA",
                "TembladeroSlough"             = "309TDW",
                "TembladeroSlough"             = "309TEH",
                "TembladeroSlough"             = "309TEM"
                )
# }}}
# ref stations #{{{
ref_codes <- c(
               "LowerUvasCreek"                      = "305CAN",
               "LowerUvasCreek"                      = "305CAR",
               "LowerPajaroRiver"                    = "305CHI",
               "SalsipuedesCreek"                    = "305COR",
               "LowerLlagasCreek"                    = "305LCS",
               "SanJuanCreek"                        = "305SJA",
               "SanJuanCreek"                        = "305SJN",
               "ElkhornSlough(WatsonvilleCreek)"     = "306WAC",
               "ChorroCreek"                         = "310CCC",
               "PismoCreek"                          = "310PIS",
               "SanLuisObispoCreek"                  = "310PRE",
               "SanLuisObispoCreek"                  = "310SLB",
               "ChorroCreek"                         = "310TWB",
               "OsoFlacoCreek"                       = "312OFN",
               "OrcuttCreek(atHighway1)"             = "312ORI",
               "LowerSanAntonioCreek"                = "313SAI",
               "SantaLuciaCanyon-SantaYnezRiver"     = "314SYF",
               "SantaMiguelitoCanyon-SantaYnezRiver" = "314SYL",
               "SantaLuciaCanyon-SantaYnezRiver"     = "314SYN"
               )
# }}}
# all seasons
# all sites all season #{{{
all_iqr_data <- data_all[, c("StationCode", "N", "turb_quan25", 
                             "turb_quan5", "turb_quan75")]
all_iqr <- all_iqr_data$turb_quan75 - all_iqr_data$turb_quan25
all_iqr_tab <- cbind(all_iqr_data, all_iqr)
all_iqr_tab <- all_iqr_tab[order(all_iqr_tab$StationCode), ]
all_all <-  cbind(season = "all", sites = "all", all_iqr_tab)
# }}}
# not rest sites all seasons #{{{
not_rest_iqr_data <- data_all[-na.omit(match(rest_codes, data_all$StationCode)), 
                              c("StationCode", "N", "turb_quan25", 
                                "turb_quan5", "turb_quan75")]
not_rest_iqr <- not_rest_iqr_data$turb_quan75 - not_rest_iqr_data$turb_quan25
not_rest_iqr_tab <- cbind(not_rest_iqr_data, not_rest_iqr)
not_rest_iqr_tab <- not_rest_iqr_tab[order(not_rest_iqr_tab$StationCode), ]
not_rest_all <-  cbind(season = "all", sites = "not_rest", not_rest_iqr_tab)
# }}}
# rest sites all seasons #{{{
rest_iqr_data <- data_all[na.omit(match(rest_codes, data_all$StationCode)), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
rest_iqr <- rest_iqr_data$turb_quan75 - rest_iqr_data$turb_quan25
rest_iqr_tab <- cbind(rest_iqr_data, rest_iqr)
rest_iqr_tab <- rest_iqr_tab[order(rest_iqr_tab$StationCode), ]
rest_all <-  cbind(season = "all", sites = "rest", rest_iqr_tab)
# }}}
# ref sites all seasons #{{{
ref_iqr_data <- data_all[na.omit(match(ref_codes, data_all$StationCode)), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
ref_iqr <- ref_iqr_data$turb_quan75 - ref_iqr_data$turb_quan25
ref_iqr_tab <- cbind(ref_iqr_data, ref_iqr)
ref_iqr_tab <- ref_iqr_tab[order(ref_iqr_tab$StationCode), ]
ref_all <-  cbind(season = "all", sites = "ref", ref_iqr_tab)
# }}}
# dry season
# all sites dry season #{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
all_iqr_data <- data_dry[, c("StationCode", "N", "turb_quan25", 
                             "turb_quan5", "turb_quan75")]
all_iqr <- all_iqr_data$turb_quan75 - all_iqr_data$turb_quan25
all_iqr_tab <- cbind(all_iqr_data, all_iqr)
all_iqr_tab <- all_iqr_tab[order(all_iqr_tab$StationCode), ]
all_dry <- cbind(season = "dry", sites = "all", all_iqr_tab)
# }}}
# not rest sites dry season #{{{
not_rest_iqr_data <- data_dry[-na.omit(match(rest_codes, data_dry$StationCode)), 
                              c("StationCode", "N", "turb_quan25", 
                                "turb_quan5", "turb_quan75")]
not_rest_iqr <- not_rest_iqr_data$turb_quan75 - not_rest_iqr_data$turb_quan25
not_rest_iqr_tab <- cbind(not_rest_iqr_data, not_rest_iqr)
not_rest_iqr_tab <- not_rest_iqr_tab[order(not_rest_iqr_tab$StationCode), ]
not_rest_dry <-  cbind(season = "dry", sites = "not_rest", not_rest_iqr_tab)
# }}}
# rest sites dry season #{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
rest_iqr_data <- data_dry[na.omit(match(rest_codes, data_dry$StationCode)), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
rest_iqr <- rest_iqr_data$turb_quan75 - rest_iqr_data$turb_quan25
rest_iqr_tab <- cbind(rest_iqr_data, rest_iqr)
rest_iqr_tab <- rest_iqr_tab[order(rest_iqr_tab$StationCode), ]
rest_dry <- cbind(season = "dry", sites = "rest", rest_iqr_tab)
# }}}
# ref sites dry season #{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
ref_iqr_data <- data_dry[na.omit(match(ref_codes, data_dry$StationCode)), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
ref_iqr <- ref_iqr_data$turb_quan75 - ref_iqr_data$turb_quan25
ref_iqr_tab <- cbind(ref_iqr_data, ref_iqr)
ref_iqr_tab <- ref_iqr_tab[order(ref_iqr_tab$StationCode), ]
ref_dry <- cbind(season = "dry", sites = "ref", ref_iqr_tab)
# }}}
# wet season
# all sites wet season #{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
all_iqr_data <- data_wet[, c("StationCode", "N", "turb_quan25", 
                             "turb_quan5", "turb_quan75")]
all_iqr <- all_iqr_data$turb_quan75 - all_iqr_data$turb_quan25
all_iqr_tab <- cbind(all_iqr_data, all_iqr)
all_iqr_tab <- all_iqr_tab[order(all_iqr_tab$StationCode), ]
all_wet <- cbind(season = "wet", sites = "all", all_iqr_tab)
# }}}
# not rest sites wet season #{{{
not_rest_iqr_data <- data_wet[-na.omit(match(rest_codes, data_wet$StationCode)), 
                              c("StationCode", "N", "turb_quan25", 
                                "turb_quan5", "turb_quan75")]
not_rest_iqr <- not_rest_iqr_data$turb_quan75 - not_rest_iqr_data$turb_quan25
not_rest_iqr_tab <- cbind(not_rest_iqr_data, not_rest_iqr)
not_rest_iqr_tab <- not_rest_iqr_tab[order(not_rest_iqr_tab$StationCode), ]
not_rest_wet <-  cbind(season = "wet", sites = "not_rest", not_rest_iqr_tab)
# }}}
# rest sites wet season #{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
rest_iqr_data <- data_wet[na.omit(match(rest_codes, data_wet$StationCode)), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
rest_iqr <- rest_iqr_data$turb_quan75 - rest_iqr_data$turb_quan25
rest_iqr_tab <- cbind(rest_iqr_data, rest_iqr)
rest_iqr_tab <- rest_iqr_tab[order(rest_iqr_tab$StationCode), ]
rest_wet <- cbind(season = "wet", sites = "rest", rest_iqr_tab)
# }}}
# ref sites wet season #{{{
# Table 17. Turbidity data summary at perennial riverine reference sites
ref_iqr_data <- data_wet[na.omit(match(ref_codes, data_wet$StationCode)), 
                         c("StationCode", "N", "turb_quan25", 
                           "turb_quan5", "turb_quan75")]
ref_iqr <- ref_iqr_data$turb_quan75 - ref_iqr_data$turb_quan25
ref_iqr_tab <- cbind(ref_iqr_data, ref_iqr)
ref_iqr_tab <- ref_iqr_tab[order(ref_iqr_tab$StationCode), ]
ref_wet <- cbind(season = "wet", sites = "ref", ref_iqr_tab)
# }}}

# create summary table by season
get_mean <- function(df) {
    df <- na.omit(df)
    means <- as.data.frame(t(as.matrix(apply(df[5:8], 2, mean))))
    N <- nrow(df)
    return(cbind(N, means))
}

tabs <- list(
             all_all, not_rest_all, rest_all, ref_all,
             all_dry, not_rest_dry, rest_dry, ref_dry,
             all_wet, not_rest_wet, rest_wet, ref_wet
             )

out <- t(sapply(tabs, get_mean))
rnames <- rep(c("all", "not_rest", "rest", "ref"), 3)
rownames(out) <- rnames
write.csv(out, paste0(project_dir, "data\\cmp_summary.csv"))

# create expanded table by sites
tabs <- list(
             rest_all,       rest_dry,       rest_wet,
             all_all,        all_dry,        all_wet,
             not_rest_all,   not_rest_dry,   not_rest_wet,
             ref_all,        ref_dry,        ref_wet
             )
for (i in 1:length(tabs)) {
    names(tabs[[i]])[ncol(tabs[[i]])] <- "IQR"
}
big_table <- do.call(rbind, tabs)
big_table <- na.omit(big_table)
write.csv(big_table, paste0(project_dir, "data\\cmp_expanded.csv"), 
          row.names = FALSE)

