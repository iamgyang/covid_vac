# To do:

# make sure that the allow.cartesian turned out ok with the vaccine schedule

# do we want to get the IRON levels from NMC as opposed to CHAT? 
# https://correlatesofwar.org/data-sets/national-material-capabilities

# change to historical income groups instead of current income groups

rm(list = ls()) # clear the workspace


# Options: ----------------------------------------------------------------

# debugging
options(error=browser)
options(error=NULL)

# disable data.table auto-indexing (causes errors w/ dplyr functions)
options(datatable.auto.index = FALSE)


# Notes -------------------------------------------------------------------



# Directories -------------------------------------------------------------

# clear environment objects
rm(list = ls())

# You will have to edit this to be your own computer's working directories:
user<-Sys.info()["user"]
root_dir <- paste0("C:/Users/", user, "/Dropbox/CGD/Projects/covid_vaccination/")
input_dir <- paste0(root_dir, "input")
output_dir <- paste0(root_dir, "output")
code_dir <- paste0(root_dir, "code")
raw_dir <- paste0(root_dir, "raw_data")

setwd(raw_dir)


# Packages ---------------------------------------------------------------
{
  list.of.packages <- c(
    "base", "car", "cowplot", "dplyr", "ggplot2", "ggthemes", "graphics", "grDevices",
    "grid", "gridExtra", "gvlma", "h2o", "lubridate", "MASS", "readxl", "rio", "rms",
    "rsample", "stats", "tidyr", "utils", "zoo", "xtable", "stargazer", "data.table",
    "ggrepel", "foreign", "fst", "countrycode", "wbstats", "quantmod", "R.utils",
    "leaps", "bestglm", "dummies", "caret", "jtools", "huxtable", "haven", "ResourceSelection",
    "betareg", "quantreg", "margins", "plm", "collapse", "kableExtra", "tinytex",
    "LambertW", "scales", "stringr", "imputeTS", "shadowtext", "pdftools", "glue",
    "purrr", "OECD", "RobustLinearReg", "forcats", "WDI", "xlsx", "googlesheets4")
}

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)
for (package in list.of.packages) {library(eval((package)), character.only = TRUE)}

# set GGPLOT default theme:
theme_set(theme_clean() + 
            theme(plot.background = 
                    element_rect(color = "white")))


# source(paste0(root_dir, "code/", "helper_functions.R"))
source(paste0("C:/Users/", user, "/Dropbox/Coding_General/personal.functions.R"))

# HPV Global Cancer Observatory ------------------------------------

# Global Cancer Observatory data on vaccinations
GCO_hpv <-
  readxl::read_xlsx('wuenic2020_hpv-estimates.xlsx') %>% as.data.table()

GCO_hpv <-
  GCO_hpv[, .(iso3c, year, vaccine, coverage, vaccine_desc)]
GCO_hpv <-
  GCO_hpv[vaccine_desc %in% c(
    'Target population who received the first dose of HPV vaccine in the reporting year',
    'Target population who received the last dose of HPV vaccine in the reporting year'
  )]
GCO_hpv[vaccine_desc == 'Target population who received the first dose of HPV vaccine in the reporting year', HPV_type := "first"]
GCO_hpv[vaccine_desc == 'Target population who received the last dose of HPV vaccine in the reporting year', HPV_type := "last"]
GCO_hpv$vaccine_desc <- NULL
check_dup_id(GCO_hpv, c("HPV_type", "year", "iso3c"))

GCO_hpv[, vaccine := paste0(vaccine, " ", HPV_type)]
GCO_hpv[, c("HPV_type") := NULL]
GCO_hpv[,source:="Global Cancer Observatory"]

# WHO/UNICEF total immunization --------------------------------------------------------------

# https://data.unicef.org/topic/child-health/immunization/
whodf <- read.xl.sheets("wuenic2020_immunization-coverage-by-antigen.xlsx")
whodf$README <- NULL
whodf <- rbindlist(whodf, fill = TRUE)
whodf <- whodf[!is.na(iso3)]
whodf <- whodf %>% 
  gather(., "year", "coverage", `2020`:`1980`) %>% 
  dplyr::select(iso3c = iso3, vaccine, year, coverage = coverage) %>% 
  na.omit() %>% 
  mutate(source = "WHO/UNICEF") %>% 
  as.data.frame() %>% 
  as.data.table()

# Flu doses per 1000 population (Palache 2015) -----------------------------------------------
# https://www.sciencedirect.com/science/article/pii/S0264410X15012359?via%3Dihub
setwd(paste0(raw_dir, "/influenza_palache_2015/"))
flu_df <- lapply(dir(), function(x) {readxl::read_xlsx(x, sheet = 2)})
flu_df <- flu_df %>% rbindlist(fill = T) %>% as.data.frame()
flu_df <- flu_df[, grep("dose|country",
                        names(flu_df),
                        ignore.case = T,
                        value = T)]
flu_df <- flu_df[, -grep("change",
                         names(flu_df),
                         ignore.case = T)]
names(flu_df) <- make.names(names(flu_df))
flu_df$WHO.Region.Country <- coalesce2(flu_df$WHO.Region.Country, flu_df$Country)
flu_df$Country <- NULL
flu_df <- flu_df %>% 
  gather(., "year","value", Doses.2004.per.1.000.Population:Doses.2012.per.1000.population) %>% 
  na.omit()
flu_df$year_clean <- flu_df$year %>% 
  gsub("1.000","",., fixed = T) %>% 
  gsub("1000","",., fixed = T) %>% 
  gsub("[^\\d]+", "", ., perl = T) %>% 
  as.numeric()
flu_df <- flu_df %>% arrange(WHO.Region.Country, year_clean)
flu_df <- flu_df %>% as.data.table() %>% unique()
flu_df[, iso3c := name2code(WHO.Region.Country)][, WHO.Region.Country := NULL][]
flu_df[,year:=year_clean][,year_clean:=NULL][,dose_pc_1k:=value][,value:=NULL][]

# make sure we have unique year and country (if duplicated, they should be identical values)
check_dup_id(unique(flu_df[,.(iso3c, year, dose_pc_1k)]), c("iso3c", "year"))
flu_df <- unique(flu_df)
check <- NULL

setnames(flu_df, "dose_pc_1k", "coverage")
flu_df[,coverage:=coverage/1000]
flu_df[,notes:="doses per per capita"]
flu_df$vaccine <- "flu"

# flu vaccination OECD ---------------------------------------------------------
# https://data.oecd.org/healthcare/influenza-vaccination-rates.htm
setwd(paste0(raw_dir, "/influenza_oecd/"))
flu_df_oecd <- fread("DP_LIVE_02112021182435555.csv")
flu_df_oecd <- flu_df_oecd[,iso3c:=LOCATION][,year:=TIME][,flu_pc_pop65:=Value][,.(iso3c, year, flu_pc_pop65)][]
setnames(flu_df_oecd, "flu_pc_pop65", "coverage")
flu_df_oecd[,notes:="denominator is population >= 65"]
flu_df_oecd$vaccine <- "flu"

# Yellow Fever Vaccination ------------------------------------------------
setwd(paste0(raw_dir, "/dryad/"))
yf_df <- fread("number_to_vaccinate&coverage_endemic&low_risk_1970_2016.csv") %>% as.data.frame()

yf_df <- yf_df[,
               c(
                 grep('coverage_untargeted_biased', names(yf_df),
                      value = T),
                 "total_population_2016",
                 "COUNTRY_ID",
                 "country_name"
               )]
setDT(yf_df)[,iso3c:=name2code(country_name)]
waitifnot(all(yf_df$iso3c == yf_df$COUNTRY_ID))
yf_df[,c('country_name', 'COUNTRY_ID'):=NULL]
yf_df <- yf_df %>% gather(.,"year","value" ,coverage_untargeted_biased_2016:coverage_untargeted_biased_2010)
yf_df$year <- yf_df$year %>% gsub("[^\\d]+", "", ., perl = T) %>% as.numeric()
yf_df <- as.data.table(yf_df)
yf_df <- yf_df[, .(value = weighted_mean(value, total_population_2016, na.rm = T)), by = c("iso3c", "year")]
yf_df <- yf_df[order(iso3c, year)]
yf_df <- yf_df[, .(iso3c, year, coverage = value)]
yf_df$vaccine <- "yellow fever"

# Manually gathered data --------------------------------------------------

setwd(raw_dir)
manu <- readxl::read_xlsx('manually gathered data.xlsx') %>% as.data.table()
manu[country == "C.A.R.", country:="Central African Republic"]
manu[country == "Trin. & Tob.", country:="Trinidad and Tobago"]
manu[,iso3c:=name2code(country)]
manu <- manu %>% rename("source" = "source_name",
                        "vaccine" = "disease") %>% as.data.frame() %>% as.data.table()

# WHO vaccine scheduling ---------------------------------------------------------------------
setwd(paste0(raw_dir, "/who_schedule/"))
vac_sched <- lapply(dir(), function(x) {readxl::read_xlsx(x, sheet = 1)})
vac_sched <- vac_sched %>% rbindlist()
vac_sched <- vac_sched[,.(ISO_3_CODE, COUNTRYNAME, YEAR, DESCRIPTION)]
setnames(vac_sched, c('ISO_3_CODE', 'COUNTRYNAME', 'YEAR', 'DESCRIPTION'),
         c('iso3c', 'country', 'year', 'description'))

# Population --------------------------------------------------------------

pop <- readstata13::read.dta13(paste0(raw_dir, "/un_pop/un_pop_estimates_cleaned.dta")) %>% as.data.table()


# Bridge between diseases and vaccine names -------------------------------
setwd(raw_dir)
bridge <- readxl::read_xlsx("bridge_disease_vaccine2.xlsx", sheet = 1) %>% as.data.table()


# WB income classifcation --------------------------------------

wb_income <- readstata13::read.dta13("historical_wb_income_classifications.dta") %>% dfdt()
wb_income <- wb_income[year == 2021, .(iso3c, income)]


# Yellow Fever Risk -------------------------------------------------------

yf_risk <- readxl::read_xlsx(paste0(raw_dir, "/yf_risk/risk_YF_WHO.xlsx"))
names(yf_risk)[1:2] <- c("country", "yf_risk")
yf_risk <- yf_risk[1:2]
yf_risk <- yf_risk %>% dfdt()
yf_risk <- yf_risk[!is.na(country)]
yf_risk[, iso3c:=name2code(country)]
yf_risk <- yf_risk[!is.na(iso3c)]
# these are the countries where there is yellow fever risk
yf_iso3c <- yf_risk[yf_risk=="Yes"]$iso3c


# Vaccine discovery -------------------------------------------------------

vac_disc <-readxl::read_xlsx("bridge_disease_vaccine2.xlsx", sheet = 2) %>% dfdt()

# Merge -------------------------------------------------------

# merge them all together
mlist <-
  list(manu,
       yf_df,
       flu_df_oecd,
       flu_df,
       whodf,
       GCO_hpv)

df <- rbindlist(mlist, fill = T)
df$year <- as.numeric(df$year)

# Population
df <- merge(df, pop, all = T, by = c("iso3c", "year"))

# Vaccine Schedule
df <- merge(df, 
            na.omit(bridge[,.(vaccine, disease)]), 
            all.x = T, 
            by = c("vaccine"), 
            allow.cartesian = T)
vac_sched <- merge(vac_sched, na.omit(bridge[,.(vaccine_schedule, disease)]), all = T, 
                   by.x = "description", by.y = "vaccine_schedule")
vac_sched <- vac_sched[,.(year = min(year, na.rm = T)),by = c("iso3c", "disease")]
df <- merge(df, vac_sched[,.(iso3c, disease, yr_sched = year)], by = c("disease", "iso3c"), all.x = T)
df[,c("source_url","country.x","country.y", "popwork"):=NULL]
df <- df[!is.na(iso3c)]

# Yellow Fever Endemic Countries
df[iso3c%in%yf_iso3c, yf_risk:= 1]

# WB income classification
df <- merge(df, wb_income, by = c("iso3c"), all.x = T) %>% dfdt()

# Vaccine discovery
df <- merge(df, vac_disc, by = c("disease"), all.x = T) %>% dfdt()

# Adjustments -------------------------------------------------------------

# we want to get the LAST dose of each vaccine: (e.g. 3rd dose of DPT)
df <- df[!(
  vaccine %in%
    c(
      "DTP 1st dose",
      "DTP 2nd dose",
      "DTP1",
      "HEPBB",
      "HPV first",
      "MCV1",
      "Polio 2nd dose",
      "Polio 1st dose",
      "MCV1"
    )
)]

# Divide Africa 1968 data by 0.22 to get estimated childhood vaccination rate. 
# This means that we would assume that all those vaccines are going to children.
df[notes == "denominator is total population", coverage := min(coverage/0.22, 100)]

# The flu data comes also in doses per capita. Delete this, as we have a longer series
# for flu vaccination coverage in people above 65.
df <- df[notes!="doses per per capita"|is.na(notes)]

# Get the maximum coverage per year-disease-country. e.g. Say the combination
# vaccine coverage was greater than the individual one. But you're
# right-probably doesn't happen too often (if at all).will let you know once
# I've matched the data. Should we take a country-year maximum of each
# vaccine? E.g. let's say we have the DTP vaccine coverage and the Pertussis
# vaccine coverage available for Nigeria. One is at 80%, another is at 50%.
# For all of pertussis, diptheria, and tetanus, we'd set the coverage rate to
# be 80%.

df <- df[, .(coverage = max(coverage, na.rm = T)),
         by = c(
           "disease",
           "iso3c",
           "year",
           "yr_sched",
           "poptotal",
           "income",
           "yf_risk",
           "emerged",
           "microbe_id",
           "licensing"
         )]
df <- df[!is.na(disease)]

# check: there should be no DTP-related category:
waitifnot(all(na.omit(df$disease!="DTP-related")))
check_dup_id(df, c("disease", "iso3c", "year"))

# Check how our data coverage is -- global estimates:
# if the vaccine summary measure is after the schedule, then set population to be 0
exp_grid <- CJ(
  disease = unique(df$disease),
  iso3c = unique(pop$iso3c),
  year = unique(df$year)
)
df <- merge(exp_grid, df, by = c("disease","iso3c","year"), all.x = TRUE)
df <- merge(df, pop, by = c("iso3c","year"), all.x = T)
df <- df %>% as.data.frame() %>% dfcoalesce.all() %>% as.data.table()
df[, yr_sched:=mean(yr_sched, na.rm = T), by = .(iso3c, disease)]

# of those countries past the scheduled license date AND, get the total population
df <- df[(!(year<yr_sched) | is.na(yr_sched) | is.na(year))]

# for yellow fever, we want to ignore the countries that are not endemic with yellow fever
df <- rbindlist(
  list(df[disease!="yellow fever"],
       df[disease=="yellow fever"][yf_risk == 1])
) %>% dfdt()

# Check how our data coverage is -- global estimates:
check_dup_id(df, c("iso3c","year", "disease"))
df[,glob_pop:=sum(na.omit(poptotal), na.rm = T), by = .(year, disease)]
df[,perc_pop:=poptotal/glob_pop]
df <- df[!is.na(coverage)]

# Table: above 75% coverage -----------------------------------------------

df_disease_avg <- 
  df[, .(coverage = weighted_mean(coverage, poptotal, na.rm = T),
      num_c = length(unique(iso3c)),
      perc_pop = sum(na.omit(perc_pop), na.rm = T)), by = c("year", "disease")] %>% dfdt()

# For each of these diseases, when was the time when we first got to 75%
# global coverage?
df_disease_avg[coverage >= 75, above_75 := 1]
df_disease_avg[coverage < 75, above_75 := 0]
df_disease_avg[, minyr := min(year, na.rm = T), by = c("disease")]
df_disease_avg[, max_cov := max(coverage, na.rm = T), by = c("disease")]
df_disease_avg[, min_cov := min(coverage, na.rm = T), by = c("disease")]
df_disease_avg[max_cov >= 75 & min_cov <= 75, 
   min_above_75 := min(year, na.rm = T), 
   by = c("disease", "above_75")]
df_disease_avg[above_75==0,min_above_75:=NA]
df_disease_avg[,min_above_75:=mean(min_above_75, na.rm = T),by = c("disease")]

# year of above 75% coverage and the number of the population that our sample covers:
df_glob_75 <- df_disease_avg[year==min_above_75 | is.na(min_above_75),.(disease, perc_pop, min_above_75)][, .(min_above_75 = mean(min_above_75), perc_pop = mean(perc_pop, na.rm = T)), by = disease]

# CURRENT coverage
df_cur_cov <- df_disease_avg[,maxyr:=max(year), by = disease][year==maxyr,.(disease, rec_cov = coverage, rec_cov_pop = perc_pop)][]

# merge the two:
smmry_tbl_75 <- merge(df_glob_75, df_cur_cov, by = c("disease"))
smmry_tbl_75[is.na(min_above_75), perc_pop:=NA]
smmry_tbl_75 <- smmry_tbl_75[order(min_above_75)]
smmry_tbl_75[,(c("perc_pop","rec_cov_pop")):=lapply(.SD, function(x) {100*signif(x, 3)}), .SDcols = c("perc_pop","rec_cov_pop")]
smmry_tbl_75[,rec_cov:=lapply(.SD, function(x) {signif(x, 3)}), .SDcols = c("rec_cov")]
smmry_tbl_75 <- smmry_tbl_75[disease!="smallpox"]
names(smmry_tbl_75) <-
  c(
    "Disease",
    "Year Above 75% Coverage",
    "Percent of Population with \nAvailable Data in Year with 75% Coverage",
    "Most recent Coverage Estimate",
    "Percent of Population with \nAvailable Data in Most Recent Coverage Estimate"
  )
smmry_tbl_75 %>% kableExtra::kbl()


# Line graph for microbe ID -----------------------------------------------

prog_df <-
  merge(smmry_tbl_75[, .(`Disease`, `Year Above 75% Coverage`)],
        vac_disc,
        by.x = c("Disease"),
        by.y = c("disease"),
        all = T) %>% dfdt()

# make the disease names pretty
custom_title <- function(x) {
  if (toupper(x) == x ) {x
    } else {
    str_to_title(x)
  }
}
prog_df$Disease <- prog_df$Disease %>% lapply(., custom_title)

# Export to google sheets for Data Wrapper:

# # designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
# # trigger auth on purpose to store a token in the specified cache
# # a broswer will be opened
# googlesheets4::sheets_auth()
# # see your token file in the cache, if you like
# list.files(".secrets/")
# # sheets reauth with specified token and email address
# gs4_deauth()
# options(
#   cache = ".secrets",
#   email = "gzyang4@gmail.com"
# )
# sheets_auth()
prog_df[is.na(`Year Above 75% Coverage`), endpoint:= 2020]
prog_df[!is.na(`Year Above 75% Coverage`), endpoint:= `Year Above 75% Coverage`]
prog_df[Disease=="Smallpox", endpoint:=1980]
write_sheet(prog_df, ss = "https://docs.google.com/spreadsheets/d/1fpwpesVd74Dr5eF4zK4RIMvKwOIGlsB1xeRPFVnWPdo/edit?usp=sharing", sheet = 1)

# Gaps by income group between vaccine licensing and addition to schedule, then to reach 75 percent of target population------------------------------------------------------------

# For each of these diseases, when was the time when we first got to 75%
# global coverage?
df[coverage >= 75, above_75 := 1]
df[coverage < 75, above_75 := 0]
df[, minyr := min(year, na.rm = T), by = c("disease", "iso3c")]
df[, max_cov := max(coverage, na.rm = T), by = c("disease", "iso3c")]
df[, min_cov := min(coverage, na.rm = T), by = c("disease", "iso3c")]
df[max_cov >= 75 & min_cov <= 75, 
   min_above_75 := min(year, na.rm = T), 
   by = c("disease", "above_75", "iso3c")]
df[above_75==0,min_above_75:=NA]
df[,min_above_75:=mean(min_above_75, na.rm = T),by = c("disease", "iso3c")]


# get the differences between a certain target and licensing:
df_income_disease <- unique(df[,.(country, iso3c, disease, yr_sched, income, microbe_id, licensing, min_above_75)])
df_income_disease[,diff_license_schedule:=yr_sched-licensing]
df_income_disease[,diff_license_75:=min_above_75-licensing]


df_income_disease <-
  df_income_disease[, .(
    diff_license_schedule = mean(diff_license_schedule, na.rm = T),
    diff_license_75 = mean(diff_license_75, na.rm = T),
    microbe_id = mean(microbe_id, na.rm = T)
  ),
  by = .(disease, income)][!is.na(income)]

df_income_disease <- df_income_disease[,.(disease, income, diff_license_75)]
df_income_disease <- df_income_disease %>% na.omit() %>% 
  spread(., income, diff_license_75)
df_income_disease$disease <- df_income_disease$disease %>% lapply(., custom_title)

write_sheet(df_income_disease, ss = "https://docs.google.com/spreadsheets/d/1cRYaM8cKGcFVTbCtvxttjOXpkH4iPLim2aYIYLdkSPw/edit?usp=sharing", sheet = 1)








# TO DO:
# recycle CHAT comin code to get coefficient of variation
# merge in GDP information for preston curves.






























