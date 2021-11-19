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
chat_dir <- paste0("C:/Users/user/Dropbox/CGD/Projects/refute_mestieri/input")
setwd(raw_dir)

# Packages ---------------------------------------------------------------
{
  list.of.packages <- c(
    "base", "car", "cowplot", "dplyr", "ggplot2", "ggthemes", "graphics", "grDevices",
    "grid", "gridExtra", "gvlma", "h2o", "lubridate", "MASS", "readxl", "rio", "rms",
    "rsample", "tidyr", "utils", "zoo", "xtable", "stargazer", "data.table",
    "ggrepel", "foreign", "fst", "countrycode", "wbstats", "quantmod", "R.utils",
    "leaps", "bestglm", "dummies", "caret", "jtools", "huxtable", "haven", "ResourceSelection",
    "betareg", "quantreg", "margins", "plm", "collapse", "kableExtra", "tinytex",
    "LambertW", "scales", "stringr", "imputeTS", "shadowtext", "pdftools", "glue",
    "purrr", "OECD", "RobustLinearReg", "forcats", "WDI", "xlsx", "googlesheets4", "RCurl")
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

# WHO/UNICEF total immunization -------------------------------------

# https://data.unicef.org/topic/child-health/immunization/

download.file("https://data.unicef.org/wp-content/uploads/2021/10/wuenic2020rev_web_update.xlsx", "wuenic_immuniz.xlsx", method = "curl")

whodf <- read.xl.sheets("wuenic_immuniz.xlsx")
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

who_keep_iso3c <- unique(whodf[year == 2020 & coverage>0]$iso3c)

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
setwd(raw_dir)
pop <- readstata13::read.dta13(paste0(raw_dir, "/un_pop/un_pop_estimates_cleaned.dta")) %>% as.data.table()

setwd(raw_dir)
pop2 <- fread('un_pop/WPP2019_PopulationBySingleAgeSex_1950-2019.csv')
pop3 <- fread('un_pop/WPP2019_PopulationBySingleAgeSex_2020-2100.csv')
pop2 <- rbindlist(list(pop2, pop3)); pop3 <- NULL
pop2 <- pop2[LocID<=894,.(country = Location, year = Time, 
                          pop = PopTotal, age = AgeGrp, fem = PopFemale)]
pop2[country == "Curaçao", country:="curacao"]
pop2[country == "Réunion", country:="reunion"]
pop2 <- pop2[country != "Channel Islands"]
bridge <- unique(pop2[,.(country)])
bridge[,iso3c:=name2code(country)]
pop2 <- merge(pop2, bridge, by = "country", all.x = T)
pop2[,country:=NULL]

# checks for US
pop2[,pop:=pop*1000]
pop2[,fem:=fem*1000]
waitifnot(sum(pop2[iso3c=="USA" & year == 2019]$pop)<=340*10^6)
waitifnot(sum(pop2[iso3c=="USA" & year == 2019]$pop)>=320*10^6)

# get variables for 
# 1) girls <= 15 (HPV), 
# 2) infants (1y.o) (most vaccines)
# 3) adults 65+ (flu)
# 4) adults < 60 (yellow fever)
pop2[age<=15,pop_f_le15:=sum(fem, na.rm = T),by = .(iso3c, year)]
pop2[age<=1,pop_t_le1:=sum(pop, na.rm = T),by = .(iso3c, year)]
pop2[age>=65,pop_t_ge65:=sum(pop, na.rm = T),by = .(iso3c, year)]
pop2[age<60,pop_t_l60:=sum(pop, na.rm = T),by = .(iso3c, year)]
pop2 <- pop2[,.(
  pop_f_le15 = mean(pop_f_le15, na.rm = T),
  pop_t_le1 = mean(pop_t_le1, na.rm = T),
  pop_t_ge65 = mean(pop_t_ge65, na.rm = T),
  pop_t_l60 = mean(pop_t_l60, na.rm = T),
  pop = sum(pop, na.rm = T)
  ), by = .(iso3c, year)]
waitifnot(sum(pop2[iso3c=="USA" & year == 2019]$pop)<=340*10^6)
waitifnot(sum(pop2[iso3c=="USA" & year == 2019]$pop)>=320*10^6)

# compare cleanings:
pop <- merge(pop2, pop, by = c("iso3c", "year"), all = T)
pop[,diff:=2*(pop-poptotal)/(pop + poptotal)]
waitifnot(all(na.omit(pop$diff<0.000001)))
pop[,c('pop', 'diff', 'country'):=NULL]
pop <- pop[!is.na(iso3c)]

# Bridge between diseases and vaccine names -------------------------------
setwd(raw_dir)
bridge <- readxl::read_xlsx("bridge_disease_vaccine2.xlsx", sheet = 1) %>% as.data.table()


# WB income classification --------------------------------------

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

# Maddison (2020) ---------------------------------------------------

# Maddison data on historical GDP per capita
mad <- rio::import(paste0(raw_dir, "/gdppc/mpd2020.dta"))
mad <- mad %>% rename(date = year, iso3c = countrycode) %>% as.data.table()
mad <- mad[date!=1,.(iso3c, gdppc, year = date)] %>% na.omit()

# Covid -------------------------------------------------------------------
myfile <- RCurl::getURL('https://covid.ourworldindata.org/data/owid-covid-data.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
df_covid <- read.csv(textConnection(myfile), header=T)
df_covid <- dfdt(df_covid)
rem <- c(
  'Africa',
  'Asia',
  'Europe',
  'European Union',
  'High income',
  'International',
  'Low income',
  'Lower middle income',
  'North America',
  'Oceania',
  'South America',
  'Upper middle income',
  'World'
)
df_covid <- df_covid[!location%in%rem]
df_covid[, iso_check := name2code(
  location,
  custom_match = c(
    'Kosovo' = "XKX",
    "Timor" = "TLS",
    "Micronesia (country)" = "FSM"))]
waitifnot(df_covid$iso_code==df_covid$iso_check)
df_covid <- 
  df_covid[,.(iso3c = iso_code, date, 
              cov = people_fully_vaccinated_per_hundred)]
df_covid$cov <- as.numeric(df_covid$cov)
df_covid <- df_covid[!is.na(cov)]
df_covid <- df_covid[,.(coverage = max(na.omit(cov), na.rm = T)), 
                     by = .(iso3c, year = lubridate::year(date))]
df_covid <- df_covid[!grepl("OWID", df_covid$iso3c),]
df_covid[,vaccine:="Covid"]

# Real GDP PPP (WDI) -------------------------------------------------

unique_country_codes <- na.omit(unique(countrycode::codelist$iso3c))

wdi_gdppc <- 
  WDI(
    indicator = c('wdi_gdppc' = 'NY.GDP.PCAP.PP.KD'),
    start = 1960,
    end = lubridate::year(Sys.Date()),
    country = unique_country_codes
  )
wdi_gdppc <- wdi_gdppc %>% as.data.table()
wdi_gdppc[,iso3c:=countrycode(iso2c, "iso2c", "iso3c")]
wdi_gdppc <- wdi_gdppc[!is.na(iso3c)]
wdi_gdppc[,`:=`(iso2c = NULL, country = NULL)]
wdi_gdppc[,keeprow:=apply(.SD, 1, function(x) length(na.omit(x)))]
wdi_gdppc <- wdi_gdppc[keeprow>2,] %>% as.data.table()
wdi_gdppc[,keeprow:=NULL]
wdi_gdppc <- wdi_gdppc %>% dplyr::select(year, iso3c, wdi_gdppc) %>% dfdt

# merge WDI and Maddison GDP PPP estimates -------------------------

mad <- merge(mad, wdi_gdppc, by = c('year','iso3c'), all = TRUE)
# confirm we have unique iso3c dates
waitifnot(nrow(distinct(mad[,.(year, iso3c)]))==nrow(mad[,.(year, iso3c)]))

# get GDP figures by using growth from WDI figures to 
# project forwards the Maddison GDP figures.
mad <- mad[order(iso3c,year)]
mad[,wdi_gdppc_gr:=wdi_gdppc/shift(wdi_gdppc), by = 'iso3c']

for (i in seq(2015, 2021)) {
  mad[, gdppc.n := shift(gdppc) * wdi_gdppc_gr, by = 'iso3c']
  mad[year == i & is.na(gdppc), gdppc := gdppc.n]
  mad <- as.data.table(mad)
}

mad[,(c('country','wdi_gdppc','wdi_gdppc_gr','gdppc.n')):=NULL]
mad <- mad[order(iso3c, year)]

setwd(input_dir)
save.image("pre_merge.RData")
# Merge -------------------------------------------------------
load("pre_merge.RData")

# merge them all together
mlist <-
  list(manu,
       yf_df,
       flu_df_oecd,
       flu_df,
       whodf,
       GCO_hpv, 
       df_covid)

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

# WB income classification
df <- merge(df, wb_income, by = c("iso3c"), all.x = T) %>% dfdt()

# Vaccine discovery
df <- merge(df, vac_disc, by = c("disease"), all.x = T) %>% dfdt()

# Merge in GDP
df <- merge(df, mad, by = c("iso3c", "year"), all.x = T) %>% dfdt()

save.image("post_merge.RData")
# Adjustments -------------------------------------------------------------
load("post_merge.RData")

# adjust vaccine names (some are purposely changed to other names to merge the
# series)
df[vaccine == "DTP 1st dose", vaccine:= "DTP1"]
df[vaccine == "DTP 2nd dose", vaccine:= "DTP2"]
df[vaccine == "DTP 3rd dose", vaccine:= "DTP3"]
df[vaccine == "Tet Tox / DTaP/Tdap", vaccine:= "DTP3"]
df[vaccine == "MCV1", vaccine:= "MCV1"]
df[vaccine == "measles", vaccine:= "MCV1"]
df[vaccine == "Measles", vaccine:= "MCV1"]
df[vaccine == "Polio 1st dose", vaccine:= "POL1"]
df[vaccine == "Polio 2nd dose", vaccine:= "POL2"]
df[vaccine == "Polio 3rd dose", vaccine:= "POL3"]
df[vaccine == "ROTAC", vaccine:= "RCV1"]
df[vaccine == "Smallpox", vaccine:= "smallpox"]
df[vaccine == "yellow fever", vaccine:= "YFV"]

# within each disease, get the vaccine that has the longest time series coverage
long_series <- na.omit(df[,.(cov = length(na.omit(coverage))),by = .(disease, vaccine)])
long_series[,mc:=max(cov),by = .(disease)]
long_series <- long_series[mc==cov][order(vaccine, decreasing = TRUE)][order(disease, decreasing = TRUE)]
long_series <- long_series[,n1:=1][,n2:=cumsum(n1), by = .(disease)][n2==1]
long_series <- long_series[,.(disease, vaccine, keep = n1)]
check_dup_id(long_series, "disease")

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
           "emerged",
           "microbe_id",
           "licensing",
           "gdppc"
         )]
df <- df[!is.na(disease)]

# check: there should be no DTP-related category:
waitifnot(all(na.omit(df$disease!="DTP-related")))
check_dup_id(df, c("disease", "iso3c", "year"))

# Check how our data coverage is -- global estimates:
# if the vaccine summary measure is after the schedule, then set population to be 0
exp_grid <- CJ(
  disease = unique(df$disease),
  iso3c = unique(who_keep_iso3c),
  year = unique(df$year)
)
df <- merge(exp_grid, df, by = c("disease","iso3c","year"), all.x = TRUE)
df <- merge(df, pop, by = c("iso3c","year"), all.x = T)
df <- df %>% as.data.frame() %>% dfcoalesce.all() %>% as.data.table()
df[, yr_sched:=mean(yr_sched, na.rm = T), by = .(iso3c, disease)]

# for yellow fever, we want to ignore the countries that are not endemic with yellow fever
df[iso3c%in%yf_iso3c, yf_risk:= 1]
df <- rbindlist(
  list(df[disease!="yellow fever"],
       df[disease=="yellow fever"][yf_risk == 1])
) %>% dfdt()

# Check how our data coverage is -- global estimates:
check_dup_id(df, c("iso3c","year", "disease"))
df[,glob_pop:=sum(na.omit(poptotal), na.rm = T), by = .(year, disease)]
df[,perc_pop:=poptotal/glob_pop]

# IF THE COVERAGE IS MISSING, REPLACE WITH 0! (checked that this is what WHO
# does with their data when estimating coverage)
df[is.na(coverage), coverage:=0]

# Create a table that shows the global coverage estimates:
df_disease_avg <- 
  df[, .(coverage = weighted_mean(coverage, poptotal, na.rm = T),
      num_c = length(unique(iso3c)),
      perc_pop = sum(na.omit(perc_pop), na.rm = T)), 
     by = c("year", "disease")] %>% dfdt()

# *****PORT OVER TO CHAT******* ----------------------------------------

custom_title <- function(x) {
  if (toupper(x) == x ) {x
  } else {
    str_to_title(x)
  }
}
setwd(input_dir)
save.image("input_dir.RData")

port_chat <-
  df[, .(
    iso3c,
    date = year,
    variable = disease,
    value = coverage,
    check.count = 1,
    categ = c("Vaccine--Covid Paper"),
    label = lapply(disease, custom_title),
    pop = poptotal,
    pop_f_le15, 
    pop_t_le1, 
    pop_t_ge65, 
    pop_t_l60,
    gdppc
  )] %>% dfdt()

setwd(chat_dir)
TAKE_ME_BACK_DIR <- input_dir
rm(list=setdiff(ls(), c("port_chat", "TAKE_ME_BACK_DIR", "pop", "df_disease_avg", "who_keep_iso3c")))
save.image("port_CHAT.RData")
setwd(TAKE_ME_BACK_DIR)
load("input_dir.RData")

# Table: above 75% & 53% coverage ----------------------------------

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




# Line: UK/Developing World across time ----------------------------

load("post_merge.RData")
toplot <- df[disease %in% c("diptheria", "tetanus", "pertussis", "polio")]
toplot <- toplot[income=="LIC" | income=="LMIC" | iso3c == "GBR"]
toplot <- toplot[vaccine %in% c("Polio 3rd dose", "POL3", "DTP3")]

# in 1974 it is estimated that fewer than 5% of children in developing countries were receiving a third dose of DTP and poliomyelitis vaccines in their first year of life
addendum <- fread("dev_uk	coverage	year	vaccine
Developing World 	0	1948	POL3
UK	0	1948	POL3
Developing World 	0	1955	DTP3
UK	0	1955	DTP3
UK	65	1964	POL3
UK	70	1964	DTP3
Developing World 	5	1974	POL3
Developing World 	5	1974	DTP3
")

toplot <- 
  toplot %>% as.data.frame() %>%
  dplyr::select(iso3c, year, coverage, vaccine, pop_t_le1) %>%
  mutate(
    dev_uk = case_when(iso3c == "GBR" ~ "UK",
                       iso3c != "GBR" ~ "Developing World"),
    vaccine = case_when(
      vaccine == "Polio 3rd dose" ~ "POL3",
      vaccine != "Polio 3rd dose" ~ vaccine
    )
  ) %>%
  group_by(dev_uk, year, vaccine) %>%
  summarise(coverage = weighted_mean(coverage, pop_t_le1)) %>%
  rbind(as.data.frame(addendum))
check_dup_id(toplot, c("dev_uk", "year", "vaccine"))
# toplot <- toplot %>% 
#   pivot_wider(names_from = c(vaccine, dev_uk), values_from = coverage)
# 
# names(toplot) <- names(toplot) %>% gsub("_", "" ,., fixed = T)
# toplot <- toplot %>% dfdt()
# toplot <- toplot[order(year)]
# toplot[, names(toplot) := lapply(.SD, na_locf), .SDcols = names(toplot)]
toplot <- toplot %>%
  mutate(vaccine = case_when(vaccine == "DTP3" ~ "DTP",
                             vaccine == "POL3" ~ "Polio"))

plot <- toplot %>%
  ggplot(.,
         aes(
           x = year,
           y = coverage,
           group = interaction(vaccine, dev_uk, sep = " "),
           color = interaction(vaccine, dev_uk, sep = " ")
         )) +
  geom_point(data = setDT(toplot)[year == 2020], show.legend = FALSE) +
  geom_line() +
  my_custom_theme +
  scale_color_custom +
  labs(y = "", x = "") +
  theme(legend.key.size = unit(3, 'mm')) +
  scale_x_continuous(breaks = seq(1940, 2020, 10))

ggsave(
  "Line - UK vs. Dev World.pdf",
  plot,
  width = 7,
  height = 5,
  limitsize = FALSE,
  dpi = 1000
)


# _____________________ ---------------------------------------------------
# _____________________ ---------------------------------------------------
# COVID Supply Chain Data -------------------------------------------------
setwd(raw_dir)
covsup <- readxl::read_xlsx("imf-who-covid-19-vaccine-supply-tracker.xlsx", 
                            sheet = 3)
names(covsup) <- 
  unlist(covsup[2,]) %>% 
  gsub("%", "perc", ., fixed = T) %>% 
  make.names() %>% 
  cleanname() %>% cleanname()
covsup <- covsup[3:nrow(covsup),] %>% dfdt()
tonum <- setdiff(names(covsup), c( "countries.and.areas", "iso3"))
covsup[,(tonum):=lapply(.SD, as.numeric), .SDcols = tonum]

# Variables to get: 
# ISO 3-digit country code
# Population
# 
# Final total secured and/or expected vaccine supply
# Vaccine needed to fully vaccinate 70% of the country or area's population
covsup <- covsup[, .(
  iso3c = iso3,
  pop = population,
  sec_cov = secured.and.or.expected.vaccine.perc.of.population.,
  needed = vaccine.needed.to.reach.70perc.of.population.millions.of.courses.,
  sec = secured.and.or.expected.vaccine.millions.of.courses.,
  bilat_cov = bilateral.deals.perc.of.population.,
  covax_cov = covax.total.perc.of.population.,
  eu_cov = eu.deal.perc.of.population.,
  other_cov = other.sources.perc.of.population.,
  dom_cov = domestic.supply.perc.of.population.
)]

# the variable `secured and or expected vaccine perc of population`
# should indicate the percent of the population covered. (just making
# sure I understand the data)
perc_dis <- function(x,y){
  2 * abs(x - y)/ (x+y)
}

waitifnot(all(perc_dis(covsup$sec_cov, (
  (covsup$sec * 10 ^ 6) / covsup$pop * 100
)) < 0.01))

covsup[,c("needed", "sec"):=NULL]

# WEO ---------------------------------------------------
weo_oct_21 <- fread("WEOOct2021all.txt")
weo_oct_21 <- 
  weo_oct_21[`Subject Descriptor` == 
               "Gross domestic product per capita, current prices" & 
               Units == "Purchasing power parity; international dollars",
             .(iso3c = ISO, gdppc = `2021`)]
weo_oct_21$gdppc <- as.numeric(gsub(",", "", weo_oct_21$gdppc))
weo_oct_21 <- na.omit(weo_oct_21)
check_dup_id(weo_oct_21, c("iso3c"))

# Economist Excess Mortality -----------------------------
exc_d <- fread("economist_excess_deaths.csv")
exc_d[country == "Congo", country:="congo kinshasa"]
exc_d[,iso3c:=name2code(country)]
exc_d <- exc_d[!is.na(iso3c)]
exc_d$country <- NULL
exc_d[,excess:=(lower + upper)/2]

# Income groups ---------------------------------------------------
income <- readstata13::read.dta13("historical_wb_income_classifications.dta")
income <- income %>% dfdt()
income <- income[year==2021]

# Merge ---------------------------------------------------
dt <- merge(exc_d, weo_oct_21, all.x = T, by = "iso3c")
dt <- merge(dt, income[,.(income, iso3c)], all.x = T, by = "iso3c")
dt <- merge(dt, covsup, by = "iso3c", all.x = T)

check_dup_id(dt, c("iso3c"))
setwd(input_dir)

# Income averages: ---------------------------------------------------
for (i in c("lower", "upper", "official", 
            "gdppc", "excess", "sec_cov", 
            "bilat_cov", "covax_cov", "eu_cov",
            "other_cov", "dom_cov")) {
  var_name <- paste0("inc_", i)
  dt <- dt[,(var_name):= weighted_mean(eval(as.name(i)), pop, na.rm = T),
     by = income][]
}
waitifnot(length(na.omit(unique(dt$inc_excess)))==4)
waitifnot(length(na.omit(unique(dt$inc_gdppc)))==4)
waitifnot(length(na.omit(unique(dt$inc_upper)))==4)
waitifnot(length(na.omit(unique(dt$inc_lower)))==4)
waitifnot(all(na.omit(dt$upper>=dt$lower)))
waitifnot(all(na.omit(dt$excess>=dt$lower)))
waitifnot(all(na.omit(dt$inc_excess>=dt$inc_lower)))
waitifnot(all(na.omit(dt$inc_upper>=dt$inc_lower)))

# GRAPH -------------------------------------------------------------------
dt$income <- dt$income %>% factor(levels = c("LIC", "LMIC", "UMIC", "HIC"))

option_log <- list(scale_x_log10(labels = scales::dollar_format(),
                     breaks = breaks_log(n = 6)))

for (i in c("sec_cov",
            "bilat_cov",
            "covax_cov",
            "eu_cov",
            "other_cov",
            "dom_cov")) {
  i <- "sec_cov"
  plot <- ggplot(dt[!is.na(income)], aes(
    x = gdppc,
    y = eval(as.name(i)),
    color = income
  )) + 
    geom_point() +
    my_custom_theme +
    scale_color_custom +
    labs(y = "Total vaccine coverage purchased\n(% of population)", x = "GDP per capita") +
    scale_x_log10(labels = scales::dollar_format(),
                  breaks = breaks_log(n = 6)) + 
    ggrepel::geom_text_repel(data = dt[income!="LIC"], 
                             aes(label = code2name(iso3c)), 
                             show.legend = FALSE)
  
  ggsave(paste0(i, "gdppc_scatter_income.pdf"),
         plot,
         width = 10,
         height = 7)
  
  plot <- ggplot(dt, aes(
    x = excess,
    y = eval(as.name(i)),
    color = income
  )) + 
    geom_point() +
    my_custom_theme +
    scale_color_custom +
    labs(y = i, x = "Excess Mortality") + 
    scale_x_log10(labels = scales::pretty_breaks(n = 6),
                  breaks = breaks_log(n = 6))
  
  ggsave(paste0(i, "excess_mort_scatter_income.pdf"),
         plot,
         width = 7,
         height = 5)
}


# Check WHO ---------------------------------------------------------------

# compare with our data:
load("C:/Users/user/Dropbox/CGD/Projects/refute_mestieri/input/index_convergence.RData")

setwd(paste0(raw_dir, "/who_check"))

dli <- dir()
dli <- lapply(dli, read_xlsx)
dli <- rbindlist(dli, fill = T)
check_dup_id(dli, c("YEAR", "ANTIGEN", "COVERAGE"))
dli <- dli[,.(year = YEAR, 
              antigen = ANTIGEN, 
              desc = ANTIGEN_DESCRIPTION, 
              coverage = COVERAGE)]

fix_countries <- setDT(fix_countries)[,
                      .(label, year, meanval = w.meanval)][order(label)]

temp_bridge <- fread("desc	label
BCG	
DTP-containing vaccine, 3rd dose	Diptheria
DTP-containing vaccine, 3rd dose	Tetanus
DTP-containing vaccine, 3rd dose	Pertussis
Hib3	Hib
HepB3	Hepb
HPV VACCINATION PROGRAM COVERAGE - FIRST DOSE - FEMALES Target population who received the first dose of HPV vaccine in the reporting year	HPV
HPV Vaccination coverage by age 15, complete schedule, females	HPV
Measles-containing vaccine, 2nd dose	Measles
Pneumococcal conjugate vaccine, final dose	Pneumococcal
Inactivated polio-containing vaccine, 1st dose	
Polio, 3rd dose	Polio
Protection at birth (PAB) against neonatal tetanus	Tetanus
Rotavirus, last dose	Rotavirus
Rubella-containing vaccine, 1st dose	Rubella
Yellow fever vaccine	
")

dli <- merge(dli, temp_bridge, by = "desc", all.x = T, allow.cartesian = T) %>% as.data.frame() %>% dfcoalesce.all()
fix_countries <- merge(fix_countries, temp_bridge, by = "label", all.x = T, allow.cartesian = T) %>% as.data.frame() %>% dfcoalesce.all() %>% dfdt()
check <- merge(fix_countries, dli, by = c("desc", "label", "year"))
plot <- ggplot(check,
       aes(
         x = coverage,
         y = meanval,
         color = label,
         group = label,
         label = year
       )) + geom_point() +
  labs(x = "WHO", y = "Our Estimate", title = "Global Estimates of Vaccine Coverage") +
  my_custom_theme +
  geom_abline(intercept = 0,
              slope = 1,
              size = 0.5) + 
  coord_cartesian(ylim = c(0, 100), xlim = c(0, 100)) + 
  geom_text_repel(show.legend = FALSE) + 
  scale_color_custom

setwd(input_dir)
ggsave("global_estimates_comparison2.pdf", plot)









