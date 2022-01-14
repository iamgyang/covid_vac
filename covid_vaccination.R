rm(list = ls()) # clear the workspace

# Options: ----------------------------------------------------------------

# debugging
options(error=browser)
options(error=NULL)

# disable data.table auto-indexing (causes errors w/ dplyr functions)
options(datatable.auto.index = FALSE)

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
overleaf_dir <- paste0("C:/Users/", user, "/Dropbox/Apps/Overleaf/Covid Rollout Historical")
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
    "purrr", "OECD", "RobustLinearReg", "forcats", "WDI", "readxl", "httr", "googlesheets4")
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

custom_title <- function(x) {
  gsub("(^|[[:space:]])([[:alpha:]])", 
       "\\1\\U\\2",
       x,
       perl = TRUE)
}

var2oleaf <- function(some_variable, name = deparse(substitute(some_variable))) {
  name_of_file <- paste0(overleaf_dir,"/", name, ".txt")
  cat(some_variable, file = name_of_file)
}

# HPV Global Cancer Observatory ------------------------------------

# Global Cancer Observatory data on vaccinations (from UNICEF)
# https://data.unicef.org/resources/dataset/immunization/

url1<-'https://data.unicef.org/wp-content/uploads/2021/07/wuenic2020_hpv-estimates.xlsx'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
GCO_hpv <- readxl::read_xlsx(tf) %>% as.data.table()

# GCO_hpv <-
  # readxl::read_xlsx('wuenic2020_hpv-estimates.xlsx') %>% as.data.table()

GCO_hpv <-
  GCO_hpv[, .(iso3c, year, vaccine, coverage, vaccine_desc)]
GCO_hpv <-
  GCO_hpv[vaccine_desc %in% c(
    'Target population who received the first dose of HPV vaccine in the reporting year',
    'Target population who received the last dose of HPV vaccine in the reporting year'
  )]
GCO_hpv[vaccine_desc == 'Target population who received the first dose of HPV vaccine in the reporting year', 
        HPV_type := "first"]
GCO_hpv[vaccine_desc == 'Target population who received the last dose of HPV vaccine in the reporting year', 
        HPV_type := "last"]
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

# Flu vaccination OECD ---------------------------------------------------------
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
yf_df <- yf_df[, .(iso3c, year, coverage = value*100)]
yf_df$vaccine <- "yellow fever"
yf_df$source <- "Dryad"

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
              # people_vaccinated_per_hundred
df_covid$cov <- as.numeric(df_covid$cov)
df_covid <- df_covid[!is.na(cov)]
df_covid_mo <- df_covid %>% as.data.frame() %>% as.data.table()
df_covid <- df_covid[,.(coverage = max(na.omit(cov), na.rm = T)), 
                     by = .(iso3c, year = lubridate::year(date))]
df_covid <- df_covid[!grepl("OWID", df_covid$iso3c),]
df_covid[,vaccine:="Covid"]


# GDP per capita from WEO -------------------------------------------------

# we get constant PPP GDP per capita from WEO's October 2021 release.
weo_oct_21 <- fread("WEOOct2021all.txt")

weo_oct_21 <-
  weo_oct_21[`Subject Descriptor` ==
               "Gross domestic product per capita, constant prices"&
               Units == "Purchasing power parity; 2017 international dollar"]
weo_oct_21 <- as.data.frame(weo_oct_21)
weo_oct_21 <-
  weo_oct_21[, c("ISO", "Country", as.character(seq(1980, 2021)))]
weo_oct_21 <- weo_oct_21 %>%
  pivot_longer(., as.character(seq(1980, 2021)),
               names_to = "year")
weo_oct_21$value <- as.numeric(gsub(",", "", weo_oct_21$value))
weo_oct_21$year <- as.numeric(weo_oct_21$year)

weo_oct_21 <- weo_oct_21 %>% 
  rename(iso3c = ISO) %>% 
  dplyr::select(iso3c, year, value) %>% 
  rename(weo_gdppc = value) %>% 
  na.omit() %>% 
  dfdt()

waitifnot(nrow(weo_oct_21)>0)

# merge WDI and Maddison GDP PPP estimates -------------------------

mad <- merge(mad, weo_oct_21,by = c('year','iso3c'), all = TRUE)

# confirm we have unique iso3c years
waitifnot(nrow(distinct(mad[,.(year, iso3c)]))==nrow(mad[,.(year, iso3c)]))

# Get 2019 and 2021 GDP figures by using growth from WEO figures to 
# project forwards the Maddison GDP figures.
mad <- mad[order(iso3c,year)]
mad[,weo_gdppc_gr:=weo_gdppc/shift(weo_gdppc), by = 'iso3c']

for (i in seq(2015, 2021)) {
  mad[, gdppc.n := shift(gdppc) * weo_gdppc_gr, by = 'iso3c']
  mad[year == i & is.na(gdppc), gdppc := gdppc.n]
  mad <- as.data.table(mad)
}

waitifnot((mad[iso3c == "USA"][year == 2021]$gdppc>0)==TRUE)

mad[,(c('gdppc.n', 'weo_gdppc', 'weo_gdppc_gr')):=NULL]
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
df[,c("source_url","country", "popwork"):=NULL]
df <- df[!is.na(iso3c)]

# WB income classification
df <- merge(df, wb_income, by = c("iso3c"), all.x = T) %>% dfdt()

# Vaccine discovery
df <- merge(df, vac_disc, by = c("disease"), all.x = T) %>% dfdt()

# Merge in GDP
df <- merge(df, mad, by = c("iso3c", "year"), all.x = T) %>% dfdt()

setwd(input_dir)
save.image("post_merge.RData")


# Adjustments -------------------------------------------------------------
setwd(input_dir)
load("post_merge.RData")

# ! We take the Yellow Fever data from 'Dryad' (Shearer et. al.'s untargeted 
# unbiased estimates of vaccination coverage), as opposed to from the WHO data,
# since Shearer's data encompasses the WHO data.
df <- df[!(disease=="yellow fever" & (source=="WHO/UNICEF")) | is.na(source)]
waitifnot("Dryad" == df[disease =="yellow fever"]$source %>% unique())

# adjust vaccine names (some are purposely changed to other names to merge the
# series)
df[vaccine == "DTP 1st dose", vaccine := "DTP1"]
df[vaccine == "DTP 2nd dose", vaccine := "DTP2"]
df[vaccine == "DTP 3rd dose", vaccine := "DTP3"]
df[vaccine == "Tet Tox / DTaP/Tdap", vaccine := "DTP3"]
df[vaccine == "MCV1", vaccine := "MCV1"]
df[vaccine == "measles", vaccine := "MCV1"]
df[vaccine == "Measles", vaccine := "MCV1"]
df[vaccine == "Polio 1st dose", vaccine := "POL1"]
df[vaccine == "Polio 2nd dose", vaccine := "POL2"]
df[vaccine == "Polio 3rd dose", vaccine := "POL3"]
df[vaccine == "ROTAC", vaccine := "RCV1"]
df[vaccine == "Smallpox", vaccine := "smallpox"]
df[vaccine == "yellow fever", vaccine := "YFV"]

# within each disease, get the vaccine that has the longest time series coverage
long_series <- na.omit(df[,.(cov = length(na.omit(coverage))),by = .(disease, vaccine)])
long_series[,mc:=max(cov),by = .(disease)]
long_series <- long_series[mc==cov][order(vaccine, decreasing = TRUE)][order(disease, decreasing = TRUE)]
long_series <- long_series[,n1:=1][,n2:=cumsum(n1), by = .(disease)][n2==1]
long_series <- long_series[,.(disease, vaccine, keep = n1)]
long_series[disease %in% c("tetanus", "diphtheria","pertussis"), vaccine:="DTP3"]
check_dup_id(long_series, "disease")
df <- merge(df, long_series, by = c("disease", 'vaccine'), all = T)
df <- df[keep == 1]
df[, keep := NULL]

# Divide Africa 1968 data by 0.22 to get estimated childhood vaccination rate. 
# This means that we would assume that all those vaccines are going to children.
df[notes == "denominator is total population", coverage := min(coverage/0.22, 100)]

# The flu data comes also in doses per capita. Delete this, as we have a longer series
# for flu vaccination coverage in people above 65.
df <- df[notes!="doses per per capita"|is.na(notes)]

# Get the maximum coverage per year-disease-country. e.g. let's say we have the DTP vaccine coverage and the Pertussis
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

setwd(input_dir)
save.image("prior to replacing w zero.RData")
load("prior to replacing w zero.RData")

# For each disease, get the years where there is AT LEAST 1 country 
# with coverage estimates:
dis_yr <- 
  distinct(na.omit(df[,.(iso3c, year, coverage, disease)])[,.(disease, year)])

df <- merge(dis_yr, df, all.x = T, by = c("disease", "year"))

setwd(input_dir)
save.image("adjustments.RData")

# Summary Statistics about data coverage: ----------------------------
# for each disease, how many countries do we have?

setwd(input_dir)
load("adjustments.RData")

# first year coverage is >40%:

a <- dfdt(df)
a <- a[order(disease,iso3c,year)]
a[!is.na(coverage), minyr := min(year), by = c("disease", "iso3c")]
a <- a[year==minyr]
check_dup_id(a, c("disease","iso3c"))
cat("across ___ diseases")
a$disease %>% unique() %>% length()
cat("across ___ countires")
a$iso3c %>% unique() %>% length()
cat("across ___ country-diseases")
a %>% nrow()
cat("we have ___ cases where the FIRST coverage is >40%")
sum(a$coverage>40)

# graph of coverage for each disease:

# percent of population that we have data for:
df[is.na(coverage), perc_pop_prior_zero := 0]
df[!is.na(coverage), perc_pop_prior_zero := perc_pop]

dta_avail_df <-
  distinct(df[, .(
    perc_pop_prior_zero = sum(perc_pop_prior_zero, na.rm = T)),
    by = .(disease, year, glob_pop)])

# check that we have the right population end result for the 
# YF endemic countries
a <-
  pop[iso3c %in% intersect(yf_iso3c, who_keep_iso3c) & year == 2016] %>% dfdt()
check_dup_id(a, "iso3c")
waitifnot(abs(sum(a$poptotal) -
                dta_avail_df[disease == "yellow fever" &
                               year == 2016]$glob_pop) == 0)
setwd(input_dir)

# capitalize names of diseases
dta_avail_df[,disease:=unlist(lapply(disease, custom_title))]

# make plot
plot <- dta_avail_df %>%
  filter(year<=2021) %>% 
  ggplot(.,
         aes(
           x = year,
           y = perc_pop_prior_zero,
           group = disease,
           color = disease
         )) +
  geom_line(size = 1) +
  my_custom_theme +
  scale_color_custom +
  labs(y = "Percent of Population\nwith Data Available", x = "")

setwd(overleaf_dir)
ggsave("line_coverage_plot.pdf", plot, width = 8, height = 5)
setwd(input_dir)

# Make sure that the microbe ID, licensing year, schedule year, 
# and income is filled:
df <- df %>%
  arrange(disease) %>%
  group_by(disease) %>%
  fill(microbe_id, .direction = "downup") %>%
  fill(licensing, .direction = "downup") %>%
  ungroup() %>%
  arrange(iso3c, disease) %>%
  group_by(iso3c, disease) %>%
  fill(yr_sched, .direction = "downup") %>%
  ungroup() %>%
  arrange(iso3c, year) %>%
  group_by(iso3c) %>%
  # IMPORTANT: for INCOME, we fill BACKWARDS (i.e. assume that 
  # if the country was LIC in 1970, then it was a LIC in 1960)
  fill(income, .direction = "downup") %>%
  as.data.frame() %>%
  as.data.table()

# IF THE COVERAGE IS MISSING, REPLACE WITH 0! (checked that this is what WHO
# does with their data when estimating coverage)

df[is.na(coverage), coverage := 0]

# Create a table that shows the global coverage estimates.
# First get a population that we use specifically for this purpose:
df[disease == "influenza", pop_wm := pop_t_ge65]
df[disease == "HPV", pop_wm := pop_f_le15]
df[disease == "yellow fever", pop_wm := poptotal]
df[disease != "influenza" & 
     disease != "HPV" & 
     disease != "yellow fever", pop_wm := pop_t_le1]
df[disease == "covid-19", pop_wm := poptotal]

df_disease_avg <- 
  df[, .(coverage = weighted_mean(coverage, pop_wm, na.rm = T),
      num_c = length(unique(iso3c)),
      perc_pop = sum(na.omit(perc_pop), na.rm = T),
      w_stdev = w.sd(coverage, pop_wm)
      ), 
     by = c("year", "disease")] %>% dfdt()

# Create a table that shows the global coverage estimates by INCOME too:
df_disease_avg_income <- 
  df[, .(coverage = weighted_mean(coverage, pop_wm, na.rm = T),
         num_c = length(unique(iso3c)),
         perc_pop = sum(na.omit(perc_pop), na.rm = T),
         w_stdev = w.sd(coverage, pop_wm)
         ), 
     by = c("year", "disease", "income")] %>% dfdt()

# Check that in each year, we're looking at the total global population:
df_disease_avg_income <- df_disease_avg_income[!is.na(income)]
df_disease_avg_income[,perc_glob_pop:=sum(perc_pop, na.rm = T), by = .(year, disease)]
waitifnot(nrow(df_disease_avg_income[abs(perc_glob_pop-1)>0.0000001])==0)

# Merge the two:
df_disease_avg[,income:="Global"]
df_disease_avg <- 
  rbindlist(
    list(
      df_disease_avg, 
      df_disease_avg_income
    ), fill = T
  )

df_disease_avg$perc_glob_pop <- NULL

setwd(input_dir)
save.image("summ_stats.RData")



# RANDOM STATS ------------------------------------------------------------

setwd(input_dir)
load("summ_stats.RData")


# subsaharan africa fully vaccinated %:
# OECD fully vaccinated date:
j <- df_covid %>% dfdt()
j[,region:=countrycode(iso3c, "iso3c", "region")]
j <- merge(j, pop, by = c('iso3c', 'year'), all.x = T)
j <- j[,.(iso3c, year, coverage, region, pop = poptotal)]
oecd_names <- c("AUSTRALIA","AUSTRIA","BELGIUM","CANADA","CHILE","COLOMBIA","COSTA RICA","CZECH REPUBLIC","DENMARK","ESTONIA","FINLAND","FRANCE","GERMANY","GREECE","HUNGARY","ICELAND","IRELAND","ISRAEL","ITALY","JAPAN","KOREA","LATVIA","LITHUANIA","LUXEMBOURG","MEXICO","NETHERLANDS","NEW ZEALAND","NORWAY","POLAND","PORTUGAL","SLOVAK REPUBLIC","SLOVENIA","SPAIN","SWEDEN","SWITZERLAND","TURKEY","UNITED KINGDOM","UNITED STATES")

j <- j[iso3c%in%name2code(oecd_names) | 
         region == "Sub-Saharan Africa"]


# manually fill in Czech Republic because it's missing:
# https://www.worldometers.info/world-population/czech-republic-population/
j[iso3c == "CZE", pop:=10737322]

waitifnot(nrow(j[iso3c == name2code("South Africa")])>=1)
waitifnot(nrow(j[iso3c == name2code("Egypt")])==0)
waitifnot(nrow(j[iso3c == name2code("Chile")])>=1)

j[region!="Sub-Saharan Africa",region:="OECD"]
j <- j[,.(coverage = weighted_mean(coverage, pop, na.rm = T)), by = region]

# SubSaharan Africa Coverage
sub_sh_cov <- j[region=="Sub-Saharan Africa",]$coverage %>% signif(2)
var2oleaf(sub_sh_cov)
# OECD coverage
oecd_cov <- j[region=="OECD",]$coverage %>% signif(2)
var2oleaf(oecd_cov)

# Global # fully vaccinated 
j <- df_covid %>% dfdt()
j <- merge(j, pop, by = c('iso3c', 'year'), all.x = T)
j <- na.omit(j[year == 2021,.(iso3c, coverage, poptotal)])
j[,(sum((coverage/100)*poptotal, na.rm = T))]/(10^9)

# Global coverage:
j <- merge(j, wb_income, by = "iso3c")
j[income == "LIC"|income == "LMIC"][order(poptotal)][coverage<40] %>% nrow()
j[income == "HIC"][order(poptotal)][coverage>=40] %>% nrow()

# time between COVID vaccine dev and 20% coverage: ---
j <- merge(CJ(iso3c = unique(df_covid_mo$iso3c), date = unique(df_covid_mo$date)), 
           df_covid_mo, all.x = T, by = c("iso3c", "date"))
j <- merge(j, pop[year == 2021,.(iso3c, poptotal)], by = "iso3c", all = T)
j <- j[order(iso3c, date)]

# fill downwards the missing data:
j <- j %>%
  group_by(iso3c) %>%
  fill(cov, .direction = "down") %>% 
  dfdt()
j[is.na(cov), cov:=0]

# earliest date for 20% coverage
j <- j[, .(cov = weighted_mean(cov, poptotal, na.rm = T)), by = c("date")]
date_20 <- j[cov>20]$date %>%na.omit %>%  min()

# Months between this date and FDA first vaccine EUA approval: The first EUA,
# issued Dec. 11, for the Pfizer-BioNTech COVID-19  Vaccine for individuals 16
# years of age and older was based on safety and effectiveness data from a
# randomized, controlled, blinded ongoing clinical trial of thousands of
# individuals.
as.numeric((as.Date(date_20) - as.Date("2020-12-11"))/31)

setwd(input_dir)
save.image('prior_to_graphs_tables.RData')
load('prior_to_graphs_tables.RData')

# GRAPHS AND TABLES -------------------------------------------------------

# Line: UK/Developing World across time ----------------------------

toplot <- df[disease %in% c("diphtheria", "polio"),.(income, iso3c, coverage, year, disease, pop_t_le1)]
toplot <- toplot[income == "LIC" | income == "LMIC" |
                   iso3c == "GBR"]

# in 1974 it is estimated that fewer than 5% of children in developing
# countries were receiving a third dose of DTP and poliomyelitis diseases in
# their first year of life
addendum <- fread(
  "dev_uk	coverage	year	disease
Developing World 	0	1948	POL3
UK	0	1948	POL3
Developing World 	0	1955	DTP3
UK	0	1955	DTP3
UK	65	1964	POL3
UK	70	1964	DTP3
Developing World 	5	1974	POL3
Developing World 	5	1974	DTP3
"
)

toplot <-
  toplot %>% as.data.frame() %>%
  dplyr::select(iso3c, year, coverage, disease, pop_t_le1) %>%
  mutate(dev_uk = case_when(iso3c == "GBR" ~ "UK",
                            iso3c != "GBR" ~ "Developing World")) %>%
  group_by(dev_uk, year, disease) %>%
  summarise(coverage = weighted_mean(coverage, pop_t_le1)) %>%
  rbind(as.data.frame(addendum))
check_dup_id(toplot, c("dev_uk", "year", "disease"))

toplot <- toplot %>%
  mutate(disease = case_when(disease == "diphtheria" ~ "DTP",
                             disease == "polio" ~ "Polio",
                             disease == "DTP3" ~ "DTP",
                             disease == "POL3" ~ "Polio"))

plot <- toplot %>%
  filter(year !=1978) %>% 
  ggplot(.,
         aes(
           x = year,
           y = coverage,
           group = interaction(disease, dev_uk, sep = " "),
           color = interaction(disease, dev_uk, sep = " ")
         )) +
  geom_point(data = setDT(toplot)[year == 2020], show.legend = FALSE) +
  geom_line() +
  my_custom_theme +
  scale_color_custom +
  labs(y = "", x = "") +
  scale_x_continuous(breaks = seq(1970, 2020, 10),
                     limits = c(1970, 2020))+
  # size of legend
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(legend.key.size = unit(3, 'mm')) +
  theme(legend.key.width = unit(2.5,"mm"))

setwd(overleaf_dir)
ggsave("line_uk_dev.pdf",
       plot,
       width = 7,
       height = 5)
setwd(input_dir)


# Table: 20, 40, 75% coverage ---------------------------------------------

# what year for each disease did we achieve XYZ% coverage?
cov_targets <- c(20, 40, 75)

disease_accomplish_out <- CJ(
  disease = na.omit(unique(df_disease_avg$disease)),
  income = c(na.omit(unique(df$income)), "Global"),
  coverage = cov_targets
)

# Loop through the table above, which gives coverage by income group for each 
# disease across time. Then, get the earliest year where we saw above an
# X% coverage rate for that group. Insert that value into the output table.
for (d in unique(df_disease_avg$disease)) {
for (t in cov_targets) {
for (i in unique(disease_accomplish_out$income)) {
      a <-
        min(df_disease_avg[coverage >= t &
                             disease == d &
                             income == i]$year, na.rm = T)
      disease_accomplish_out[disease == d &
                           coverage == t & income == i, year := a]
      disease_accomplish_out <- as.data.table(disease_accomplish_out)
}
}
}

# replace infinite values with finite ones:
invisible(lapply(names(disease_accomplish_out),function(.name) set(disease_accomplish_out, which(is.infinite(disease_accomplish_out[[.name]])), j = .name,value =NA)))

# pivot wider:
disease_accomplish_out <- disease_accomplish_out %>% spread(., coverage, year)

# for the following graph we're only interested in global estimates:
disease_accomplish <- as.data.table(disease_accomplish_out[income == "Global"])
disease_accomplish$income <- NULL
options(knitr.kable.NA = '')
# make table presentable:
kbl_20_75 <- disease_accomplish %>% 
  mutate(disease = unlist(lapply(disease, custom_title))) %>% 
  rename_with(str_to_title) %>% 
  kbl("latex", booktabs = T, 
      caption = "Year world achieved a certain percent of vaccination",
      label = "table:kbl_20_75") %>%
  kable_styling(latex_options = "striped", position = "center")

setwd(overleaf_dir)
save_kable(kbl_20_75, "kbl_20_75.tex", header = FALSE)
setwd(input_dir)

# Dot-Line: Microbe identify, vaccine produced -----------------------------------------------

prog_df <-
  merge(disease_accomplish,
        vac_disc,
        by = c("disease"),
        all = T) %>% dfdt()

# make the disease names pretty
prog_df$disease <- prog_df$disease %>% lapply(., custom_title)

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
prog_df[is.na(`75`), endpoint:= 2020]
prog_df[!is.na(`75`), endpoint:= `75`]
prog_df[disease=="Smallpox", endpoint:=1980]

# STOP HERE ---------------------------------------------------------------
write_sheet(prog_df, ss = "https://docs.google.com/spreadsheets/d/1fpwpesVd74Dr5eF4zK4RIMvKwOIGlsB1xeRPFVnWPdo/edit?usp=sharing", sheet = 1)


# For infections with vaccines, excluding smallpox and Covid-19, the average
# period between microbe isolation and vaccine development was [x] years.  For
# vaccines that have reached a given milestone, the average time between
# vaccine development and 20 percent global coverage was [x] years, 40 percent
# coverage was [y] years and 75% coverage [z] years.  

mean(unlist(prog_df[disease != "Covid-19" &
    disease != "Smallpox"][, 
    .(licensing - microbe_id)]), na.rm = T)
mean(unlist(prog_df[disease != "Covid-19" &
    disease != "Smallpox"][, 
    .(`20` - licensing)]), na.rm = T)
mean(unlist(prog_df[disease != "Covid-19" &
    disease != "Smallpox"][, 
    .(`40` - licensing)]), na.rm = T)
mean(unlist(prog_df[disease != "Covid-19" &
    disease != "Smallpox"][, 
    .(`75` - licensing)]), na.rm = T)
setwd(input_dir)
save.image("graphs_prior_global_means.RData")
# Gaps by income group between vaccine licensing and addition to schedule, 
# Line: Global means and stdevs -------------------------------------------
# restrict to be all from 0-100 on y axis and 1980-2020 on x axis:
setwd(input_dir)
load("graphs_prior_global_means.RData")

# restrict to be all from 0-100 on y axis and 1980-2020 on x axis:

ggplot_restrictions <-
  list(coord_cartesian(ylim = c(0, 105), xlim = c(1980, 2021)))

# separate data-table to graph:
jx <- df_disease_avg %>% dfdt()

# prettify disease names:
jx[,disease:=unlist(lapply(disease, custom_title))]

# merge DTP vaccines together after checking that AFTER 1980, 
# they indeed are the same.
a <-jx[disease == "Pertussis" | disease == "Diphtheria" | disease == "Tetanus"]
a$disease <- NULL
a <- unique(a)
waitifnot(nrow(unique(a[year>=1980]))==nrow(jx[disease=="Pertussis" & year>=1980]))
jx[disease == "Pertussis" | disease == "Diphtheria" | disease == "Tetanus", 
   disease:="DTP"]
jx <- jx[!(disease=="DTP" & year <= 1980),]
waitifnot(length(unique(jx$disease))==14)

# Exclude smallpox b/c data only goes from pre-1970 and flu because 
# data is also super patchy
d2graf <- unique(jx$disease)
d2graf <- setdiff(d2graf, c('Smallpox', 'Influenza'))

# for diseases NOT COVID, delete 2021 data because it's so patchy:
jx[year %in% c(2021) & disease != 'Covid-19', coverage := NA]
jx[year %in% c(2021) & disease != 'Covid-19', w_stdev := NA]
jx[year %in% c(2021) & disease != 'Covid-19', w_stdev := NA]
jx[year %in% c(2021) & disease != 'Covid-19', coverage := NA]

# For HPV, delete 2020 data because it's so patchy:
# As of 11-18-2021, only 44 countries that we have HPV for in 
# 2020 (compared to 22 and 54 for 2019, respectively).
jx[year %in% c(2020) & disease %in% c('HPV'),
   (c('coverage', 'w_stdev')) := NA]

# make covid come last:
jx$disease <- jx$disease %>%
  factor(., levels = c(setdiff(sort(unique(jx$disease)), 'Covid-19'),'Covid-19'))

plot <-
  ggplot(jx[disease %in% d2graf & income == "Global" & year <= 2021], # we're including a year restriction because COVID data for 2022 as of 1/7/2022 is pretty crappy right now
         aes(x = year, group = disease, color = disease)) +
  geom_line(aes(y = coverage, group = disease, color = disease)) +
  geom_ribbon(
    aes(
      ymin = coverage - 1 * w_stdev,
      ymax = coverage + 1 * w_stdev,
      group = disease
    ),
    color = 'grey83',
    alpha = 0.2
  ) +
  my_custom_theme +
  labs(y = 'Coverage (%)', x = '') +
  theme(legend.position = 'none') +
  ggplot_restrictions +
  facet_wrap(. ~ disease, scales = 'free') +
  scale_color_custom

setwd(overleaf_dir)
ggsave(paste0('mean_w_stdev_facetted.pdf'),
       plot,
       width = 10,
       height = 7)
setwd(input_dir)

# Line: Income group coverage over time ------------------------------
jx[,income:=factor(income, levels = c("HIC", "UMIC", "LMIC", "LIC"))]
plot <- ggplot(jx[disease %in% d2graf & income != "Global"],
       aes(
         x = year,
         y = coverage,
         color = income,
         group = income
       )) +
  scale_x_continuous(limits = c(1980, 2021)) +
  geom_line() +
  facet_wrap( ~ disease) +
  scale_color_custom +
  my_custom_theme +
  labs(y = "Coverage (%)", x = "") + 
  guides(color = guide_legend(override.aes = list(size = 3))) +
      theme(legend.key.size = unit(3, 'mm')) +
      theme(legend.key.width = unit(2.5,"mm"))
setwd(overleaf_dir)
ggsave("line_income.pdf", plot, width = 11, height = 6)
setwd(input_dir)

# random stats:

for (i in c("HIC", "UMIC", "LMIC", "LIC")) {
  assign(paste0(tolower(i), "_meas_minyr"), 
         min(jx[disease == "Measles" & coverage > 20 & income == i]$year, na.rm = T)
         )
}
for (i in c("UMIC", "LMIC", "LIC")) {
  assign(paste0(tolower(i), "_addtl_meas_yr"), 
         eval(as.name(paste0(tolower(i), "_meas_minyr"))) - hic_meas_minyr
  )
}

var2oleaf(hic_meas_minyr)
var2oleaf(umic_addtl_meas_yr)
var2oleaf(lmic_addtl_meas_yr)
var2oleaf(lic_addtl_meas_yr)

# get the covid coverage:
h <- merge(CJ(iso3c = unique(df_covid_mo$iso3c), 
              date = unique(df_covid_mo$date)), 
              df_covid_mo, 
              all.x = T, by = c("iso3c", "date"))
h <- merge(h, pop[year == 2021,.(iso3c, poptotal)], by = "iso3c", all = T)

# fill downwards the missing data:
h <- h[order(iso3c, date)]
h <- h %>%
group_by(iso3c) %>%
fill(cov, .direction = "down") %>% 
dfdt()
h[is.na(cov), cov:=0]
h <- h[!is.na(date)]

# income groups
h <- merge(h,wb_income,by =c("iso3c"),all.x = T)

# global coverage covid by income group:
hii <-
  h[, .(coverage = weighted_mean(cov, poptotal, na.rm = T)), by = c("income", "date")] %>%
  na.omit %>% dfdt()

# Line: Early stage COVID -------------------------------------------------------
h <- h[,.(coverage=weighted_mean(cov, poptotal, na.rm = T)), by = "date"] %>% 
     na.omit %>% dfdt()
h[,year:=as.numeric(2021+(as.Date(date) - as.Date("2021-01-01"))/365)]
h[,disease:="Covid-19"]


# merge with average disease:
j <- df_disease_avg[income == "Global"& 
                      disease!="covid-19" & 
                      disease!="Covid-19",
                    .(disease, year, coverage)] %>% dfdt()
j <- rbindlist(list(j, h), fill = T)

j[,minyr:=min(year, na.rm = T), by = "disease"]
j[,yr:=year-minyr]
j[,disease:=unlist(lapply(disease, custom_title))]

# exclude the last year of flu:
j <- j[!(disease=="Influenza" & year == 2020)]

j <- j[order(disease, yr)]
j[,fin_yr:=max(yr),by = "disease"]
j$date <- NULL
j <- j %>% na.omit

# checks:
check_dup_id(j, c("disease", "yr"))
a <- j$disease %>% unique() %>% length()
waitifnot(a>10)
   

for (i in c(1, 2)) {
  if (i == 2) {
    # first 3 years:
    j <- j[yr <= 3]
    # j[disease!="Covid-19", fin_yr:=3]
  }
  set.seed(481494561)
  plot <- j %>% filter(disease != "Covid-19") %>%
    ggplot(., aes(x = yr, y = coverage, group = disease)) +
    geom_line(color = "grey79") +
    geom_line(
      data = j[disease == "Covid-19"],
      aes(x = yr, y = coverage),
      color = "maroon",
      size = 1
    ) +
    geom_point(
      data = j[disease != "Covid-19" & yr == fin_yr],
      aes(x = yr, y = coverage),
      size = 1.3,
      color = "grey79"
    ) +
    geom_point(
      data = j[disease == "Covid-19" & yr == fin_yr],
      aes(x = yr, y = coverage),
      size = 2,
      color = "maroon"
    ) +
    ggrepel::geom_text_repel(data = j[disease != "Covid-19" &
      yr == fin_yr],
      aes(x = yr, y = coverage, label = disease),
      color = "grey79") +
    geom_text(data = j[disease == "Covid-19" &
      yr == fin_yr],
      aes(x = yr+0.2, y = coverage+4, label = disease)) +
    my_custom_theme +
    scale_color_custom +
    labs(x = "Years from First Available Data", y = "Coverage (%)") +
    # theme(text = element_text(family = "Tahoma")) +
    scale_y_continuous(limits = c(0, 100))
  
  
  setwd(overleaf_dir)
  ggsave(paste0("line_early_covid", i, ".pdf"),
         plot,
         width = 8,
         height = 5)
  setwd(input_dir)
}

# Bar: Flu Measles Covid --------------------------------------------------
# Palache, A., Rockman, S., Taylor, B., Akcay, M., Billington, J. K., & Barbosa, P. (2021). Vaccine complacency and dose distribution inequities limit the benefits of seasonal influenza vaccination, despite a positive trend in use. Vaccine, 39(41), 6081-6087.
# Absolute number of seasonal influenza vaccine doses distributed - The total number of doses distributed in 2004 was approximately 262 million and this had risen to about 531 million in 2019, an overall 103% increase (Supplemental Fig. 1). However, compared to the peak number of doses distributed in 2014 of 534 million doses, the 2019 total represents a 0.3% decline. The overall growth in the number of doses distributed has largely been driven by the increase in absolute number of doses distributed in the Americas (AM) (a 154 million dose difference between 2004 and 2019).

j <- df_disease_avg[income == "Global" & 
               disease == "measles" & 
               year %in% c(2019)]$coverage

# we are using MCV1 for the measles comparison:
n_meas <- 
  (j/100) * sum(df[disease == "measles" & 
  year %in% c(2019)]$pop_wm, 
  na.rm = T)

# for flu, take it from the Palache paper:
n_flu <- 531 * 10^6

# for covid, get from OWID:
setwd(input_dir)
myfile <-
  RCurl::getURL(
    'https://covid.ourworldindata.org/data/owid-covid-data.csv',
    ssl.verifyhost = FALSE,
    ssl.verifypeer = FALSE
  )
df_covid <- read.csv(textConnection(myfile), header = T) %>% as.data.frame() %>% as.data.table()
n_covid <-
  df_covid[iso_code == "OWID_WRL"]$total_vaccinations %>% na.omit() %>% max() %>% (function(x) {
    x
  })/2

p <- data.table(
  disease = c("Measles", "Flu", "Covid-19"),
  n = c(n_meas, n_flu, n_covid)
)

# covid vaccinated is ___ x bigger than measles
x_meas <- (n_covid/n_meas) %>% signif(2)
setwd(overleaf_dir)
write.table(x_meas, file = "x_meas.txt", sep = "",
            row.names = FALSE, col.names = FALSE)

# covid vaccinated is ___ x bigger than flu
x_flu <- (n_covid/n_flu) %>% signif(2)
setwd(overleaf_dir)
write.table(x_flu, file = "x_flu.txt", sep = "",
            row.names = FALSE, col.names = FALSE)
setwd(input_dir)


color_ordered <- 
  c(
    "#800000",
    "#7B92A8",
    "#693C5E",
    "#008BBC",
    "#1B365D",
    "#97B6B0",
    "#D7D29E",
    "#ACA39A",
    "#3E647D",
    "#B6CFD0",
    "#1A476F",
    "#00677F",
    "#6E8E84",
    "#319B42",
    "#DFD1A7",
    "#FFBF3F",
    "#E4002B",
    "#9C8847",
    "#90353B",
    "#C10534",
    "#938DD2",
    "#BFA19C",
    "#2D6D66",
    "#696158",
    "#CAC27E",
    "#D6D2C4",
    "#82C0E9",
    "#DC4405"
  )


scale_color_custom <- 
  list(
    scale_color_manual(values = color_ordered),
    scale_fill_manual(values = color_ordered)
  )

plot <- 
  ggplot(p, aes(x = disease, y = n / (10 ^ 6), fill = disease)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "",
       y = "Annual Vaccinations\nDelivered (m)") +
  scale_y_continuous(
    breaks = scales::pretty_breaks(n = 13),
    labels = scales::comma_format(accuracy = 1)
  ) +
  my_custom_theme + 
  scale_color_custom + 
  theme(legend.position = "none")

setwd(overleaf_dir)
ggsave("bar_num_doses.pdf", plot, width = 8, height = 5)
setwd(input_dir)


# https://www.who.int/data/gho/indicator-metadata-registry/imr-details/4756
# Global and regional coverage is a weighted sum of WHO/UNICEF estimates of national coverage by target population from the United Nations Population Division's World Population Prospects. The size of the target population is the national annual number of infants surviving their first year of life. 



# Line: Coefficient of variation across time -----------------------

# get weighted mean
fix_countries <-
  df[, .(
    w.meanval = weighted_mean(coverage, pop_wm, na.rm = TRUE),
    w.stdev = w.sd(coverage, pop_wm)
  ),
  by = c("disease", "year")]
fix_countries[, w.coef.var := w.stdev / w.meanval]

# we drop smallpox because data is too patchy:
fix_countries[,disease:=unlist(lapply(disease, custom_title))]
fix_countries <- fix_countries[disease!="Smallpox" & disease != "smallpox"]
waitifnot(any(na.omit(fix_countries$disease=="Covid-19")))

# merge DPT again:
fix_countries <- fix_countries[!(disease %in%c("Pertussis", "Tetanus"))]
fix_countries[disease == "Diphtheria", disease:="DTP"]

setwd(input_dir)
save.image("index_convergence.RData")

setwd(input_dir)
load("index_convergence.RData")

# plot convergence
plot <-
  # we drop Flu in 2020 data sample is very small and likely is the reason for the artificial drop in coverage
  # (see Appendix figure where we plot data availability)
  ggplot(data = na.omit(fix_countries[disease!="Covid-19" & !(disease == "Influenza" & year == 2020)]), 
         aes(year, w.coef.var, 
             group = disease, color = disease)) +
  geom_line() +
  geom_point(show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1980, 2020, 10)) +
  coord_cartesian(xlim = c(1980, 2021)) + 
  labs(
    x = "",
    y = "Coefficient of Variation"
  ) +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 3))) +
  my_custom_theme +
  scale_color_custom
  # scale_y_log10(breaks = breaks_log(n = 10),
  #               labels = scales::comma_format(accuracy = 0.01)) + 
  # annotation_logticks(sides = "l")

setwd(overleaf_dir)
ggsave(
  paste0("coef_var_plot.pdf"),
  plot,
  width = 12,
  height = 7
)
setwd(input_dir)

# Between
# 2000 and 2020, the weighted coefficient for HiB fell from {[}X{]} to
# {[}Y{]}. For measles it fell from {[}X{]} to {[}Y{]}. For Covid-19
# vaccination, the coefficient of variation was {[}X{]} in {[}date{]} and
# dropped to {[}y{]} in {[}date{]}. {[}GY: how do we have two values for
# Covid-19? when are they?{]}
setwd(overleaf_dir)
for (y in c(2000, 2020)) {
  for (d in c("Measles", "Covid-19", "HiB")) {
    if(d == "Covid-19") {y <- 2021}
    assign(paste0("cv_", tolower(d), y),
           fix_countries[disease == d][year == y]$w.coef.var %>%
             signif(2))
    cat(
      eval(as.name(paste0("cv_", tolower(d), y))),
      file = paste0("cv_", tolower(d), y, ".txt")
    )
  }
}

# Dot: Preston curve ----------------------------------
setwd(input_dir)
save.image("preston_curve.RData")

setwd(input_dir)
load("preston_curve.RData")

jx <- df[, .(iso3c, year, coverage, disease)]
# relabel DPT:
a <-jx[disease == "pertussis" | disease == "diphtheria" | disease == "tetanus"]
a$disease <- NULL
a <- unique(a)
waitifnot(nrow(unique(a[year>=1980]))==nrow(jx[disease=="pertussis" & year>=1980]))
jx[disease == "pertussis",disease:="DTP"]
jx <- jx[!(disease%in%c("tetanus", "diphtheria"))]
jx <- jx[!(disease=="DTP" & year < 1980),]

# for Preston curves, we do not want to plot HPV, Smallpox, or Flu:
jx <- jx %>%
  filter(
    disease != "smallpox" &
      # Didn't want flu or HPV because we never reached 20%
      # global coverage.
      disease != "influenza" &
      disease != "HPV"
  ) %>%
  dfdt()

# make sure that the min year has more than 20 countries in it,
# and that we've achieved 20% vaccine coverage.
jx[coverage!=0 ,n:=.N, by = .(disease, year)]
a <- unique(na.omit(jx[,.(disease, year, n)]))
a <- a[n>10]
jx <- merge(a, jx, by = c("disease", "year"), all.x = T)
df_disease_avg <- df_disease_avg[!(disease=="diphtheria"|disease=="pertussis")]
df_disease_avg[disease=="tetanus", disease:="DTP"]

df_disease_avg <- df_disease_avg %>% unique() %>% filter(year>=1980) %>% dfdt()
jx <- merge(jx, df_disease_avg[income=="Global",.(year, 
                                                  disease, 
                                                  glob_c = coverage)],
            by = c("disease", "year"), all.x = T)
jx <- jx[glob_c>10]
jx[,minyr:=min(year), by = .(disease)]
jx[,maxyr:=max(year), by = .(disease)]
jx <- jx[year == minyr|year==maxyr]

# and remove any countries in the end year where it is 0.
# do this by taking the difference between the end year and the start year, and seeing if that EQUALs negative the start year value.
jx <- jx[order(disease, iso3c, year)]
jx[,fdiff:=coverage - shift(coverage,n=1,type = "lead"),by = .(disease, iso3c)]
jx <- jx[fdiff!=coverage | is.na(fdiff) | is.na(coverage)]

# get a fixed sample of countries in the start and end years
jx <- jx %>%
  dplyr::select(disease, iso3c, coverage, year) %>%
  pivot_wider(names_from = c(year), values_from = coverage) %>% 
  dfdt()
jx[, rsum := rowSums(!is.na(.SD)), 
   .SDcols = setdiff(names(jx), c("disease", "iso3c", "gdppc"))]
jx <- jx[rsum >= 2 | disease == "covid-19"]
waitifnot(all(jx$rsum[jx$disease!="covid-19"]==2))
jx <- jx %>% 
  dplyr::select(-rsum) %>% 
  pivot_longer(`1980`:`2016`, names_to = "year", values_to = "coverage") %>% 
  na.omit %>% 
  dfdt()

# check we've removed the final year == 0 situation
a <- jx %>% dfdt() 
a[,maxyr:=max(year), by = .(disease, iso3c)]
waitifnot(all(a[maxyr == year & disease!="covid-19" ]$coverage != 0))
a <- NULL

# make sure that HPV and Flu aren't included
jx$disease %>% 
  unique() %>% 
  grepl("smallpox|hpv|influenza", ., ignore.case = T) %>% 
  (function(x) {x==FALSE}) %>% 
  all() %>% 
  waitifnot()

# Create variables for labeling the start and end
jx[,minyr:=min(year), by = .(disease)]
jx[,maxyr:=max(year), by = .(disease)]
jx <- jx[year == minyr|year == maxyr]
jx[year==minyr, year_lab:="Start"]
jx[year==maxyr, year_lab:="End"]
jx[,disease_lab:=paste0(disease, "\n(", minyr, " - ", maxyr, ")")]

# make COVID-19 come last:
jx$disease_lab <- jx$disease_lab %>% lapply(custom_title) %>% unlist()
jx$disease_lab <- jx$disease_lab %>% 
  factor(levels = c(unique(jx[disease != "covid-19"]$disease_lab), 
                    unique(jx[disease == "covid-19"]$disease_lab)))

# merge back in GDPPC:
jx$year <- as.numeric(jx$year)
mad$year <- as.numeric(mad$year)
jx <- merge(jx, mad, by = c("year", "iso3c"), all.x = T)
jx <- jx %>% as.data.frame() %>% dfcoalesce.all() %>% dfdt()

# graph:
plot <- 
  ggplot(jx, 
         aes(x = gdppc, 
             y = coverage, 
             color = as.factor(year_lab), 
             group = as.factor(year_lab))) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(
    se = FALSE,
    size = 0.4,
    method = "lm",
    formula = (y~x),
    aes(
      x = gdppc,
      y = coverage,
      group = as.factor(year_lab),
      color = as.factor(year_lab)
    )
  ) + 
  annotation_logticks(sides = "b") +
  scale_x_continuous(labels=scales::dollar_format(accuracy = 1), trans = "log10") + 
  my_custom_theme + 
  labs(y = "Coverage (%)", subtitle = "", x = "GDP per capita")+
  scale_color_custom + 
  guides(colour = guide_legend(override.aes = list(size = 3))) + 
  facet_wrap(~disease_lab, scales = "free") + 
  scale_y_continuous(breaks = seq(0,100,20)) + 
  coord_cartesian(ylim = c(0, 100), xlim = c(377,154000)) + 
  theme(panel.spacing.x = unit(2, "lines"))

setwd(overleaf_dir)
ggsave(
  paste0("point_preston_linear.pdf"),
  plot,
  width = 10,
  height = 14,
  limitsize = FALSE#,
  # dpi = 400
)
setwd(input_dir)

# data on Preston curves:
preston_meas_yr_start <- jx[disease=="measles"]$minyr %>% unique() %>% as.numeric()
preston_meas_yr_end <- jx[disease=="measles"]$maxyr %>% unique() %>% as.numeric()
fit <- lm(coverage~gdppc, 
          data = jx[disease == "measles" & year_lab == "Start"])
pred <- predict(fit, data.frame(gdppc = c(1000,10000)))
preston_meas_cov_poor_start <- pred[1] %>% signif(2)
preston_meas_cov_rich_start <- pred[2] %>% signif(2)
fit <- lm(coverage~gdppc, 
          data = jx[disease == "measles" & year_lab == "End"])
pred <- predict(fit, data.frame(gdppc = c(1000,10000)))
preston_meas_cov_poor_end <- pred[1] %>% signif(2)
preston_meas_cov_rich_end <- pred[2] %>% signif(2)
hib_minyr <- jx[disease == "HiB"]$minyr %>% unique() %>% as.numeric()
diff_hib_minyr <- hib_minyr-1987

var2oleaf(preston_meas_yr_start)
var2oleaf(preston_meas_yr_end)
var2oleaf(preston_meas_cov_poor_start)
var2oleaf(preston_meas_cov_rich_start)
var2oleaf(preston_meas_cov_poor_end)
var2oleaf(preston_meas_cov_rich_end)
var2oleaf(hib_minyr)
var2oleaf(diff_hib_minyr)

# Dot - Bloomberg time to 75% vaccination: ----------------------------

# bloomberg data:
bloom <- readxl::read_xlsx(paste0(raw_dir, "/bloomberg_covid_vaccination_time_to_full_cov.xlsx"))
bloom <- dfdt(bloom)
bloom[grepl("coverage", time_to_full_cov), cov:=time_to_full_cov %>% 
        gsub("has ", "",.) %>% 
        gsub(" coverage", "",.) %>% 
        gsub("\\%", "", .) %>% 
        as.numeric()]
bloom[grepl("coverage", time_to_full_cov), time_to_full_cov:=NA]

# add a factor to calculate days to full coverage:
bloom[grepl("week", time_to_full_cov), factor:=7]
bloom[grepl("month", time_to_full_cov), factor:=30]
bloom[grepl("year", time_to_full_cov), factor:=365]
bloom[, time_to_full_cov:=time_to_full_cov %>% 
        gsub("weeks","",.) %>% 
        gsub("months","",.) %>% 
        gsub("years","",.) %>% 
        gsub("week","",.) %>% 
        gsub("month","",.) %>% 
        gsub("year","",.) %>% 
        gsub("> ","",.) %>% 
        gsub("< ","",.) %>% 
        stringr::str_trim()]
waitifnot(nrow(bloom[is.na(factor) & !is.na(time_to_full_cov)])==0)
waitifnot(!all(is.na(bloom$cov)))
bloom[,time_to_full_cov:=as.numeric(time_to_full_cov)]

# days to full coverage
bloom[,time_to_full_cov:=time_to_full_cov * factor] 

# ISO codes:
bloom[,iso3c:=name2code(country)]

# GDP per capita:

bloom <- merge(bloom, na.omit(df[year == 2021,.(gdppc, iso3c, poptotal)]), 
               by = "iso3c", all = FALSE)
bloom <- bloom[!is.na(gdppc)]

# where coverage is above 75, put days to 75% coverage as 0:
bloom[cov>=75, time_to_full_cov:=0]

# get WB country groups:
bloom[,region:=countrycode(iso3c, "iso3c", "region")]

setwd(input_dir)
save.image("prior_bloom_plot.RData")


setwd(input_dir)
load("prior_bloom_plot.RData")


# label for population
bloom[, lab_ := paste0(country, " (", signif(poptotal / 10 ^ 6, 2), "m)")]
bloom <- bloom[order(-poptotal)]
bloom[21:nrow(bloom),lab_:=NA]

# time to full coverage should be in years
bloom[, time_to_full_cov := (0.1+time_to_full_cov) / 365]

plot <-
  ggplot(bloom) +
  geom_point(alpha = 0.5,
             aes(
               x = gdppc,
               y = time_to_full_cov,
               size = poptotal,
               color = region
             )) +
  my_custom_theme +
  labs(y = "Years to 75% coverage",
       x = "GDP per capita",
       subtitle = "") +
  scale_color_custom +
  scale_y_log10(breaks = breaks_log(n = 10),
                labels = scales::comma_format(accuracy = 0.01)) +
  scale_x_log10(labels = scales::dollar_format(accuracy = 1),
                breaks = breaks_log(n = 6)) +
  annotation_logticks(side = "bl") +
  guides(color = guide_legend(ncol = 2)) +
  scale_size_area(max_size = 20, guide = "none") +
  annotate(
    "text",
    x = 25000,
    y = 0.001,
    label = "Already at 75% coverage"
  ) +
  ggrepel::geom_text_repel(data = bloom,
                           aes(x = gdppc, y = time_to_full_cov, label = lab_),
                           size = 3, direction = "y",
                           segment.color = "grey65")

setwd(overleaf_dir)
ggsave(paste0("dot_time_to_75_cov.pdf"),
       plot,
       width = 8,
       height = 8)
setwd(input_dir)

