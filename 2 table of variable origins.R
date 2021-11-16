# Big Table of Variable Origins --------------------------------------

load("relabel_variable_names.RData")

orig_tab <- chatting[!is.na(value)][, .(
    mean.gdppc = mean(gdppc, na.rm = T),
    n = length(unique(na.omit(iso3c)))
),
by = c("variable", "date", "categ", "label")][, .(
    mean.gdppc = mean(mean.gdppc, na.rm = T),
    med.gdppc = median(mean.gdppc, na.rm = T),
    mean.n = mean(n),
    med.n = median(n),
    min.date = min(date),
    max.date = max(date)
),
by = c("variable", "categ", "label")]

# label whether it is a percent variable:

percent.labels <- c(
    "HepB3",
    "Hib3",
    "IPV1",
    "MCV1",
    "MCV2",
    "PCV3",
    "Pol3",
    "RCV1",
    "RotaC",
    "YFV",
    "BCG",
    (chatting$label %>% unique %>% grep("pct|%", ., value = T)),
    "Fertilizer per arable land",
    "Electric power consumption (kWh)"
)
orig_tab[label%in%percent.labels, percent_var:="Yes"]
orig_tab[!(label%in%percent.labels), percent_var:="No"]

# Label how variables were spliced:--------------------------------------------

# For just telephones, we ended up using a mean splice (un-weighted)
to_mean_splice <- c(to_mean_splice, "telephone_canning_wdi")

orig_tab[variable %in% to_mean_splice, 
         how_spliced := 'arithmetic mean']
orig_tab[variable %in% to_growth_splice, 
         how_spliced := 'growth']
orig_tab[variable %in% to_replace_splice, 
         how_spliced := 
             'replaced CHAT variable with our variable']
orig_tab[variable %in% to_interpolate_splice, 
         how_spliced := 
             'interpolated using a linear regression of the overlapping years']
orig_tab[variable %in% to_weight_mean_splice, 
         how_spliced := 
             'weighted arithmetic mean of the most recent band of overlapping years']

orig_tab[is.na(how_spliced), how_spliced:='']

# label whether it came from the WB, and if so, which code ------------------

# (MANUALLY UPDATE)
wb_codes <- 
fread(
'variable	WDI_code
fertilizer per arable land	AG.CON.FERT.ZS
fert_total	AG.CON.FERT.ZS
aviationtkm	IS.AIR.GOOD.MT.K1
railtkm	IS.RRS.GOOD.MT.K6
railpkm	IS.RRS.PASG.KM
railline_wdi	IS.RRS.TOTL.KM
cell.subsc	IT.CEL.SETS.P2
telephone_canning_wdi	IT.MLT.MAIN
internetuser	IT.NET.USER.ZS
bed_hosp	SH.MED.BEDS.ZS
computer	IT.CMP.PCMP.P2
elec_cons	EG.USE.ELEC.KH.PC
'
)

orig_tab <- merge(orig_tab, wb_codes, all.x = T, by = 'variable')
orig_tab[is.na(WDI_code),WDI_code:='']

# label whether the variable is a technology variable or not: -------------------------

# Default that everything is a technology variable. Then, we specify which
# ones are not technology variables.

orig_tab[,tech_var:="Yes"]

# NOTE: KEEP IN MIND THE CASE OF THE JOINS HERE! EVERYTHING IS NOT LOWER CASE!

non_tech_var <- c("% Irrigated area as a share of cultivated land",
                  "Area equipped to provide water to crops",
                  "Beds in hospitals and rehabilitation centers",
                  "Land Agricultural land Area 1000 ha",
                  "Land Naturally regenerating forest Area 1000 ha",
                  "Land Planted Forest Area 1000 ha",
                  "Patents, residents",
                  "Visitor beds (hotels, etc.)",
                  "Visitor rooms (hotels, etc.)",
                  "Land Arable land Area 1000 ha",
                  "Land Arable land Share in Agricultural land pct")
orig_tab[label%in%non_tech_var, tech_var:="No"]

# label where the variable came from:
orig_tab[variable%in%setdiff(names(chat),
         c("iso3c", "date", "country_name")), 
         source:="CHAT"]

# First, create a dictionary with the dataset and the label. Then, get the
# names of this dataset. IF the variable in CHAT has the name from this
# dictionary, then label with the appropriate source.

dictionary <-
    list(
        list(raww, "World Steel"),
        list(oecd, "OECD"),
        list(wdidata, "WDI"),
        list(alldf, "OWID"),
        list(fao, "FAO"),
        list(mvprod, "World Motor Vehicle Production (BTS)"),
        list(dfcars, "OICA"),
        list(oecd_cars, "OECD"),
        list(electric, "OWID"),
        list(creditdebit, "WB Global Payment Systems Survey (GPSS)"),
        list(world_inf, "Canning"),
        list(aluminum, "CLIO")
        )

for (i in 1:length(dictionary)) {
tech_names <- 
        setdiff(names(dictionary[[i]][[1]]), c("iso3c", "date")) %>% 
        gsub("per 1 000 inhabitants per day|Per 1 000 population| per one thousand inhabitants", "", .) %>%
        gsub("(per 100 people)", "", .) %>%
        gsub("% population", "", ., fixed = T) %>%
        gsub("per 100K adults", "", .) %>%
        gsub(" $", "", .) %>%
        gsub("\\.$", "", .) %>%
        gsub(" \\(\\)$", "", .) %>%
        gsub("^ *", "", .) %>% copy()
    source_name <- 
        as.character(copy(dictionary[[i]][[2]]))
    orig_tab[variable%in%tech_names, source:=source_name]
    orig_tab <- as.data.table(orig_tab)
}

dictionary <- NULL

# Remove from the source the variables that we've spliced: (e.g. if we've
# spliced airline passengers, then the above procedure by using the ORIGINAL
# data sources won't work)

spliced_vars <- na.omit(
    c(
        to_mean_splice,
        to_growth_splice,
        to_replace_splice,
        to_interpolate_splice,
        to_weight_mean_splice
    )
)

orig_tab[variable %in% spliced_vars, source := NA]

# For these spliced variables, re-define where the variable came from
# MANUALLY:

origin_var <-
    fread(
        "variable	source
pctirrigated	FAO; CHAT
pctimmunizdpt	OWID
pctimmunizmeas	OWID
fert_total	WDI
ag_harvester	FAO
ag_tractor	FAO
irrigatedarea	FAO; CHAT
aviation_pass	WDI
bed_hosp	WDI
aviationtkm	WDI; CHAT
electric_gen_capacity	Canning
elecprod	CHAT; OWID
internetuser	WDI
steel_production	World Steel
telephone_canning_wdi	Canning; WDI
atm	WDI; CHAT
cell.subsc	WDI
vehicle_com	OICDA; CHAT
ct_scans	OECD
ag_milkingmachine	FAO
mri_scans	OECD
railline_wdi	WDI
railpkm	WDI; CHAT
railtkm	WDI; CHAT
vehicle_car	OICA; CHAT
computer	WDI
"
    )

# merge the manually updated spliced variables with the original source:

orig_tab <- merge(orig_tab, origin_var, by = 'variable', all.x = T)

# check: make sure that this manual update doesn't differ from the automated
# stuff above:

waitifnot(nrow(orig_tab[(source.x!=source.y & !is.na(source.y) & !is.na(source.x))])==0)
orig_tab <- orig_tab %>% as.data.frame() %>% dfcoalesce.all() %>% as.data.table()

# output:
orig_tab %>%
    rename(
        "Variable" = "variable",
        "Category" = "categ",
        "Variable Label" = "label",
        "Mean of the annual average GDP per capita (2011 dollars)" = "mean.gdppc",
        "Median of the annual average GDP per capita (2011 dollars)" = "med.gdppc",
        "Mean of the annual average number of countries in sample" = "mean.n",
        "Median of the annual average number of countries in sample" = "med.n",
        "Earliest date with available data" = "min.date",
        "Most recent date with available data" = "max.date",
        "Is this a percentage?" = "percent_var",
        "How spliced" = "how_spliced",
        "WDI code" = "WDI_code",
        "Is this a technology variable" = "tech_var",
        "Source" = "source"
    ) %>%
    write.csv("total_orig_tab.csv", na = "", row.names = FALSE)

save.image("table_variable_origins.RData")












