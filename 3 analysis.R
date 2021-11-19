load("table_variable_origins.RData")

# Restrict Sample -------------------------------------------------------

# find the arable land variable

arable_land_df <- 
    chatting[variable == "Land Arable land Area 1000 ha"][,.(iso3c, date, value)] %>% 
    unique() %>% 
    na.omit
setnames(arable_land_df, c("value"), c("arable_land"))

# Drop country-years where the value is missing.
chatting[, check.count := NULL]
chatting <- chatting[!is.na(value)]
chatting <- chatting[!is.na(date)]

# Check: make sure that the GDP per capita data we get from COVID-vaccination
# project is the same as that we have in this TECH project
check <- na.omit(unique(chatting[,.(gdppc, iso3c, date)]))[
  order(iso3c, date)][, .(sd = sd(gdppc, na.rm = T)), by = .(date, iso3c)][]
waitifnot(all((check$sd %>% na.omit())<0.00001))
chatting[,gdppc:=mean(gdppc, na.rm = T), by = .(iso3c, date)]

# Create data.frame grouped by country-year with how many countries we have 
# in each quintile:


# First, define a function for getting percentiles of a group using empirical CDF:

per_cdf <- function(value, vector) {
    empirical_cdf <- ecdf(vector)
    return(empirical_cdf(value) )
}

# Then, get a dataframe of all the GDP per capita values and countries:
pcap_percentiles <- unique(chatting[,.(iso3c, gdppc, date)])
pcap_percentiles <- na.omit(pcap_percentiles)
check_dup_id(pcap_percentiles, c("iso3c","date"))
pcap_percentiles[, percentile := per_cdf(gdppc, as.vector(unlist(.SD))),
                 .SDcols = c('gdppc'), by = c('date')]

# Apply that function on the GDP per capita metric across each year:
chatting <- merge(chatting, pcap_percentiles,
                  by = c('date', 'iso3c', 'gdppc'),
                  all.x = T)

# Split percentiles to quantiles:

chatting[percentile > 0 & percentile <= .2, quantile := 1]
chatting[percentile > .2 & percentile <= .4, quantile := 2]
chatting[percentile > .4 & percentile <= .6, quantile := 3]
chatting[percentile > .6 & percentile <= .8, quantile := 4]
chatting[percentile > .8 & percentile <= 1, quantile := 5]

# keep only the countries where WHO has a value in 2020:
chatting <- chatting[iso3c%in%(port_chat$iso3c %>% unique())]

# restrict to above 20% coverage:
income.check <- chatting[, .(
    q1 = sum(quantile == 1, na.rm = T),
    q2 = sum(quantile == 2, na.rm = T),
    q3 = sum(quantile == 3, na.rm = T),
    q4 = sum(quantile == 4, na.rm = T),
    q5 = sum(quantile == 5, na.rm = T),
    num.countries = length(unique(na.omit(iso3c))),
    all.zero = all(value == 0)
),
by = .(date, variable, label)]

income.check <- merge(income.check, df_disease_avg, 
      by.x = c("date", "variable"),
      by.y = c("year","disease"), all.x = T)

income.check <- 
  income.check[coverage>=20 | variable == "influenza" | variable == "HPV" | 
                 variable == "covid-19"]

# For each technology, make sure that they have
# 1) start and end date have a sample of ___ countries
# 2) at least one date that is prior to 1990 and that
# 3) the time from START to END is ____ years
# 4) at least 2 countries within the poorest 2 quintiles

# 1) start and end date have a sample of ____ countries
# (use ___ as a buffer because some get ommited. Then, at the end,
# we re-filter for ___ countries).

# Some technologies have this issue where they have fewer countries in recent
# years. So, delete those recent years where there is a rapid drop in the
# number of countries (e.g. internet users). Do this by 1st getting a 3 yr
# moving average of the number of countries in the sample. Then, taking a
# percentage difference between that 3-yr moving average and the current
# number of countries. Then, dropping if we have a 30% decline in the number
# of countries compared to this 3yr moving average:

income.check <- income.check[order(variable, date),]
income.check[, ma.num.c := frollmean(
    num.countries,
    n = 3,
    align = "right",
    algo = "fast",
    na.rm = T
) %>% shift(1), by = variable]
income.check[, per.diff.num.coun.vs.3.MA := num.countries / ma.num.c - 1]
income.check[is.na(per.diff.num.coun.vs.3.MA),per.diff.num.coun.vs.3.MA:=0]
income.check <- income.check[!(per.diff.num.coun.vs.3.MA < (-0.3)) | 
                               is.na(per.diff.num.coun.vs.3.MA),]

# 2) at least one date that is prior to 1990 and that
# 3) the time from START to END is 20 years
# Okay, so the START date might include variables where everything is zero.
# So, re-define the start date to be variables where everything is not zero.
income.check[, start.date := min(date, na.rm = T), by = "variable"]
income.check[all.zero == FALSE, end.date := max(date, na.rm = T), by = "variable"]

# keep the data where there the date is equal to either the start or end date:
income.check <- income.check[(start.date == date) | (end.date == date)]

# We should have now 2 observations per technology (IF that technology indeed
# has a start date prior to 1990 AND has more than _____ countries). So, we
# want to keep those technologies that have 2 observations. We drop the other
# ones, since that means that the technology DOESN'T have a start date prior
# to 1990 OR it has less than ____ countries.
income.check[, count := 1]
income.check[, count := sum(count), by = variable]
income.check <- income.check[count==2]

chatting <- merge(chatting, income.check, by = c("date","variable", "label"), all.x = T)
chatting <- chatting[count==2]
chatting[,count:=NULL]
chatting[,variable:=variable %>% as.character()]

# check that we still have HPV and flu
waitifnot(any(chatting$variable=="HPV")==TRUE)
waitifnot(any(chatting$variable=="influenza")==TRUE)
waitifnot(any(chatting$variable=="covid-19")==TRUE)

# Convert Agricultural Variables to 'per-AREA' ----------------------------
# (will need to add into the percent variable and any manually coded later
# variables...)

chatting <- merge(chatting, arable_land_df, by = c("iso3c", "date"), all.x = TRUE)

# find all AG variables
ag_var <- 
    chatting[categ == "Agricultural Technologies",.(variable)] %>% unique() %>% 
    unlist() %>% as.vector()

non_pct_ag_var <- 
    setdiff(ag_var, 
            ag_var %>% grep("pct|%| per ", ., value = T))

# replace AG variables with AG / arable land
chatting[variable %in% non_pct_ag_var, value:= value / arable_land]
chatting$arable_land <- NULL

# replace label to be per arable land:
chatting[variable %in% non_pct_ag_var, label:=paste0(label, " per arable land")]

# add these new AG variables to percent labels:
percent.labels <- 
      c(percent.labels,
      chatting[variable %in% non_pct_ag_var | 
# IF the data comes from our vaccine paper, 
# then it should be a percent variable:
      categ %in% "Vaccine--Covid Paper"
      ,.(label)] %>% unlist() %>% unique())

# Now, we go on to create our summary numbers (standard deviations, means,
# etc.). Since we will be summarizing the data by dividing by population
# across all the technologies, to NOT make the vaccines, etc. per capita
# (they're already per capita), we set population to be 1 here. (then, later,
# we divide by population for all the variables.)

chatting[label %in% percent.labels, pop := 1]


percent.vars <- chatting[label%in%percent.labels]$variable %>% unique()
chatting[, value := value / pop]

# Check that variable like percent irrigated is still in percentage form

chatting[grepl('Irrigated area as a share of cultivated', label)
         & !(value <= 0), .(value)] %>% 
    unique() %>% unlist %>% as.vector %>% 
    hist(50)

# yes, it is! great.

# For aesthetics, include only OVERALL fertilizer metrics for agriculture
# (we have too many other metrics from FAOSTAT):
chatting[(label %in% (chatting[variable %in% non_pct_ag_var,.(label)] %>% unlist() %>% unique()) &
      !(
          label %in%
              c(
                  "Fungicides and Bactericides Agricultural Use tonnes per arable land",
                  "Herbicides Agricultural Use tonnes per arable land",
                  "Insecticides Agricultural Use tonnes per arable land",
                  "Pesticides (total) Agricultural Use tonnes per arable land",
                  "Agricultural harvesters per arable land",
                  "Milking units per arable land",
                  "Agricultural tractors per arable land",
                  "Metric tons of fertilizer consumed. Aggregate of 25 types per arable land",
                  # "Area equipped to provide water to crops per arable land",
                  # "Land Agricultural land Area 1000 ha per arable land",
                  # "Land Agriculture area actually irrigated Area 1000 ha per arable land",
                  # "Land Agriculture area actually irrigated Share in Agricultural land pct",
                  # "Land Naturally regenerating forest Area 1000 ha per arable land",
                  # "Land Planted Forest Area 1000 ha per arable land",
                  "Machinery Agricultural tractors In Use No per arable land",
                  "Machinery Combine harvesters - threshers In Use No per arable land",
                  "Machinery Milking machines In Use No per arable land",
                  "Fertilizer per arable land",
                  "fert_total"
              )
      ))
 , todel := 1]
chatting[is.na(todel),todel:=0]
chatting <- chatting[todel!=1,]
chatting[,todel:=NULL]

# our dataframe now is much smaller because it only has the 
# country-years that "matter".

save.image("restrict_sample.RData")
load("restrict_sample.RData")

# Create Table Measures of Convergence ------------------------------------

# For those technologies, split our dataset into a list of
# technology-dataframes.
chatting[,variable:=variable %>% as.character() %>% as.factor()]
chatting <- split(chatting, chatting$variable)

# Then, for each data.frame in the list, pivot to a wider data.frame with
# years as the columns and the countrys as a row.
chatting <- lapply(chatting,
           function(x) {
               x %>%
                   as.data.table %>%
                   data.table::dcast(.,
                                     variable + label + iso3c + categ ~ date,
                                     value.var = "value" , fill = NA)
           })

# check that within each data.frame, each country should appear only once:
waitifnot(all(unlist(lapply(chatting, function(x) {length(x$iso3c) == length(unique(x$iso3c))}))))

# Then, for each data.frame in the list, omit the countries where we have
# missing observations.
chatting <- lapply(chatting, function(x) { x %>% na.omit %>% as.data.table()})

# save
save.image("before_coef.RData")
load("before_coef.RData")

id.vars.chat <- c('variable', 'label','iso3c','categ')

check.before <- 
    lapply(chatting, dim) %>% 
    lapply(t) %>% 
    lapply(as.data.frame) %>% 
    rbindlist(idcol = 'var') %>% 
    rename(row = V1, col = V2)

add_in_gdppc <- function(df.f) {
    # melt back to a dataframe that is in long format
    df.f <- melt(df.f, 
                 id.vars = id.vars.chat, 
                 measure.vars = setdiff(names(df.f), 
                                        id.vars.chat),
                 variable.name = 'date')
    df.f[,date:=as.numeric(as.character(date))]
    
    # merge in GDP per capita from WDI and Maddison
    df.f <- merge(df.f, mad, by = c('iso3c','date'), all.x = T)
    df.f[,pop:=NULL]
    
    # convert again to wide format, but this time also with the 
    # GDP per capita variables
    df.f <- data.table::dcast(df.f,
                              variable +label+ iso3c + categ ~ date,
                              value.var = c('value', 'gdppc'), fill = NA)
    
    return(as.data.table(df.f))
}

chatting <- lapply(chatting, add_in_gdppc)

# check: the rows and columns of this new dataset vs. of the prior one.

check.after <- lapply(chatting, dim) %>% 
    lapply(t) %>% 
    lapply(as.data.frame) %>% 
    rbindlist(idcol = 'var') %>% 
    rename(row = V1, col = V2)

check <- as.data.frame(cbind(check.before, check.after))

check[,c(2,3,5,6)] <- 
    as.data.frame(matrix(as.numeric(as.matrix(check[,c(2,3,5,6)])), 
                         ncol = 4))

# all the rows should be the same
waitifnot(all(check[,2]==check[,5]))

# we should have an additional 2 GDP columns (start & finish year)
waitifnot(all(2+check[,3]==check[,6]))

for (tec in names(chatting)) {
    
    cat(paste0(tec,"\n"))
    
    # get the duration of the technology where we have observations.
    chatting[[tec]] <- chatting[[tec]] %>% dfdt
    dates <- setdiff(names(chatting[[tec]]),
                     c("variable","label", "categ", "iso3c")) %>% 
        gsub("value_","",.) %>% 
        gsub("gdppc_","",.) %>% 
        unique() %>% 
        as.numeric()
    waitifnot(length(dates)==2)
    yrs_growth <- abs(dates[1] - dates[2])
    
    # rename the end and start dates to be from a number to be `end date` and
    # `start date`
    names(chatting[[tec]]) <- names(chatting[[tec]]) %>% 
        gsub(as.character(dates[1]), "start_date",.) %>% 
        gsub(as.character(dates[2]), "end_date",.)
    
    # find out the growth of the technology variable from the start date to the
    # end date.
    chatting[[tec]][, growth := 
                        ((value_end_date - value_start_date) / 
                             value_start_date) ^ (1 / yrs_growth)]
    
    # confirm that each observation is a country.
    waitifnot(length(unique(chatting[[tec]]$iso3c)) == length(chatting[[tec]]$iso3c))
    chatting[[tec]][,num.countries:=length(unique(chatting[[tec]]$iso3c))]
    
    # regress LN(1+%growth)~LN(initial value)
    fit <- NA
    try(fit <-
            chatting[[tec]] %>%
            filter(is.finite(growth)) %>%
            filter(growth > 0) %>%
            lm(log(growth) ~ log(value_start_date), data = .))
    
    if (class(fit)=="lm") {
        # insert regression results into the data.frame
        chatting[[tec]][, growth := NULL]
        chatting[[tec]][, alpha := fit$coefficients[1] %>% as.numeric()]
        chatting[[tec]][, beta := fit$coefficients[2] %>% as.numeric()]
        chatting[[tec]][, num.countries.fit := fit$model %>% nrow()]
    } else {
        chatting[[tec]][, growth := NULL]
        chatting[[tec]][, alpha := NA]
        chatting[[tec]][, beta := NA]
        chatting[[tec]][, num.countries.fit := NA]
    }
    
    # For the start and end date, get the coefficient of variation, mean,
    # standard deviation, beta from regression, and coefficient from
    # regression:
    chatting[[tec]] <- chatting[[tec]] %>% dfdt %>%
        data.table::melt(
            id.vars =
                c(
                    "variable",
                    "label",
                    "categ",
                    "iso3c",
                    "alpha",
                    "beta",
                    "num.countries.fit",
                    "num.countries"
                ),
            measure.vars	= c("value_start_date", "value_end_date", 
                             "gdppc_start_date", "gdppc_end_date"),
            variable.name = "year",
            value.name = "value"
        ) %>%
        dfdt
    chatting[[tec]] <-
        setDT(chatting[[tec]])[,
                               # Importantly, this gives GDP per capita for
                               # the  countries where we HAVE GDP per capita
                               # estimates! i.e. if we do not have GDP per
                               # capita estimates, we still will be able to
                               # get an estimate for beta and alpha
                               # convergence, but the mean GDP per cpaita and
                               # stdev listed here will be for the ones where
                               # the data is available.
                               .(meanval = mean(value, na.rm=T),
                                 stdev = sd(value, na.rm=T)),
                               by = .(variable, label, categ, year, alpha,
                                      beta, num.countries.fit, num.countries)]
    chatting[[tec]][, coef.var := stdev / meanval]
    
    # put the start and end date into the table:
    chatting[[tec]][, start_date := dates[1]]
    chatting[[tec]][, end_date := dates[2]]
    chatting[[tec]][, dates_label := paste0(label, " (", dates[1], "-", dates[2], ")")]
    
    # pivot the table to have rows be the technology and columns be the values
    # of interest
    chatting[[tec]] <-
        chatting[[tec]] %>% 
        dcast(.,categ + variable + start_date + end_date + label + dates_label + 
                  alpha + beta + num.countries.fit + num.countries ~ year,
              value.var = c("meanval", "stdev", "coef.var"),
              fun.aggregate = sum, fill = NA
        ) %>% 
        dplyr::select(-contains('coef.var_gdppc')) %>% 
        dfdt
}

cat("Don't worry if there are 4 errors here. We tried to fit a regression for
these countries. But, IF all the observations in the beginning year were 0,
then the regression (log(0) DNE) would yield an error. That's okay.")

# Append together our list, yielding a dataset of all technologies with the
# countries as rows and years as columns.
chatting <- rbindlist(chatting)

# keep only the variables that have more than 30 fixed-sample countries:
# chatting <- chatting[num.countries>=20,]

save.image("create_table_measures_of_convergence.RData")
load("create_table_measures_of_convergence.RData")

# Table Aesthetics: --------------------------------------------------------

# label whether the variable is a technology variable or not:
chatting[,tech_var:="Yes"]
chatting[label%in%non_tech_var, tech_var:="No"]

# check: the number of variables that are labeled spliced is less than 
# or equal to the number of variables that are actually spliced
chatting <- chatting[order(categ, beta)]
waitifnot(sum(chatting$how_spliced != "") <
              nonempty_len(
                  c(
                      to_mean_splice,
                      to_weight_mean_splice,
                      to_growth_splice,
                      to_replace_splice,
                      to_interpolate_splice
                  )
              ))

# check: there is a unique variable for the table
check_dup_id(chatting, 'variable')

# Rename for presentation:
chatting <- as.data.frame(chatting)
chatting <- chatting %>% 
    rename(
        "Variable" = "variable",
        "Label" = "label",
        "Category" = "categ",
        "Start Date" = "start_date",
        "End Date" = "end_date",
        "Technology (with date)" = "dates_label",
        "Intercept" = "alpha",
        "Beta" = "beta",
        "Number of Countries in Beta Convergence Regression" = "num.countries.fit",
        "Number of Countries in Mean and Standard Deviation" = "num.countries",
        "Mean value at the start" = "meanval_value_start_date",
        "Mean value at the end" = "meanval_value_end_date",
        "Mean GDP per capita at the start" = "meanval_gdppc_start_date",
        "Mean GDP per capita at the end" = "meanval_gdppc_end_date",
        "Standard deviation of the value at the start" = "stdev_value_start_date",
        "Standard deviation of the value at the end" = "stdev_value_end_date",
        "Standard deviation of the GDP per capita at the start" = "stdev_gdppc_start_date",
        "Standard deviation of the GDP per capita at the end" = "stdev_gdppc_end_date",
        "Coefficient of variation of the value at the start" = "coef.var_value_start_date",
        "Coefficient of variation of the value at the end" = "coef.var_value_end_date",
        "Is this a technology variable?" = "tech_var"
    ) %>% dfdt()

chatting %>% write.csv("Table 1.csv", na = "", row.names = FALSE)

chatting <- split(chatting, chatting$`Is this a technology variable?`)

save.image("table_aesthetic.RData")
load("table_aesthetic.RData")














