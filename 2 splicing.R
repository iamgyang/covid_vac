load("merged2.RData")

# Compare with CHAT dataset--------------------------------------------------
# Attempting to merge this with the CHAT dataset and resolve any differences

chat[,`:=`(iso3c = name2code(country_name, 
                             custom_match = c(
                                 "Czechoslovakia" = "CSK",
                                 "South Vietnam" = NA,
                                 "South Yemen" = NA,
                                 "Venezuala" = "VEN")), 
           date = as.numeric(year))]
chat <- chat[!is.na(iso3c)]
chat$year <- NULL

# for steel production, OUR dataset should have the '.y', while CHAT
# should have no ending.
m.alldf <- 
    m.alldf %>% 
    rename(steel_production.y = steel_production) %>% 
    dfdt

merge.grid <- CJ(date = seq(min(m.alldf$date),2021),
                 iso3c = unique(unique(m.alldf$iso3c), unique(chat$iso3c)))

tomerge <- list(merge.grid, m.alldf, chat)

chatting <-
    Reduce(function(x, y)
        merge(
            x,
            y,
            by = c("date", "iso3c"),
            all.x = T,
            allow.cartesian = FALSE
        ),
        tomerge)

m.alldf <- NULL

# Calculate CHAT steel production as a sum of steel from acid bessemer, basic
# bessemer, and bof processes
chatting[, steel_production :=
           steel_acidbess + steel_basicbess + steel_bof + steel_eaf + steel_ohf + steel_other]
chatting[, c(
  "steel_acidbess",
  "steel_basicbess",
  "steel_bof",
  "steel_eaf",
  "steel_ohf",
  "steel_other"
) := NULL]

# Calculate total gross energy production from the OWID dataset to match the
# CHAT dataset variable (gross energy production):
chatting[, elecprod.y :=
             `Electricity from coal (TWh)` +
             `Electricity from gas (TWh)` +
             `Electricity from hydro (TWh)` +
             `Electricity from other renewables (TWh)` +
             `Electricity from solar (TWh)` +
             `Electricity from oil (TWh)` +
             `Electricity from wind (TWh)` +
             `Electricity from nuclear (TWh)`]

# Change some of our variables to be of the same name (with a .y at the end)
# as those of CHAT's variable. This will be the indicator that facilitates 
# comparison between the two.
chatting <- 
    chatting %>% 
    rename(
        atm.y = ATMs,
        aviationtkm.y = `Air transport, freight (million ton-km)`,
        aviationpkm.y = `Air transport, passengers carried`,
        med_catscanner.y = `Computed Tomography exams, total`,
        pctimmunizdpt.y = DTP3,
        med_mriunit.y = `Magnetic Resonance Imaging exams, total`,
        cellphone.y = `Mobile cellular subscriptions`,
        computer.y = `Personal computers`,
        creditdebit.y = creditdebit_volume,
        pctimmunizmeas.y = measles,
        internetuser.y = `using internet`,
        railline.y = rail.lines,
        railline.z = rail_line_world_inf, 
        railpkm.y = rail.passengers.km,
        railtkm.y = rail.goods,
        vehicle_car.y = pc_vehicles,
        vehicle_com.y = cv_vehicles,
        fert_total.y = fertilizer,
        bed_hosp.y = `hospital beds`,
        ag_milkingmachine.y = `Machinery Milking machines In Use No`,
        ag_harvester.y = `Machinery Combine harvesters - threshers In Use No`,
        ag_tractor.y = `Machinery Agricultural tractors In Use No`,
        irrigatedarea.y = `Land Agriculture area actually irrigated Area 1000 ha`,
        pctirrigated.y = `Land Agriculture area actually irrigated Share in Agricultural land pct`,
        telephone.y = `fixed telephone subscriptions`,
        elecprod.z = elec_gen_capacity_world_inf
    ) %>% dfdt

# get the variables that have this ".y" or ".z" at the end ==> indicator that 
# we want to compare them:
ov_lap <- grep("\\.y$|\\.z$", names(chatting), value = T)
chat.vs.ky <- data.table(variable = ov_lap)

# CHAT pdf says that credit debit numbers are in millions: 
chatting[,creditdebit:=creditdebit*(10^6)]

# Also make some scale adjustments to some other variables:
chatting[, internetuser.y := internetuser.y / (100)]
chatting[, pctimmunizmeas := pctimmunizmeas * 100]
chatting[, pctimmunizdpt := pctimmunizdpt * 100]
chatting[, vehicle_com := vehicle_com * 1000]
chatting[, fert_total := fert_total * 1000]
chatting[, pctirrigated := pctirrigated * 100]

# electricity production in the CHAT dataset is in KWH, but our data is in TWH, 
# so we divide by 1B.
chatting[, elecprod := elecprod / (10^9)]

# store the regressions here:
reg.chat.ky <- list()

# let's compare the errors of the overlapping variables between our's and
# CHAT's
for (var in ov_lap) {
  
  # temporary df with the overlapping observations as two columns:
  temp_df <- chatting[,.(
    one = eval(as.name(gsub("\\.y$|\\.z$", "",var))), 
    two = eval(as.name(var)), date)] %>% as.data.table()
  
  # get the minimum date and the maximum date of the CHAT dataset and our
  # dataset
  chat.vs.ky[variable == var, yr_start_chat:=
               temp_df[!is.na(one)]$date %>% min]
  chat.vs.ky[variable == var, yr_end_chat:=
               temp_df[!is.na(one)]$date %>% max]
  chat.vs.ky[variable == var, yr_start_ours:=
               temp_df[!is.na(two)]$date %>% min]
  chat.vs.ky[variable == var, yr_end_ours:=
               temp_df[!is.na(two)]$date %>% max]
  
  # this is the % difference in error between CHAT and ours at the
  # country-year level:
  temp_df <- na.omit(temp_df) %>% as.data.table()
  temp_df[,three:=2*abs((one-two)/(one+two))]
  
  chat.vs.ky[variable == var, country_yr_overlap:=temp_df %>% nrow]
  chat.vs.ky[variable == var, mean_error:=temp_df$three %>% na.omit %>% mean]
  
  # get the p values from the regression of the overlapping terms from CHAT to
  # predict our values. in the end, for the variables that did overlap, we
  # did not find any  large statistically significant differences between
  # them using the OLS.
  if (!any(dim(temp_df)==0)){
    # our var~CHAT variable data.
    fit <- lm(ourvar~chatvar, data = temp_df[one>0 & two>0,
                                             .(ourvar = (two),
                                               chatvar = (one))])
    reg.chat.ky[["unlogged"]][[var]] <- fit
    fit <-  fit %>% summary()
    chat.vs.ky[variable == var, incpt_p := fit$coefficients[1, 4]]
    chat.vs.ky[variable == var, slope_p := fit$coefficients[2, 4]]
    
    # get the regression coefficients from the regression of the overlapping
    # terms
    chat.vs.ky[variable == var, incpt := fit$coefficients[1, 1]]
    chat.vs.ky[variable == var, slope := fit$coefficients[2, 1]]
    
    # DO THE SAME THING BUT WITH A LOG-LOG FIT:
    fit <- lm(ourvar~chatvar, data = temp_df[one>0 & two>0,
                                             .(ourvar = log(two),
                                               chatvar = log(one))])
    reg.chat.ky[["logged"]][[var]] <- fit
    fit <-  fit %>% summary()
    chat.vs.ky[variable == var, incpt_p_log := fit$coefficients[1, 4]]
    chat.vs.ky[variable == var, slope_p_log := fit$coefficients[2, 4]]
    
    # get the regression coefficients from the regression of the overlapping
    # terms
    chat.vs.ky[variable == var, incpt_log := fit$coefficients[1, 1]]
    chat.vs.ky[variable == var, slope_log := fit$coefficients[2, 1]]
  }
  chat.vs.ky <- as.data.table(chat.vs.ky)
}

chat.vs.ky %>% write.csv(., "exported comin mesitieri comparison.csv", 
                         row.names = FALSE, na = "")

save.image("comparing_with_CHAT.RData")
load("comparing_with_CHAT.RData")

# Splice with CHAT dataset ------------------------------------------------

# Define variables to splice ----------------------------------------------

# Define the variables that we want to splice together & those that we don't
# want to splice.

# A) variables to splice by taking an arithmetic mean:
to_mean_splice <- NA

# B) variables to splice by taking a weighted arithmetic mean:
to_weight_mean_splice <-
  c(
    'elecprod',
    'vehicle_car',
    'atm',
    'railtkm',
    'railpkm',
    'pctirrigated',
    'irrigatedarea',
    'vehicle_com'
    )

# C) variables to splice via the growth method:
to_growth_splice <-
  c('aviationtkm')

# D) variables where we will keep our metric and delete CHAT's: 
to_replace_splice <-
  c(
    'steel_production',
    'pctimmunizmeas',
    'pctimmunizdpt',
    'computer',
    'internetuser',
    'fert_total',
    'bed_hosp',
    'ag_tractor',
    'ag_harvester',
    'ag_milkingmachine',
    'cellphone'
  )

# E) variables where CHAT's data is better than ours:
ours_to_delete_splice <-
  c('railline.z')

# We compared the rail line data between Canning and that of CHAT in overlapping country-years and saw that they were significantly different. In particular, we find that there were many country-years where there were more than 10% difference between Canning's rail-line data and that of CHAT. WDI data on rail lines does not do better, but we keep it as a separate series in the data, since it provides a more RECENT picture of rail-lines (WDI data goes from 1995-2019, while Canning goes from 1950-1995). Thus, we decided to exclude Canning's rail line data from the final dataset.
# 
# This shows the number of *years* that are over 10% difference for
# railline Canning for each country:
# 
# Malaysia	43
# Japan	37
# Spain	27
# Kenya	25
# Thailand	24
# Chile	22
# Uganda	16
# Saudi Arabia	14
# Canada	12
# Myanmar (Burma)	10
# Egypt	9
# Syria	7
# Ethiopia	5
# Philippines	5
# Sierra Leone	5
# Costa Rica	4
# Botswana	3
# El Salvador	3
# Colombia	2
# New Zealand	1
# Portugal	1

# F) variables where CHAT has a different dataset than we do, 
# but we have a SIMILAR dataset that is more recent but NOT the same,
# we INTERPOLATE these variables:
to_interpolate_splice <- NA

# A) Splice: Arithmetic Mean -----------------------------------------------

# For the variables where CHAT has an older, more comprehensive, series than
# our data, we splice together the CHAT data by taking a mean in the
# overlapping years. 
if (nonempty_len(to_mean_splice)>0) {
    for (vars_ in unique(to_mean_splice)) {
        chatting[, (vars_) := apply(.SD, 1, function(x)
            mean((x), na.rm = T)),
            .SDcols = c(vars_, paste0(vars_, ".y"))]
        chatting[, (paste0(vars_, ".y")) := NULL]
        chatting <- as.data.table(chatting)
    }
}
save.image('splice_arithmetic_mean.RData')
load('splice_arithmetic_mean.RData')


# B) Splice: Weighted Mean ------------------------------------------------

weight_mean_splice <- function(dfr, vbs, vbm = paste0(vbs, ".y")) {
    # this function takes a dataframe, a BASE variable (vbs), and a REPLACEMENT
    # variable (vbm) as inputs, and then splices them together based on WEIGHTs,
    # slowly transitioning from one variable to another.
    
    # dfr <- test %>% dfdt()
    dfr <- dfr %>% dfdt()
    dfr[, c(vbs, vbm) := lapply(.SD, as.numeric), .SDcols = c(vbs, vbm)]
    
    # only analyze the variables of interest:
    # dfr <- dfr[, c('date', 'iso3c', vbs, vbm), with = FALSE]
    
    # throw an error if duplicated dates:
    dfr[, dup := duplicated(date), by = 'iso3c']
    if (any(dfr$dup))
        stop('you have duplicated dates within the country grouping')
    dfr$dup <- NULL
    
    # sort by iso3c date
    dfr <- dfr[order(iso3c, date), ]
    
    # get the most recent year where there's overlap
    
    dfr[is.na(eval(as.name(vbs))) & !is.na(eval(as.name(vbm))),
        maxyr_1 := min(date) - 1, by = 'iso3c']
    dfr[, maxyr_1 := mean(na.omit(maxyr_1)), by = 'iso3c']
    
    dfr[!is.na(eval(as.name(vbs))) & is.na(eval(as.name(vbm))),
        maxyr_2 := min(date) - 1, by = 'iso3c']
    dfr[, maxyr_2 := mean(na.omit(maxyr_2)), by = 'iso3c']
    
    dfr[!is.na(eval(as.name(vbs))) & !is.na(eval(as.name(vbm))),
        maxyr_3 := max(date), by = 'iso3c']
    dfr[, maxyr_3 := mean(na.omit(maxyr_3)), by = 'iso3c']
    
    dfr[!is.na(eval(as.name(vbs))), maxyr_4 := max(na.omit(date)), by = 'iso3c']
    dfr[, maxyr_4 := mean(na.omit(maxyr_4)), by = 'iso3c']
    
    dfr[, maxyr := max(maxyr_1, maxyr_2, maxyr_3, maxyr_4, na.rm = T), by = 'iso3c']
    
    dfr[, (grep("maxyr_", names(dfr),  value = T)) := NULL]
    
    # store original dataset:
    store <- dfr %>% dfdt()
    
    # delete where prior to maxyr
    dfr <- dfr[date <= maxyr]
    
    # get the the oldest CONTINUOUS year where there's overlap
    dfr[is.na(eval(as.name(vbs))) & !is.na(eval(as.name(vbm))),
        minyr_1 := max(date) + 1, by = 'iso3c']
    dfr[, minyr_1 := mean(na.omit(minyr_1)), by = 'iso3c']
    
    dfr[!is.na(eval(as.name(vbs))) & is.na(eval(as.name(vbm))),
        minyr_2 := max(date) + 1, by = 'iso3c']
    dfr[, minyr_2 := mean(na.omit(minyr_2)), by = 'iso3c']
    
    dfr[!is.na(eval(as.name(vbs))) & !is.na(eval(as.name(vbm))),
        minyr_3 := min(date), by = 'iso3c']
    dfr[, minyr_3 := mean(na.omit(minyr_3)), by = 'iso3c']
    
    dfr[, minyr := max(minyr_1, minyr_2, minyr_3, na.rm = T), by = 'iso3c']
    
    dfr[, (grep("minyr_", names(dfr),  value = T)) := NULL]
    
    # delete all other years
    dfr <- dfr[date >= minyr & date <= maxyr]
    
    # get the LENGTH of this strip
    dfr[, len_str := length(date) + 1, by = 'iso3c']
    
    # construct weights based on this length
    dfr[, num := ave(iso3c == iso3c, iso3c, FUN = cumsum)]
    dfr[, w1 := (len_str - num) / (len_str)]
    dfr[, w2 := num / (len_str)]
    
    # do a weighted mean
    dfr[, new_var := eval(as.name(vbs)) * w1 + eval(as.name(vbm)) * w2]
    dfr <- dfr[, .(date, iso3c, new_var, maxyr)]
    
    # merge the original dataset with this, with intentions of replacing:
    a <- nrow(store)
    dfr <- merge(store,
                 dfr,
                 by = c('iso3c', 'date'),
                 all.x = T)
    waitifnot(nrow(dfr) == a)
    store <- NULL
    
    dfr <-
        dfr[, maxyr := coalesce2(maxyr.x, maxyr.y)][, (c('maxyr.x', 'maxyr.y')) :=
                                                        NULL][]
    
    # splice the ORIGINAL dataset together with this by replacing the years where
    # there's overlap
    
    dfr[!is.na(new_var), (vbs) := new_var]
    if (all(is.na(dfr$maxyr))) {
        dfr[!is.na(val1), maxyr := max(date), by = 'iso3c']
    }
    dfr[, maxyr := mean(maxyr, na.rm = T), by = 'iso3c']
    dfr[date > maxyr, (vbs) := coalesce2(eval(as.name(vbs)), eval(as.name(vbm)))]
    
    # delete the excess
    dfr[, c(vbm, 'new_var', 'maxyr'):=NULL]
    
    return(dfr[])
}

# Weighted-Mean Unit Tests:----------------------------------------------------

# weight_mean_splice is a function that accepts a GROUP argument, a
# TIME argument, and a value argument.
# It merges the duplicated value arguments.

# 0 years overlap? --> just replace the values

test <- NULL
test_answer <- NULL
set.seed(3762)
test <-
    data.table(
        date = c(seq(1, 10), seq(12, 20)),
        iso3c = c(rep('ABW', 10 + 20 - 12 + 1)),
        val1 = c(runif(10), rep(NA, 20 - 12 + 1)),
        val1.y = c(rep(NA, 10), runif(20 - 12 + 1))
    )
set.seed(3762)
test_answer <-
    data.table(
        date = c(seq(1, 10), seq(12, 20)),
        iso3c = c(rep('ABW', 10 + 20 - 12 + 1)),
        val1 = c(runif(10), runif(20 - 12 + 1))
    )
test_answer$date <- as.integer(test_answer$date)
test$date <- as.integer(test$date)

waitifnot(all.equal(dfdt(weight_mean_splice(test, 'val1')), 
                    dfdt(test_answer[order(iso3c, date)]), 
                    ignore.col.order = TRUE) == TRUE)

# duplicated dates --> throw error
test <- NULL
test_answer <- NULL
set.seed(3762)
test <-
    data.table(
        date = c(seq(1, 10), seq(10, 13)),
        iso3c = c(rep('ABW', 10 + 13 - 10 + 1)),
        val1 = c(runif(10), rep(NA, 13 - 10 + 1)),
        val1.y = c(rep(NA, 10), rep(1, 13 - 10 + 1))
    )
set.seed(3762)
test_answer <-
    data.table(
        date = c(seq(1, 10), seq(12, 20)),
        iso3c = c(rep('ABW', 10 + 20 - 12 + 1)),
        val1 = c(runif(10), rep(NA, 20 - 12 + 1))
    )
an.error.occured <- FALSE
tryCatch({
    result <- weight_mean_splice(test, 'val1')
    print(res)
}, error = function(e) {
    an.error.occured <<- TRUE
})
waitifnot(an.error.occured)

# 1 year of overlap --> do a 50-50 weight
test <- NULL
test_answer <- NULL
set.seed(3762)
test <-
    data.table(
        date = c(seq(1, 13)),
        iso3c = c(rep('ABW', 13)),
        val1 = c(runif(10), rep(NA, 3)),
        val1.y = c(rep(NA, 9), rep(1, 4))
    )
set.seed(3762)
test_answer <-
    data.table(
        date = c(seq(1, 13)),
        iso3c = c(rep('ABW', 13)),
        val1 = c(runif(9), mean(c(runif(
            1
        ), 1)), rep(1, 3))
    )
test_answer$date <- as.integer(test_answer$date)
test$date <- as.integer(test$date)

waitifnot(all.equal(dfdt(weight_mean_splice(test, 'val1')), 
                    dfdt(test_answer[order(iso3c, date)]), 
                    ignore.col.order = TRUE) == TRUE)

# all NA values --> remain all NA values
test <- NULL
test_answer <- NULL
test <-
    data.table(
        date = c(seq(1, 10), seq(12, 20)),
        iso3c = c(rep('ABW', 10 + 20 - 12 + 1)),
        val1 = c(rep(NA, 10 + 20 - 12 + 1)),
        val1.y = c(rep(NA, 10 + 20 - 12 + 1))
    )
test_answer <-
    data.table(
        date = c(seq(1, 10), seq(12, 20)),
        iso3c = c(rep('ABW', 10 + 20 - 12 + 1)),
        val1 = as.numeric(c(rep(NA, 10 + 20 - 12 + 1)))
    )
test_answer$date <- as.integer(test_answer$date)
test$date <- as.integer(test$date)

waitifnot(all.equal(dfdt(weight_mean_splice(test, 'val1')), 
                    dfdt(test_answer[order(iso3c, date)]),
                    ignore.col.order = TRUE) == TRUE)

# all NA values of NEW --> keep the base values
test <- NULL
test_answer <- NULL
set.seed(3762)
test <-
    data.table(
        date = c(seq(1, 10), seq(12, 20)),
        iso3c = c(rep('ABW', 10 + 20 - 12 + 1)),
        val1 = c(runif(10), rep(NA, 20 - 12 + 1)),
        val1.y = c(rep(NA, 10 + 20 - 12 + 1))
    )
set.seed(3762)
test_answer <-
    data.table(
        date = c(seq(1, 10), seq(12, 20)),
        iso3c = c(rep('ABW', 10 + 20 - 12 + 1)),
        val1 = c(runif(10), rep(NA, 20 - 12 + 1))
    )
test_answer$date <- as.integer(test_answer$date)
test$date <- as.integer(test$date)

waitifnot(all.equal(dfdt(weight_mean_splice(test, 'val1')), 
                    dfdt(test_answer[order(iso3c, date)]), 
                    ignore.col.order = TRUE) == TRUE)

# all NA values of BASE --> replace with the new values
test <- NULL
test_answer <- NULL
set.seed(3762)
test <-
    data.table(
        date = c(seq(1, 10), seq(12, 20)),
        iso3c = c(rep('ABW', 10 + 20 - 12 + 1)),
        val1 = c(rep(NA, 10 + 20 - 12 + 1)),
        val1.y = c(runif(10), rep(NA, 20 - 12 + 1))
    )
set.seed(3762)
test_answer <-
    data.table(
        date = c(seq(1, 10), seq(12, 20)),
        iso3c = c(rep('ABW', 10 + 20 - 12 + 1)),
        val1 = c(runif(10), rep(NA, 20 - 12 + 1))
    )
test_answer$date <- as.integer(test_answer$date)
test$date <- as.integer(test$date)

waitifnot(all.equal(dfdt(weight_mean_splice(test, 'val1')), 
                    dfdt(test_answer[order(iso3c, date)]), 
                    ignore.col.order = TRUE) == TRUE)

# just make sure that the weights work:
test <- NULL
test_answer <- NULL
test <- fread(
    "date	iso3c	val1	val1.y
1	ABW	0.761145783	0.761888573
2	ABW	0.920824863	0.593345689
3	ABW	0.071897651	0.944762696
4	ABW	0.999810629	0.380166193
5	ABW	0.4043324	0.233562523
6	ABW		0.279891356
7	ABW		
8	ABW		
9	ABW		
10	ABW		
11	ZWE	0.696350327	0.798177025
12	ZWE	0.58799149	0.446975286
13	ZWE	0.902998495	0.882708353
14	ZWE	0.085207355	0.35281139
15	ZWE	0.270606703	0.033803516
16	ZWE		0.277193069
17	ZWE		
18	ZWE		
19	ZWE		
20	ZWE		
"
)

test_answer <- 
    fread('
date	iso3c	val1
1	ABW	0.761269581
2	ABW	0.811665138
3	ABW	0.508330174
4	ABW	0.586714338
5	ABW	0.262024169
6	ABW	0.279891356
7	ABW	
8	ABW	
9	ABW	
10	ABW	
11	ZWE	0.713321443
12	ZWE	0.540986089
13	ZWE	0.892853424
14	ZWE	0.263610045
15	ZWE	0.073270714
16	ZWE	0.277193069
17	ZWE	
18	ZWE	
19	ZWE	
20	ZWE	
')

test_answer$date <- as.integer(test_answer$date)
test$date <- as.integer(test$date)

waitifnot(all.equal(dfdt(weight_mean_splice(test, 'val1')), 
                    dfdt(test_answer[order(iso3c, date)]), 
                    ignore.col.order = TRUE) == TRUE)

# weird overlap:: IF there's 0 NEW years, then don't replace the new values:

test <- fread(
    'date	iso3c	val1	val1.y
2000	ZWE		0.597614
2001	ZWE		0.970935
2002	ZWE		0.068916
2003	ZWE	0.730373	0.798177
2004	ZWE	0.587991	0.446975
2005	ZWE	0.902998	
2006	ZWE	0.085207	
2007	ZWE	0.270607	0.033804
2008	ZWE		0.692665
2009	ZWE	0.119431	0.232597
2010	ZWE	0.370224	0.165159
2011	ZWE	0.023417	
2012	ZWE	0.702965	
'
)

test_answer <- fread(
    'date	iso3c	val1
2000	ZWE	
2001	ZWE	
2002	ZWE	
2003	ZWE	0.730373
2004	ZWE	0.587991
2005	ZWE	0.902998
2006	ZWE	0.085207
2007	ZWE	0.270607
2008	ZWE	
2009	ZWE	0.119431
2010	ZWE	0.370224
2011	ZWE	0.023417
2012	ZWE	0.702965
'
)

test_answer$date <- as.integer(test_answer$date)
test$date <- as.integer(test$date)

waitifnot(all.equal(dfdt(weight_mean_splice(test, 'val1')), 
                    dfdt(test_answer[order(iso3c, date)]), 
                    ignore.col.order = TRUE) == TRUE)

# 1) weird overlap:: keep the base and do the weighting for the years that are
# overlapped
# 2) multiple iso3cs
# 3) other variables that I want to keep

test <- fread(
    'date	iso3c	val1	val1.y	other_variable1	other_variable2
2000	ZWE		0.597614	0.508969016	0.006070559
2001	ZWE		0.970935	0.881238317	0.343891872
2002	ZWE		0.068916	0.305629275	0.867798371
2003	ZWE	0.730373	0.798177	0.553483096	0.559408592
2004	ZWE	0.587991	0.446975	0.71319933	0.219462517
2005	ZWE	0.902998		0.232617004	0.489673586
2006	ZWE	0.085207		0.348250652	0.870568581
2007	ZWE	0.270607	0.033804	0.03549821	0.868881646
2008	ZWE		0.692665	0.410009647	0.401345661
2009	ZWE	0.119431	0.232597	0.296749345	0.300723357
2010	ZWE	0.370224	0.165159	0.205948699	0.334076617
2011	ZWE	0.023417	0.165159	0.971888277	0.333898566
2012	ZWE	0.702965	0.165159	0.773791925	0.91972832
2013	ZWE		0.542388001	0.8502448	0.914647173
2014	ZWE		0.19385125	0.898306121	0.792831992
2015	ZWE		0.044905743	0.323221101	0.660822824
2016	ZWE		0.344174649	0.520340077	0.769013109
2017	ZWE		0.586416395	0.940391808	0.996755881
1980	KNW		0.663011225	0.902544553	0.740958619
1981	KNW		0.273060632	0.466424298	0.732484496
1982	KNW		0.408574683	0.700588663	0.544846022
1983	KNW		0.704385602	0.205000436	0.288315002
1984	KNW		0.147940185	0.085595917	0.096801371
1985	KNW		0.48322337	0.879848876	0.439089646
1986	KNW		0.292074232	0.568286596	0.643209569
1987	KNW		0.260324671	0.108320082	0.249646452
1988	KNW	0.115763038	0.696165795	0.312603864	0.03195159
1989	KNW		0.073544634	0.715751093	0.769520441
1990	KNW		0.405899138	0.1997054	0.416988574
1991	KNW		0.051465075	0.231586633	0.854926096
1992	KNW		0.725561828	0.160227635	0.120144957
1993	KNW			0.554805984	0.30776646
1994	KNW			0.077634493	0.947391116
1995	KNW			0.100343641	0.409725882
'
)

test_answer <- fread(
    'date	iso3c	val1	other_variable1	other_variable2
2000	ZWE		0.508969016	0.006070559
2001	ZWE		0.881238317	0.343891872
2002	ZWE		0.305629275	0.867798371
2003	ZWE	0.730373	0.553483096	0.559408592
2004	ZWE	0.587991	0.71319933	0.219462517
2005	ZWE	0.902998	0.232617004	0.489673586
2006	ZWE	0.085207	0.348250652	0.870568581
2007	ZWE	0.270607	0.03549821	0.868881646
2008	ZWE		0.410009647	0.401345661
2009	ZWE	0.1420642	0.296749345	0.300723357
2010	ZWE	0.288198	0.205948699	0.334076617
2011	ZWE	0.1084622	0.971888277	0.333898566
2012	ZWE	0.2727202	0.773791925	0.91972832
2013	ZWE	0.542388001	0.8502448	0.914647173
2014	ZWE	0.19385125	0.898306121	0.792831992
2015	ZWE	0.044905743	0.323221101	0.660822824
2016	ZWE	0.344174649	0.520340077	0.769013109
2017	ZWE	0.586416395	0.940391808	0.996755881
1980	KNW		0.902544553	0.740958619
1981	KNW		0.466424298	0.732484496
1982	KNW		0.700588663	0.544846022
1983	KNW		0.205000436	0.288315002
1984	KNW		0.085595917	0.096801371
1985	KNW		0.879848876	0.439089646
1986	KNW		0.568286596	0.643209569
1987	KNW		0.108320082	0.249646452
1988	KNW	0.405964417	0.312603864	0.03195159
1989	KNW	0.073544634	0.715751093	0.769520441
1990	KNW	0.405899138	0.1997054	0.416988574
1991	KNW	0.051465075	0.231586633	0.854926096
1992	KNW	0.725561828	0.160227635	0.120144957
1993	KNW		0.554805984	0.30776646
1994	KNW		0.077634493	0.947391116
1995	KNW		0.100343641	0.409725882
'
)

test_answer$date <- as.integer(test_answer$date)
test$date <- as.integer(test$date)

waitifnot(all.equal(dfdt(weight_mean_splice(test, 'val1')), 
                    dfdt(test_answer[order(iso3c, date)]), 
                    ignore.col.order = TRUE) == TRUE)

# Weighted Mean implementation -------------------------------------

for (i in to_weight_mean_splice) {
    chatting <- weight_mean_splice(chatting, i)
}

save.image("weighted_mean_implementation.RData")
load('weighted_mean_implementation.RData')

# C) Splice: Growth ----------------------------------------------------------

# Note: when regressing passenger car vehicles cross-country values  from CHAT
# database with the dataset that we have, we get a slope  that is close to 1
# (0.981). However, there are still kinks in the data, as the US saw a HUGE
# drop in passenger cars between the CHAT dataset and the new data. This is
# likely due to a revision of the WB archived dataset from 2007 to 2008,
# though we cannot be for sure. Here, what we do is that we splice the two
# datasets together by  back-casting with growths.

# For other variables, get the growth rates between all two years  of the CHAT
# dataset take the oldest year of the new data multiply  that year by the
# growth rate for each year.

# sort by iso3c and date:
chatting <- chatting[order(iso3c, date),]

# a list to store the output:
list_growth_splicing <- list()

# convert to data.frame:
chatting <- as.data.frame(chatting)

if (nonempty_len(to_growth_splice) > 0) {
    for (var_ in to_growth_splice) {
        dtem <- NULL
        
        chatting[, var_] <- as.numeric(chatting[, var_])
        
        # check that everything is numeric
        waitifnot(class(chatting[, var_]) == "numeric")
        
        # create a temp data.frame with only that dataset:
        dtem <- chatting[, c('iso3c', 'date', var_, paste0(var_, '.y'))]
        
        # label the most recent date for OUR dataset:
        recent_dates <-
            dtem %>%
            group_by(iso3c) %>%
            filter(!is.na(eval(as.name(
                paste0(var_, '.y')
            )))) %>%
            mutate(recent = min(date)) %>%
            ungroup() %>%
            filter(date == recent) %>%
            dplyr::select(iso3c, recent)
        
        before <- nrow(dtem)
        dtem <-
            dtem %>% merge(., recent_dates, by = "iso3c", all.x = T)
        waitifnot(nrow(dtem) == before)
        waitifnot(nrow(dtem[, c("iso3c", "date")]) ==
                      nrow(distinct(dtem[, c("iso3c", "date")])))
        
        # if there is a duplicate error, run this code:
        # dtem %>% group_by(iso3c, date) %>%
        # mutate(counter = length(iso3c)) %>% View()
        
        dtem <- dtem[order(dtem$iso3c, dtem$date), ]
        
        # label where our data is more recent than CHAT's:
        dtem$recent <- (dtem$date >= dtem$recent)
        dtem$recent[is.na(dtem$recent)] <- FALSE
        
        # where the CHAT database is older and has data,
        # delete the NAs in between years:
        dtem <-
            dtem[!((dtem$recent == FALSE) & (is.na(dtem[, var_]))), ]
        
        # create a ratio between the indicator value and its value 1 time period ahead.
        dtem <- dtem[order(dtem$iso3c, dtem$date), ]
        dtem$recent <- NULL
        
        # convert dtem to data.table and start using data.table
        # syntax
        dtem <- dfdt(dtem)
        dtem[, chat_back_growth := shift(eval(as.name(var_))) / eval(as.name(var_)),
             by = .(iso3c)]
        dtem[, chat_back_growth_lead := shift(chat_back_growth, type = "lead"), 
             by = "iso3c"]
        dtem <- split(dtem, dtem$iso3c)
        
        # Then, we loop through each country and create this ratio between the OLDER
        # CHAT variable and the more RECENT corresponding variable. We then
        # recursively multiply that ratio to the existing most recent non-CHAT
        # variable until we get as far back in time as we can.
        for (iso_ in names(dtem)) {
            dtem[[iso_]] <- as.data.frame(dtem[[iso_]])
            start_loop_index <-
                nrow(dtem[[iso_]]) -
                length(na.omit(dtem[[iso_]][, paste0(var_, '.y')]))
            dtem[[iso_]]$chat_back_interp <-
                dtem[[iso_]][, paste0(var_, ".y")]
            dtem[[iso_]] <- dfdt(dtem[[iso_]])
            
            for (i in seq(start_loop_index, 1)) {
                # chat_back_interp_lead is the LEAD of the variable that we  want to
                # impute with the growth measure. For each run of the loop, we will
                # create this LEAD variable, use it, and then delete it.
                dtem[[iso_]][, chat_back_interp_lead := 
                                 shift(chat_back_interp, type = "lead")]
                
                # where the more recent data (NON CHAT) is empty (i.e. older data), get
                # the values by multiplying the value 1 year ahead (lead) with the
                # growth ratio.
                dtem[[iso_]][is.na(chat_back_interp),
                             chat_back_interp := 
                                 chat_back_interp_lead * chat_back_growth_lead]
                dtem[[iso_]][, chat_back_interp_lead := NULL]
                dtem[[iso_]] <- dfdt(dtem[[iso_]])
            }
            
        }
        dtem <- as.data.frame(rbindlist(dtem, fill = T))
        dtem <- dtem[, c('iso3c', 'date', 'chat_back_interp')]
        dtem$chat_back_interp[is.nan(dtem$chat_back_interp)] <- NA
        names(dtem)[names(dtem) == 'chat_back_interp'] <-
            paste0(var_, '.spliced')
        list_growth_splicing[[var_]] <- dfdt(dtem)
        dtem <- NULL
    }
    
    # bind all these columns with spliced datasets together with the  CHAT
    # dataset:
    to_expand <-
        rbindlist(list_growth_splicing, fill = T)[, .(iso3c, date)]
    to_expand <- CJ(iso3c = unique(chatting$iso3c),
                    date = seq(min(chatting$date, na.rm = T),
                               max(chatting$date, na.rm = T),
                               1))
    tomerge <- c(list(to_expand),
                 list_growth_splicing,
                 list(chatting))
    
    all_spliced <-
        Reduce(function(x, y) {
            merge(x, y, by = c('iso3c', 'date'), all.x = T)
        },
        tomerge)
    
    tomerge <- NULL
    
    chatting <- dfdt(all_spliced)
    
    all_spliced <- NULL
    
    # check that we still have unique iso3c and dates:
    waitifnot(nrow(chatting[, .(iso3c, date)]) == nrow(distinct(chatting[, .(iso3c, date)])))
    
    # check that where we have recent data, the spliced values are the SAME as the
    # recent data:
    chatting <- as.data.frame(chatting)
    
    for (var_ in to_growth_splice) {
        waitifnot(all(abs(na.omit(
            chatting[, paste0(var_, '.y')] - chatting[, paste0(var_, '.spliced')]
        )) < 0.0001))
        
        chatting[, paste0(var_, '.y')] <- NULL
        chatting[, var_] <- chatting[, paste0(var_, '.spliced')]
        chatting[, paste0(var_, '.spliced')] <- NULL
    }
    
    chatting <- dfdt(chatting)
}

save.image('splice_growth.RData')
load('splice_growth.RData')

# D) Splice: Keep our Metric; delete CHAT's --------------------------------

# For MRIs and CTs agree just to plot them as separate series. For the steel
# production variable and total fertilizer, let's just use our data. For Civil
# aviation passenger-KM traveled and *number* of passengers carried, we keep
# both as separate series. Cell phone number very closely track cell phone
# subscriptions. So, we keep both.
if (nonempty_len(to_replace_splice)>0) {
    chatting[, (to_replace_splice) := NULL]
}

# rename cellphone to be cellular subscriptions (more true description of variable)
chatting <- 
  chatting %>%
  rename(
    cell.subsc = cellphone.y
         ) %>%
  as.data.table()

save.image('splice_keep_our_metric.RData')
load('splice_keep_our_metric.RData')

# E) Splice: Keep CHAT's Metric; delete our's ------------------------------

# For MRIs and CTs agree just to plot them as separate series. For the steel
# production variable and total fertilizer, let's just use our data. For Civil
# aviation passenger-KM traveled and *number* of passengers carried, we keep
# both as separate series. Cell phone number very closely track cell phone
# subscriptions. So, we keep both.
if (nonempty_len(ours_to_delete_splice)>0) {
  chatting[, (ours_to_delete_splice) := NULL]
}

save.image('splice_keep_chat_metric.RData')
load('splice_keep_chat_metric.RData')


# F) Splice: Interpolate ---------------------------------------------------
chatting <- as.data.frame(chatting)

if (nonempty_len(to_interpolate_splice) > 0) {
    for (variable_ in to_interpolate_splice) {
        # Create a prediction of the telephone line (CHAT) using our new  WDI fixed
        # telephone subscription data. We still have the fit from the linear model
        # when doing the CHAT comparison earlier, so we use that.
        chatting$predicted <-
            predict(reg.chat.ky[[variable_]],
                    data.frame(chatvar = chatting[, variable_]))
        
        # Where CHAT does not have an older dataset, replace the telephone variable
        # with this interpolated variable.
        chatting[is.na(chatting[, paste0(variable_, ".y")]),
                 paste0(variable_, ".y")] <-
            chatting$predicted[is.na(chatting[, paste0(variable_, ".y")])]
        chatting[, variable_] <- chatting[, paste0(variable_, '.y')]
        chatting[, paste0(variable_, '.y')] <- NULL
        chatting$predicted <- NULL
    }
    
    chatting <- dfdt(chatting)
    
    # # Confirm graphically that there are some biazarre kinks in dataset from a
    # # sample of HICs with good quality data:
    # chatting[, .(date, iso3c, telephone, telephone.predicted)] %>%
    #   filter(iso3c %in% c("RUS")) %>%
    #   ggplot() +
    #   geom_line(aes(
    #     x = date,
    #     y = telephone,
    #     group = iso3c,
    #     color = iso3c,
    #     linetype = 'actual & interpolated'
    #   )) +
    #   geom_line(
    #     aes(
    #       x = date,
    #       y = telephone.predicted,
    #       group = iso3c,
    #       color = iso3c,
    #       linetype = 'interpolated'
    #     )
    #   )
}

save.image('interpolate.RData')
load('interpolate.RData')



# G) Don't Splice: Keep as separate series --------------------------------

# MRIs, cat scans, aviation passengers, cell subscriptions. 
chatting <- chatting %>%
    rename(
        ct_scans = med_catscanner.y,
        mri_scans = med_mriunit.y,
        aviation_pass = aviationpkm.y,
        electric_gen_capacity = elecprod.z,
        railline_wdi = railline.y,
        telephone_canning_wdi = telephone.y,
        creditdebit_volume = creditdebit.y
    ) %>%
    as.data.table()


# Remove indicator (.y/.z) -----------------------------------------------

# Now that we're done splicing, rename the technologies that have ".y" at the
# end to remove the ".y".
names(chatting) <- names(chatting) %>% gsub("\\.y$|\\.z$","", .)

save.image("spliced_dataset.RData")
load("spliced_dataset.RData")

# Classify Technologies ---------------------------------------------------

# we want to classify all of our technologies into these groups:
# Manufacturing technologies
# Communications technologies
# Transport technologies
# Hospital (/non-drug medical) technologies
# Drug/vaccine technologies
# Financial technologies
chatting[, `:=`(country_name = NULL,
                xlpopulation = NULL,
                xlrealgdp = NULL)]
id.vars.chat <- c("date", "iso3c", "gdppc", 
                  "pop", "adults")
chatting[,(setdiff(names(chatting), id.vars.chat)):=lapply(.SD, as.numeric), 
         .SDcols = setdiff(names(chatting), id.vars.chat)]
chatting <- melt(chatting,
                 id.vars = id.vars.chat,
                 measure.vars = setdiff(names(chatting), id.vars.chat))

# check that we have 1 observation per country-year FOR EACH variable:
chatting[,check.count:=1]
check.a <- chatting[,.(sum(check.count)), by = .(date, iso3c, variable)] %>% as.data.table()
waitifnot(all(as.vector(unlist(check.a$V1))==1))
check.a <- NULL

# create a variable that indicates the overarching category of the technologies:
variable_categories <- fread('variable_categories.csv')
chatting <- merge(chatting, variable_categories, by = 'variable', all.x = T)
chatting[,categ:=as.factor(categ)]

# check that there are 8 new categories
waitifnot(length(unique(chatting$categ))==8)

save.image("merge_with_chat_dataset.RData")
load("merge_with_chat_dataset.RData")


# Label Variables --------------------------------------------------

# Manually relabel the variables:
chatting[,variable:=as.character(variable)]

# get the labels that I've manually done:
labels <- fread('CHAT_variable_labels.csv')

# check that we do not have any duplicate variables or labels
labels[label=="", label := NA]
labels[variable=="", variable := NA]

check_dup_id(labels,'variable', na.rm=TRUE)
check_dup_id(labels,'label', na.rm=TRUE)

# check that the label does not equal the variable
waitifnot(all(na.omit(labels[,label!=variable])))

# replace missing labels with 'to delete'
labels[is.na(label), label:='to_delete']

# merge with the dataset
a <- nrow(chatting)
chatting <- merge(chatting, labels, by = c('variable'), all.x = T)
waitifnot(nrow(chatting)==a)

# check that num_missing_obs gets deleted
chatting <- chatting[variable != "num_missing_obs", ]

# delete the ones that were marked for deletion
chatting[is.na(label),label:=variable]
chatting <- chatting[!label=="to_delete",]
waitifnot(all(is.na(chatting$label)==FALSE))

# check we don't have duplicated IDs in data.frame
check_dup_id(chatting, c('variable','date','iso3c'))

# Create a function that makes the first word uppercase.
firstup <- function(x) {
  x <- as.character(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# make the label uppercase
chatting[, label:=firstup(label)]
chatting[,label:=
           label %>% 
           gsub("Telephone_canning_wdi","Telephone (Canning WDI)",.) %>% 
           gsub("Railline_wdi", "Rail-line (WDI)",.) %>% 
           gsub("_"," ",., fixed = T)]

# replace label with variable:
# chatting[,variable:=label]
# chatting[,label:=NULL]


# load data from COVID project -----------------------------
load("port_CHAT.RData")
port_chat$label <- port_chat$label %>% 
  unlist() %>% as.vector() %>% as.character()
chatting <- rbindlist(list(chatting, port_chat), fill = T)

save.image("relabel_variable_names.RData")
load("relabel_variable_names.RData")