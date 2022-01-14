chatting <- chatting[iso3c%in%(port_chat$iso3c %>% unique())]

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

load("restrict_sample.RData")
df.toplot <-
    chatting[, .(iso3c, gdppc, date, value,
                 variable, start.date, end.date, categ)]
df.toplot <- df.toplot[categ == "Vaccine--Covid Paper"]
fdEGZGv35H2nt <- df.toplot %>% as.data.frame() %>% as.data.table()

load("table_aesthetic.RData")
start.end.dates <-
    as.data.table(chatting$Yes[, .(`Technology (with date)`,
                                   Variable,
                                   `Start Date`,
                                   `End Date`)])
df.toplot <- fdEGZGv35H2nt %>% as.data.frame() %>% as.data.table()

# for Preston curves, we do not want to plot HPV, Smallpox, or Flu:
df.toplot <- df.toplot %>%
    filter(
        variable != "Smallpox" &
            # Didn't want flu or HPV because we never reached 20%
            # global coverage.
            variable != "influenza" &
            variable != "HPV"
    ) %>%
    dfdt()

# check that start and end dates align
start.end.dates[, `Technology (with date)` :=
                    gsub("\\s*\\([^\\)]+\\)", "",
                         `Technology (with date)`)]
df.toplot <-
    merge(df.toplot, start.end.dates,
          by.y = c("Variable"),
          by.x = c("variable"),
          allow.cartesian = FALSE,
          all.x = T)
waitifnot(nrow(df.toplot[start.date!=`Start Date`])==0)
waitifnot(nrow(df.toplot[end.date!=`End Date`])==0)

df.toplot <- 
    df.toplot %>% 
    as.data.frame() %>% 
    dplyr::select(
        `Technology (with date)`, iso3c, gdppc, date, value, `Start Date`, `End Date`
    ) %>% 
    gather(., "period","date_start_end",`Start Date`:`End Date`) %>% 
    mutate(
        period = gsub(" Date", "", period)
    ) %>% 
    filter(date == date_start_end) %>% 
    as.data.table() 

df.toplot$`Technology (with date)` %>% 
    unique() %>% 
    grepl("smallpox|hpv|influenza", ., ignore.case = T) %>% 
    (function(x) {x==FALSE}) %>% 
    all() %>% 
    waitifnot()

# filter so that 2020/2021 is not 0:
df_fil <- 
    df.toplot %>%
    dplyr::select(-c(period, date_start_end, gdppc)) %>%
    pivot_wider(names_from = date, values_from = value) %>%
    mutate(
        tokeep = case_when(
            `Technology (with date)` == "Covid-19" & `2021` != 0 ~ 1,
            `Technology (with date)` != "Covid-19" & `2020` != 0 ~ 1,
            TRUE ~ 0
        )
    ) %>% 
    filter(tokeep == 1) %>%
    dplyr::select(`Technology (with date)`, iso3c) %>%
    mutate(keep = 1) %>%
    dfdt()
check_dup_id(df_fil, c("Technology (with date)", "iso3c"))

df.toplot <- merge(df.toplot, df_fil, all.x = T)
df.toplot <- df.toplot %>% filter(keep == 1)

# for cellular subscriptions, sometimes we have greater subscriptions 
# than the population
df.toplot[value>=100,value:=100]
waitifnot(df.toplot$value==100<=0.5)

# make cellular subscriptions come last:
df.toplot$`Technology (with date)` <- df.toplot$`Technology (with date)` %>% 
    factor(.,
           levels = c(
               setdiff(sort(unique(
                   df.toplot$`Technology (with date)`
               )), "Cellular subscriptions"),
               "Cellular subscriptions"
           ))

plot <- 
    ggplot(df.toplot, 
           aes(x = gdppc, 
               y = value, 
               color = as.factor(date_start_end), 
               group = as.factor(date_start_end))) + 
    geom_point(show.legend = FALSE) + 
    geom_smooth(
        se = FALSE,
        size = 0.4,
        method = "lm",
        formula = (y~x),
        aes(
            x = gdppc,
            y = value,
            group = as.factor(date_start_end),
            color = as.factor(date_start_end)
        )
    ) + 
    annotation_logticks(sides = "b") +
    scale_x_continuous(labels=scales::dollar_format(), trans = "log10") + 
    my_custom_theme + 
    labs(y = "Coverage (%)", subtitle = "", x = "GDP per capita (Log Scale)")+
    scale_color_custom + 
    guides(colour = guide_legend(override.aes = list(size = 3))) + 
    facet_wrap(~`Technology (with date)`) + 
    scale_y_continuous(breaks = seq(0,100,20)) + 
    coord_cartesian(ylim = c(0, 100))


ggsave(
    paste0("point_preston_linear.pdf"),
    plot,
    width = 10,
    height = 14,
    limitsize = FALSE#,
    # dpi = 400
)
