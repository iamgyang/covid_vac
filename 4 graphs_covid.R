# curve 1: coefficient of variation ---------------------------------------

load("table_aesthetic.RData")

df.toplot <- 
chatting$Yes %>%
  as.data.frame() %>%
  filter(Category == "Vaccine--Covid Paper") %>% 
  dplyr::select(
    `Technology (with date)`,
    Category,
    `Coefficient of variation of the value at the start`,
    `Coefficient of variation of the value at the end`
  ) %>%
  gather(
    .,
    "Period",
    "Coefficient of Variation",
    `Coefficient of variation of the value at the start`:`Coefficient of variation of the value at the end`
  ) %>%
  mutate(
    Period = 
      Period %>% 
           gsub("Coefficient of variation of the value at the ", "", .) %>% 
           factor(., levels = c("start", "end"))
         ) %>% 
  filter(`Technology (with date)` %>% grepl("Smallpox",., ignore.case = T)==FALSE) %>% 
  as.data.table()

df.toplot$Category %>% unique()

# Agricultural Technologies
# Communications technologies
# Drug/vaccine technologies
# Manufacturing technologies
# Other
# Transport technologies

plot <-
  ggplot(
    df.toplot,
    aes(
      x = Period,
      y = `Coefficient of Variation`,
      group = `Technology (with date)`,
      color = `Category`
    )
  ) + geom_line() +
  labs(x = "", y = "",
       subtitle = "Coefficient of Variation (Log Scale)") +
  # geom_text_repel(
  #   data = df.toplot[Period == "end" &
  #                      (
  #                        Category == "Transport technologies" |
  #                          Category == "Communications technologies" |
  #                          Category == "Drug/vaccine technologies"
  #                      )],
  #   aes(
  #     x = Period,
  #     y = `Coefficient of Variation`,
  #     label = `Technology (with date)`,
  #     hjust = -0.1
  #   ),
  #   direction = "y",
  #   segment.colour = "grey85",
  #   segment.size = 0,
  #   show.legend = FALSE
  # ) +
  geom_text_repel(
    data = df.toplot[Period == "start" &
                       !(
                         Category == "Transport technologies" |
                           Category == "Communications technologies" |
                           Category == "Drug/vaccine technologies"
                       )],
    aes(
      x = Period,
      y = `Coefficient of Variation`,
      label = `Technology (with date)`,
      hjust = 1.1
    ),
    direction = "y",
    segment.colour = "grey85",
    segment.size = 0,
    show.legend = FALSE
  ) +
  scale_color_custom + 
  geom_point(show.legend = FALSE) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  my_custom_theme +
  scale_y_log10()

ggsave(
  paste0("point_start_end_coef_variation.png"),
  plot,
  width = 10,
  height = 14,
  limitsize = FALSE,
  dpi = 1000
)


# curve 2: preston curve ----------------------------------
load("table_aesthetic.RData")
start.end.dates <-
  as.data.table(chatting$Yes[,.(`Technology (with date)`,
                Variable,
                `Start Date`,
                `End Date`)])
load("restrict_sample.RData")
df.toplot <- 
  chatting[,.(iso3c, gdppc, date, value, 
              variable, start.date, end.date, categ)]
df.toplot <- df.toplot[categ=="Vaccine--Covid Paper"]

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
  filter(`Technology (with date)`!="Smallpox") %>% 
  as.data.table() 
  
for (tec in unique(df.toplot$`Technology (with date)`)) {
  plot <- 
    ggplot(df.toplot[`Technology (with date)` == tec], 
    aes(x = gdppc, 
        y = value, 
        color = as.factor(date_start_end), 
        group = as.factor(date_start_end))) + 
    geom_point() + 
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
    labs(y = "", subtitle = tec)+
    scale_color_custom + 
    guides(colour = guide_legend(override.aes = list(size = 3)))
  
  ggsave(
    paste0("point_preston_poly2", make.names(cleanname(tec)),".png"),
    plot,
    width = 10,
    height = 14,
    limitsize = FALSE,
    dpi = 400
  )
  
}


# curve 3: index of yes/no convergence across time ---------------------------------

# get the population data from beginning data cleaning:
load("merged1.RData")
land_and_pop <- 
  m.alldf[,.(pop, iso3c, date, land = `Land Arable land Area 1000 ha`)]

# get disease-specific population data here:
load("port_CHAT.RData")
pop_disease <- pop[,.(iso3c, year, pop, pop_f_le15, pop_t_le1, pop_t_ge65, pop_t_l60)]
pop_disease <- unique(pop_disease)

# get the start and final dates from the other table
load("table_aesthetic.RData")
start.end <- chatting$Yes[, .(Variable, `Start Date`, `End Date`, Label)]

load("relabel_variable_names.RData")
chatting <- chatting[date %in% seq(1950, 2019, 5)]
chatting <- 
  merge(chatting, start.end, 
        by.x = c('variable'), 
        by.y = 'Variable', 
        allow.cartesian = FALSE,
        all.x = TRUE)

# make all variables per capita
chatting$label <- NULL
chatting[Label %in% percent.labels, pop := 1]
chatting[, value := value / pop]

# narrow down to a fixed sample of countries (those present in every year)
chatting <- chatting %>% filter(!is.na(`Start Date`))
fix_countries <- chatting %>% split.data.frame(chatting$Label)
fix_countries <- lapply(fix_countries,
                        function(x)
                        {
                          x <- x %>%
                            filter(!is.na(value)) %>%
                            dplyr::select(iso3c, value, date, categ)

                          # If for 1 date / year, we just have a paucity of
                          # country coverage, then we exclude it. We do this
                          # by getting the number of countries per date
                          # (5 years). And then, if one of those dates has
                          # less than 50 the count of countries at the
                          # maximum date, that date is excluded.
                          counts_date <- as.numeric(table(x$date))
                          max_count <- max(counts_date)
                          exclude_date <-
                            as.numeric(
                              names(table(x$date)[counts_date < max_count - 50]))

                          x <- x %>%
                            filter(!(date %in% exclude_date)) %>%
                            spread(date, value) %>%
                            na.omit() %>%
                            gather(., "year", "value", !c("iso3c", "categ"))
                        })
fix_countries <- rbindlist(fix_countries, id = "label")
fix_countries <- chatting %>% dfdt()

# merge back in population (will need for weighted coefficient of variation)
fix_countries <- merge(
  fix_countries[,.(label = Label,iso3c,categ,value,
                   year = as.numeric(date))], 
                   land_and_pop[, .(year = date,
                                        iso3c, 
                                        pop, 
                                        land)],
      by = c("iso3c", "year"),
      all.x = TRUE)

fix_countries <- merge(
  fix_countries,
  pop_disease,
  by = c('iso3c','year'),
  all.x = TRUE
)

# weighted standard deviation function:
w.sd <- function(x, wt) {sqrt(wtd.var(x, wt))}

# weighted mean function (with NA.RM):
weighted_mean <-  function(x, w, ..., na.rm = FALSE) {
  if (na.rm == TRUE) {
    x1 = x[!is.na(x) & !is.na(w)]
    w = w[!is.na(x) & !is.na(w)]
    x = x1
  }
  weighted.mean(x, w, ..., na.rm = FALSE)
}

# limit to the vaccines for vaccine paper:
fix_countries <- fix_countries[categ=="Vaccine--Covid Paper"]

# calculate coefficient of variation across each year
# and a weighted coefficient of variation. NOTE: weighted coefficient of variation may have FEWER countries than the standard coefficient of variation due to lack of data for population or land IF the variable was a percent variable.

# base our population variable on the population of the target population
fix_countries[label == "Influenza", pop := pop_t_ge65]
fix_countries[label == "HPV", pop := pop_f_le15]
fix_countries[label == "Yellow Fever", pop := pop_t_l60]
fix_countries[label != "Influenza" & 
              label != "HPV" & 
              label != "Yellow Fever", pop := pop_t_le1]

waitifnot(all(unlist(fix_countries[!(label%in%percent.labels),.(is.na(pop))])==FALSE))

fix_countries <- fix_countries[, .(meanval = mean(value, na.rm = T),
            stdev = sd(value, na.rm = T),
            w.meanval = weighted_mean(value, pop, na.rm = TRUE),
            w.stdev = w.sd(value, pop),
            w.ag.meanval = weighted_mean(value, land, na.rm = TRUE),
            w.ag.stdev = w.sd(value, land)
            ), 
            by = c("label", "year", "categ")]
fix_countries[, w.ag.coef.var := w.ag.meanval / w.ag.stdev]
fix_countries[, coef.var := stdev / meanval]
fix_countries[, w.coef.var := w.stdev / w.meanval]
fix_countries[categ == "Agricultural Technologies", w.coef.var := w.ag.coef.var]
fix_countries$w.ag.coef.var <- NULL

# we drop smallpox because data is too patchy:
fix_countries <- fix_countries[label!="Smallpox"]

save.image("index_convergence.RData")
load("index_convergence.RData")

# plot convergence

title_wording <- " Values are weighted by vaccine target population."
coef_var_type_ <- "w.coef.var"

plot <-
  ggplot(data = fix_countries[year != 1980]
         , aes(year, eval(as.name(coef_var_type_)), 
               group = label, color = label)) +
  geom_line() +
  geom_point(show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  scale_fill_stata() +
  my_custom_theme +
  labs(
    x = "",
    y = "",
    title = paste0("Coefficient of Variation over Time"),
    subtitle = paste0(strwrap(paste0(title_wording),
               100), collapse = "\n")
  ) +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 3))) +
  scale_color_custom + 
  coord_cartesian(xlim = c(1970, 2020))

ggsave(
  paste0("yes_no_tech", "_non_log_", coef_var_type_ , ".pdf"),
  plot,
  width = 12,
  height = 7
)

# curve 4: means and stdevs ----------------------------------------

# plot means and standard deviations across time for variable in each 
# category with the longest time series:

load("index_convergence.RData")

# restrict to be all from 0-100 on y axis and 1980-2020 on x axis:

ggplot_restrictions <- 
       list(coord_cartesian(ylim = c(0, 100), xlim = c(1980, 2020)))

toloop <- unique(fix_countries$label)
toloop <- setdiff(toloop, "Smallpox")
plot <-
  ggplot(fix_countries[label%in%toloop], 
         aes(x = year, group = label, color = label)) +
  geom_line(aes(y = meanval, group = label, color = label)) +
  geom_ribbon(
    aes(
      ymin = meanval - 1 * stdev,
      ymax = meanval + 1 * stdev,
      group = label
    ),
    color = "grey83",
    alpha = 0.2
  ) +
  my_custom_theme + 
  labs(y = "") + 
  theme(legend.position = "none") + 
  ggplot_restrictions + 
  facet_wrap(.~label, scales = "free") + 
  scale_color_custom

ggsave(paste0("mean_stdev_facetted.pdf"),
       plot,
       width = 10,
       height = 7)