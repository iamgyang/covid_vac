# curve 1: preston curve ----------------------------------
load("table_aesthetic.RData")
start.end.dates <-
  as.data.table(chatting$Yes[, .(`Technology (with date)`,
                                 Variable,
                                 `Start Date`,
                                 `End Date`)])
load("restrict_sample.RData")
df.toplot <-
  chatting[, .(iso3c, gdppc, date, value,
               variable, start.date, end.date, categ)]
df.toplot <- df.toplot[categ == "Vaccine--Covid Paper"]

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
  scale_y_continuous(breaks = seq(0,100,20))

ggsave(
  paste0("point_preston_linear.pdf"),
  plot,
  width = 10,
  height = 14,
  limitsize = FALSE#,
  # dpi = 400
)


# curve 2: coefficient of variation across time -----------------------

# get the start and final dates from the other table
load("table_aesthetic.RData")
start.end <- chatting$Yes[, .(Variable, `Start Date`, `End Date`, Label)]

load("relabel_variable_names.RData")
chatting <- chatting[date %in% seq(1951, 2021, 1)]
chatting <- 
  merge(chatting, start.end, 
        by.x = c('variable'), 
        by.y = 'Variable', 
        allow.cartesian = FALSE,
        all.x = TRUE)

# narrow down to a fixed sample of countries (those present in every year)
chatting <- chatting %>% filter(!is.na(`Start Date`))
fix_countries <- chatting %>% dfdt()
chatting <- NULL

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
fix_countries[,poptotal:=pop]
fix_countries[label == "Influenza", pop := pop_t_ge65]
fix_countries[label == "HPV", pop := pop_f_le15]
fix_countries[label == "Yellow Fever", pop := pop_t_l60]
fix_countries[label != "Influenza" & 
                label != "HPV" & 
                label != "Yellow Fever", pop := pop_t_le1]
fix_countries[label == "Covid-19", pop := poptotal]

waitifnot(all(unlist(fix_countries[!(label%in%percent.labels),.(is.na(pop))])==FALSE))

fix_countries <- fix_countries %>% rename(year = date) %>% dfdt()
fix_countries[label == "Covid-19" & year == 2021] %>% 
  write.csv("check.covid.csv", na = "", row.names = FALSE)
fix_countries <- fix_countries[, .(meanval = mean(value, na.rm = T),
                                   stdev = sd(value, na.rm = T),
                                   w.meanval = weighted_mean(value, pop, na.rm = TRUE),
                                   w.stdev = w.sd(value, pop)
), 
by = c("label", "year", "categ")]
fix_countries[, coef.var := stdev / meanval]
fix_countries[, w.coef.var := w.stdev / w.meanval]

# we drop smallpox because data is too patchy:
fix_countries <- fix_countries[label!="Smallpox"]

save.image("index_convergence.RData")
load("index_convergence.RData")

# plot convergence

title_wording <- " Values are weighted by vaccine target population."
coef_var_type_ <- "w.coef.var"

plot <-
  ggplot(data = na.omit(fix_countries), 
         aes(year, eval(as.name(coef_var_type_)), 
             group = label, color = label)) +
  geom_line() +
  geom_point(show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1980, 2020, 10), 
                     limits = c(1980,2021)) +
  scale_fill_stata() +
  my_custom_theme +
  labs(
    x = "",
    y = "Coefficient of Variation"#,
    # title = paste0("Coefficient of Variation over Time"),
    # subtitle = paste0(strwrap(paste0(title_wording),
    # 100), collapse = "\n")
  ) +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 3))) +
  scale_color_custom + 
  coord_cartesian(xlim = c(1980, 2021))

ggsave(
  paste0("yes_no_tech", "_non_log_", coef_var_type_ , ".pdf"),
  plot,
  width = 12,
  height = 7
)

# curve 3: means and stdevs ----------------------------------------

# plot means and standard deviations across time for variable in each 
# category with the longest time series:

load("index_convergence.RData")

# restrict to be all from 0-100 on y axis and 1980-2020 on x axis:

ggplot_restrictions <- 
       list(coord_cartesian(ylim = c(0, 100), xlim = c(1980, 2021)))

toloop <- unique(fix_countries$label)
toloop <- setdiff(toloop, "Smallpox")

# for diseases NOT COVID, delete 2021 data because it's so patchy:
fix_countries[year%in%c(2021) & label != "Covid-19", meanval:=NA]
fix_countries[year%in%c(2021) & label != "Covid-19", stdev:=NA]
fix_countries[year%in%c(2021) & label != "Covid-19", w.stdev:=NA]
fix_countries[year%in%c(2021) & label != "Covid-19", w.meanval:=NA]

# For flu and HPV, delete 2020 data because it's so patchy:
# As of 11-18-2021, there are only 9 countries that we have flu data for,
# and only 44 countries that we have HPV for in 2020 (compared to 22 and 54
# for 2019, respectively).
fix_countries[year%in%c(2020) & label %in% c("Influenza", "HPV"), 
          (c("meanval", "stdev", "w.stdev", "w.meanval")):=NA]

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
  labs(y = "Coverage (%)", x = "") + 
  theme(legend.position = "none") + 
  ggplot_restrictions + 
  facet_wrap(.~label, scales = "free") + 
  scale_color_custom

ggsave(paste0("mean_stdev_facetted.pdf"),
       plot,
       width = 10,
       height = 7)

