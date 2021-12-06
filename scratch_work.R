# curve 3: coefficient of variation across time -----------------------

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

