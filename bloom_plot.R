
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
