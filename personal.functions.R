# set GGPLOT default theme:
theme_set(
  theme_clean() + theme(
    plot.background = element_rect(color = "white"),
    axis.text.x = element_text(angle = 90),
    legend.title = element_blank()
  )
)
cleanname <- function(x) {
  x <- tolower(gsub(" ", ".", x))
  x <- gsub("%", " percent", x)
  x <- gsub("_", ".", x)
  x <- gsub("-", ".", x)
  x <- gsub("\\.\\.", ".", x)
  x <- gsub("\\.xlsx", "", x)
  x <- gsub("N/A", NA, x)
  x <- gsub("Unknown", NA, x)
  x <- gsub(".csv", "", x)
  return(x)
}
read.xl.sheets <- function(Test_Cases) {
  names.init <- excel_sheets(Test_Cases)
  test.ex <- list()
  counter <- 1
  for (val in names.init) {
    test.ex[[counter]] <-
      as.data.frame(read_excel(Test_Cases, sheet = val))
    counter <- counter + 1
  }
  names(test.ex) <- names.init
  test.ex <- lapply(test.ex, as.data.table)
  test.ex
}
# function that gets means and stdevs for each regression
summary.stats <-
  function(fit_) {
    data.frame(
      mean = apply(fit_$model, 2, function(x)
        mean(x, na.rm = TRUE)),
      stdev = apply(fit_$model, 2, function(x)
        (sd(na.omit(
          x
        ))))
    )
  }
code2name <-
  function(x, ...) {
    countrycode(x, "iso3c", "country.name", ...)
  }
name2code <-
  function(x, ...) {
    countrycode(x, "country.name", "iso3c", ...)
  }
name2region <-
  function(x) {
    countrycode(x, "country.name", "un.region.name")
  }
coalesce2 <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    if (class(x)[1] != class(y)[1])
      stop("ahh! classes don't match")
    x[i] <- y[i]
    x
  },
  list(...))
} # function from mrip https://stackoverflow.com/questions/19253820/how-to-implement-coalesce-efficiently-in-r
dfcoalesce <- function(df_, newname, first, second) {
  df_ <- as.data.frame(df_)
  df_[, newname] <- coalesce2(df_[, first],
                              df_[, second])
  df_[, first] <- NULL
  df_[, second] <- NULL
  df_
}
# function that coalesces any duplicates (.x, .y):
# note: it only merges the names that have .x and .y at the END
# you must make sure that things are properly labeled as "NA" from the beginning
dfcoalesce.all <- function(df_) {
  tocolless.1 <- names(df_)[grep("\\.x", names(df_))]
  tocolless.2 <- names(df_)[grep("\\.y", names(df_))]
  tocolless.1 <- gsub("\\.x", "", tocolless.1)
  tocolless.2 <- gsub("\\.y", "", tocolless.2)
  tocolless <- intersect(tocolless.1, tocolless.2)
  
  for (n_ in tocolless) {
    first <- paste0(n_, ".x")
    second <- paste0(n_, ".y")
    different <-
      sum(na.omit(df_[, first] == df_[, second]) == FALSE)
    # error if there is something different between the two merges:
    cat(
      paste0(
        " For the variable ",
        n_,
        ", you have ",
        different,
        " differences between the x and y column. \n Coalesced while keeping x column as default. \n"
      )
    )
    df_ <- dfcoalesce(
      df_,
      newname = n_,
      first = paste0(n_, ".x"),
      second = paste0(n_, ".y")
    )
  }
  df_
}
grepname <-
  function(df_, string_) {
    names(df_)[grep(string_, names(df_))]
  }
mergealist <- function(list_, id_elements) {
  alldf <-
    merge(
      list_[[1]],
      list_[[2]],
      by = id_elements,
      all = TRUE,
      allow.cartesian = FALSE
    )
  for (i in c(3:length(list_))) {
    alldf <-
      merge(
        alldf,
        list_[[i]],
        by = id_elements,
        all = TRUE,
        allow.cartesian = FALSE
      )
  }
  alldf
}

# find names based on inclusion exclusion grep:
clean_ilostat <- function(id) {
  dt <- get_ilostat(id) %>% label_ilostat
  names(dt) <- gsub("\\.label", "", names(dt))
  try(setDT(dt)[, sex := gsub("Sex: ", "", sex)])
  if (any(grepl("M", dt$time))) {
    setDT(dt)[, time := paste0(time, "-01")]
  }
  setDT(dt)[, time := time %>%
              gsub("M",
                   "-", .) %>%
              gsub("Q1",
                   "-3-30", .) %>%
              gsub("Q2",
                   "-6-30", .) %>%
              gsub("Q3",
                   "-9-30", .) %>%
              gsub("Q4",
                   "-12-30", .)]
  try(dt[, time := as.Date(time)])
  dt <- dt[!(is.na(obs_value))]
  try(dt[order(time), last_time := tail(time, 1), by = c("ref_area", "sex", "indicator", "classif1")])
  try(dt[order(time), last_time := tail(time, 1), by = c("ref_area", "sex", "indicator", "classif1", "classif2")])
  dr <- as.data.table(dt)
  dt <- NULL
  dr
}
na.mean <- function(x)
  mean(x, na.rm = TRUE)
view.empty <- function(x) {
  data.frame(
    Variable.Names = names(x),
    Percent.Empty = sapply(x, function(x)
      sum(is.na(x))) / nrow(x),
    class = unlist(lapply(x, class))
    
  ) %>% View
}
smmry <- function(x) {
  data.frame(
    Variable.Names = names(x),
    Percent.Empty = sapply(x, function(x)
      sum(is.na(x))) / nrow(x),
    Example.values = unlist(lapply(x, function(x)
      paste0(unique(x)[1:5], collapse = "; "))),
    Class = unlist(lapply(x, class))
    
  )
}
# delete column in NA is above ___ % of threshold
delete.col.na <- function(df_, c_) {
  a_ <- apply(df_, 2, function(x)
    sum(is.na(x)))
  b_ <- apply(df_, 2, length)
  df_[, which(a_ / b_ >= c_)] <- NULL
  df_
}

# smart merge:
smerge <- function(x_, y_, by_) {
  before <- nrow(x_)
  x_ <- merge(x_,
              y_,
              by = by_,
              all.x = TRUE,
              allow.cartesian = FALSE)
  stopifnot(before == nrow(x_))
  x_
}
# save all scatterplot matrix individual plots:
allpairs <- function(df_) {
  graphics.off()
  par("mar")
  par(mar = c(1, 1, 1, 1))
  blob <- (select_if(df_, is.numeric)) %>% as.data.frame()
  names(blob) <- names(blob) %>% make.names
  for (i in names(blob)) {
    for (j in names(blob)) {
      closeAllConnections()
      png(paste(j, "--", i, ".png"))
      try(plot(blob[, j] ~ blob[, i],
               xlab = i,
               ylab = j))
      dev.off()
    }
  }
}

waitifnot <- function(cond) {
  if (!cond) {
    msg <- paste(deparse(substitute(cond)), "is not TRUE")
    if (interactive()) {
      message(msg)
      while (TRUE) {
        
      }
    } else {
      stop(msg)
    }
  }
}


my_custom_theme <- list(
  theme(
    plot.subtitle = element_text(margin = margin(0, 0, 10, 0)),
    legend.background = element_blank(),
    legend.position = "top",
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.title = element_blank(),
    legend.text = element_text(
      size = 12,
      color = "black"
    )
  )
)


color_ordered <-
  c(
    "#000000",
    "#f82387",
    "#53c24c",
    "#a464e0",
    "#99bb2b",
    "#5753bf",
    "#85c057",
    "#c93da3",
    "#50942f",
    "#e073d3",
    "#49c380",
    "#9646a7",
    "#b3b23d",
    "#667ce9",
    "#daae40",
    "#655ea2",
    "#de8c31",
    "#618dcf",
    "#e4662c",
    "#4cb5dd",
    "#c53822",
    "#5eccb7",
    "#e03d47",
    "#3da08b",
    "#db3963",
    "#35772f",
    "#e1478c",
    "#5da266",
    "#ad3a76",
    "#9ac074",
    "#b78fdd",
    "#7d8220",
    "#cf87b9",
    "#52701e",
    "#e5769b",
    "#307646",
    "#be4b49",
    "#24765a",
    "#eb8261",
    "#536c31",
    "#924f7a",
    "#8b9751",
    "#a4485b",
    "#c2b572",
    "#a75521",
    "#636527",
    "#e28889",
    "#a6812a",
    "#a05940",
    "#d79e6c",
    "#84662e"
  )


scale_color_custom <- 
  list(
    scale_color_manual(values = color_ordered),
    scale_fill_manual(values = color_ordered)
  )

scatter_matrix <- function(df_, excluded_vectors_name = c("")) {
  closeAllConnections()
  df_ <- as.data.frame(df_)
  rs <- NULL
  rs <- expand.grid(names(df_), names(df_))
  rs <-
    setDT(rs)[!(Var1 %in% excluded_vectors_name) &
                !(Var2 %in% excluded_vectors_name), ]
  rs <- rs[Var1 != Var2, ]
  setDF(rs)
  for (row_ in 1:nrow(rs)) {
    first_variable <- as.character(rs[row_, "Var1"])
    second_variable <- as.character(rs[row_, "Var2"])
    class.x <- class(df_[, first_variable])
    class.y <- class(df_[, second_variable])
    if (((is.factor(class.x) | is.character(class.x)) &
         (is.numeric(class.y))) |
        ((is.factor(class.y) | is.character(class.y)) &
         (is.numeric(class.x)))) {
      p <-
        ggplot(df_, aes(x = eval(as.name(
          as.character(first_variable)
        )),
        y = eval(as.name(
          as.character(second_variable)
        )))) +
        geom_boxplot() +
        labs(x = first_variable, y = second_variable)
    } else {
      p <-
        ggplot(df_, aes(x = eval(as.name(
          as.character(first_variable)
        )),
        y = eval(as.name(
          as.character(second_variable)
        )))) +
        geom_jitter(shape = 1) +
        labs(x = first_variable, y = second_variable)
    }
    namesave <-
      paste0(first_variable, "-X_vs_", second_variable, "-Y.png")
    ggsave(namesave, p, dpi = 60)
  }
  
  
  for (var_ in unique(rs$Var1)) {
    class.x <- class(df_[, var_])
    if (is.numeric(class.x)) {
      h1 <- ggplot(df_, aes(eval(as.name(var_)))) +
        geom_histogram(bins = 50) +
        labs(x = var_)
    }
    ggsave(paste0(var_, "_hist.png"), h1, dpi = 60)
  }
}


string2oxford <- function(string.vector) {
  string.vector <- na.omit(string.vector)
  string.vector <- as.character(string.vector)
  
  # idea is to get the last element of a vector, separate it out
  # put the other vectors in a comma list, but the splice on the last vector
  # at the end. This is mostly for cosmetic stuff.
  last_one <- string.vector[length(string.vector)]
  string.vector <- string.vector[1:(length(string.vector) - 1)]
  
  first_part <- paste(string.vector, collapse = ", ")
  last_part <- paste0(", and ", last_one)
  
  output <- paste0(first_part, last_part)
  
  output
}

# get comments from all R files in the current directory
get_comments <- function(dir_ = getwd()) {
  setwd(dir_)
  
  # create a folder
  dir.create("txt_temp")
  
  # identify the folders
  current.folder <- ""
  new.folder <- "txt_temp"
  
  # find the files that you want
  list.of.files <- list.files(pattern = "*\\.R$")
  
  # copy the files to the new folder
  file.copy(list.of.files, new.folder)
  
  # get list of .R files in current directory
  setwd(new.folder)
  
  # specify new name for each file
  new_file_names <-
    sub(pattern = "\\.R$",
        replacement = ".txt",
        x = list.of.files)
  
  # rename files
  file.rename(from = list.of.files, to = new_file_names)
  
  # read files:
  rfiles <- lapply(new_file_names, read.delim)
  names(rfiles) <- new_file_names
  
  # Loop through all the files available.
  
  # Create a new list for this loop.
  
  file.comment.list <- list()
  
  for (filename_ in new_file_names) {
    dft <- NULL
    dft <- rfiles[[filename_]]
    names(dft) <- "code_"
    
    # Make a variable describing whether the row is a comment.
    dft$is.comment <- dft$code_ %>% grepl("#", .)
    
    # Filter so that we only have comments.
    dft <- dft[dft$is.comment == TRUE,]
    
    # Delete the hashtag and double spaces from the file. 
    dft$code_ <-
      dft$code_ %>% gsub("#", "", .) %>% gsub("  ", " ", .) %>% gsub("  ", " ", .) %>% 
      gsub("  ", " ", .) %>% gsub("  ", " ", .) %>% gsub("  ", " ", .) %>%
      gsub("  ", " ", .) %>% gsub("  ", " ", .) %>% gsub("  ", " ", .)
    
    # Make a row in the top row indicating which file this is the comments for.
    dft <-
      append(data.frame(
        code_ =
          c(
            "=========================================",
            paste0(
              "These are all of the comments from the ",
              filename_,
              " file."
            ),
            "========================================="
          )
      ),
      dft$code_)
    
    file.comment.list[[filename_]] <- dft %>% unlist()
  }
  
  # Write the txt file.
  setwd(dir_)
  fileConn <- file("code_comments.txt")
  writeLines(file.comment.list %>% unlist, fileConn)
  close(fileConn)
  
  # Delete the folder txt_temp
  unlink("txt_temp", recursive = T)
}

SameElements <- function(a, b) return(identical(sort(a), sort(b)))

dfdt <- function(x) as.data.table(as.data.frame(x))

# insert line breaks into paragraph for commenting
con <- function(string_) {cat(strwrap(string_, 60),sep="\n")}

# import all sheets into list from excel
read.xl.sheets <- function(Test_Cases,...) {
  names.init<-excel_sheets(Test_Cases)
  test.ex<-list()
  counter<-1
  for (val in names.init) {
    test.ex[[counter]]<-as.data.frame(read_excel(Test_Cases,sheet=val,...))
    counter<-counter+1
  }
  names(test.ex)<-names.init
  test.ex <- lapply(test.ex, as.data.table)
  test.ex
}

# clean names
name.df <- function(df){names(df) <- tolower(make.names(names(df))) %>% 
  gsub("..",".",.,fixed=TRUE) %>% gsub("[.]$","",.); df}

# country code interactive
code2name <- function(x,...) {countrycode(x,"iso3c","country.name",...)}
name2code <- function(x,...) {countrycode(x,"country.name","iso3c",...)}

# automatically convert columns to numeric:
auto_num <- function(df_, sample_=10 , cutoff_ = 7){
  to_numeric <- sample_n(df_, sample_, replace = T) %>%
    lapply(function(x)
      sum(as.numeric(grepl("[0-9]", x))) >= cutoff_)
  to_numeric <- unlist(names(to_numeric)[to_numeric == TRUE])
  setDT(df_)[, (to_numeric) := lapply(.SD, as.numeric), .SDcols = to_numeric]
  df_
}

# create a function that counts number of NA elements within a row
fun4 <- function(indt) indt[, num_missing_obs := Reduce("+", lapply(.SD, is.na))]  

lm.ass <- function(fit, dat) {
  try(plot(fit))
  # Bonferonni p-value for most extreme obs
  try(print(outlierTest(fit)))
  # leverage plots
  try(leveragePlots(fit))
  # added variable plots (same as leverage plots)
  try(avPlots(fit))
  # Influential Observations
  # Cook's D plot
  # identify D values > 4/(n-k-1)
  try(cutoff <- 4 / ((nrow(dat) - length(fit$coefficients) - 2)))
  try(plot(fit, which = 4, cook.levels = cutoff))
  # Influence Plot
  try(  influencePlot(fit,
                      id.method = "identify",
                      main = "Influence Plot",
                      sub = "Circle size is proportial to Cook's Distance"))
  # Normality of Residuals
  # qq plot for studentized resid
  try(qqPlot(residuals(fit), main = "QQ Plot"))
  try(qqPlot(residuals(fit, type="deviance"), main = "QQ Plot"))
  # distribution of studentized residuals
  try(library(MASS))
  try(sresid <- studres(fit))
  try(hist(sresid,
           freq = FALSE,
           main = "Distribution of Studentized Residuals",
           breaks = 100))
  try(xfit <- seq(min(sresid), max(sresid), length = 40))
  try(yfit <- dnorm(xfit))
  try(lines(xfit, yfit))
  # Evaluate homoscedasticity
  # non-constant error variance test
  try(ncvTest(fit))
  # plot studentized residuals vs. fitted values
  try(spreadLevelPlot(fit))
  # Evaluate Collinearity
  try(print(car::vif(fit, singular.ok = TRUE)))
  try(print(sqrt(vif(fit)) > 2))
  # Evaluate Nonlinearity
  # component + residual plot
  try(crPlots(fit))
  # Ceres plots
  try(ceresPlots(fit))
  # Test for Autocorrelated Errors
  try(print(durbinWatsonTest(fit)))
  # Global test of model assumptions
  library(gvlma)
  try(gvmodel <- gvlma(fit))
  try(summary(gvmodel)
  )
  print("the AIC is")
  try(AIC(fit))
  closeAllConnections()
}

nonempty_len <- function(x) {length(unique(na.omit(x)))}

check_dup_id <-
  function(df, id.vars, na.rm = FALSE) {
    require(dplyr)
    df <- as.data.frame(df)
    if (nonempty_len(id.vars) > 1 & na.rm == FALSE) {
      waitifnot(nrow(distinct(df[, id.vars])) == nrow(df[, id.vars]))
    } else if (nonempty_len(id.vars) > 1 & na.rm == TRUE) {
      waitifnot(nrow(distinct(na.omit(df[, id.vars]))) ==
                  nrow(na.omit(df[, id.vars])))
    } else if (nonempty_len(id.vars) == 1 & na.rm == FALSE) {
      waitifnot(length(unique(df[, id.vars])) == length(df[, id.vars]))
    } else if (nonempty_len(id.vars) == 1 & na.rm == TRUE) {
      waitifnot(length(na.omit(unique(df[, id.vars]))) == 
                  length(na.omit(df[, id.vars])))
    } else if (nonempty_len(id.vars) <= 0) {
      cat('id.vars must exist')
    }
  }

plot_grouped_boxplot <-
  function(df, x, y, xlab, ylab) {
    require(ggplot2)
    df <- as.data.frame(df)
    df[, c(x, y)] %>%
      na.omit %>%
      ggplot(aes(y = eval(as.name(y)),
                 x = eval(as.name(x)))) +
      geom_boxplot(shape = 1, alpha = 0.1) +
      labs(y = ylab, x = xlab)
  }

tovariable_name <- function(x) {
  require(dplyr)
  # converts a sentence variable to reasonable-sounding name:
  # (used for CHAT project)
  if(length(grep(" ", x))>0) {
    x %>%
      # split by the spaces (later will merge these with underscores)
      strsplit(., split = " ") %>%
      unlist() %>%
      # remove consonants except at the beginning of string:
      gsub("(?<=\\S)[aeiou]",
           "",
           .,
           perl = TRUE,
           ignore.case = TRUE) %>%
      paste(collapse = "_") %>%
      tolower() %>%
      gsub("\\-", "_", .) %>%
      # remove parentheses
      gsub("\\(|\\)", "", .) %>% 
      make.names() %>% 
      return()
  } else {
    return(x)
  }
  
}

# weighted standard deviation function:
w.sd <- function(x, wt) {sqrt(Hmisc::wtd.var(x, wt))}

# weighted mean function (with NA.RM):
weighted_mean <-  function(x, w, ..., na.rm = FALSE) {
  if (na.rm == TRUE) {
    x1 = x[!is.na(x) & !is.na(w)]
    w = w[!is.na(x) & !is.na(w)]
    x = x1
  }
  weighted.mean(x, w, ..., na.rm = FALSE)
}

# wait for X seconds
pauseit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}

# tons of beeps
rep_beep <- function() {
  require(beepr)
  for (i in 1:10000) {
    beep()
    pauseit(runif(1, 0, max = 0.8))
  }
}

# Capitalize the first word, but if it's all caps, leave it be:
custom_title <- function(x) {
  if (toupper(x) == x ) {x
  } else {
    str_to_title(x)
  }
}

# format a number with a comma (good for exporting to latex)
num_format_comma <- function(num) {
  format(round(as.numeric(num), 1), nsmall=1, big.mark=",")
}


# alert that R code is finished running:
alert_email_george <- function(email = "gyang@cgdev.org") {
  require(RDCOMClient)
  ## init com api
  OutApp <- COMCreate("Outlook.Application")
  ## create an email
  outMail = OutApp$CreateItem(0)
  ## configure  email parameter
  outMail[["To"]] = email
  outMail[["subject"]] = paste0("R Code Finished Running ", Sys.time())
  outMail[["body"]] = 
    paste0(
      "It is finished."
    )
  
  ## send it
  outMail$Send()
  closeAllConnections()
  
}

# define a function for getting percentiles of a group using empirical CDF: <--
# ok, literally is an inefficient way to call quantile(vector, probs = value)
per_cdf <- function(value, vector) {
  empirical_cdf <- ecdf(vector)
  return(empirical_cdf(value))
}


# get the number of decimal places after a number
decimalplaces <- function(x) {
  if ((x %% 1) != 0) {
    strs <- strsplit(as.character(format(x, scientific = F)), "\\.")
    n <- nchar(strs[[1]][2])
  } else {
    n <- 0
  }
  return(n) 
}

# function that formats a number with spaces for thousands place.
format_num_with_marker <- function(x, decimals_ = 0, market = " ") {
  return(format(round(as.numeric(x), 1), nsmall=decimals_, big.mark=" "))
}

# this function converts numbers to characters in either scientific notation or
# rounded. used for custom formatting of tables.
gen_format_num <- function(x, sig_figs = 3) {
  # options(scipen = 999)
  x <- signif(x, sig_figs)
  if (x < 10) {
    # if number is less than 10, report 3 decimals
    x <- round(x, 3)
  } else if (x < 100) {
    # if number is less than 100, report 2 decimal
    x <- round(x, 2)
  } else if (x < 1000) {
    # if number is less than 1000, report 1 decimal
    x <- round(x, 1)
  } else if (x > 1000) {
    # if number is greater than 1000, report 0 decimals
    x <- round(x, 0)
  }
  
  # turn x into characters, by either adding a comma, or reporting to 
  # scientific notation, or if it's smaller than 1000, then just turning 
  # it into a plain character:
  if (x > 1000 & x <= 100000){
    x <- format_num_with_marker(x)
  } else if (x > 100000) {
    # if number is greater than 10,000, report using scientific notation
    x <- formatC(x, format = "e", digits = 2)
  }
  x <- as.character(x)
  # options(scipen = 0, digits = 7)
  return(x)
}

vec_gen_format_num <- function(x) {
  return(unlist(lapply(x, gen_format_num)))
}


# get the packages used for these files: ===========
# file names:

get_used_packages <- function(directory) {
  setwd(directory)
  
  file_names <-
    list.files(
      path = ".",
      pattern = NULL,
      all.files = FALSE,
      full.names = FALSE,
      recursive = FALSE,
      ignore.case = FALSE,
      include.dirs = FALSE,
      no.. = FALSE
    )
  
  R_file_names <- grep(".R", file_names, value = T)
  
  list_packages <-
    lapply(R_file_names, NCmisc::list.functions.in.file)
  
  return(list_packages)
}

# rename coefficients from a linear regression:
coef_rename <-
  function(fit_, old_name, new_name) {
    # first rename the coefficients attribute
    coef_names <- names(fit_$coefficients)
    names(fit_$coefficients)[coef_names == old_name] <- new_name
    
    # then, rename the coefficient table
    if (class(fit_) == "fixest"){
      coef_names <- rownames(fit_$coeftable)
      rownames(fit_$coeftable)[coef_names == old_name] <- new_name
    }
    
    return(fit_)
  }


# this function uses method of moments to get the initial 
# parameters of the beta distribution
beta_mom <- function(x) {
  x[x==0] <- 0.0001
  m_x <- mean(x, na.rm = TRUE)
  s_x <- sd(x, na.rm = TRUE)
  
  alpha <- m_x * ((m_x * (1 - m_x) / s_x ^ 2) - 1)
  beta <- (1 - m_x) * ((m_x * (1 - m_x) / s_x ^ 2) - 1)
  
  return(list(alpha = alpha, beta = beta))
}
