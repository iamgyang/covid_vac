# Data ------------------------------------------------------------
# Countries that Comin considers "Western" ----------------------

# Austria, Belgium, Denmark, Finland, France, Germany, Italy,
# Netherlands, Norway, Sweden, Switzerland, Untied Kingdom,
# Japan, Australia, New Zealand, Canada, and the United
# States


# CHAT Comin Technology Dataset --------------------------------
chat <- rio::import("chat.dta") %>% as.data.table()
tonum <- names(chat) %>% setdiff(., c("country_name"))
chat[,(tonum):=lapply(.SD, as.numeric), .SDcols = tonum]
chat <- chat[!country_name %in% c("North Vietnam", "Indochina"), ]
chat <- fun4(as.data.table(chat))
chat <- chat[num_missing_obs!=max(chat$num_missing_obs,na.rm=TRUE)]

# Maddison (2020) ---------------------------------------------------

# Maddison data on historical population trends and GDP per capita
mad <- rio::import("mpd2020.dta")
mad <- mad %>% rename(date = year, iso3c = countrycode) %>% as.data.table()
mad <- mad[date!=1,]

# Real GDP PPP (WDI) -------------------------------------------------

unique_country_codes <- na.omit(unique(countrycode::codelist$iso3c))

wdi_gdppc <- 
  WDI(
    indicator = c('wdi_gdppc' = 'NY.GDP.PCAP.PP.KD',
                  'deflator' = 'NY.GDP.DEFL.ZS'),
    start = 1960,
    end = lubridate::year(Sys.Date()),
    country = unique_country_codes
  )
wdi_gdppc <- wdi_gdppc %>% as.data.table()
wdi_gdppc[,iso3c:=countrycode(iso2c, "iso2c", "iso3c")]
wdi_gdppc <- wdi_gdppc[!is.na(iso3c)]
wdi_gdppc[,`:=`(iso2c = NULL, country = NULL)]
wdi_gdppc[,keeprow:=apply(.SD, 1, function(x) length(na.omit(x)))]
wdi_gdppc <- wdi_gdppc[keeprow>2,] %>% as.data.table()
wdi_gdppc[,keeprow:=NULL]
wdi_gdppc <- wdi_gdppc %>% rename(date = year) %>% dfdt
wdi_deflator <- wdi_gdppc[,.(date, iso3c, deflator)]
wdi_gdppc <- wdi_gdppc[,.(date, iso3c, wdi_gdppc)]

# restrict the deflator to be only the US (because converting 
# international currency)
wdi_deflator <- distinct(wdi_deflator[iso3c=='USA', .(date, deflator)])

# get the base year deflator for 2011:
def_yr_mad <- as.vector(wdi_deflator[date==2011]$deflator)
def_yr_wdi <- as.vector(wdi_deflator[date==2017]$deflator)
def_factor <- def_yr_mad / def_yr_wdi

# merge WDI and Maddison GDP PPP estimates -------------------------

mad <- merge(mad, wdi_gdppc, by = c('date','iso3c'), all = TRUE)
# confirm we have unique iso3c dates
waitifnot(nrow(distinct(mad[,.(date, iso3c)]))==nrow(mad[,.(date, iso3c)]))

# multiply WB GDPpc (2017) by the deflator for 2011 / deflator for 2017:
mad[,wdi_gdppc:=wdi_gdppc * def_factor]


# get 2019 and 2020 GDP figures by using growth from WDI figures to 
# project forwards the Maddison GDP figures.
mad <- mad[order(iso3c,date)]
mad[,wdi_gdppc_gr:=wdi_gdppc/shift(wdi_gdppc), by = 'iso3c']

for (i in seq(2015, 2021)) {
  mad[, gdppc.n := shift(gdppc) * wdi_gdppc_gr, by = 'iso3c']
  mad[date == i & is.na(gdppc), gdppc := gdppc.n]
  mad <- as.data.table(mad)
}

ggplot(mad, aes(x = log(wdi_gdppc), y = log(gdppc))) + geom_point()
mad[,(c('country','wdi_gdppc','wdi_gdppc_gr','gdppc.n')):=NULL]
mad[,pop:=pop*1000]
waitifnot(mad[date==2016 & iso3c == "USA"]$pop<=(400*(10^6)))
waitifnot(mad[date==2016 & iso3c == "USA"]$pop>=300*(10^6))
mad <- mad[order(iso3c, date)]

# Steel Production (World Steel) ------------------------------------------

# Here, I get the PDFs of the World Steel Association and the read in the 
# tables from ~1980-2020 for "Apparent Crude Steel Consumption." I first have
# a vector of all the pdf URLs. I create the vector using the "glue package".
# Then, I have to do some regex searching for the specific page of the PDF
# (searching for  terms like "Apparent", but not "Annex", since I don't want
# the appendix.)  I then do a lot of data cleaning of the table itself. The
# reason we want apparent crude steel consumption is because it's actually
# describing the *demand* for steel within a country.

# I did manually clean 1978 crude steel production just because it was such a hot mess:
manual_prod_1978 <- fread("1977_manual_crude_steel_production.csv", header = T)

url_tail <- 
  c("051f894c-fab0-40bd-bacd-2c089438f409/Steel%2520statistical%2520yearbook%25201999.pdf",
    "065a2dd1-02f0-4214-95c4-78f6a33f4761/Steel%2520statistical%2520yearbook%25201998.pdf",
    "08b20e40-78a2-4971-bcb2-7a99ee2c7b99/Steel%2520statistical%2520yearbook%25202001.pdf",
    "09ed87af-366e-44c6-9f55-63c09f76b520/A%2520handbook%2520of%2520world%2520steel%2520statistics%25201978.pdf",
    "1044cace-dd58-4bf6-a59a-139249fd5170/Steel%2520statistical%2520yearbook%25202008.pdf",
    "18a77bd2-7c38-4fc5-8aef-963553c3cb0f/Steel%2520statistical%2520yearbook%25202000.pdf",
    "1ef195b3-1a46-41c2-b88b-6072c2687850/Steel%2520statistical%2520yearbook%25202010.pdf",
    "25fb9686-2c56-47da-9cdd-4dac03f02d90/Steel%2520statistical%2520yearbook%25201985.pdf",
    "27b40e2e-a455-4f84-bf71-6afe8d7d9933/Steel%2520statistical%2520yearbook%25202005.pdf",
    "27e1308f-69a9-4a4e-b1f4-223a788aab0b/Steel%2520statistical%2520yearbook%25201982.pdf",
    "302c064f-fde1-4fff-9ba8-7ae2c3f88f94/Steel%2520statistical%2520yearbook%25202004.pdf",
    "373b3927-26ff-4591-8e70-11347b08f46c/Steel%2520statistical%2520yearbook%25201989.pdf",
    "37ad1117-fefc-4df3-b84f-6295478ae460/Steel%2520Statistical%2520Yearbook%25202016.pdf",
    "3c3401a5-df5e-46bf-9a25-ab2040cd3c9c/Steel%2520statistical%2520yearbook%25201986.pdf",
    "3e275c73-6f11-4e7f-a5d8-23d9bc5c508f/Steel%2520Statistical%2520Yearbook%25202017_updated%2520version090518.pdf",
    "3e501c1b-6bf1-4b31-8503-a2e52431e0bf/Steel%2520Statistical%2520Yearbook%25202015%2520r3.pdf",
    "4164bf8d-9b5f-4f04-b785-a6c4f6c6a534/Steel%2520statistical%2520yearbook%25201993.pdf",
    "5001dac8-0083-46f3-aadd-35aa357acbcc/Steel%2520Statistical%2520Yearbook%25202020%2520%2528concise%2520version%2529.pdf",
    "5901f746-ec8f-45a9-9e03-62f0e01b5d5f/Steel%2520statistical%2520yearbook%25201984.pdf",
    "5a3cd3bc-79f9-44e5-ac54-ed231832cb21/Steel%2520statistical%2520yearbook%25202007.pdf",
    "63e65c0a-4634-47df-a26a-922299d69084/Steel%2520statistical%2520yearbook%25202002.pdf",
    "6640a8bf-6b7d-41e2-bf79-83e290b5fbdf/Steel%2520statistical%2520yearbook%25201980.pdf",
    "6fec21af-3ca4-43a2-bbbf-d46bc25cbdf3/Steel%2520statistical%2520yearbook%25201992.pdf",
    "706777d8-1453-406b-aa75-1128abf14d1c/Steel%2520statistical%2520yearbook%25201990.pdf",
    "7aa2a95d-448d-4c56-b62b-b2457f067cd9/SSY19%2520concise%2520version.pdf",
    "7b1e1280-1b11-4faf-83e3-31a307a307e3/Steel%2520statistical%2520yearbook%25201981.pdf",
    "7bb9ac20-009d-4c42-96b6-87e2904a721c/Steel-Statistical-Yearbook-2013.pdf",
    "7cd3de6e-ff49-4731-acc0-01bb9e8e3773/Steel%2520statistical%2520yearbook%25201991.pdf",
    "81073f61-10b6-464f-83cd-1d4cb740a9d0/Steel%2520statistical%2520yearbook%25201996.pdf",
    "818a3c9e-325a-472b-b9da-2889e38e2cad/Steel%2520statistical%2520yearbook%25202009.pdf",
    "8934c308-45c5-48d9-ad21-109fbc1c717b/Steel%2520statistical%2520yearbook%25201983.pdf",
    "9e6e990a-939c-4967-8db0-d86bfd84cad4/Steel%2520statistical%2520yearbook%25201995.pdf",
    "a0d5110b-80e1-4f1d-a6f0-a9054c07b672/Steel%2520Statistical%2520Yearbook%25202012.pdf",
    "c0151c3f-9fa4-41cb-bc69-514b3af8c557/Steel%2520statistical%2520yearbook%25201997.pdf",
    "c0f3e326-0b3d-4d91-adca-096502aae51e/Steel%2520statistical%2520yearbook%25201987.pdf",
    "c12843e8-49c3-40f1-92f1-9665dc3f7a35/Steel%2520statistical%2520yearbook%25202011.pdf",
    "c7c68146-a56c-4f33-bac9-68737f6c2242/Steel%2520statistical%2520yearbook%25202003.pdf",
    "c8b1c111-ce9b-4687-853a-647ddbf8d2ec/Steel-Statistical-Yearbook-2014.pdf",
    "ca9a6c9b-dbaa-4207-bcf2-1b187070b5ae/Steel%2520statistical%2520yearbook%25201988.pdf",
    "e20c1da7-ed4f-4429-8195-ade85c39e38a/Steel%2520statistical%2520yearbook%25202006.pdf",
    "e5a8eda5-4b46-4892-856b-00908b5ab492/SSY_2018.pdf",
    "eb8daa65-d700-4116-85e1-b5becc18437e/Steel%2520statistical%2520yearbook%25201994.pdf")

# get a list of the urls:
url_head <- "https://www.worldsteel.org/en/dam/jcr:{url_tail}"
urls <- glue(url_head)
pdf_names <- paste0("steel_",substr(url_tail, nchar(url_tail)-7, nchar(url_tail)-4),".pdf")
pdf_names <- 
  pdf_names %>% 
  gsub("0518","2017",.) %>% 
  gsub("20r3","2015",.) %>% 
  gsub("2529","2020",.) %>% 
  gsub("sion","2019",.)

# if 2010 and 2018 PDFs are not in the input directory, then actually download
# the pdfs from the urls:
if(!any(dir()%in%c("steel_2010.pdf", "steel_2017.pdf"))){
  walk2(urls, pdf_names, download.file, mode = "wb")}

# get the pdf text from the downloaded pdfs:
raww <- map(pdf_names, pdf_text)

# this outputs a list; name the list the years of the pdf reports:
names(raww) <- pdf_names %>% 
  gsub("steel_", "",.,fixed=T) %>% 
  gsub(".pdf","",.,fixed=T) %>% 
  as.character()

# if all of the values within the pdf are empty strings, then delete that part
# from the list
to_remove <- raww %>% lapply(., function(x)
  all(x == "")
) %>% unlist()

raww <- raww[!to_remove]
raww <- raww[lapply(raww,length)>0]

# Now we will loop through the entire contents of each PDF (each element of the list):
# !!!!!!!the beginning of our giant loop!!!!!!!

scrape_table <- function(DF_, SEARCH_STRING_INPUT, OMIT_STRING_INPUT) {
  for (i_ in sort(names(DF_))) {
    
    table <- DF_[[i_]]
    
    # display what year PDF I'm at:
    cat(i_,sep = "\n")
    
    # if the number of characters within the pdf is less than 500, then delete
    # that pdf from our list: indicates that we won't find anything in the pdf itself.
    if(sum(nchar(table)) <= 500) {
      DF_[[i_]] <- NULL
      next}
    
    # Find the locations where there is use of these words in "search", but not
    # use of these words in "omit". "search" words indicate that we have a table
    # that is about  apparent crude steel consumption or use, while omitted words
    # indicate that we have something in the appendix, something that is about
    # "finished steel use" as opposed to crude steel, or something that is in the
    # table of contents.
    search <- Reduce(intersect,
                     map(SEARCH_STRING_INPUT,
                         function(x) grep(x, table, ignore.case = T)))
    omit <- Reduce(union, map(OMIT_STRING_INPUT,
                              function(x) grep(x, table, ignore.case = T)))
    table_index <- setdiff(search, omit)
    table <- table[table_index]
    
    # Split up the PDF into many character strings. "\n" indicates that we
    # are going to the next line. 
    table <- table %>% str_split(., "\n") %>% unlist
    
    # replace any space bigger than 2 spaces with '|' symbol, later used
    # as a delimiter for the read.csv function
    table <- gsub("\\s{2,}", "|", table)
    
    # Only keep the rows in the PDF where we have 5 or more numbers. 
    # This will mean that we're in the meat of the table.
    indices_with_data <- 
      which(unlist(lapply(gregexpr("[0-9]",table), length))>4)
    table <- table[indices_with_data]
    
    # The "start" of the table is where there are YEARS, so we search for
    # locations where there is "19(80-99)" or "20(01-20)", and delete the
    # part BEFORE that line (which will likely say something like, "Table
    # 5 Apparent steel consumption")
    index_start <- 
      which(unlist(lapply(gregexpr("19|20",table), length))>4)[1]
    
    if (is.na(index_start) | is.na(length(table))) {
      DF_[[i_]] <- NULL
      next
    }
    table <- table[index_start:length(table)]
    
    # read the table in.
    text_con <- textConnection(table)
    DT <- read.csv(text_con, sep = "|")
    DT <- as.data.table(DT)
    
    # The next problem is that we have a data table that looks like this:   
    #         2011           2012          2013  2014
    # France  2 000 3 000    9 560 8 648    ""    ""
    # Uruguay 2,000 3,000    9,560 8,648    ""    ""
    # Germany 2000           3000          9560  8648
    # We want France and Uruguay to look like Germany, but it's got these
    # spaces in between the thousands sign and also has empty values for 2013 and
    # 2014. 
    
    
    # So, first, make these missing observations equal NA
    f_dowle2 <- function(DT) {
      for (i in names(DT))
        DT[(get(i)) == "", (i) := NA]
      DT
    }
    DT <- f_dowle2(DT) %>% as.data.table()
    
    # Now, we replace commas with spaces:
    yam <- names(DT) %>% copy
    
    DT[, (yam) := lapply(.SD,
                         function(x) gsub(",", " ", x)), .SDcols = yam]
    
    # This function counts the number of NAs within a row:
    fun4(DT)
    if (max(DT$num_missing_obs) > 0) {
      DT <- DT[num_missing_obs != max(DT$num_missing_obs)]
    }
    
    # Fix some of the country name labels and do some extra cleaning:
    DT <- distinct(DT)
    DT$X <- DT$X %>% 
      gsub("\\s*\\([^\\)]+\\)","",.) %>% 
      gsub(" ","_",.,fixed=T)
    DT <- DT[X!="World"]
    
    # Separate our data frame into two: one where there is missing data, and one
    # where there is NOT missing data.
    
    miss_data <- DT[num_missing_obs>0,] %>% as.data.table()
    full_data <- DT[num_missing_obs==0,] %>% as.data.table()
    
    # Now, IF we have some rows of the table where there are some missing values,
    # we have to clean them. 
    if (nrow(miss_data)>1){
      miss_data[,num_missing_obs:=NULL]
      names_of_data <- names(miss_data) %>% copy
      
      # Function to split each cell into a vector of numbers: Since
      # we have spaces in between the thousands place, we look for
      # the even occurences of the space and split each cell across
      # those locations.
      split_cell <- function(vector_) {
        even <-
          as.vector(unlist(gregexpr(" ", vector_)))[
            (1:length(unlist(gregexpr(" ", vector_)))) %%2 == 0]                
        vector_ <- substring(vector_, c(0, even), c(even, nchar(vector_)))
        vector_ <- vector_ %>% gsub(" ", "", .)
        vector_
      }
      
      # Applying this function on each row:
      split_row <-
        function(row_) {
          lapply(row_, split_cell) %>% unlist
        }
      
      # Using this function to apply this on each row of the data.table:
      miss_data <- miss_data[, na.omit(split_row(.SD)), by = seq_len(nrow(miss_data))]
      miss_data[, varname := rep(names_of_data, length.out = .N),by = seq_len]
      
      # remove observations with duplicated keys  prior to
      # pivoting from long to wide:
      miss_data[,dup:=as.numeric(duplicated(miss_data[,.(seq_len, varname)]))]
      miss_data[,dup:=max(dup), by = seq_len]
      miss_data <- miss_data[dup==0][,dup:=NULL]
      miss_data <- miss_data %>% spread(.,varname, V1) %>% as.data.table()
      miss_data[,seq_len:=NULL]
      
      # Convert columns with numbers in them to be numeric:
      miss_data <- auto_num(miss_data) %>% as.data.table()
      
      # Drop the rows where there is any NA values:
      fun4(miss_data)
      miss_data <- miss_data[num_missing_obs==0]
      
      # Done cleaning the missing data table:
    }
    
    # rowbind this data.frame to the original data.frame after converting
    # some columns to numeric values:
    tonum <- setdiff(names(full_data),"X")
    full_data[,(tonum):=lapply(.SD, function(x) 
      gsub(" ","",x) %>% as.numeric()),.SDcols = tonum]
    DT <- rbindlist(list(miss_data, full_data),fill=T) %>% as.data.table()
    
    # convert country names to iso 3 codes:
    
    # first clean names:
    DT[X=="Bol_via", X:="Bolivia"]
    DT[X=="BoliyÃ¬a", X:="Bolivia"]
    DT[X=="6uatemala", X:="Guatemala"]
    DT[X=="_Kuvait", X:="Kuwait"]
    DT[X=="_German_Dem_Rep", X:="East Germany"]
    DT[X=="_9atar", X:="Qatar"]
    DT[X=="South_Korea", X:="South Korea"]
    DT[X=="Republic_nf_Korea", X:="South Korea"]
    DT[X=="Republic_of_Korea", X:="South Korea"]
    DT[X=="Spa_Ã¯_n", X:="Spain"]
    DT[X=="Spa_io", X:="Spain"]
    DT[X=="Taian", X:="Taiwan"]
    DT[X=="Zimbebe", X:="Zimbabwe"]
    DT[X=="Cn)ombia",X:="Colombia"]
    DT[X=="F_nl_and",X:="Finland"]
    DT[X=="German_Dei_Rep", X:="East Germany"]
    DT[X=="6erman_Dem_Rep", X:="East Germany"]
    DT[X=="Czechoslovakia",X:="Czechia"]
    DT[X=="_Czechoslovakia",X:="Czechia"]
    DT[X=="_Ronania", X:="Romania"]
    DT[X=="Austral_a", X:="Australia"]
    DT[X=="_Czechoslovakia", X:="Czechia"]
    DT[X=="Fio_land", X:="Finland"]
    DT[X=="East Germany", X:="Germany"]
    DT[X=="Ita_y", X:="Italy"]
    DT[X=="Lx_embo_org", X:="Luxembourg"]
    DT[X=="_Ronania", X:="Romania"]
    DT[X=="_Republic_of_Korea", X:="South Korea"]
    DT[X=="Spa_n", X:="Spain"]
    DT[X=="Boliyìa", X:="Bolivia"]
    DT[X=="Spa_ï_n", X:="Spain"]
    
    # some countries/locations we exclude because they're *regions*:
    not_interested <-
      c("_Czechoslosakia", "_Iren", "_O.an", "_Other", "_Other_Africa",
        "_Other_Asian_CPE's", "_Total_Africa", "_Total_Asia",
        "_TOTAL_CPE'S", "_TOTAL_DEVELOPING_CTS", "_Total_Middle_East",
        "_TOTAL_WESTERN_WORLD", "_WORLD_TOTAL", "Accession_Cts.",
        "Africa", "Asia", "Baltic_States", "Belgium-Luxembourg",
        "BelgiumLuxembourg", "C.I.S.", "CIS", 
        "EC_Total", "European_Union", "F.R._Yugoslavia", "Middle_East",
        "North_America", "Oceania", "Other", "Other_Africa", "Other_Asia",
        "Other_C.I.S.", "Other_Europe", "Other_Middle_East",
        "Other_North_America", "Other_Oceania", "Other_South_America",
        "Other_Western_Europe", "Others", "Serbia_and_Montenegro",
        "South_America", "Total_Latin_America", "Total_Western_Europe",
        "Yugoslavia", "Zeubia",  "former_U.S.S.R.",
        "_TOTAL_DEVELOPING_CTS_",
        "_Total_Eestern_Europe",
        "_Total_Latin_America",
        "Central_America",
        "fusti_a",
        "Ne",
        "thuguay",
        "Total_Africa",
        "Total_Middle_East",
        "_Central_America",
        "_Tota'_Latin_A.erica",
        "Total_Asia",
        "TOTAL_DEVELOPING_CTS.")
    DT <- DT[!(X%in%not_interested),]
    
    # then make 2 attempts at converting to iso3c codes; one by using
    # the raw string; another attempt after deleting all underscores:
    DT[,iso3c.x:=name2code(X)]
    DT[,iso3c.y:=name2code(gsub("_","",X))]
    
    
    # merge these ISO codes
    DT[,iso3c:=coalesce2(iso3c.x, iso3c.y)]
    waitifnot(all(na.omit(DT$iso3c.x==DT$iso3c.y)))
    DT[,`:=`(iso3c.x = NULL,iso3c.y = NULL)]
    
    # place the cleaned table into the original list:
    DF_[[i_]] <- DT
    
    # !!!!!!!this marks the end of our giant loop!!!!!!!
  }
  DF_
}

crude_steel_demand <-
  scrape_table(
    DF_ = raww,
    SEARCH_STRING_INPUT =
      c("apparent", "consumption|use", "crude"),
    OMIT_STRING_INPUT =
      c("annex", "contents", "finished", "capita", "head")
  )

get_indices <- function(year){
  raww %>% names %>% sort %>% (function(x)
    ifelse(substr(x, 1, 2) == year,  x, NA)) %>% na.omit %>% as.vector()}

crude_steel_production_2000s <-
  scrape_table(
    #2012 has a typo in their table. BUT, since it's solidly in the middle of
    #every other year, its data is already included anyways from the other
    #PDFs (each PDF has multipl years of data), so we exclude it here.
    DF_ = raww[setdiff(get_indices("20"), "2012")], 
    SEARCH_STRING_INPUT =
      c("Total Production of Crude Steel"),
    OMIT_STRING_INPUT =
      c("annex", "contents", "finished", "capita", "head")
  )

crude_steel_production_1990s <-
  scrape_table(
    DF_ = raww[get_indices("19")],
    SEARCH_STRING_INPUT =
      c("CRUDE STEEL PRODUCTION"), # these old tables have terrible spelling
    OMIT_STRING_INPUT =
      c("annex", "contents", "finished", "capita", 
        "head", "monthly", "percentage", "process")
  )

names(manual_prod_1978) <- names(manual_prod_1978) %>% 
  gsub(".00","",.) %>% gsub("V1","X",.) %>% paste0("X", .) %>% 
  gsub("XX","X",.)

manual_prod_1978[X=="Grocce",X:="Greece"]
manual_prod_1978[X=="Czechoslovakia",X:="Czechia"]
manual_prod_1978[X=="German Dem. Rep.",X:="Germany"]
manual_prod_1978[X=="Yugoslavia",X:=NA]
manual_prod_1978[,iso3c:=name2code(X)]
manual_prod_1978 <- manual_prod_1978[!is.na(iso3c)]

crude_steel_production_1990s[["1978"]] <- manual_prod_1978 %>% as.data.table()

crude_steel_production <- c(crude_steel_production_1990s, crude_steel_production_2000s)

crude_steel_production <- crude_steel_production[lapply(crude_steel_production,length)>0]
crude_steel_demand <- crude_steel_demand[lapply(crude_steel_demand,length)>0]

# check that the number of iso codes is not duplicated for each data.frame & 
# correct some areas where there are duplicated iso3c codes:

fix_1978_germany <- function(x) {
  tosum <- setdiff(names(x), 
                   c("X","num_missing_obs","iso3c","dup")) %>% copy
  setDT(x)[, (tosum) := lapply(.SD, as.numeric), .SDcols = tosum]
  x <-
    x[, (tosum = lapply(.SD, sum)), .SDcols = tosum, by = iso3c]
  x
}

crude_steel_production$`1978` <- fix_1978_germany(crude_steel_production$`1978`)
crude_steel_production$`1985` <- fix_1978_germany(crude_steel_production$`1985`)
crude_steel_production$`1986` <- fix_1978_germany(crude_steel_production$`1986`)
crude_steel_demand$`1978` <- fix_1978_germany(crude_steel_demand$`1978`)
crude_steel_demand$`1985` <- fix_1978_germany(crude_steel_demand$`1985`)
crude_steel_demand$`1986` <- fix_1978_germany(crude_steel_demand$`1986`)

raww <- NULL


save.image("saved.results.RData")
load("saved.results.RData")
# merge both lists together:
crude_steel_demand <- crude_steel_demand %>% 
  lapply(., function(x) {x[,type:="demand"]})
crude_steel_production <- crude_steel_production %>% 
  lapply(., function(x) {x[,type:="production"]})
names(crude_steel_production) <- names(crude_steel_production) %>% paste0("prod_",.) %>% copy
names(crude_steel_demand) <- names(crude_steel_demand) %>% paste0("dem_",.) %>% copy
raww <- c(crude_steel_production, crude_steel_demand)

# find duplicated iso codes
find_dup_iso <- function(list_of_x) {
  lapply(list_of_x,
         function(x) {
           x[, dup := duplicated(iso3c)]
           dup_iso <- x[dup == TRUE, .(iso3c)] %>% unlist
           if (length(dup_iso) >= 1) {
             dup_country <- x[iso3c %in% dup_iso, .(X)] %>% unlist
             dup_country
           }
         })
}

find_dup_iso(raww)

# if there are duplicated iso codes, pause code:
check_dup_iso <- function(df_){
  waitifnot(length(df_$iso3c)==length(unique(df_$iso3c)))
}
lapply(raww, check_dup_iso)

# table that shows which pdf years have duplicated iso codes:
smry.tbl.dup <- rbindlist(lapply(raww, function(df_) {
  data.frame(
    len = length(df_$iso3c),
    uni = length(unique(df_$iso3c)),
    tf = length(df_$iso3c) == length(unique(df_$iso3c))
  )
}), idcol = "name", fill = TRUE)

smry.tbl.dup <- smry.tbl.dup[order(tf,-as.numeric(name))]

# Merge all pdf years. First pivot from wide to long, then rbind all of them.

raww <- lapply(raww,
               function(x) {
                 topivot <- names(x) %>% setdiff(., c("iso3c","X", "type"))
                 x[,(topivot):=lapply(.SD, as.numeric),.SDcols = topivot]
                 x <-
                   x %>% pivot_longer(all_of(topivot), 
                                      names_to = "year", 
                                      values_to = "steel") %>%
                   as.data.table()
               })
raww <- rbindlist(raww, fill = T, idcol = "pdf_yr")
raww <- raww[!year%in%c("num_missing_obs","dup"),]

# Convert PDF years to numeric values:

# prepare regular expression
regexp <- "[[:digit:]]+"

# process string
raww[,year:=str_extract(year, regexp) %>% as.numeric]
waitifnot(sum(is.na(raww$year))==0)

# typo w/ year
raww[year==197, year:=1977]

# Data quality check: for different PDF years, what is the variation in the
# country-year reported steel?
raww <- raww[order(iso3c, year),]

raww <- 
  raww %>%
  mutate(pdf_yr = pdf_yr %>% 
           gsub("dem_","",.) %>% 
           gsub("prod_","",.) %>% 
           as.numeric)
raww <- raww[!is.na(steel)]
s <- raww %>% as.data.table()

# Keep the most recent year of data"
raww[,maxyr:=max(pdf_yr, na.rm=T),by=c("iso3c","year", "type")]
raww <- raww[pdf_yr==maxyr,]
raww[,`:=`(pdf_yr=NULL,maxyr=NULL, X=NULL)]
raww <- raww %>%
  spread(., type, steel) %>%
  rename(steel_demand = demand,
         steel_production = production) %>% 
  as.data.table()

# Checking the mean average percent error within  the steel industry:
s[,`:=`(steel.m = mean(steel, na.rm=T), steel.sd = sd(steel, na.rm=T)), 
  by=.(iso3c, year, type)]
s[,steel.ape:=abs(steel - steel.m)/steel.m]
s[is.na(steel.sd),steel.sd:=0]

s[,pdf_yr:=as.numeric(pdf_yr)]
s[,minyr:=min(pdf_yr),by=.(iso3c, year, type)]
s[,centeryr:=pdf_yr-minyr]
ggplot(s, aes(centeryr, steel.ape, group = interaction(iso3c, year))) + 
  geom_point(alpha = 0.05)+
  theme(legend.position = "none")
s[,mape:=mean(steel.ape,na.rm=T),by=.(centeryr, type)]
weird_pattern_in_error <- 
  ggplot(distinct(s[, .(centeryr, mape, type)]), aes(centeryr, mape, color = type)) +
  geom_point() +
  theme(legend.position = "top",
        axis.title.y = element_text(angle = 360, vjust = 0.5)) +
  labs(x = "Year of pdf (minus earliest year)",
       y = "Average distance\nfrom the mean",
       subtitle =
         "") + 
  my_custom_theme + 
  scale_color_colorblind()
ggsave("weird_pattern_in_error.png", 
       weird_pattern_in_error, width = 6, height = 4, 
       limitsize = FALSE, dpi = 400)

# now, we're done cleaning
raww <- rename(raww, date = year) %>% as.data.table()

save.image("steel.production.world.steel.RData")
load("steel.production.world.steel.RData")

# Credit Cards (WB Global Payment Systems Survey (GPSS) 2004-2015) ----------
gpss1 <- rio::import("manual_clean_WB_credit_debit1.xlsx")
gpss2 <- read.xl.sheets("manual_clean_WB_credit_debit2.xlsx")
gpss3 <- read.xl.sheets("GPSSpaymentsdata2015draft.xlsx")
gpss3 <- gpss3$`C. Retail transactions` %>% as.data.table()

# at the end, I want:
# country | year | # cards | # transactions (debt/credit) | # $$ transactions

# get credit cards and transactions for pre-2010:
gpss2 <- lapply(gpss2, name.df)
numcrd <- gpss2$`number of cards` %>% as.data.table()
gpss2 <-
  rbindlist(gpss2[2:length(gpss2)], id = "indic") %>% as.data.table()
setDT(gpss2)[, date := as.numeric(substr(indic, nchar(indic) - 3, nchar(indic)))]
gpss2[, value := grepl("value", indic)]
gpss2[value == TRUE, indic := "value"]
gpss2[value == FALSE, indic := "volume"]
waitifnot(gpss2$indic %>% unique %>% length == 2)
gpss2[, `:=`(value = NULL)]
tonum <- setdiff(names(gpss2), c("indic", "country"))
gpss2[, (tonum) := lapply(.SD, as.numeric), .SDcols = tonum]
gpss2 <-
  gpss2 %>%
  gather(., "type", "amount" , cheques:credit.cards) %>%
# the data on checks is messy (sometimes includes intrabank transfers, sometimes
# does not)
  filter(type%in%c("debit.cards","credit.cards")) %>% 
  na.omit %>%
  as.data.table()
gpss2 <- gpss2 %>%
  spread(., indic, amount) %>%
  as.data.table()
gpss2[,iso3c:=countrycode(country, 
                          origin = "country.name",
                          destination = "iso3c", 
                          custom_match = c("ECCU" = NA, "BCEAO" = "SEN"))]

gpss2[,type:=type %>% gsub(".","_",., fixed=TRUE)]
gpss2[,country:=NULL]
gpss2 <- gpss2[!is.na(iso3c),]

# get credit cards and transactions for post-2010:
gpss3 <- name.df(gpss3)
gpss3 <- setDT(gpss3)[,.(iso3c = country.code, 
                         date = year, 
                         type = variable.name.see.variable.key.in.c1,
                         volume, 
                         value = value.in.usd)]
gpss3 <- gpss3[type%in%c("debit_card","credit_card"),]

# append
gpss2 <- rbindlist(list(gpss3, gpss2), fill=TRUE) %>% as.data.table()
gpss3 <- NULL
gpss2 <- gpss2[, 
               .(volume = sum(volume, na.rm = FALSE),
                 value = sum(value, na.rm = FALSE)), by = .(iso3c,date)]

# get debit and credit cards amounts for 2010-2015:
gpss1 <- name.df(gpss1) %>% as.data.frame()
gpss1 <- gpss1[,grep("cards|country",names(gpss1),value=TRUE)]
gpss1 <- gpss1 %>% 
    gather(., "indic","number",
      x2.number.of.debit.cards.in.circulation2015:
      x3.number.of.credit.cards.in.circulation2010) %>% na.omit()
setDT(gpss1)[,date:=as.numeric(substr(indic, nchar(indic)-3,nchar(indic)))]
gpss1 <- gpss1[,.(number = sum(number,na.rm=T)),by=.(date,country)]

# get debit and credit cards amounts for pre-2010:
numcrd <-
  numcrd %>% 
  gather(., "indic", "number", debit.cards2006:credit.cards2002) %>% 
  as.data.table()
numcrd[, number := as.numeric(number)]
numcrd <- na.omit(numcrd)
setDT(numcrd)[, date := 
                as.numeric(substr(indic, nchar(indic) - 3, 
                                  nchar(indic)))]
numcrd <-
  numcrd[, .(number = sum(number, na.rm = T)), by = .(date, country)]
numcrd <- rbindlist(list(numcrd, gpss1))
numcrd[, iso3c := countrycode(
  country,
  origin = "country.name",
  destination = "iso3c",
  custom_match = c(
    "BCEAO" = "SEN",
    "Eastern Caribbean" = NA,
    "ECCU" = NA,
    "Kosovo" = "XKX"
  )
)]
numcrd <- numcrd[!is.na(iso3c)]

# merge everything:
max.row <- nrow(gpss2) + nrow(numcrd)
creditdebit <- 
  merge(gpss2, numcrd, by = c("iso3c","date"), all = TRUE, 
        allow.cartesian = FALSE) %>% 
  as.data.table()
waitifnot(nrow(creditdebit)<=max.row)

creditdebit[,country:=NULL]
names(creditdebit)[names(creditdebit)%in%c("volume","value","number")] <- 
  paste0(
    "creditdebit_",
    names(creditdebit)[names(creditdebit)%in%
                         c("volume","value","number")])

# IF there are positive values for ANY creditdebit_volume, creditdebit_value,
# or # creditdebit_volume, BUT SOME value is 0, then we delete that row
# (because that doesn't make logical sense)--must have been a data entry
# error or some fault of my cleaning process (hopefully the former).

creditdebit_vars <- c('creditdebit_number',
                      'creditdebit_value',
                      'creditdebit_volume')
creditdebit[,any.zero := apply(.SD, 1, 
                       function(x) {any(x == 0)}), 
                      .SDcols = creditdebit_vars]
creditdebit[,any.pos := apply(.SD, 1, 
                       function(x) {any(x > 0)}), 
                      .SDcols = creditdebit_vars]
creditdebit[any.pos==TRUE & any.zero==TRUE, 
            (creditdebit_vars):= NA]
creditdebit[,c("any.pos", "any.zero"):=NULL]

save.image("credit_cards_wb_global_payment_systems_survey_gpss_2004_2015.RData")
load("credit_cards_wb_global_payment_systems_survey_gpss_2004_2015.RData")

# OECD health numbers (pharma/surgery/bed utilization)-------------------------
oecd_list <- list(
  fread("OECD_health_measures_3.csv"),
  fread("OECD_health_measures_2.csv"),
  fread("OECD_health_measures_1.csv")
) %>% rbindlist()

oecd <-
  setDT(oecd_list)[, .(iso3c = COU, Variable, 
                       Measure, date = Year, Value)][Measure != 
                                                       "Per scanner"] %>% 
  as.data.table()
oecd[, Measure := Measure %>% gsub("Number", "", .)]
oecd[, var := paste(Variable, Measure, sep = "_") %>% gsub('^\\_|\\_$', '', .)]
oecd <- oecd[!is.na(Value)]
oecd[, `:=`(Variable = NULL, Measure = NULL)]
oecd <- oecd %>%
  spread(., var, Value)
delete.vars <- 
  c(
    "Computed Tomography exams, in ambulatory care_Per 1 000 population",
    "Computed Tomography exams, in hospitals_Per 1 000 population",
    "Computed Tomography exams, total_Per 1 000 population",
    "Magnetic Resonance Imaging exams, in ambulatory care_Per 1 000 population",
    "Magnetic Resonance Imaging exams, in hospitals_Per 1 000 population",
    "Magnetic Resonance Imaging exams, total_Per 1 000 population",
    "Positron Emission Tomography (PET) exams, in ambulatory care_Per 1 000 population",
    "Positron Emission Tomography (PET) exams, in hospitals_Per 1 000 population",
    "Positron Emission Tomography (PET) exams, total_Per 1 000 population"
  )

oecd[,(delete.vars) := lapply(.SD, function(x) x <- NULL), .SDcols = delete.vars]

save.image("oecd_health_numbers_pharma.RData")
load("oecd_health_numbers_pharma.RData")

# World Bank WDI Data ---------------------------------------------------------
wdidata <- WDI(
  indicator = c(
    "% population using internet" = "IT.NET.USER.ZS",
    "Personal computers (per 100 people)" = "IT.CMP.PCMP.P2",
    "Mobile cellular subscriptions (per 100 people)" = "IT.CEL.SETS.P2",
    "Air transport, passengers carried" = "IS.AIR.PSGR",
    "Air transport, freight (million ton-km)" = "IS.AIR.GOOD.MT.K1",
    "patents, residents" = "IP.PAT.RESD",
    "ATMs per 100K adults" = "FB.ATM.TOTL.P5",
    "Secure Internet Servers" = "IT.NET.SECR",
    "pop.wb" = "SP.POP.TOTL",
    "rail.lines" = "IS.RRS.TOTL.KM",
    "rail.passengers.km" = "IS.RRS.PASG.KM",
    "rail.goods" = "IS.RRS.GOOD.MT.K6",
    'fertilizer per arable land' = 'AG.CON.FERT.ZS',
    'hospital beds Per 1 000 population' = 'SH.MED.BEDS.ZS',
    'fixed telephone subscriptions' = 'IT.MLT.MAIN',
    'pop_above_65' = 'SP.POP.65UP.TO',
    'pop_15_64' = 'SP.POP.1564.TO',
    'elec_cons' = 'EG.USE.ELEC.KH.PC'
  ),
  start = 1960,
  end = lubridate::year(Sys.Date()),
  country = unique_country_codes
)
wdidata <- wdidata %>% as.data.table()
wdidata[,iso3c:=countrycode(iso2c, "iso2c", "iso3c")]
wdidata <- wdidata[!is.na(iso3c)]
wdidata[,`:=`(iso2c = NULL, country = NULL)]
wdidata[,keeprow:=apply(.SD, 1, function(x) length(na.omit(x)))]
wdidata <- wdidata[keeprow>2,] %>% as.data.table()
wdidata[,keeprow:=NULL]
wdidata <- wdidata %>% rename(date = year) %>% dfdt
wdidata[, adults:=pop_above_65 + pop_15_64]
wdidata[, c("pop_above_65", "pop_15_64") := NULL]

save.image("world_bank_data.RData")
load("world_bank_data.RData")

# Vaccine, Incomes -----------------------------------------------

# real GDP per capita from WDI and measles vaccination rates:
alldf <- WDI(indicator = 
               c("measles" = "SH.IMM.MEAS")) %>% 
              as.data.table()
alldf[,iso3c:=countrycode(iso2c, "iso2c", "iso3c")]
alldf <- alldf[!is.na(iso3c)]
alldf[,`:=`(iso2c = NULL, country = NULL)]
alldf <- alldf %>% as.data.frame() %>% rename(date = year) %>% dfdt

# Vaccine information from Our World in Data
vac <- fread("share-of-children-immunized-dtp3.csv")
vac <- vac[Code!=""]
vac2 <- fread("global-vaccination-coverage.csv")
vac2 <- vac2[Code!=""]
vac <- merge(vac, vac2, all=TRUE, by = c("Code","Year"), allow.cartesian = FALSE) %>% as.data.table()
vac <- dfcoalesce.all(as.data.frame(vac)) %>% as.data.table()
vac[,Entity:=NULL]
locat <- names(vac) %>% gregexpr(" \\(",.) %>% unlist
locat[locat==-1] <- 5
nems <- substring(names(vac),1,locat-1)
names(vac) <- nems
names(vac)[names(vac)=="Code"] <- "iso3c"
names(vac)[names(vac)=="Year"] <- "date"

# merge
df_list <- list(alldf, vac)
before <- nrow(alldf)
alldf <- Reduce(function(d1, d2) 
  merge(d1, d2, by = c("iso3c", "date"), 
        all.x = TRUE,
        allow.cartesian = FALSE), df_list)
waitifnot(nrow(alldf) == before)

# Fix empty rows:
keeprow <- apply(alldf, 1, function(x) length(na.omit(x)))
alldf[,keeprow:=keeprow]
alldf <- alldf[keeprow>5]
alldf[,keeprow:=NULL]

save.image("vaccine_incomes.RData")
load("vaccine_incomes.RData")

# Fertilizer/Pesticide/Arable Land (FAO) -------------------------------------

# Import datasets:
# get all the folders in this directory that have the word "input" in them
# (typically technology related)
setwd(paste0(input_dir, "/faostat_agriculture"))
start.dir <- getwd()
folder.names <- list.dirs()
file.list <- list()

folders.to.get <- 
  c(
    grep("FertilizersProduct", folder.names, value = T),
    grep("Pesticides_Use", folder.names, value = T),
    grep("LandUse", folder.names, value = T),
    grep('Investment_Machinery', folder.names, value = T)
  )

folders.to.get <- setdiff(
  folders.to.get,
  grep("Archive", folder.names, value = T)
)

for (dir.lp in folders.to.get) {
  setwd(start.dir)
  setwd(dir.lp)
  files.in.dir <- list.files()
  
  # Then, get the file within that says "normalized" and read it into the list
  files.to.get <- grep("Normalized", files.in.dir, value = T)
  waitifnot(length(files.to.get) == 1)
  name.in.list <-
    files.to.get %>% gsub("(Normalized).csv", "", ., fixed = T)
  file.list[[name.in.list]] <- fread(files.to.get)
}

# check that all the files have the same names
names.of.fao.files <- lapply(file.list, names)
waitifnot(SameElements(names.of.fao.files[[1]], names.of.fao.files[[2]]))

# rbind all of the files into one big dataset
fao <- rbindlist(file.list, idcol = "ag_product") %>% as.data.table()

file.list <- NULL

# save
setwd(input_dir)
save.image("FAO.prior.conversion.wide.RData")
load("FAO.prior.conversion.wide.RData")

# go from long to wide:
fao <- fao[`Area Code` < 5000,
           .(country = Area, Item, Element, Year, Unit, Value, ag_product)]
fao[, iso3c := name2code(
  country,
  custom_match = c(
    'Belgium-Luxembourg' = NA,
    'Channel Islands' = NA,
    'Czechoslovakia' = NA,
    'Netherlands Antilles (former)' = NA,
    'Pacific Islands Trust Territory' = NA,
    'Serbia and Montenegro' = NA,
    'Yugoslav SFR' = NA,
    'Ethiopia PDR' = NA,
    'USSR' = NA,
    'Sudan (former)' = NA,
    # China here includes Taiwan and Hong Kong, which is separate in our
    # other dataset. There is also a "China, mainland", which we will
    # take to be "our" China.
    'China'	= NA,
    'French Guyana' = 'GUF'
  )
)]
fao <- fao[!is.na(iso3c), ]

# Only get the values where units are in "tonnes" or actual numbers, not in 
# dollars.
fao <- fao[!(Unit%in%c("1000 US$"))]

# keep these variables:
fao <- fao[Item %in% c(
  "Ammonia, anhydrous",
  "Ammonium nitrate (AN)",
  "Ammonium sulphate",
  "Calcium ammonium nitrate (CAN) and other mixtures with calcium carbonate",
  "Diammonium phosphate (DAP)",
  "Disinfectants",
  "Fungicides and Bactericides",
  "Fertilizers n.e.c.",
  "Herbicides",
  "Insecticides",
  "Mineral Oils",
  "Monoammonium phosphate (MAP)",
  "NPK fertilizers",
  "Other nitrogenous fertilizers, n.e.c.",
  "Other NK compounds",
  "Other NP compounds",
  "Other Pesticides nes",
  "Other phosphatic fertilizers, n.e.c.",
  "Other potassic fertilizers, n.e.c.",
  "Pesticides (total)",
  "Phosphate rock",
  "PK compounds",
  "Plant Growth Regulators",
  "Potassium chloride (muriate of potash) (MOP)",
  "Potassium nitrate",
  "Potassium sulphate (sulphate of potash) (SOP)",
  "Rodenticides",
  "Sodium nitrate",
  "Superphosphates above 35%",
  "Superphosphates, other",
  "Urea",
  "Urea and ammonium nitrate solutions (UAN)",
  
  # Cropland has a more narrow definition than agricultural land, in that
  # cropland is used for cultivation of crops only, while agricultural land
  # includes that used for both crops and husbandry. Thus, agricultural land
  # includes permanent meadows and pastures, while cropland doesn't. Moreover,
  # cropland includes arable land and permanent crops. The CHAT dataset for
  # the variable 'pctirrigated' says, "Irrigated area (as defined above) as a
  # share of cultivated land, which includes land used for permanent and
  # temporary crops, **pasture**, land used for temporary crops, and land
  # lying temporarily fallow". Thus, we take this to mean AGRICULTURAL land
  # actually irrigated. CHAT has a % value as well as a numeric actual value.
  
  "Agricultural land",
  "Agriculture area actually irrigated",
  "Cropland area actually irrigated",
  "Arable land",
  "Naturally regenerating forest",
  "Planted Forest",
  "Agricultural Machines",
  "Harvesters-Threshers",
  "Milking machines",
  "Soil machinery",
  "Tractors Agric Total",
  "Agricultural tractors",
  "Agricultural tractors, total",
  "Balers (straw and fodder balers including pick-up balers)",
  "Combine harvesters - threshers",
  "Manure spreaders and Fertiliser distributors",
  "Milking machines",
  "Other Agricultural tractors (two-axle tractors)",
  "Pedestrian controlled tractors (single axle tractors)",
  "Ploughs (e.g. reversible and non-reversible ploughs)",
  "Root or tuber harvesting machines",
  "Seeders, planters and transplanters",
  "Threshing machines",
  "Track-laying tractors (crawlers)"
), ]

# remove the data that is for import and export amounts:
fao <- fao[!(Element%in%
               c('Import Quantity','Production','Export Quantity',
               'Share in Forest land', 'Share in Land area'))]

# classify each as fertilizer or pesticide:
fao[grepl("Fertilizer", ag_product), ag_product:="Fertilizer"]
fao[grepl("Pesticide", ag_product), ag_product:="Pesticide"]
fao[grepl("Machinery", ag_product), ag_product:="Machinery"]
fao[grepl("Land", ag_product), ag_product:="Land"]

fao[, thing := paste(ag_product, Item, Element, Unit)]
fao <- fao[, .(iso3c, year = Year,
               thing, val = Value,
               country)]
fao <- fao %>% distinct() %>% dfdt

# check that for each technology, year, and country, there is 1 observation:
check <- fao[, .(country, thing, year, iso3c)] %>% dfdt
check[, count := 1]
check[, check.sum := sum(count, na.rm = T), by = .(thing, year, iso3c)]
waitifnot(all(check$check.sum == 1))

# get how many country-years each measure has, and discard measures that have
# less than 850 country-years.
fao[,count:=1]
fao[,sumcount:=sum(count,na.rm=T), by = .(thing)]
fao <- fao[sumcount>=850,]
fao[,`:=`(count = NULL, sumcount = NULL)]

# replace the percent sign with the word "pct" as an indicator that this is a 
# percentage value (later, we divide everything by population to get per capita
# values, except for these values).
fao[, thing := gsub("%", "pct", thing) %>% 
      gsub(
        "Fertilizer Superphosphates above 35pct Agricultural Use tonnes",
        "Fertilizer Superphosphates above 35 percent Agricultural Use tonnes",.)]
fao$thing %>% unique()

# finally, pivot to a wide format:
fao <- dcast(fao, iso3c + year ~ thing, value.var = "val", fill = NA) %>% dfdt
fao <- rename(fao, date = year) %>% dfdt

# get the names of our new agriculture and pesticides variables:
names.pesticides.agri <- setdiff(names(fao), c("date", "iso3c"))

save.image("fao_fertilizer_pesticide.RData")
load("fao_fertilizer_pesticide.RData")

# World Motor Vehicle Production (BTS) ------------------------------------------

# Global motor vehicle production from:
# https://www.bts.gov/bts/archive/publications/national_transportation_statistics/table_01_23

mvprod <- fread('US_dept_transportation_table_01_23_2.csv') %>% as.data.frame()

# rename the columns into the year names
names_head <- mvprod[3,] %>% as.character()
names_head[1] <- 'cou'
names(mvprod) <- names_head

# drop rows after 'KEY: U = data are unavailable.'
mvprod <- mvprod[1:grep('KEY: U = data are unavailable.', mvprod$cou),]

# create a new column indicating the type of car:
# 'Passenger cars'
# 'Commercial vehicles'
# 'Total passenger cars and commercial vehicles'
# 'KEY: U = data are unavailable.'

split_locs <-
  c(
    'Passenger cars',
    'Commercial vehicles',
    'Total passenger carsa and commercial vehicles'
  )
split_locs <- unlist(lapply(split_locs, function(x) grep(x, mvprod$cou)))
mvprod[split_locs, 'split'] <- mvprod$cou[split_locs]
mvprod <- mvprod %>% fill(split)
mvprod <- mvprod[mvprod$`1961`!="" & mvprod$cou!="",]

# pivot from wide to long using the dates
mvprod <- mvprod %>% gather("date","value",`1961`:`2015`)

# convert things to numeric:
mvprod$value <- mvprod$value %>% gsub(",","",.) %>% gsub("U","",.) %>% as.numeric()
mvprod <- mvprod %>% na.omit()

# pivot from long to wide using the vehicle type
mvprod <- mvprod %>% spread(split,value)

# remove certain non-country IDs:
# Total world
# Total worlde
# U.S. percent of world

mvprod <- mvprod[!(mvprod$cou %in%
                   c('Total world',
                     'Total worlde',
                     'U.S. percent of world')),]

mvprod <- 
  mvprod %>% 
  filter(!grepl("Yugoslav", cou)) %>% 
  mutate(
    iso3c = name2code(cou),
  ) %>% 
  rename("BTS commercial vehicles" = "Commercial vehiclesd",
         "BTS passenger cars" = "Passenger carsa",
         "BTS total vehicles" = "Total passenger carsa and commercial vehiclesd"
         ) %>% 
  as.data.table()

mvprod$cou <- NULL
check_dup_id(mvprod, c('iso3c','date'))

# everything's in thousands:
mvprod %>% names()

# convert to thousands:
to_thou <- c("BTS commercial vehicles",
             "BTS passenger cars",
             "BTS total vehicles")
mvprod[,(to_thou):=lapply(.SD, function(x) x* 1000), .SDcols = to_thou]

# Motor Vehicles / Cars (OICA, OECD) -----------------------------------------

veh_list <- list()

for (file_ in c(
  "PC_Vehicles-in-use.xlsx",
  "Total_in-use-All-Vehicles.xlsx",
  "CV_Vehicles-in-use.xlsx"
)) {
  dfcars <- NULL
  
  dfcars <-
    read_excel(file_,
               sheet = 1,
               skip = 5)
  dfcars <-
    dfcars[, c(
      "REGIONS/COUNTRIES",
      "2005",
      "2006",
      "2007",
      "2008",
      "2009",
      "2010",
      "2011",
      "2012",
      "2013",
      "2014",
      "2015"
    )]
  dfcars$`REGIONS/COUNTRIES`[grepl("OTHER EUROPE", dfcars$`REGIONS/COUNTRIES`)] <-
    NA
  dfcars$`REGIONS/COUNTRIES` <-
    dfcars$`REGIONS/COUNTRIES` %>% name2code(
      custom_match = c(
        "ALL COUNTRIES"	= NA,
        "AFRICA" = NA,
        "AMERICA" = NA,
        "AZERBAIDJAN" = "AZE",
        "MOLDAVIA" = "MDA",
        "ASIA/OCEANIA/MIDDLE EAST"	= NA,
        "EU 15 countries + EFTA"	= NA,
        "EUROPE"	= NA,
        "NAFTA"	= NA,
        "CENTRAL & SOUTH AMERICA"	= NA,
        "EU 28 countries + EFTA"	= NA,
        "EUROPE NEW MEMBERS"	= NA
      )
    )
  dfcars <- dfcars[!is.na(dfcars$`REGIONS/COUNTRIES`), ]
  dfcars <- dfcars %>%
    gather(., "date", "amt", `2005`:`2015`) %>%
    mutate(date = as.numeric(date),
           # all vehicle numbers are in thousands
           amt = as.numeric(amt) * 1000) %>%
    rename(iso3c = `REGIONS/COUNTRIES`) %>%
    dfdt
  
  # check we have unique country-years:
  a <- dfcars %>% dplyr::select(iso3c, date) %>% distinct %>% nrow()
  b <- dfcars %>% dplyr::select(iso3c, date) %>% nrow()
  waitifnot(a == b)
  
  name_file <- 
    file_ %>% 
    gsub(".xlsx","",.,fixed = T) %>% 
    gsub("-in-use", "", .) %>% 
    gsub("in-use-", "", .) %>% 
    gsub("-", "_", .) %>% 
    tolower()
  
  veh_list[[name_file]] <- dfcars
  
  dfcars <- NULL
}

dfcars <- rbindlist(veh_list, idcol = "type")
dfcars <- dfcars %>% as.data.frame() %>% spread(., type, amt) %>% 
  rename(all_vehicles = total_all_vehicles) %>% dfdt

# get oecd vehicle dataset
oecd_cars <- fread("OECD_cars_data.csv")
oecd_cars <- oecd_cars[oecd_cars$Indicator%in%c(
  "Passenger cars per one thousand inhabitants"
  )]
oecd_cars <- 
  oecd_cars[,.(iso3c = COUNTRY, variable = Indicator, date = Year, value = Value)]
oecd_cars <-
  oecd_cars %>% dcast(.,
                      iso3c + date ~ variable,
                      value.var = "value",
                      fun.aggregate = sum,
                      fill = NA)

save.image("motor_vehicles.RData")
load("motor_vehicles.RData")

# Electricity Production (OWID) -------------------------------------------
# OWID:
# https://ourworldindata.org/electricity-mix?country=
# Data is compiled by Our World in Data based on two sources:
# - BP Statistical Review of World Energy: https://www.bp.com/en/global/corporate/energy-economics/statistical-review-of-world-energy.html
# - Ember: https://ember-climate.org/data/

electric <- fread("electricity-prod-source-stacked.csv") %>% as.data.frame()
electric <-
  electric %>%
  rename(iso3c = Code,
         date = Year) %>%
  dplyr::select(-Entity) %>% 
  dfdt
electric <- electric[iso3c!=""]
electric <- electric[!is.na(iso3c)]

# check that there aren't duplicated ISO3C and dates:
a <- electric[,.(iso3c, date)] %>% nrow
b <- electric[,.(iso3c, date)] %>% distinct %>% nrow
waitifnot(a==b)

# adjust certain iso3c codes
electric <- electric[iso3c == "OWID_KOS", iso3c:= "XKX"]
electric <- electric[!(iso3c%in%c("ANT", "OWID_WRL")),]

save.image("electric.RData")
load("electric.RData")

# World Infrastructure Stocks (Canning) ---------------------------------

# paved roads and railways from:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/3UYTIQ

setwd(paste0(getwd(), '/world stocks infrastructure'))

# import all our data
file_names <- grep(".xls", dir(), value = T)
world_inf <- lapply(file_names, function(x) dfdt(readxl::read_xls(x)))
names(world_inf) <- gsub(".xls", "", file_names)
names(world_inf$power_capacity)[names(world_inf$power_capacity) == 'code'] <- 'id'

# the world infrastructure dataset makes an error in which it mixes the labels
# for id 45/44 with Congo Kinsasha and Brazaville. I regressed (not shown)
# Log(GDP) on Log(rail) for ALL countries excluding these two, and predicted
# rail amounts using that regression based on 2 different merges. The correct
# merge turns out to be the one based on the NAME of country (Zaire, Congo),
# not the ID (44/45), so I make this correction. A similar thing happens with
# Lithuania and Liechtenstein in ID 119, oddly enough, so I delete that one
# (it's completely empty for the non-telephone variables)
world_inf$railway[id==44,correction:=45]
world_inf$railway[id==45,correction:=44]
world_inf$railway[!is.na(correction),id:=correction]
world_inf$railway$correction <- NULL

# create a separate data.frame that shows the IDs with their ISO labels:
id_world_inf <- rbindlist(world_inf, fill = TRUE)
id_world_inf <- id_world_inf[,.(id, country = tolower(country))]
id_world_inf <- unique(id_world_inf)
id_world_inf <- id_world_inf[!is.na(country)][order(id,country)]
id_world_inf[, country:=zoo::na.locf(country, na.rm = FALSE), by = "id"]

# we remove Channel Islands, Czechoslovakia, USSR, Yugoslavia
id_world_inf <- id_world_inf[!(
                                country %in% c(
                                  "channel islands",
                                  "czechoslovakia",
                                  "u.s.s.r.",
                                  "yugoslavia",
                                  "yugoslavia, fed. rep."
                                )
                              )]
id_world_inf[country=="papua n.guinea", country:="papua new guinea"]
id_world_inf[country=="united arab e.", country:="united arab emirates"]
id_world_inf[country=="central afr.r.", country:="central african republic"]
id_world_inf[country=="guinea-biss", country:="guinea bissau"]
id_world_inf[country=="korea, dem. rep.", country:="north korea"]

unique(id_world_inf[id==119])

id_world_inf[,iso3c:=countrycode(country, 
                                 origin = 'country.name',
                                 destination = 'iso3c',
                                 custom_match = c(
                                 "netherlands antilles" = "ANT"
                               )
                            )]

# check that I've not botched the codes by cross-checking with the roads dataset
# which has iso3c codes in them.
check_codes <- unique(dfdt(na.omit(world_inf$road[,.(id, country_code)])))
id_world_inf <- merge(id_world_inf, check_codes, by = 'id', all.x = T)

# okay, ID differences now appear mostly a result of older ISO3C codes.
id_world_inf[iso3c!=country_code]

# check we only have 1 iso3c code per id:
id_world_inf <- unique(id_world_inf[,.(id, iso3c)])
id_world_inf <- id_world_inf[id!=119,]
check_dup_id(id_world_inf, 'id')

# Remove country and country_code from the world_inf dataset
for (i in names(world_inf)) {
  if (any(names(world_inf[[i]])%in%c('country'))) {world_inf[[i]]$country <- NULL}
  if (any(names(world_inf[[i]])%in%c('country_code'))) {world_inf[[i]]$country_code <- NULL}
}

# merge all variables in world_inf data set
world_inf <- Reduce(f = function(x,y) merge(x, y, by = c('year','id'), all = T), world_inf)

# merge back in ISO3C codes.
world_inf <- merge(world_inf, id_world_inf, by = 'id')
names(world_inf)[names(world_inf)=='year'] <- 'date'
world_inf$id <- NULL

# check duplicated:
check_dup_id(world_inf, c('date','iso3c'))


# only select the merged variables:
tokeep <- grep('merge|iso3c|date', names(world_inf), value = T)
world_inf <- world_inf[,(tokeep), with = FALSE] %>% dfdt
names(world_inf) <- gsub("merge", "", names(world_inf))

# rename variables to something else because we already have that variable name
# in the CHAT database:
names(world_inf)[names(world_inf)=='egc'] <- 'elec_gen_capacity_world_inf'
names(world_inf)[names(world_inf)=='rail'] <- 'rail_line_world_inf'
names(world_inf)[names(world_inf)=='telephone'] <- 'telephone_world_inf'

setwd(input_dir)

save.image("world_inf.RData")
load("world_inf.RData")


# Aluminum Production (CLIO) -------------------------------------------
# https://clio-infra.eu/Indicators/AluminiumProduction.html
# Aluminum primary production, in thousand metric tons 
# British Geological Survey (BGS) U.S. Bureau of Mines, 
# U.S. Geological Survey (USGS) 

aluminum <- 
  read_xlsx('CLIO_AluminiumProduction_Broad.xlsx', 
            sheet = 'Data Long Format') %>% 
  as.data.table()
aluminum[,iso3c:=name2code(country.name)]
stopifnot(nrow(aluminum[is.na(iso3c)])==0)
check_dup_id(aluminum, c('iso3c','year'))
aluminum[,`:=`(ccode = NULL, country.name = NULL)]
aluminum <- aluminum[order(iso3c, year),.(date = year, aluminum = value, iso3c)]

# Container Capacity (NOT OBTAINED) ---------------------------------------------------------

# emailed the guy -- no reply yet
# Can you see if we can get access on container capacity from here:
# https://www.sciencedirect.com/science/article/abs/pii/S0022199615001403


# ... ---------------------------------------------------------------------


# Merge -------------------------------------------------------------------
tomerge <- list(
  # WDI and Maddison GDP PPP estimates
  mad[date >= 1820],
  # Steel Production (World Steel)                
  raww,
  # Credit Cards (WB Global Payment Systems Survey (GPSS) 2004-2015)
  creditdebit,
  # OECD health numbers (pharma/surgery/bed utilization)
  oecd,
  # World Bank WDI Data
  wdidata,
  # Vaccine, Incomes
  alldf,
  # FAO: Fertilizer/Pesticide/Arable Land
  fao,
  # World Motor Vehicle Production (BTS)
  mvprod,
  # Motor Vehicles / Cars (OICA, OECD)
  dfcars,
  oecd_cars,
  # Electricity Production (OWID)
  electric,
  # World Infrastructure Stocks (Canning)
  world_inf,
  # Aluminum Production (CLIO)                
  aluminum
)

# create a data.frame with all permutations of years and country codes:
all_country_codes <- lapply(tomerge, function(x) {
  x$iso3c %>% unlist %>% unique
}) %>% unlist %>% unique %>% na.omit
all_years <- lapply(tomerge, function(x) {
  x$date %>% na.omit %>% min
}) %>% unlist %>% min
all_years <- seq(all_years, lubridate::year(Sys.Date()))
m.alldf <- CJ(iso3c = all_country_codes,
              date = all_years) %>% as.data.table()
tomerge <- c(list(m.alldf), tomerge)

# convert country to character and date to numeric
proper_type <- function(x) {
  x[, date := as.numeric(date)]
  x[, iso3c := as.character(iso3c)]
  x %>% as.data.table()
}
tomerge <- lapply(tomerge, proper_type)

# make sure that we have unique year - country pairs
unique_yr_iso <- function(x) {
  a <- nrow(x[, .(date, iso3c)])
  b <- nrow(distinct(x[, .(date, iso3c)]))
  waitifnot(a == b)
}
for (i in 1:length(tomerge)) {
  cat(i)
  unique_yr_iso(tomerge[[i]])
}

m.alldf <-
  Reduce(function(x, y)
    merge(
      x,
      y,
      by = c("date", "iso3c"),
      all.x = T,
      allow.cartesian = T
    ),
    tomerge)

save.image("merged1.RData")
load("merged1.RData")

# Adjust certain variables after merge ---------------------------------------

# If Maddison does not have population values (e.g. 2019), then fill in 
# with World Bank population values by using growth from WDI figures to 
# project forwards the Maddison GDP figures.

m.alldf <- m.alldf[order(iso3c, date)]
m.alldf[, pop.wdi.gr := pop.wb / shift(pop.wb), by = 'iso3c']

for (i in seq(2015, 2021)) {
  m.alldf[, pop.n := shift(pop) * pop.wdi.gr, by = 'iso3c']
  m.alldf[date==i & is.na(pop), pop:=pop.n]
  m.alldf <- as.data.table(m.alldf)
}

ggplot(m.alldf, aes(x = log(pop.wb), y = log(pop))) + geom_point()
m.alldf[, c('pop.wdi.gr', 'pop.n', 'pop.wb') := NULL]

# Convert certain technologies that are currently in per capita to 
# be actual values by multiplying by population. (e.g. if 
# in per 100 people, then multiply by population / 100).
convert <- 
  m.alldf %>% names %>% 
  grep("% population", .,fixed=T, value=T)
convert_100 <-
  m.alldf %>% names %>%
  grep("per 100 people", .,
       value = T, ignore.case = T)
convert_1000 <-
  m.alldf %>% names %>%
  grep("per 1 000 inhabitants per day|Per 1 000 population| per one thousand inhabitants",
       .,
       value = T,
       ignore.case = T)
convert_100K_adults <-
  m.alldf %>% names %>%
  grep("per 100K adults",
       .,
       value = T,
       ignore.case = T)
setDT(m.alldf)[, (convert) := lapply(.SD, function(x)
  x * pop),
  .SDcols = convert]
setDT(m.alldf)[, (convert_100) := lapply(.SD, function(x)
  x * pop / 100),
  .SDcols = convert_100]
setDT(m.alldf)[, (convert_1000) := lapply(.SD, function(x)
  x * pop / 1000),
  .SDcols = convert_1000]
setDT(m.alldf)[, (convert_100K_adults) := lapply(.SD, function(x)
  x * adults / 100000),
  .SDcols = convert_100K_adults]

# clean the resulting data.frame:
m.alldf[, `:=`(
  country.x = NULL,
  country.y = NULL
)]

waitifnot(all(is.na(m.alldf$iso3c) == FALSE))
names(m.alldf) <- names(m.alldf) %>%
  gsub("per 1 000 inhabitants per day|Per 1 000 population| per one thousand inhabitants", 
       "", .) %>%
  gsub("(per 100 people)", "", .) %>%
  gsub("% population", "", ., fixed=T) %>%
  gsub("per 100K adults","",.) %>%
  gsub(" $","",.) %>% 
  gsub("\\.$","",.) %>% 
  gsub(" \\(\\)$","",.) %>% 
  gsub("^ *","",.)

# check: make sure that the percent of people with internet is 
# a number between 0 and 1:
checkthis <- function(a,b,num) {waitifnot(all(na.omit(a/b)<=num))}
checkthis(m.alldf$`using internet`, m.alldf$pop, 100)

# check: U.S. has ~174 ATMs per 100,000 people, so that should be an upper bound
checkthis(m.alldf$ATMs, m.alldf$pop, 1)
fn <- ecdf(m.alldf$ATMs/m.alldf$pop)
waitifnot(fn(174/100000)>=0.75)

# Compare the series for motor vehicles between OICA and OECD.
# Passenger cars <> pc_vehicles

# of note: the passenger cars variable from OECD is statistically significantly
# larger than that of OICA when using a linear fit.
bob <- na.omit(m.alldf[, .(pc_vehicles, `Passenger cars`)])
fit <- lm((pc_vehicles)~(`Passenger cars`), 
          data = bob[pc_vehicles>0 & `Passenger cars`>0 ])
pval <- summary(fit)$coefficients[1,4]
abs(summary(fit)$coefficients[2,1] - 1) < 0.05

# Coalesce the passenger car variables from OICA and OECD using an 
# arithmetic mean, since we have no reason to prefer one series over another:

m.alldf[, pc_vehicles := 
          apply(.SD, 1, 
                function(x) (mean((x), na.rm=T))
          ), 
        .SDcols = c("pc_vehicles", "Passenger cars")]
m.alldf[, (c("Passenger cars")) := NULL]

# Coalesce the telephone variables from Canning and WDI using an 
# arithmetic mean, since we have no reason to prefer one series over another:
m.alldf[, `fixed telephone subscriptions` := 
          apply(.SD, 1, 
                function(x) (mean((x), na.rm=T))
          ), 
        .SDcols = c("fixed telephone subscriptions", "telephone_world_inf")]
m.alldf[, (c("telephone_world_inf")) := NULL]

# make the 'per hectare arable land' measurements actual measurements based on 
# multiplying through by FAO arable land estimates.
m.alldf$fertilizer <- 
  m.alldf$`fertilizer per arable land` * 
  m.alldf$`Land Arable land Area 1000 ha` * 
  1000

save.image("merged2.RData")
load("merged2.RData")