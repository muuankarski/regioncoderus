# load packages
library(RCurl)
library(XML)
library(stringr)

# 1. Lets parse the table of  all sovereign states from here: http://en.wikipedia.org/wiki/Autonomous_okrugs_of_Russia
# to get url's to English page of each state

##

html <- getURL("http://en.wikipedia.org/wiki/Autonomous_okrugs_of_Russia", followlocation = TRUE)

# parse html
doc <-  htmlParse(html, asText=TRUE)
tbls <- xpathSApply(doc, "//table[@class='nowraplinks navbox-subgroup']", saveXML)
x <- tbls[nchar(tbls) == max(nchar(tbls))]
str <- unlist(strsplit(x, '<li><a href=\"/wiki/'))
str <- gsub(pattern = '\\\"(.*)', replacement = '', x = str, perl = TRUE)
str <- gsub(pattern = '(\\\n)(.*)', replacement = "", x = str, perl = TRUE)

# one custom fix
str[str == "North_Ossetia%E2%80%93Alania"] <- "North_Ossetia–Alania"

regions1 <- str[-1]

## ---------------------------------------------------------- ##

## Federal districts from http://en.wikipedia.org/wiki/Federal_districts_of_Russia

html <- getURL("http://en.wikipedia.org/wiki/Federal_districts_of_Russia", followlocation = TRUE)

# parse html
doc <-  htmlParse(html, asText=TRUE)
tbls <- xpathSApply(doc, "//table[@class='wikitable sortable']", saveXML)
x <- tbls
str <- unlist(strsplit(x, '<a href=\"/wiki/'))
str <- gsub(pattern = '\\\"(.*)', replacement = '', x = str, perl = TRUE)
str <- gsub(pattern = '(\\\n)(.*)', replacement = "", x = str, perl = TRUE)
# only including "federal_district"

regions2 <- str[grep("Federal_District", str)]

## ---------------------------------------------------------- ##

## Economical regions from http://en.wikipedia.org/wiki/Economic_regions_of_Russia

html <- getURL("http://en.wikipedia.org/wiki/Economic_regions_of_Russia", followlocation = TRUE)

# parse html
doc <-  htmlParse(html, asText=TRUE)
tbls <- xpathSApply(doc, "//div[@class='legend']", saveXML)
str <- tbls
str <- gsub(pattern = '(.*)<a href=\"/wiki/', replacement = '', x = str, perl = TRUE)
str <- gsub(pattern = '\\\"(.*)', replacement = '', x = str, perl = TRUE)
regions3 <- str


## Autonomous okrugs from : http://en.wikipedia.org/wiki/Autonomous_okrugs_of_Russia

html <- getURL("http://en.wikipedia.org/wiki/Autonomous_okrugs_of_Russia", followlocation = TRUE)

# parse html
doc <-  htmlParse(html, asText=TRUE)
tbls <- xpathSApply(doc, "//table[@class='wikitable sortable']", saveXML)
x <- tbls[2]
str <- unlist(strsplit(x, '<a href=\"/wiki/'))
str <- gsub(pattern = '\\\"(.*)', replacement = '', x = str, perl = TRUE)
str <- gsub(pattern = '(\\\n)(.*)', replacement = "", x = str, perl = TRUE)

# one custom fix
str[str == "Khanty%E2%80%93Mansi_Autonomous_Okrug"] <- "Khanty-Mansi_Autonomous_Okrug"

regions4 <- str[-1]


## ---------------------------------------------------------- ##

## Russian Federation

regions5 <- "Russia"

regions <- c(regions1,regions2,regions3,regions4,regions5)
# remove duplicated url's  (Some scemes are not exclusive)
regions <- regions[!duplicated(regions)]

# custom fix- remove parenthesis from Northern_economic_region_(Russia) 
regions[regions == "Northern_economic_region_(Russia)"] <- "Northern_economic_region,_Russia"
regions[regions == "Urals_economic_region"] <- "Ural_economic_region"
regions <- regions[regions != "Kaliningrad_economic_region"]  # Manually adding this on row ~116

urls <- paste0("http://en.wikipedia.org/wiki/",regions)


for (i in 1:length(urls)){
  
  # download html
  html <- getURL(urls[i], followlocation = TRUE)
  
  # parse html
  doc <-  htmlParse(html, asText=TRUE)
  lists <- xpathSApply(doc, "//div[@class='body']/ul", saveXML)
  x <- lists[nchar(lists) == max(nchar(lists))]
  str <- unlist(strsplit(x, "title="))[-1]
  str <- gsub(pattern = '(\\\n)(.*)', replacement = "", x = str, perl = TRUE)
  str <- gsub(pattern = "(lang)(.*)", replacement = "", x = str, perl = TRUE)
  str <- str_replace_all(str, '\\\"','')
  
  str <- str_replace_all(str, ' – ',';')
  str <- str[grep(";", str)]
  str <- str_trim(str)
  str <- str_replace_all(str, "'","")
  
  ## as there are some strings with no ; marks, we get rid of them
  dd <- read.table(text = str, sep = ";", colClasses = "character")
  names(dd) <- c(regions[i],"lang")
  if (i == 99) names(dd)[1] <- str_replace_all(names(dd)[1], ',_Russia','') 
  ## English language region name from title-element
  title <- xpathSApply(doc, "//title", saveXML)
  title <- gsub(pattern = "( - Wikipedia)(.*)", replacement = "", x = title, perl = TRUE)
  title <- str_replace_all(title, '<title>','')
  # fix this exception ""Northern economic region, Russia"
  if (i == 99) title <- str_replace_all(title, ', Russia','')
  if (i == 108) title <- str_replace_all(title, 'Koryak Okrug','Koryak Autonomous Okrug')
  newrow <- data.frame(title,"English")
  titlename <- str_replace_all(title, " ", "_")
  names(newrow) <- c(titlename,"lang")
  dd <- rbind(dd,newrow)
  #
  if (i == 1) dat <- dd
  dat <- dat[!duplicated(dat["lang"]),]
  if (i != 1) dat <- merge(dat,dd,by="lang", all.x=TRUE)
}

kaliningrad_economic_region <- c(rep(NA, 22),
                                 "Kaliningrad Economic Region",
                                 rep(NA, 3),
                                 "Kaliningradin talousalue",
                                 rep(NA, 40),
                                 "Калининградский экономический район",
                                 rep(NA, 27))
dat <- cbind(dat,kaliningrad_economic_region)

# finally, transpose the data
data <- as.data.frame(t(dat[-1]))

# manually add Kaliningrad economic regions




# create language names from row.names of the dat and refine them a bit
new_names <- paste0("region.name.",tolower(as.character(dat[[1]])))
new_names <- str_replace_all(new_names, " ", ".")
new_names <- str_replace_all(new_names, "/", ".")
new_names <- str_replace_all(new_names, "\\.{3}", ".")
new_names <- str_replace_all(new_names, "\\.{2}", ".")
new_names <- str_replace_all(new_names, "\\)", "")
new_names <- str_replace_all(new_names, "\\(", "")
names(data) <- new_names
# # then the english country names from row.names of the data
# region.name <- row.names(data)
# ## remove the .x's and .y's
# region.name <- str_replace_all(region.name, "\\.x", "")
# region.name <- str_replace_all(region.name, "\\.y", "")
# ## apply the new naMES
# data$region.name.english <- english.name

for (i in 1:ncol(data)) {
  data[[i]] <- as.character(data[[i]])
}

# seuraavaksi lisää ne aluekoodit shapefileen!!


# load the wiki-key data to be able to combine with original data in prep.R script
# library(RCurl)
# GHurl <- getURL("https://raw.githubusercontent.com/muuankarski/data/master/world/wiki_key.csv")
# wiki <- read.csv(text = GHurl)
# wiki[2][wiki[2] == ""] <- NA

# data <- merge(data,wiki[1:2],by="country.name.english")
# data <- data[c(max(ncol(data)),1:max(ncol(data))-1)]

write.csv(data, "data/regioncoderus_data.csv", row.names = FALSE)

## Colnames to exclude in countrycode()-function
# cat(paste(shQuote(names(data), type="cmd"), collapse=", "))