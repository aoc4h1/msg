# ---------------------- MESSAGES WITH BARBI ----------------------

library(jsonlite)
library(plyr)
library(doParallel)
library(foreach)
library(stringi)
library(text2vec)
library(lubridate)

library(dplyr)
library(tidytext)
library(ggplot2)

# --------- DATA
setwd("/Users/palmamogyorosi/Documents/Palma/Kreatív/2022 Barbi Messenger/data/json_files")
files <- list.files()

coln <- c("sender_name","timestamp_ms","content","type","is_unsent","photos","reactions","call_duration","videos","sticker","gifs","files")

for(i in 1:length(files)) {
  if (i==1) {
    data <- fromJSON(files[i])
    data <- data$messages
    
    coln.kiszed <- coln[which(coln %in% colnames(data))]
    data <- select(data, coln.kiszed)
    data$sticker <- data$sticker$uri
    
  } else {
    x <- fromJSON(files[i])
    x <- x$messages
    
    coln.kiszed <- coln[which(coln %in% colnames(x))]
    print(coln[which(!(coln %in% colnames(x)))])
    x <- select(x, all_of(coln.kiszed))
    x$sticker <- x$sticker$uri
    
    data <- rbind.fill(data,x)
    rm(x)
    print(paste(files[i]," join complete"))
  }
}

rm(coln,coln.kiszed,i,files)

data.safe <- data
#data <- data.safe


# --------- DATE

data$timestamp_ms <- as_datetime(data$timestamp_ms/1000)
data <- data %>% arrange(timestamp_ms)


# --------- ENCODING

data$sender_name <- iconv(data$sender_name,"UTF-8","latin1")
Encoding(data$sender_name) <- "UTF-8" 

data$content2 <- iconv(data$content,"UTF-8","latin1")
Encoding(data$content2) <- "UTF-8" 


# ------------------- EMOJI

# Átalakítjuk ASCII-ra a formátumot
x <- data$content2
x <- iconv(x, from = "latin1", to = "ascii", sub = "byte")
data$text <- x

emDict <- emDict[order(nchar(emDict$r.encoding), decreasing = T),]
emDict$description <- paste("",emDict$description,"")

core.number=2
cl <- makeCluster(core.number, setup_timeout = 0.5)
registerDoParallel(cl)

split.num <- round(nrow(data) / 200, 0)
parsplit2 <- rep(1:split.num, round(nrow(data) / split.num, 0) + 1)[seq_len(nrow(data))]
parsplit2 <- parsplit2[order(parsplit2, decreasing = FALSE)]

do.emoji.change <- function(i) {
  con <- x[which(parsplit2 == i)]
  
  x[which(parsplit2 == i)] <- stri_replace_all_fixed(x[which(parsplit2 == i)], pattern = emDict$r.encoding, replacement = emDict$description, vectorize_all = FALSE)
  
  res1 <- x[which(parsplit2 == i)]
  
  assign("res1", res1, envir = .GlobalEnv)}


z <- foreach::foreach(i = 1:split.num, .packages = c("stringi", "text2vec"), .combine = c, .export = "x") %dopar%
  do.emoji.change(i)

x <- z
rm(z)

# GET BACK ACCENTS
data$text <-  stri_replace_all_fixed(x, pattern = ekezet.ascii$kod, replacement = ekezet.ascii$ekezet, vectorize_all = FALSE)
rm(x,cl,core.number,parsplit2,split.num,do.emoji.change)
rm(emDict,dbpedia.ki,ekezet.ascii,ellentet,nevek)


# --------- OTHER

# reorder columns
data <- select(data, sender_name:content,content2,text,type:files) 
# lowercase
data$text <- tolower(data$text)
# year
data$year <- substr(data$timestamp_ms,1,4)
data$month <- substr(data$timestamp_ms,1,7)
data$day <- substr(data$timestamp_ms,1,10)

# emoji jav.
data$text <-  stri_replace_all_fixed(data$text, "<f3><be><8c><b0>", "$e_grinning_face")
data$text <-  stri_replace_all_fixed(data$text, "$e_heart_suit_emoji", "$e_red_heart")
data$text <-  stri_replace_all_fixed(data$text, "<3", "$e_red_heart")


# --------- TOKENS

freqw <- data %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)

freqw <- data %>%
  group_by(sender_name) %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE)

# --------- EMOJI SEARCH

search_emoji('scream')
emoji(search_emoji('scream'))

unicode <- read_html("https://unicode.org/emoji/charts/full-emoji-list.html")
ut <- unicode %>%
  html_node("table") %>%
  html_table()
ut <- ut %>% html_table()
all_emoji <- ut[,3]



# --------- SAVE

save.image("~/Documents/Palma/Kreatív/2022 Barbi Messenger/data/messages_data.RData")
