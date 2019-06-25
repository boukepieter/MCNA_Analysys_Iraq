test <- read.csv("test/choices.txt", header=T, sep="\t", fileEncoding = "UTF-16LE",blank.lines.skip = T,
                 encoding = "UTF-8", skipNul = T)
test <- read.csv("test/choices.csv", encoding = "UTF-8", stringsAsFactors = F)
test2 <- readLines("test/choices.txt", encoding = "UTF-16LE")
Sys.setlocale("LC_ALL","Arabic")


install.packages("googleLanguageR")
library(googleLanguageR)
gl_auth("test/My First Project-9df13c475b8e.json")
text <- test[200,4]
text <- "to administer medicince to animals is frequently a very difficult matter, and yet sometimes it's necessary to do so"
## translate British into Danish
gl_translate(text, target = "en")$translatedText

install.packages("RYandexTranslate")
library(RYandexTranslate)
source("yandex_function_fix.R")
api_key="trnsl.1.1.20190625T081348Z.a938a387b806253f.a84220af4d46bb9a83e50f7d634df7d49650aece"
data=detect_language(api_key,text=text)
data
data=translate(api_key,text=text,lang="ar-en"  )
data$text

install.packages("translateR")
