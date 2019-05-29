# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

# load libraries
require(plyr)
require(data.table)
require(tidyverse)
require(xgboost)
library(stringi)
map.func = function(x, y = 2){
  map = x %>% 
    sapply(FUN = function(x){strsplit(x, '[,.:""]')[[1]][y]}) %>% 
    sub("[[:punct:]]", '',.) %>% 
    sub("'",'',.) %>% as.factor() %>% as.numeric()
  return(map)
}

######### data
df = read_csv("../input/train.csv")
test = read_csv("../input/test.csv")

for (i in 1:nrow(df)) {
  if(df[i,"budget"] > 1000 & df[i,"revenue"] < 100){
    df[i,"revenue"] = df[i,"revenue"] * 10^6
   }
}

train.id = df$id
label = df$revenue
test.id = test$id

df = df %>% within(rm("id","revenue"))
test = test %>% within(rm("id"))
df = rbind(df,test)
train_raw = df
date.format <- as.Date(df$release_date, format="%m/%d/%Y")

##### MISSINGG VAUES
df[df== ""] <- NA
#check for columns with missing values
na.cols <- which(colSums(is.na(df)) > 0)
na.cols <- sort(colSums(sapply(df[na.cols], is.na)), decreasing = TRUE)
paste('There are', length(na.cols), 'columns with missing values')


####
sel.cols = c("budget","original_language","popularity","spoken_languages",
             "production_countries","genres","runtime","tagline_nword",
             "tag_hasNA","lan_isEN","same_title","crew_len","cast_len",
             "crewlen_nword_div","homepage_hasNA","collection_hasNA",
             "tag_length","keywords_len","year","month","weekday","week",
             "p_comp_len","dayofweek2","cast_nword")
###
year.fun = function(x){
  if(is.na(x)){
    return(paste0("2000"))
  }else if(x < 10){
    return(paste0("200",x))
  }else if(x >=10 & x <= 18){
    return(paste0("20", x))
  }else{
    return(paste0("19",x))
  }
}
###
###
df = df %>% 
  mutate(year = year(date.format),
         year = sapply(year, year.fun) %>% as.numeric(),
         month = month(date.format),
         weekday =as.numeric(as.factor(weekdays(date.format))),
         quarter = year * 4 + (month) %/% 3 - 8051, 
         dayofweek2 = (as.numeric(lubridate::days(date.format)) + 3) %% 7,
         week = week(date.format),
         cast_len = str_length(cast),
         cast_nword = str_count(cast, "\\w+"),
         p_comp_len = str_length(production_companies),
         p_comp_nword = str_count(production_companies, "\\w+"),
         tag_length = str_length(tagline),
         tagline_nword = str_count(tagline, "\\w+"),
         tag_hasNA = ifelse(is.na(tagline),0,1),
         lan_isEN = ifelse(original_language =="en",1,0),
         collection_id = map.func(belongs_to_collection),
         collection_hasNA = ifelse(is.na(belongs_to_collection),0,1),
         homepage_hasNA = ifelse(is.na(homepage),1,0),
         same_title = ifelse(title == original_title,1,0),
         crew_len = str_length(crew),
         crew_nword = str_count(crew, "\\w+"),
         crewlen_nword_div = crew_len / crew_nword,
         keywords_len = str_length(Keywords),
         keywords_nword = str_count(Keywords,"\\w+"),
         production_countries = map.func(production_countries),
         comp1 = map.func(production_companies),
         spoken_languages = map.func(spoken_languages),
         genres = map.func(genres),
         status = as.numeric(as.factor(status)),
         original_title = as.numeric(as.factor(original_title)),
         original_language = as.numeric(as.factor(original_language))) %>% 
  select(sel.cols)
  
###  Borrow aome ideas from this kernel https://www.kaggle.com/wallgren/boxofficeprediction-using-randomforest
train = train_raw %>%
  separate(belongs_to_collection, 'idPart', sep = 'name', remove = TRUE) %>%
  separate(release_date, c('releaseMonth', 'releaseDay', 'releaseYear'), sep = '/', remove = TRUE) %>%
  mutate(collectionID = ifelse(is.na(idPart) == FALSE, gsub("\\D", "", idPart), idPart),
         collectionID = ifelse(is.na(collectionID) == TRUE, 0, collectionID),
         partOfCollection = ifelse(is.na(idPart) == FALSE, 1, 0),
         hasHomePage = ifelse(is.na(homepage) == TRUE, 1, 0),
         genres = ifelse(is.na(genres) == TRUE, 'NoGen', genres),
         genComedy = ifelse(stri_detect_fixed(genres, 'Comedy'),1, 0),
         genDrama = ifelse(stri_detect_fixed(genres, 'Drama'),1, 0),
         genThriller = ifelse(stri_detect_fixed(genres, 'Comedy'),1,0),
         genAction = ifelse(stri_detect_fixed(genres, 'Action'),1,0),
         genAnimation = ifelse(stri_detect_fixed(genres, 'Comedy'),1,0),
         genHorror = ifelse(stri_detect_fixed(genres, 'Horror'),1, 0),
         genDocumentary = ifelse(stri_detect_fixed(genres, 'Documentary'),1,0),
         genAdventure = ifelse(stri_detect_fixed(genres, 'Adventure'),1, 0),
         genCrime = ifelse(stri_detect_fixed(genres, 'Crime'),1, 0),
         genMystery = ifelse(stri_detect_fixed(genres, 'Mystery'),1, 0),
         genFantasy = ifelse(stri_detect_fixed(genres, 'Fantasy'),1, 0),
         genWar = ifelse(stri_detect_fixed(genres, 'War'),1, 0),
         genScienceFiction = ifelse(stri_detect_fixed(genres, 'Science Fiction'),1, 0),
         genRomance = ifelse(stri_detect_fixed(genres, 'Romance'),1, 0),
         genMusic = ifelse(stri_detect_fixed(genres, 'Music'),1, 0),
         genWestern = ifelse(stri_detect_fixed(genres, 'Western'),1, 0),
         genFamily = ifelse(stri_detect_fixed(genres, 'Family'),1, 0),
         genHistory = ifelse(stri_detect_fixed(genres, 'Comedy'),1, 0),
         genForeign = ifelse(stri_detect_fixed(genres, 'Foreign'),1,0),
         genTVMovie = ifelse(stri_detect_fixed(genres, 'TV Movie'),1,0),
         #US = ifelse(stri_detect_fixed(production_countries, 'United States of America'),1,0),
         #Russia= ifelse(stri_detect_fixed(production_countries, 'Russia'),1,0),
         #Keywords1 = ifelse(stri_detect_fixed(Keywords, 'independent film'),1,0),
         #keywords2 = ifelse(stri_detect_fixed(Keywords, 'duringcreditsstinger'),1,0),
         # keywords3 = ifelse(stri_detect_fixed(Keywords, 'aftercreditsstinger'),1,0),
         #keywords2 = ifelse(stri_detect_fixed(Keywords, 'sequel'),1,0),
         production_companies = ifelse(is.na(production_companies) == TRUE, 'NoProd', production_companies),
         prodUniversal = ifelse(stri_detect_fixed(production_companies, 'Universal Pictures'),1, 0),
         prodParamount = ifelse(stri_detect_fixed(production_companies, 'Paramount Pictures'),1, 0),
         prodTCF = ifelse(stri_detect_fixed(production_companies, 'Twentieth Century Fox Film Corporation'),1, 0),
         prodColumbia = ifelse(stri_detect_fixed(production_companies, 'Columbia Pictures'),1, 0),
         #  prodDune = ifelse(stri_detect_fixed(production_companies, 'Dune Entertainment'),1, 0),
         # prodDream = ifelse(stri_detect_fixed(production_companies, 'DreamWorks SK'),1, 0),
         # prodEon = ifelse(stri_detect_fixed(production_companies, 'Eon Productions'),1, 0),
         prodWarner = ifelse(stri_detect_fixed(production_companies, 'Warner Bros.'),1, 0),
         prodNLC = ifelse(stri_detect_fixed(production_companies, 'New Line Cinema'),1, 0),
         prodDisney = ifelse(stri_detect_fixed(production_companies, 'Walt Disney Pictures'),1,0),
         prodColumbiaPictures = ifelse(stri_detect_fixed(production_companies, 'Columbia Pictures Corporation'),1,0),
         prodTriStar = ifelse(stri_detect_fixed(production_companies, 'TriStar Pictures'),1, 0),
         sizeOfCast = str_count(cast, 'cast_id'), # Size of cast
         sizeOfCrew = str_count(crew, 'name'), # Size of crew
         sizeOfCrew = ifelse(is.na(sizeOfCrew), 0, sizeOfCrew),
         numberOfGenres = str_count(genres, 'name'),
         #prodMGM = ifelse(stri_detect_fixed(production_companies, 'Metro-Goldwyn-Mayer (MGM)'),1, 0),
         # prodOG = ifelse(stri_detect_fixed(production_companies, 'Original Film'),1, 0),
         #prodUnitedArtists = ifelse(stri_detect_fixed(production_companies, 'United Artists'),1,0),
         #prodMiramax = ifelse(stri_detect_fixed(production_companies, 'Miramax Films'),1, 0),
         #prodRelativlyM = ifelse(stri_detect_fixed(production_companies, 'Relativity Media'),1, 0),
         #prodVRP = ifelse(stri_detect_fixed(production_companies, 'Village Roadshow Pictures'),1, 0),
         # prodTSG = ifelse(stri_detect_fixed(production_companies, 'TSG Entertainment'),1, 0),
         # prodFox2000 = ifelse(stri_detect_fixed(production_companies, 'Fox 2000 Pictures'),1, 0),
         prodFoxSearchlight2 = ifelse(stri_detect_fixed(production_companies, 'Fox Searchlight Pictures'),1, 0),
         # releaseYear = ifelse(as.integer(releaseYear) <= 18, paste0('20', releaseYear), paste0('19', releaseYear)),
         #release_date = as.Date(paste(releaseYear, releaseMonth, releaseDay, sep = '-')),
         #age = as.integer(today() - release_date) / 365,
         #quarterRelease = quarter(release_date),
         #weekRelease = week(release_date),
         #dayRelease = wday(release_date),
         # budget = ifelse(budget < 1000, mBudgetTrain, budget),
         # runtime = ifelse(is.na(runtime) == TRUE, mRuntimeTrain, runtime),
         collectionID = as.factor(collectionID)) %>%  
  group_by(collectionID) %>%
  mutate(sizeOfCollection = n()) %>%
  ungroup() %>%
  mutate(sizeOfCollection = ifelse(sizeOfCollection > 1000, 0, sizeOfCollection)) %>%
  select(-idPart, -homepage, -imdb_id, -poster_path, -original_title, -genres, -overview, 
         -tagline, -production_companies, -spoken_languages, -cast, -crew, -Keywords, 
         -production_countries, -status, -releaseYear, -releaseMonth, -releaseDay,
         -title, -collectionID)

train = train[,9:ncol(train)]
df = cbind(df,train)
rm(train)

## few interactions
df$fe2 = df$budget / df$popularity
df$fe3 = df$budget / (df$year* df$year)
df$fe4 = df$year / df$popularity
df$fe5 = df$popularity * df$runtime

# Split the data set
df_train <- df[1:length(train.id),]
df_test <- df[(length(train.id)+1):nrow(df),]

label2 = log1p(label)
#####
dtrain <- xgb.DMatrix(as.matrix(df_train), label = label2)
dtest <- xgb.DMatrix(as.matrix(df_test))

# Fit Model 
param <- list(booster="gbtree",
              eta=0.03,
              colsample_bytree = 0.3,
              max_depth = 6,
              min_child_weight = 2,
              base_score = mean(label2),
              subsample = 0.9)

set.seed(1235)
mod.xgb <- xgb.train(data=dtrain,params = param, nrounds= 276,print_every_n = 50)

# Predict on test set
pred = predict(mod.xgb, newdata = dtest)
pred = exp(pred)-1
sub <- data.frame(test.id, pred)
colnames(sub) <- c("id", "revenue")
write.csv(sub, file =paste0(Sys.Date(),"_xgb.csv"), row.names = FALSE)




# Any results you write to the current directory are saved as output.