#### Libraries and locale ####
dir.create("/Users/ewynn/Documents/R/Aeroflot/ESS_button")
setwd("/Users/ewynn/Documents/R/Aeroflot/ESS_button")

require(RGoogleAnalytics)
require(scales)
library(readxl)
library(plotly)
library(dplyr)
library(ggplot2)

Sys.setlocale(category = "LC_ALL", locale = "RU_ru")

client.id <- "424432578443-o5aolvvfe82id0mi8e62m9fpmd69ub4j.apps.googleusercontent.com"
client.secret <- "nUt-tflwFadf_dYF2FVbVdx5"
token <- Auth(client.id,client.secret)
save(token, file="./token_file")
ValidateToken(token)


####  Defining functions  ####
getData <- function(filters, dimensions, dates){
  #this functions obtaining data via Google Analytics 
  data.metrics = "ga:users" #metrics
  data.dimensions = paste0("ga:date, ga:dimension21,",dimensions)
  splitDaywise = TRUE
  query.list <- Init(start.date = dates[['start']], 
                     end.date = dates[['end']], 
                     metrics = data.metrics, 
                     dimensions = data.dimensions,
                     filters = filters, 
                     max.results = 10000,
                     table.id = "ga:85649231")
  funnel <- GetReportData(QueryBuilder(query.list), token, 
                          paginate_query = F, 
                          split_daywise = splitDaywise)
  return(funnel)
}

remove_variants <- function(df, by, column, variants){
  #Remove intersections in variants 
  temp <- merge(df[df[,column] == variants[1], ], df[df[,column] == variants[2], ],
                by = c(by))
  df <- subset(df, ! is.element(df[, by], temp[,by]))
  return(df)
}

create_funnel <- function(df, first_name, what, by, date = TRUE){
  #Create hard funnel from raw data frame
  #first name - name of the first column from which the aggregation works
  #what - x in aggregation function
  #by - by in aggregateion function
  
  start <- which(names(df) == first_name)
  end <- length(names(df))
  row <- 2
  if (missing(date)) {
    df_aggr <- aggregate(df[, what], by = list(df[,by]), function(x) length(unique(x)))
    for (i in ((start+1):end) ){
      temp <- df[rowSums(df[, (start:i)]) == row,]
      temp <- aggregate(temp[,what], by = list(temp[,by]), function(x) length(unique(x)))
      df_aggr[,names(df)[i]] <- temp[,'x']
      row <- row + 1
    }
    names(df_aggr) <- c('variant', names(df)[start:end])
  } else {
    df_aggr <- aggregate(df[, what], by = list(df[,by], df[,'date']), function(x) length(unique(x)))
    for (i in ((start+1):end) ){
      temp <- df[rowSums(df[, (start:i)]) == row,]
      temp <- aggregate(temp[,what], by = list(temp[,by], temp[,'date']), function(x) length(unique(x)))
      df_aggr[,names(df)[i]] <- temp[,'x']
      row <- row + 1
    }
    names(df_aggr) <- c('variant', 'date', names(df)[start:end])
  }
  return(df_aggr)
}

add_conf.int <- function(df, main, target){
  for (i in (1:length(df[,main]))){
    temp_min[i] <- 100*binom.test(df[, target][i], 
                                  df[, main][i], conf.level = 0.95)$conf.int[1]
    temp_max[i] <- 100*binom.test(df[, target][i], 
                                  df[, main][i], conf.level = 0.95)$conf.int[2]
  }
  df[,paste(target,'_min',sep='')] <- temp_min
  df[,paste(target,'_max',sep='')] <- temp_max
  return(df)
}

progress_bar <- function(total_bars, progress){
  #total_bars - visual length of progress bar
  #progress - amount of complete loops / total amount of loops (number from 0 to 1)
  bars <- floor(total_bars * progress)
  fill <- strrep('???', bars)
  space <- strrep('???', total_bars - bars)
  result <- paste('[:', fill, space, ':] - ', format(round(progress*100,2), nsmall = 2 ), "%", sep = "")
  cat('\r', result)
}

####  Defining variables  ####
date = list(start = "2018-08-04", end = "2018-08-18")
gloss <- list('APPROVED','DEPOSITED','REFUNDED')

filter_exp_id <- "ga:experimentId==qhSTZDvwQeKvYXlQlcuMrg"
filter_transaction <- "ga:transactionId=~2018"
filter_button_visible <- "ga:eventAction==A_on_ess_payment_visible_on_bottom"
filter_button_click <- "ga:eventAction==A_on_ess_payment_click_on_bottom"

variant <- "ga:experimentVariant"
label <- "ga:eventLabel"
transaction <- "ga:transactionId"


#### Downloading Data #### 
users <- getData(filter_exp_id, variant, date)

action_visible <- getData(paste(filter_exp_id, filter_button_visible, sep=';'), variant, date)
action_click <- getData(paste(filter_exp_id, filter_button_click, sep=';'), variant, date)

transactions <- getData(filter_transaction, transaction, date)

aggregate(users$dimension21, by = list(users$experimentVariant), function(x) length(unique(x)))
aggregate(action_visible$dimension21, by = list(action_visible$experimentVariant), function(x) length(unique(x)))
aggregate(action_click$dimension21, by = list(action_click$experimentVariant), function(x) length(unique(x)))


#### Editing Data ####
names(users)[4] <- "total"
names(action_visible)[4] <- "visible"
names(action_click)[4] <- "click"

head(users)

funnel <- Reduce(function(x, y) 
  merge(x, y, by = c('date', 'dimension21', 'experimentVariant'), all.x = TRUE), 
  list(users, action_visible, action_click))
funnel[is.na(funnel)] <- 0
funnel$date <- as.Date(funnel$date, "%Y%m%d")
names(funnel) <- c('date', 'cid', 'var', 'total', 'visible', 'click')
head(funnel)

funnel_aggr <- create_funnel(funnel, 'total', 'cid', 'var')
funnel_aggr
