# R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(httr)
library(rvest)
library(XML)

# Data Import and Cleaning
google_scholar <- function(url) {
  total_search <- read_html(url)
  search_text <- html_text(html_nodes(total_search,("#gs_ab_md .gs_ab_mdw")))
  num_hit <- as.integer(str_extract(search_text,"(?<=About )\\d+(?= results)")) # get number of hits
  num_pages <- ceiling(num_hit/20) # how many pages when 20 per page
  first <- 1:num_pages*20-20
  
  # Collect urls as list (one list per page, 20 on each page)
  urls <- paste("http://scholar.google.com/scholar?start=",first,"&q=",search,"&num=100&as_sdt=1&as_vis=1",sep="")
  
  per_page <- function(x) {
    doc <- htmlParse(rawToChar(GET(urls[[1]])$content),encoding="UTF-8")
    title <- xpathSApply(doc,"//h3[@class='gs_rt']",xmlValue) # titles
    publication <- xpathSApply(doc,"//div[@class='gs_a']",xmlValue) # publications
    link <- xpathSApply(doc,"//h3[@class='gs_rt']/a",xmlAttrs)["href",] # links 
    dat <- data.frame(Title=title,Publication=publication,Link=link) # combine the data
    return(dat)
  }
  
  # run per_page function for each page available to extract info for the query and rbind all results
  scholar_data <- do.call("rbind",lapply(urls,per_page))
  return(scholar_data)
}

search <- "'covid-19'+source:psychology"
covid <- google_scholar(url=paste("http://scholar.google.com/scholar?q=",search,"&num=1&as_sdt=1&as_vis=1",sep=""))

covid <- covid %>%
  mutate(Authors=str_extract(Publication,".*?(?=-)"), # grab everything before the first - as author list
         Journal=str_trim(tolower(str_extract(Publication,"(?<=-).*?(?=-|\\d{4}|,)"))), # grab everything after the first - and before the second - or the 4 digit year; turn to all lowercase then remove whitespace
         Year=str_extract(Publication,"\\d{4}")) 
covid_count <- covid %>%
  group_by(Journal,Year) %>%
  count(sort=T) %>%
  top_n(n,10) %>%
  filter(!is.na(Year))

# Data Visualization
ggplot(covid_count,aes(x=Year,y=n,color=Journal)) +
  geom_point()
  
  