# Twitter Data NLP Shiny App

library(shiny)
library(tidyverse)
library(lubridate)
library(rtweet)
library(tm)
library(qdap)
library(textstem)
library(wordcloud)
library(wesanderson)

# Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("App for Analyzing Twitter COVID Data with Natural Language Processing"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("hashtag",
                        "Which hashtag would you like to use?",
                        c("#COVID "="#COVID ",
                          "#COVID19"="#COVID19",
                          "#COVID-19"="#COVID-19",
                          "#COVID_19"="#COVID_19"),
                        selected="COVID "),
        ),
        
        # Show table, wordcloud, and summary
        mainPanel(
            tableOutput("table"),
            plotOutput("wordcloud"),
            tableOutput("summary"),
            plotOutput("bar")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    token <- create_token(app="Charlene Zhang R Interface",
                          consumer_key="xxx",
                          consumer_secret="xxx",
                          access_token="xxx",
                          access_secret="xxx")
    dat <- rbind(search_tweets("#COVID ",
                               n=500,
                               include_rts=F,
                               lang="en",
                               token=token),
                 search_tweets("#COVID19",
                               n=500,
                               include_rts=F,
                               lang="en",
                               token=token),
                 search_tweets("#COVID-19",
                               n=500,
                               include_rts=F,
                               lang="en",
                               token=token),
                 search_tweets("#COVID_19",
                               n=500,
                               include_rts=F,
                               lang="en",
                               token=token)) %>%
        select(text)
    
    # Preprocessing function
    preprocessing <- function(x){
        myCorpus <- VCorpus(VectorSource(x$text)) 
        myCorpus <- tm_map(myCorpus,PlainTextDocument)
        myCorpus <- tm_map(myCorpus,content_transformer(replace_abbreviation))
        myCorpus <- tm_map(myCorpus,content_transformer(replace_contraction))
        myCorpus <- tm_map(myCorpus,content_transformer(str_to_lower))
        myCorpus <- tm_map(myCorpus,content_transformer(rm_url)) 
        myCorpus <- tm_map(myCorpus,removeNumbers)
        myCorpus <- tm_map(myCorpus,removePunctuation)
        myCorpus <- tm_map(myCorpus,removeWords,c(stopwords("en"),"covid")) # remove the word covid
        myCorpus <- tm_map(myCorpus,stripWhitespace)
        tweet_Corpus <- tm_map(myCorpus,content_transformer(lemmatize_strings))
        twitter_dtm <- DocumentTermMatrix(tweet_Corpus) %>%
            removeSparseTerms(.99)
        cleaned_dtm <- as_tibble(as.matrix(twitter_dtm[apply(twitter_dtm,1,sum)>0,]))
        return(cleaned_dtm)
    }
    
    # # Interactively choose subset of tweets
    # df_subset <- reactive({
    #   tweet_subset <- dat[grep(input$hashtag,ignore.case=T,dat$text),"text"] # subsetted data based on hashtag
    #   return(tweet_subset)
    # })
    
    # Word count for each of the four subsets
    count <- list()
    for(i in c("#COVID ","#COVID19","#COVID-19","#COVID_19")){
        tweet_subset <- dat[grep(i,ignore.case=T,dat$text),"text"]
        cleaned_dtm <- preprocessing(tweet_subset)
        count[[i]] <- tibble(words=colnames(cleaned_dtm),
                             freq=apply(cleaned_dtm,2,sum)) %>%
            arrange(desc(freq))
    }
    
    # display head of the table using hashtag selected
    output$table <- renderTable({
        head(dat[grep(input$hashtag,dat$text),"text"])
    })
    
    # display wordcloud
    output$wordcloud <- renderPlot({
        
        # draw the scatterplot
        wordcloud(words=count[[input$hashtag]]$words,
                  freq=count[[input$hashtag]]$freq,
                  colors=wes_palette("Chevalier1"),
                  scale=c(3,.5), # range of font sizes
                  random.order=F,
                  max.words=50,
                  main="Wordcloud of the 50 Most Common Words in Tweets")
    })
    
    # display summary table
    output$summary <- renderTable({
        # pick out terms with over 5 mentions for each hashtag
        top_terms <- lapply(count,function(x) x%>%
                                filter(freq>5)%>%
                                select(words)) 
        data.frame(Hashtag1=c("#COVID ","#COVID ","#COVID ","#COVID19","#COVID19","#COVID-19"),
                   Hashtag2=c("#COVID19","#COVID-19","#COVID_19","#COVID-19","#COVID_19","#COVID_19"),
                   TopOverlap=c(length(intersect(top_terms[[1]]$words,top_terms[[2]]$words)),
                                length (intersect(top_terms[[1]]$words,top_terms[[3]]$words)),
                                length (intersect(top_terms[[1]]$words,top_terms[[4]]$words)),
                                length(intersect(top_terms[[2]]$words,top_terms[[3]]$words)),
                                length(intersect(top_terms[[2]]$words,top_terms[[4]]$words)),
                                length(intersect(top_terms[[3]]$words,top_terms[[4]]$words))))
    })
    
    # display top 20 terms of all hashtags
    output$bar <- renderPlot({
        count_overall <- count[[1]] %>%
            full_join(count[[2]],by="words",suffix=c("COVID ","COVID19")) %>%
            full_join(count[[3]],by="words") %>%
            full_join(count[[4]],by="words",suffix=c("COVID-19","COVID_19")) %>%
            mutate(totalFreq=rowSums(.[2:5],na.rm=T)) %>%
            arrange(desc(totalFreq)) %>%
            top_n(20)
        ggplot(count_overall,aes(x=reorder(words,-totalFreq),y=totalFreq))+
            geom_bar(stat="identity",fill=wes_palette("Chevalier1",1))+
            theme(axis.text.x=element_text(angle=45,hjust=1))+
            ggtitle("Top 20 Common Words Across All Four Hashtags")+
            xlab("Words")+
            ylab("Total Frequency")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

# Link to online app
## https://zhan5449.shinyapps.io/nlpapp/
