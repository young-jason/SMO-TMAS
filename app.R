##################################################################
##################################################################
# Twitter Data Analysis App
# Jason Young
# 14 October 2019
##################################################################
##################################################################

# library to attach to the Twitter API
if(!require('rtweet')) install.packages('rtweet')
library('rtweet')

# library to manipulate data
if(!require('dplyr')) install.packages('dplyr')
library('dplyr') #data wrangling
if(!require('lubridate')) install.packages('lubridate')
library('lubridate')
if(!require('tidytext')) install.packages('tidytext')
library('tidytext')
if(!require('stringr')) install.packages('stringr')
library('stringr') #string detecting
if(!require('tm')) install.packages('tm')
library('tm')

# library to visualize data
if(!require('plotly')) install.packages('plotly')
library('plotly') #interactive plots
if(!require('wordcloud')) install.packages('wordcloud')
library('wordcloud') #word clouds
if(!require('quanteda')) install.packages('quanteda')
library('quanteda') #text analysis
if(!require('stopwords')) install.packages('stopwords')
library('stopwords') #multilingual stopword lists
if(!require('RColorBrewer')) install.packages('RColorBrewer')
library('RColorBrewer') #color palettes used for word cloud
library('ggplot2')


# shiny related
if(!require('shiny')) install.packages('shiny')
library('shiny')
if(!require('shinydashboard')) install.packages('shinydashboard')
library('shinydashboard')
if(!require('DT')) install.packages('DT')
library('DT') #creating interactive tables that can display on HTML pages
if(!require('crosstalk')) install.packages('crosstalk')
library('crosstalk')

# Twitter authentication
token <- create_token(
  app = "PLACEHOLDER",
  consumer_key = 'PLACEHOLDER',
  consumer_secret = 'PLACEHOLDER',
  access_token = 'PLACEHOLDER',
  access_secret = 'PLACEHOLDER')

# Setting-up a function to harvest tweets of specific Twitter handles and arranging the results into a data frame 
pull_tweets <- function(twitter_handle, n){
  tweets <- get_timelines(c(twitter_handle),                                           
                          n = n, 
                          excludeReplies = TRUE) %>%                                        # source tweets
    dplyr::select(screen_name, description, text, created_at, retweet_count, favorite_count, source, hashtags, urls_expanded_url, media_type, 
                  mentions_screen_name, followers_count, friends_count, statuses_count, favourites_count, account_created_at, verified) %>%  # select useful rows
    dplyr::mutate(retweet_favorite_ratio = retweet_count/favorite_count,                 # ratio: retweet/favorite
                  day_of_week = wday(created_at, label = TRUE),                          # day of the week 
                  hour = hour(created_at),                                               # hour 
                  month = month(created_at))                                             # month
  return(tweets)
}

# Setting-up a function to harvest tweets of keywords / hashtags and arranging the results into a data frame 
pull_topics <- function(twitter_topic, n){
  tweets <- search_tweets(c(twitter_topic),                                            
                          n = n, 
                          include_rts = FALSE,                                            # exclude retweets
                          retryOnRateLimit=120) %>%                                       # wait and retry when rate limited
    dplyr::select(screen_name, description, text, created_at, retweet_count, favorite_count, source, hashtags, urls_expanded_url, media_type, 
                  mentions_screen_name, followers_count, friends_count, statuses_count, favourites_count, account_created_at, verified) %>%  
    # select useful rows
    dplyr::mutate(retweet_favorite_ratio = retweet_count/favorite_count,                 # ratio: retweet/favorite
                  day_of_week = wday(created_at, label = TRUE),                          # day of the week 
                  hour = hour(created_at),                                               # hour 
                  month = month(created_at))                                             # month
  return(tweets)
}

# Setting-up a function to create a bar chart depicting the days of the week that the tweets were created
tweets_per_day <- function(temp_tweets){
  freq_table <- data.frame(table(temp_tweets$day_of_week)) # forming a dataframe of frequency per day
  plot <- plot_ly(x = freq_table$Var1,                     # plotting 
                  y = freq_table$Freq,
                  type = "bar") %>% 
    layout(title = "Tweets per Day of the Week")
  
  return(plot)
}

# Setting-up a function to create a line plot depicting the time at which the tweets were created:
tweets_per_hour <- function(temp_tweets){
  freq_table <- data.frame(table(temp_tweets$hour))               # forming data frame of frequency per hour
  plot <- plot_ly(x = freq_table$Var1,                            # plotting 
                  y = freq_table$Freq,
                  type = "scatter",
                  mode = "line+markers") %>% 
    layout(title = "Distribution of Tweets in a Day",
           xaxis = list(title = "Hour"),
           yaxis = list(title = "Frequency"))
  return(plot)
}

# Setting-up a function to create a pie graph depicting the source from which the tweets were sent:
tweet_source <- function(temp_tweets){
  temp_df <- data.frame(table(temp_tweets$source))          # forming data frame of sources
  colnames(temp_df) <- c("source", "freq")                  # changing column names accorindingly
  
  pie <- plot_ly(data = temp_df,                            # plotting
                 labels = ~source,
                 values = ~freq,
                 type = "pie",
                 textinfo = 'label+percent',
                 textposition = 'inside')
  return(pie)
}

# Setting-up a function to create a histogram depicting the distributions of the retweet count and favorited count metrics
distributions <- function(temp_tweets){
  plot <- plot_ly(data = temp_tweets,         # plotting both RT and Fav distribution
                  alpha = 0.6) %>%
    add_histogram(x = ~retweet_count, name = "Retweet") %>%
    add_histogram(x = ~favorite_count, name = "Favorited") %>%
    layout(barmode = "overlay",
           xaxis = list(title = " "),
           title = "Distributions of Retweet and Favorited Counts")
  return(plot)
}

# Setting-up a function to create a word cloud
tweets_cloud <- function(temp_tweets){
  PostsCorpus <- iconv(temp_tweets,to = "ASCII",sub = "") #transforming text to remove all non-ASCII characters (emoticons, foreign characters etc. which are not represented properly)
  PostsCorpus <- corpus(PostsCorpus, text_field='text')   # Creating corpus of tweet text
  Corpus <- dfm(PostsCorpus, tolower = T, stem = T, remove_punct = T, remove_twitter = T, remove_numbers = T,
                remove_symbols = T, remove_separators = T, remove_hyphens = T, remove_url = T,
                remove = c(stopwords("en"), stopwords("de"), stopwords("fr"), stopwords("it"), 'rt'))
  #Creating dfm and cleaning text: removing German, English, French and Italian stopwords and words with low information value
  textplot_wordcloud(Corpus, color=brewer.pal(8, "Dark2"), min_size = 1.5, max_size = 7,  max_words = 100)
  return(textplot_wordcloud)
}

####  1) UI Component ####  
ui <- dashboardPage(
  # title
  dashboardHeader(title = "Twitter Data Analysis Application"),
  # menu
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "About",
        tabName = "About",
        icon = icon("twitter", lib = "font-awesome")
      ),
      menuItem(
        "Tweets",
        tabName = "Table",
        icon = icon("table", lib = "font-awesome")
      ),
      menuItem(
        "Statistics",
        tabName = "Charts",
        icon = icon("stats", lib = "glyphicon")
      ),
      menuItem(
        "Text Analysis",
        tabName = "Analysis",
        icon = icon("text-width", lib = "font-awesome")
      ),
      # text input for Twitter handles
      textInput(
        inputId = "twitter_handle",
        label = "Enter a Twitter Handle",
        width = 300,
        value = "BredowInstitut"
      ),
      # slider input
      sliderInput(
        inputId = "tweet_num",
        label = "Number of Tweets",
        min = 0, max = 3200,
        value = 3200
      ),
      # action button to scrape users
      actionButton(
        inputId = "click",
        width = 200,
        label = ("Download tweets"), br("of specific users"),
        icon = icon("users", lib = "font-awesome")
      ),
      # text input for keywords / hashtags 
      textInput(
        inputId = "twitter_topic",
        label = "Enter a Keyword / Hashtag",
        width = 300,
        value = "LeibnizWGL"
      ),
      # slider input
      sliderInput(
        inputId = "topic_num",
        label = "Number of Tweets",
        min = 0, max = 18000,
        value = 18000
      ),
      # action button to scrape hashtags
      actionButton(
        inputId = "click2",
        width = 200,
        label = ("Download tweets"), br("with specific keywords"),
        icon = icon("hashtag", lib = "font-awesome")
      )
    )
  ),
  # body
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "About",
        h2('Interactive Shiny App for Twitter Data Analysis', align = "center"), p(''),
        p(em('Leibniz-Institut für Medienforschung | Hans-Bredow-Institut'), align = "center"), p(em('Social Media Observatory'), align = "center"), 
        p("This app was created by Jason Young for basic Twitter data harvesting and analysis.", align = "center"), 
        p('It gathers public data from Twitter’s REST API by querying the GET statuses/user_timeline or search/tweets endpoint with the', tags$a(href="https://rtweet.info", 'rtweet package.'), align = "center"),
        p('Users are invited to enter a Twitter handle (no @ required) or keyword / hashtag (no # required) and specify a number of tweets to harvest.',
          style='font-family:arial; font-si16pt; color:red', align = "center"), 
        p('The first tab', span('Tweets', style='color:blue'), 'shows a searchable table of all scraped Tweets.', align = "center"),
        p('The second tab', span('Statistics', style ='color:blue'), 'shows interactive charts to analyse the meta data of the scraped Tweets.', align = "center"), 
        p('The third tab', span('Text Analysis', style='color:blue'), 'shows interactive charts to analyse the text of the scraped Tweets.', align = "center"),
        p(''),
        p(em('NOTICE: The app can return up to 3,200 of the most recent Tweets of a user and up to 18,000 (non-retweeted) tweets containing specific search terms.
             There is a limit of 100,000 requests per day to the statuses/user_timeline endpoint (scrape users) and the app should be used in strict accordance with', tags$a(href="https://developer.twitter.com/en/developer-terms/more-on-restricted-use-cases", 'Twitter’s developer terms.'), 
             'The search/tweets endpoint (scrape keywords) only returns data from the past 6-9 with a focus on relevancy and not completeness. The scraped data is equivalent to the information publicly seen on Twitter. 
             That is, tweets belonging to protected users are not returned.'), align = "center")
        ),
      tabItem(
        tabName = "Table",
        h2("Table of scraped Tweets", align = "center"),
        p("The table with the text and further meta data of the scraped tweets can be downloaded by pressing the download botton on the bottom left.", align = "center"),
        p("The ratio is calculated by dividing the retweetCount through the favoriteCount.", align = "center"),
        DT::dataTableOutput(outputId = "x1"),
        downloadButton("download", "Download File")
      ),
      tabItem(
        tabName = "Charts",
        fluidPage(
          h2("Charts of scraped Tweets", align = "center"),
          p("The interactive charts below visualize the meta data of the scraped Tweets", align = "center"),
          box(plotlyOutput(outputId = "tweets_per_day")),
          box(plotlyOutput(outputId = "tweets_per_hour")),
          box(plotlyOutput(outputId = "metric_histograms")),
          box(plotlyOutput(outputId = "tweet_source"))
        )
      ),
      tabItem(
        tabName = "Analysis",
        h2("Text analysis of scraped Tweets", align = "center"),
        p("The chart below shows a world cloud of the 100 most used words in the scraped Tweets.", align = "center"),
        p("The color and size of the words indicates their frequency.", align = "center"),
        box(plotOutput(outputId = "tweets_cloud"))
      )
      )
    )
  )
### 2) Server Component ###
server <- function(input, output, ...){
  observeEvent(input$click, {
    # defining user handle and sample size
    handle <- isolate({as.character(input$twitter_handle)})
    sample_size <- isolate({input$tweet_num})
    
    # pulling tweets
    temp_tweets <- reactive({
      pull_tweets(handle, sample_size)
    })
    
    # bar chart depicting the days of the week that the tweets were created
    output$tweets_per_day <- renderPlotly({
      tweets_per_day(temp_tweets())
    })
    
    # line plot depicting the time at which the tweets were created
    output$tweets_per_hour <- renderPlotly({
      tweets_per_hour(temp_tweets())
    })
    
    #histogram depicting the distributions of the retweet count and favorited count metrics
    output$metric_histograms <- renderPlotly({
      distributions(temp_tweets())
    })
    
    # pie graph depicting the source from which the tweets were sent:
    output$tweet_source <- renderPlotly({
      tweet_source(temp_tweets())
    })
    
    # table
    m <- temp_tweets() %>% 
      tibble::rownames_to_column()
    d <- SharedData$new(m, ~rowname)
    
    output$x1 <- DT::renderDataTable({
      m2 <- m[d$selection(),]
      dt <- DT::datatable(m)
      if (NROW(m2) == 0) {
        dt
      } else {
        DT::formatStyle(dt, "rowname", target = "row",
                        color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                        backgroundColor = DT::styleEqual(m2$rowname, rep("blue", length(m2$rowname))))
      }
    })
    output$download <- downloadHandler(filename = function() {paste(input$handle, '.csv', sep='')},
                                       content = function(file){
                                         write.csv(temp_tweets(), file)
                                       })
    # word cloud
    output$tweets_cloud <- renderPlot({
      tweets_cloud(temp_tweets())
    })
  })
  
  observeEvent(input$click2, {
    # defining topic and sample size
    handle <- isolate({as.character(input$twitter_topic)})
    sample_size <- isolate({input$topic_num})
    
    # pulling topics
    temp_tweets <- reactive({
      pull_topics(handle, sample_size)
    })
    
    # bar chart depicting the days of the week that the tweets were created
    output$tweets_per_day <- renderPlotly({
      tweets_per_day(temp_tweets())
    })
    
    # line plot depicting the time at which the tweets were created
    output$tweets_per_hour <- renderPlotly({
      tweets_per_hour(temp_tweets())
    })
    
    #histogram depicting the distributions of the retweet count and favorited count metrics
    output$metric_histograms <- renderPlotly({
      distributions(temp_tweets())
    })
    
    # pie graph depicting the source from which the tweets were sent:
    output$tweet_source <- renderPlotly({
      tweet_source(temp_tweets())
    })
    
    # table
    m <- temp_tweets() %>% 
      tibble::rownames_to_column()
    d <- SharedData$new(m, ~rowname)
    
    output$x1 <- DT::renderDataTable({
      m2 <- m[d$selection(),]
      dt <- DT::datatable(m)
      if (NROW(m2) == 0) {
        dt
      } else {
        DT::formatStyle(dt, "rowname", target = "row",
                        color = DT::styleEqual(m2$rowname, rep("white", length(m2$rowname))),
                        backgroundColor = DT::styleEqual(m2$rowname, rep("blue", length(m2$rowname))))
      }
    })
    output$download <- downloadHandler(filename = function() {paste(input$handle, '.csv', sep='')},
                                       content = function(file){
                                         write.csv(temp_tweets(), file)
                                       })
    # word cloud
    output$tweets_cloud <- renderPlot({
      tweets_cloud(temp_tweets())
    })
  })
}


### 3) shinyApp component ###
shinyApp(ui = ui, server = server)