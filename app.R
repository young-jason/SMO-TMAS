##################################################################
##################################################################
# Social Media Observatory - Twitter Mining and Analysis Suite [SMO-TMAS]
# Jason Young
# 24 October 2019
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


# shiny related library
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
  app = "Delibration index",
  consumer_key = 'PLACEHOLDER',
  consumer_secret = 'PLACEHOLDER',
  access_token = 'PLACEHOLDER',
  access_secret = 'PLACEHOLDER')

# Setting-up a function to harvest tweets of specific Twitter handles and arranging the results into a data frame 
pull_tweets <- function(twitter_handle, n){
  withProgress(message = "Collecting tweets...", value = 0, {                            # Prompting the user that tweet collection has started
  tweets <- get_timelines(twitter_handle,                                                # twitter_handle = text input by user in text box
                          n = n,                                                         # n =  specified number of tweets by user
                          excludeReplies = TRUE) %>%                                     # Excluding replies (i.e. only source tweets)
    dplyr::select(screen_name, description, text, created_at, retweet_count,             # Selecting useful rows
                  favorite_count, source, hashtags, urls_expanded_url, media_type, 
                  mentions_screen_name, followers_count, friends_count, statuses_count, 
                  favourites_count, account_created_at, verified) %>%  
    dplyr::mutate(retweet_favorite_ratio = retweet_count/favorite_count,                 # Calculating retweet/favorite ratio
                  day_of_week = wday(created_at, label = TRUE),                          # Converting created_at to day of the week 
                  hour = hour(created_at),                                               # Converting created_at to hour  
                  month = month(created_at))                                             # Converting created_at to month 
  incProgress(message = "Finished collecting tweets!")                               # Prompting the user that tweet collection has finished
  return(tweets)
  })
}

# Setting-up a function to harvest tweets of keywords / hashtags and arranging the results into a data frame 
pull_topics <- function(twitter_topic, n){
  withProgress(message = "Collecting tweets...", value = 0, {                            # Prompting the user that tweet collection has started
  tweets <- search_tweets(twitter_topic,                                                 # twitter_topic = text input by user in text box
                         n = n,                                                          # n =  specified number of tweets by user
                         include_rts = FALSE,                                            # Excluding retweets (i.e. only source tweets)
                         retryOnRateLimit=120) %>%                                       # wait and retry when rate limited
    dplyr::select(screen_name, description, text, created_at, retweet_count, source,     # Selecting useful rows
                  favorite_count, hashtags, urls_expanded_url, media_type, 
                  mentions_screen_name, followers_count, friends_count, statuses_count, 
                  favourites_count, account_created_at, verified) %>%  
    dplyr::mutate(retweet_favorite_ratio = retweet_count/favorite_count,                 # Calculating retweet/favorite ratio
                  day_of_week = wday(created_at, label = TRUE),                          # Converting created_at to day of the week  
                  hour = hour(created_at),                                               # Converting created_at to hour   
                  month = month(created_at))                                             # Converting created_at to month 
  incProgress(message = "Finished collecting tweets!")                                   # Prompting the user that tweet collection has finished
  return(tweets)
  })
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

# Setting-up a function to create a line plot depicting the time at which the tweets were created
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

# Setting-up a function to create a pie graph depicting the source from which the tweets were sent
tweet_source <- function(temp_tweets){
  temp_df <- data.frame(table(temp_tweets$source))          # forming data frame of sources
  colnames(temp_df) <- c("source", "freq")                  # changing column names accorindingly
  
  pie <- plot_ly(data = temp_df,                            # plotting
                 labels = ~source,
                 values = ~freq,
                 type = "pie",
                 textinfo = 'label+percent',
                 textposition = 'inside')%>%
    layout(title = "Distributions of Tweet Sources")
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
                remove = c(stopwords("en"), stopwords("de"), stopwords("fr"), stopwords("it"), 'rt', 'na'))
  #Creating dfm and cleaning text: removing German, English, French and Italian stopwords and words with low information value
  wordcloud <- textplot_wordcloud(Corpus, color=brewer.pal(8, "Dark2"), min_size = 0.5, max_size = 7,  max_words = 100)
  return(wordcloud)
}

# Setting-up a function to create a word frequency table
word_frequency <- function(temp_tweets){
  PostsCorpus <- iconv(temp_tweets,to = "ASCII",sub = "") #t ransforming text to remove all non-ASCII characters (emoticons, foreign characters etc. which are not represented properly)
  PostsCorpus <- corpus(PostsCorpus, text_field='text')   # Creating corpus of tweet text
  Corpus <- dfm(PostsCorpus, tolower = T, stem = T, remove_punct = T, remove_twitter = T, remove_numbers = T,
                remove_symbols = T, remove_separators = T, remove_hyphens = T, remove_url = T,
                remove = c(stopwords("en"), stopwords("de"), stopwords("fr"), stopwords("it"), 'rt'))
  #Creating dfm and cleaning text: removing German, English, French and Italian stopwords and words with low information value
  frequency <- textstat_frequency(Corpus)              # word frequency table
  return(frequency)
}

# Setting-up a function to create a network of usernames
top_users <- function(temp_tweets){
  PostsCorpus <- corpus(temp_tweets, text_field='text')   # Creating corpus of tweet text
  Corpus <- dfm(PostsCorpus)                              # Converting into DFM
  user_dfm <- dfm_select(Corpus, pattern = "@*")          # Selecting  @*mentions
  topuser <- names(topfeatures(user_dfm, 50))             # Getting the names of the top 50 users  
  user_fcm <- fcm(user_dfm)                               # Constructing feature-occurrence matrix of users
  user_fcm <- fcm_select(user_fcm, pattern = topuser)     # Selecting the top 50 users  
  user_network <- textplot_network(user_fcm, min_freq = 0.1, edge_color = "orange", edge_alpha = 0.8, edge_size = 5) # Visualizing Network
  return(user_network)
}     

# Setting-up a function to create a network of hashtags
top_hashtags <- function(temp_tweets){
  PostsCorpus <- corpus(temp_tweets, text_field='text')   # Creating corpus of tweet text
  Corpus <- dfm(PostsCorpus)                              # Converting into DFM
  tag_dfm <- dfm_select(Corpus, pattern = ("#*"))         # Selecting  #*hashtags
  toptag <- names(topfeatures(tag_dfm, 50))               # Getting the top 50 hashtags  
  tag_fcm <- fcm(tag_dfm)                                 # Constructing feature-occurrence matrix of hashtags
  topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)     # Selecting the top 50 hashtags  
  hashtag_network <- textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5) # Visualizing Network
  return(hashtag_network)
}  

####  1) UI Component ####  
ui <- fluidPage(
  # title
  titlePanel(""),
  # menu
  sidebarLayout(
    sidebarPanel(width = 2,
      # Get timelines
      h3("Get Timelines", align = "center"),
      helpText(
        ("Returns up to 3,200 tweets"), br("of specified Twitter users."), align = "center"),
      # text input for Twitter handles
      textInput(
        inputId = "twitter_handle",
        label = "Enter a Twitter Handle",
        width = 200,
        value = "BredowInstitut"      
        ),
      # slider input
      sliderInput(
        inputId = "tweet_num",
        label = "Number of Tweets",
        min = 0, max = 3200,
        value = 3200,
        width = 200
      ),
      # action button to scrape users
      actionButton(
        inputId = "click",
        width = 175,
        label = ("Download tweets"), br("of specific users"),
        icon = icon("users", lib = "font-awesome")
      ),
      # Search tweets
      h3("Search Tweets", align = "center"),
      #help text
      helpText(
        ("Returns up to 18,000"), br("tweets containing specified keywords."), align = "center"),
      # text input for keywords / hashtags 
      textInput(
        inputId = "twitter_topic",
        label = ("Enter a Keyword"),
        width = 200,
        value = "LeibnizWGL"
      ),
      # slider input
      sliderInput(
        inputId = "topic_num",
        label = "Number of Tweets",
        min = 0, max = 18000,
        value = 18000,
        width = 200
      ),
      # action button to scrape hashtags
      actionButton(
        inputId = "click2",
        width = 175,
        label = ("Download tweets"), br("with specific keywords"),
        icon = icon("hashtag", lib = "font-awesome")
      )
    ),
    mainPanel(
      tabsetPanel(
      tabPanel("About",
      h2('Social Media Observatory - Twitter Mining and Analysis Suite [SMO-TMAS]', align = "center"), p(''),
      p(em('Leibniz-Institut für Medienforschung | Hans-Bredow-Institut'), align = "center"), p(em('Social Media Observatory'), align = "center"), 
      p('Users are invited to enter a Twitter handle (no @ required) or keyword / hashtag (no # required) and specify a number of tweets to harvest.',
        style='font-family:arial; font-si16pt; color:red', align = "center"), 
      p('The second tab', span('Tweets', style='color:blue'), 'shows a searchable and downloadable table of the scraped Tweets.', align = "center"),
      p('The third tab', span('Statistics', style ='color:blue'), 'shows interactive charts to analyse the meta data of the scraped Tweets.', align = "center"), 
      p('The fourth tab', span('Text Analysis', style='color:blue'), 'shows a word cloud and word frequency table to analyse the text of the scraped Tweets.', align = "center"),
      p('The fifth tab', span('Network Analysis', style='color:blue'), 'shows network graphs to analyse the 50 most frequently mentioned users and hashtags of the scraped Tweets.', align = "center"),
      p(''),
      p(em('NOTICE: The app gathers public data from Twitter’s REST API by querying the GET statuses/user_timeline or search/tweets endpoint.
           There is a limit of 100,000 requests per day to the statuses/user_timeline endpoint (Get Timelines).
           The search/tweets endpoint (Search Tweets) only returns data from the past 6-9 days with a focus on relevancy and not completeness. 
           The scraped data is equivalent to the information publicly seen on Twitter (i.e.tweets belonging to protected users are not returned).
           The number of tweets returned can be less than what was specified or the connection can terminate (i.e. server connection lost - reload). 
           This can happen because (a) the search query did not return many results (the search pool is already thinned out from the population of tweets to begin with), 
           (b) because user hitting the rate limit, or (c) of recent activity 
           (either more tweets, which affect pagination in returned results or deletion of tweets).
           The app should be used in strict accordance with', tags$a(href="https://developer.twitter.com/en/developer-terms/more-on-restricted-use-cases",
                                                                         'Twitter’s developer terms.')), align = "center"),
      p(''),
      tags$p("This app was created with", HTML("&#x2764;&#xFE0F;"), "+",  HTML("\u2615\uFE0F"), "by", tags$a(href = 'https://www.linkedin.com/in/jasonyoung4/', 'Jason Young'),
             'with',  HTML("&#x1F4AA;"), 'from',
             tags$a(href = 'http://rtweet.info/', 'rtweet'), ',',
             tags$a(href = 'https://www.rstudio.com/', 'RStudio'), ',',
             tags$a(href = 'https://shiny.rstudio.com/', 'Shiny'), ',',
             tags$a(href = 'https://plot.ly/r/', 'plotly'), 'and',
             tags$a(href = 'https://quanteda.io', 'quanteda'),'.',
             align = "center"),
      p("Final update:", Sys.Date(), align = "center")
  ),
  tabPanel(
    "Tweets",
    h2("Table of scraped Tweets", align = "center"),
    p("The table with the text and further meta data of the scraped tweets can be downloaded by pressing the download botton on the bottom left.", align = "center"),
    DT::dataTableOutput(outputId = "x1"),
    downloadButton("download", "Download Tweets")
  ),
  tabPanel(
    "Statistics",
    fluidPage(
      h2("Charts of scraped Tweets", align = "center"),
      p("The interactive charts below visualize the meta data of the scraped Tweets", align = "center"),
      box(plotlyOutput(outputId = "tweets_per_day")),
      box(plotlyOutput(outputId = "tweets_per_hour")),
      box(plotlyOutput(outputId = "metric_histograms")),
      box(plotlyOutput(outputId = "tweet_source"))
    )
  ),
  tabPanel(
    "Text Analysis",
    fluidPage(
      h2("Text analysis of scraped Tweets", align = "center"),
      p("The word cloud and word frequency table below visualize the most frequently used terms in the text of the scraped Tweets.", align = "center"),
      h3("Word cloud of the 100 most used words", align = "center"),
      column(3, p("")),                                             # Indicating position column 8 out of a 12-wide grid system
      box(plotOutput(outputId = "tweets_cloud")), 
      column(4, p("")),
      column(5, p( em("The color and size of the words indicates their frequency."), style='font-size:10pt')),
      column(11, p(""),
        h3("Word Frequency Table"), align = "center"),      
      DT::dataTableOutput(outputId = "x2"))
  ),
  tabPanel(
    "Network Analysis",
    fluidPage(
      h2("Network analysis of scraped Tweets", align = "center"),
      p("The networks below visualize the co-occurences of the 50 most frequently mentioned users and hashtags in the text of the scraped Tweets.", align = "center"),
      box(h3("User Network"), align = 'center', plotOutput(outputId = "user_network")), 
      box(h3("Hashtag Network"), align = 'center', plotOutput(outputId = "hashtag_network")),
      p(em("Edges in the above semantic networks show co-occurrences of the 50 most frequently mentioned users and hashtags"), align = 'center', style='font-size:10pt')
  ))
  )
    )
      ))
### 2) Server Component ###
server <- function(input, output, ...){
  
  #### User Tweets
  observeEvent(input$click, {
    # Defining user handle and sample size
    handle <- isolate({as.character(input$twitter_handle)})
    sample_size <- isolate({input$tweet_num})
    
    # Pulling tweets
    temp_tweets <- reactive({
      pull_tweets(handle, sample_size)
    })
    
    # Creating bar chart depicting the days of the week that the tweets were created
    output$tweets_per_day <- renderPlotly({
      tweets_per_day(temp_tweets())
    })
    
    # Creating line plot depicting the time at which the tweets were created
    output$tweets_per_hour <- renderPlotly({
      tweets_per_hour(temp_tweets())
    })
    
    # Creating histogram depicting the distributions of the retweet count and favorited count metrics
    output$metric_histograms <- renderPlotly({
      distributions(temp_tweets())
    })
    
    # Creating pie graph depicting the source from which the tweets were sent:
    output$tweet_source <- renderPlotly({
      tweet_source(temp_tweets())
    })
    
    # Creating table of scraped Tweets
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
    # Download table as .csv and creating a function to use input twitter handle and system date as file name
    })
    output$download <- downloadHandler(filename = function() {paste(input$twitter_handle, Sys.Date(), '.csv', sep='_')},
                                       content = function(file){           
                                         save_as_csv(temp_tweets(), file)
                                       })
    
    # Creating word cloud
    output$tweets_cloud <- renderPlot({
      tweets_cloud(temp_tweets())
    })
    
    # Creating frequency table
    output$x2 <- DT::renderDataTable({
      word_frequency(temp_tweets())
    })
    
    # Creating user network
    output$user_network <- renderPlot({
      top_users(temp_tweets())
      })
    
    # Creating hashtag network
    output$hashtag_network <- renderPlot({
      top_hashtags(temp_tweets())
      })
    })  
  
  #### Topic Tweets
  observeEvent(input$click2, {
    # Defining topic and sample size
    handle <- isolate({as.character(input$twitter_topic)})
    sample_size <- isolate({input$topic_num})
    
    # Pulling topics tweets
    temp_tweets <- reactive({
      pull_topics(handle, sample_size)
    })
    
    # Creating bar chart depicting the days of the week that the tweets were created
    output$tweets_per_day <- renderPlotly({
      tweets_per_day(temp_tweets())
    })
    
    # Creating line plot depicting the time at which the tweets were created
    output$tweets_per_hour <- renderPlotly({
      tweets_per_hour(temp_tweets())
    })
    
    # Creating histogram depicting the distributions of the retweet count and favorited count metrics
    output$metric_histograms <- renderPlotly({
      distributions(temp_tweets())
    })
    
    # Creating pie graph depicting the source from which the tweets were sent
    output$tweet_source <- renderPlotly({
      tweet_source(temp_tweets())
    })
    
    # Creating table of scraped Tweets
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
    
    #Download table as .csv and creating a function to use input twitter handle and system date as file name
    output$download <- downloadHandler(filename = function() {paste(input$twitter_topic, Sys.Date(), '.csv', sep="_")},
                                       content = function(file){
                                         save_as_csv(temp_tweets(), file)
                                       })
    # Creating word cloud
    output$tweets_cloud <- renderPlot({
      tweets_cloud(temp_tweets())
    })
    
    # Creating frequency table
    output$x2 <- DT::renderDataTable({
      word_frequency(temp_tweets())
    })
    
    # Creating user network
    output$user_network <- renderPlot({
      top_users(temp_tweets())
    })
    
    # Creating hashtag network
    output$hashtag_network <- renderPlot({
      top_hashtags(temp_tweets())
      })
  })
  }

### 3) shinyApp component ###
shinyApp(ui = ui, server = server)
