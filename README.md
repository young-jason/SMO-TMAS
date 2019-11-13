## Social Media Observatory - Twitter Mining and Analysis Suite [SMO-TMAS]

SMO-TMAS is a simple Shiny app that makes it easy to collect and analyze small Twitter data sets. 

SMO-TMAS allows users to pull tweets of specified Twitter handles and tweets containing specified keywords by querying Twitter's REST API GET search/tweets endpoint and statuses/user_timeline endpoint as well as Twitter's STREAM API. The collected tweets can be downloaded as .csv file and SMO-TMAS also provides data analysis components that can be used to analyze and visualize the collected data right away.

SMO-TMAS is suitable for exploratory data analysis of small Twitter data sets (≈18,000 tweets or 3,200 user statuses). The big plus of SMO-TMAS is it’s easy use, which allows researcher without any prior computing knowledge to get first insights into Twitter data with the simple click of a button. In addition, the source code is fully transparent and available for free for customization and further development.

A running version of the application is available at: https://jason-young.shinyapps.io/twitter-analysis/. Enjoy.

*NOTE*: Importing a copy of the code into R will not result in a functioning app, unless personal keys for the Twitter authentication are inserted into the _PLACEHOLDERS_. Instructions to produce personal keys for the Twitter authentication are available at [IAG](https://iag.me/socialmedia/how-to-create-a-twitter-app-in-8-easy-steps/).

A detailed tutorial can be found in the [Wiki](https://github.com/Leibniz-HBI/SMO-TMAS/wiki) of this repository.
