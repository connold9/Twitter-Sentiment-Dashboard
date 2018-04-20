##ST3002 Statistical Analysis 3 Final Assignment
##Author: Devin Connolly
##Student ID: 15315916
##Submission Date: 13th April 2018
##This following code was used to produce all plots and figures for the report
##It should be noted that there is repetition of plots, once inside and once outside the dashboard
##I did this to ensure plots were fully functional as desired before placing them in the dashboard
##And it allowed easier tracking of any changes that were made. It is also often beneficial to view plots in isolation

##Installing all the necessary packages
##Shiny and Dashboard are used for the dashbooard
install.packages("shinydashboard")
install.packages("shiny")

##Used for plotting
install.packages("ggplot2")

##Devtools and Slam are prerequisite to install tm, which is used with snowball and worldcloud
##to plot wordclouds
install.packages("devtools")
install.packages("slam")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

##A package for colour mixing
install.packages("RColorBrewer")

##A package for plotting on a map
install.packages("leaflet")

##Ensuring all these packages are loaded and ready to use
require(ggplot2)
require(scales)
require(reshape2)
require(shinydashboard)
require(shiny)
require(devtools)
require(slam)
require(tm)
require(SnowballC)
require(wordcloud)
require(RColorBrewer)
require(leaflet)

##--------------------------------------------------------------------------##
##Set the working directory to my folder
dir = "C:/Users/Devin/Documents/Third Year Assorted/Statisical Analysis III/Assignment"
setwd(dir)

##Read in the dataset of tweets
tweets <- read.csv(file = "US-tweet-dat.csv", header = T)

##Create a dataset of negative tweets only
neg_tweets<-tweets[-which(tweets$negativereason == ""),]

##Create a dataframe for the reasons for negative tweets
reasons_table <- table(neg_tweets$negativereason)
reasonsdf <- melt(reasons_table, c("reason"))
reasonsdf <- reasonsdf[-1,]

##Isolate those tweets that have atleast one retweet
rt_neg <- subset(neg_tweets, retweet_count >= 1)
rt_all <- subset(tweets, retweet_count >= 1)
neg_retweets_sum <- sum(rt_neg$retweet_count)
all_retweets_sum <- sum(rt_all$retweet_count)
neg_rt_prop <- neg_retweets_sum/all_retweets_sum

##Create a dataframe for tweet location through timezone
timezone <- tweets$user_timezone
levels(timezone)[1] <- "N/A"
tz_table <- table(timezone)
tz_df <- melt(tz_table, c("timezone"))
tz_df_sign <- subset(tz_df, value >=50)

##Remove empty timezones
tz_df_sign <- tz_df_sign[-1,]

##Create a plot illustrating the most significant timezones
##N/A timezones were left blank and were removed
ggplot(tz_df_sign, aes(factor(timezone), value)) + 
  geom_bar(stat="identity", position = "dodge", fill = "#FF6666") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "Timezone", y = "Tweets",title = "Significant Timezones")

##Table for the number of tweets and negative tweets for each airline
airlinecount <- table(tweets$airline)
airlinecountdf <- melt(airlinecount, c("airline"))
neg_airlinecount <- table(neg_tweets$airline)

##A table for the proportion of tweets about an airline that are negative
neg_prop <- neg_airlinecount/airlinecount
prop_table <- melt(neg_prop, c("airline"))

##A plot of proportion of tweets that are negative for each airline
ggplot(prop_table, aes(factor(airline), value)) + 
  geom_bar(stat="identity", position = "dodge", fill = "#FF6666") +
  labs(x = "Airline", y = "Proportion",title = "Proportion of Negative Tweets")

##A pie chart for tweets pertaining to each airline
total <- sum(airlinecountdf$value)

bp<- ggplot(airlinecountdf, aes(x="", y=value, fill=airline))+
  geom_bar(width = 1, stat = "identity")
bp + coord_polar("y", start=0) + 
  scale_fill_brewer(palette="Reds")+
  theme_minimal() +
  labs(x = "", y = "",title = paste("Tweets per Airline -", total, "Total"))

##Creating a subset of the data for United, the most popular
united <- subset(neg_tweets, airline == "United")

##Isolate the airlines and plot a distribution of the airline the negative tweets are about
ggplot(airlinecountdf, aes(factor(airline), value)) + 
  geom_bar(stat="identity", position = "dodge", fill = "#FF6666" ) +
  labs(x = "Airline", y = "Number of Tweets",title = "Tweets per Airline")

##A barchart for the reasons for the negative tweets
ggplot(reasonsdf, aes(factor(reason), value)) + 
  geom_bar(stat="identity", position = "dodge", fill = "#FF6666") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "",y = "Number of Tweets",title = "Tweets per Negative Reason")

##Isolating the negative data I need
negdata=data.frame(
  as.character(neg_tweets$negativereason),
  neg_tweets$airline
)

##And for the united dataset specifcally
unitedreason = data.frame(
  reason = as.character(united$negativereason)
)

##United data including the creation date
datedunited=data.frame(
  reason = as.character(united$negativereason),
  created = united$tweet_created
)

##Will regroup the negative reasons for easier display in a bar chart
##Customer Service = longlines, flight attendant, customer service issue
##Flight = bad flight, cancelled flight, late flight
##Luggage = lost and damaged
##Booking = flight booking problems
##Cant tell = blank (0) and cant tell
grouped <- neg_tweets

##Create the two vectors of new levels, one that includs "" and one that has it removed.
newlevels <- c("Can't Tell","Flight", "Can't Tell", "Flight", "Customer Service", "Luggage", "Customer Service", "Booking", "Flight", "Customer Service", "Luggage")
newlevels2 <- c("Flight", "Can't Tell", "Flight", "Customer Service", "Luggage", "Customer Service", "Booking", "Flight", "Customer Service", "Luggage")

levels(grouped$negativereason) <- newlevels
levels(united$negativereason) <- newlevels
levels(datedunited$reason) <- newlevels2
levels(reasonsdf$reason) <- newlevels
levels(rt_neg$negativereason) <- newlevels

##Converting the data into a workable format using melt function
colnames(negdata) <- c("reasons", "airline")
neg_table <- table(negdata)
dat.m <- melt(neg_table, c("reasons","airline"))
u_reason_table <- table(unitedreason)
u_dat_reason <- melt(u_reason_table, c("reason"))

##Converting the united dated dataframe into workable format
datedunited$created <- as.Date(datedunited$created)
datedunitedtable <- table(datedunited)
united.dated <- melt(datedunitedtable, c("reason","created"))
total_dated_united <- aggregate(united.dated$value, by=list(created=united.dated$created), FUN=sum)

##A pie chart of the reasons for negative tweets, after grouping 
total2 <- sum(reasonsdf$value)
bp2<- ggplot(reasonsdf, aes(x="", y=value, fill=reason))+
  geom_bar(width = 1, stat = "identity")
bp2 + coord_polar("y", start=0) + 
  scale_fill_brewer(palette="Reds")+
  theme_minimal() +
  labs(x = "", y = "",title = paste("Tweets per Negative Reason -", total2, "Total"))

###United only tweets split by category over the days
ggplot(united.dated, aes(factor(created), value, fill = reason)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "Date", y = "Number of Tweets",title = "United Negative Tweets by Day")

#Line chart for number of tweets against reason, split by airline (non-grouped)
ggplot(dat.m, aes(x = factor(reasons), y = value, group = airline, colour = airline)) + 
  geom_line() + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "Negative Reason", y = "Number of Tweets",title = "Negative Tweets by Airline/Reason")

##Creating a new dataframe for this grouped data
##Isolating the negative data I need
grouped_neg =data.frame(
  as.character(grouped$negativereason),
  grouped$airline
)

##Converting the data into a workable format using melt
colnames(grouped_neg) <- c("reasons", "airline")
grouped_table <- table(grouped_neg)
dat.grouped <- melt(grouped_table, c("reasons","airline"))

##Manipulating the retweeted dataset in order to plot graphs
grouped_rt_neg =data.frame(
  reason <- as.character(rt_neg$negativereason),
  airline <- rt_neg$airline
)

##Converting the data into a workable format using melt
grouped_rt_neg_table <- table(grouped_rt_neg)
dat.rt.grouped <- melt(grouped_rt_neg_table, c("reasons","airline"))

##Grouped barchart of tweet reason by airline
ggplot(dat.grouped, aes(factor(airline), value, fill = reasons)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "Airline", y = "Number of Tweets",title = "Breakdown of Tweet Reason")

##Flip the axis to compare airlines rather than complaints
ggplot(dat.grouped, aes(factor(reasons), value, fill = airline)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "Negative Reason", y = "Number of Tweets",title = "Breakdown of Tweet Reason by Airline")

##Grouped barchart of tweet reason by airline - RETWEETED TWEETS ONLY
ggplot(dat.rt.grouped, aes(factor(airline), value, fill = reasons)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "Airline", y = "Number of Tweets",title = "Breakdown of Tweet Reason (Retweeted Only)")

##Flipping the axis
ggplot(dat.rt.grouped, aes(factor(reasons), value, fill = airline)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "Negative Reason", y = "Number of Tweets",title = "Breakdown of Tweet Reason by Airline (Retweeted Only)")

##Creating a new data frame of negative tweets including the data tweeted
datednegdata=data.frame(
  as.character(neg_tweets$negativereason),
  neg_tweets$airline,
  neg_tweets$tweet_created
)

##Converting the data into a workable format
levels(datednegdata$reasons) <- newlevels2
colnames(datednegdata) <- c("reasons", "airline","created")
datednegdata$created <- as.Date(datednegdata$created)
datedtable <- table(datednegdata)
dat.dated <- melt(datedtable, c("reasons","airline","created"))
levels(dat.dated$reasons) <- newlevels2

##Manipulating the data into a workable, aggregated format
red_datedneg <- datednegdata[,-1]
red_datedneg_reason <- datednegdata[,-2]
red_datedtable <- table(red_datedneg)
red_dated_reaon_table <- table(red_datedneg_reason)
reduced_dated_neg <- melt(red_datedtable, c("airline", "created"))
reduced_dated_neg_reason <- melt(red_dated_reaon_table, c("reason", "created"))
levels(reduced_dated_neg_reason$reason) <- newlevels2
total_dated_neg <- aggregate(reduced_dated_neg$value, by=list(created=reduced_dated_neg$created), FUN=sum)

##Negative tweets per day, split by airline
ggplot(reduced_dated_neg, aes(factor(created), value, fill = airline)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "Date", y = "Number of Tweets",title = "Negative Tweets by Day")

##Negative tweets per day, split by reason
ggplot(reduced_dated_neg_reason, aes(factor(created), value, fill = reason)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "Date", y = "Number of Tweets",title = "Negative Tweets by Day")

##Total number of negative tweets per day
ggplot(total_dated_neg, aes(factor(created), x)) + 
  geom_bar(stat="identity", position = "dodge", fill = "#FF6666" ) + 
  scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
  labs(x = "Date", y = "Number of Tweets",title = "Negative Tweets by Day")

##---------------------------------------------------------------##
##Building a dashboard to store my graphs and controls
##First constructing all the rows for all pages
frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "Negative Tweets by Airline"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("plot1", height = "300px")
  ) ##Need a comma to add another box -> ,box()
 ,box(
  title = "Negative Proportion by Airline"
  ,status = "primary"
  ,solidHeader = TRUE 
  ,collapsible = TRUE 
 ,plotOutput("plot2", height = "300px")
  ) 
)
frow3 <- fluidRow( 
  box(
    title = "Negative Tweets Breakdown"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot3", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
frow4 <- fluidRow( 
  box(
    title = "Negative Tweets Breakdown"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot4", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
frow5 <- fluidRow(
  box(
    title = "Negative Tweets over Time"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot5", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
frow6 <- fluidRow(
  box(
    title = "Negative Tweets by Airline over Time"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot6", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
frow7 <- fluidRow(
  valueBoxOutput("value4")
  ,valueBoxOutput("value5")
  ,valueBoxOutput("value6")
)
frow8 <- fluidRow(
  box(
    title = "Chosen Airline - Negative Reason"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot7", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
frow9 <- fluidRow(
  box(
    title = "Negative Tweets @ Chosen Airline Over Time"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot8", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
frow10 <- fluidRow(
  box(
    title = "Negative Tweets by Airline/Reason"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot9", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
frow11 <- fluidRow(
  box(
    title = "Negative Tweet Reason by Day"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot10", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
frow12 <- fluidRow(
  valueBoxOutput("value7")
  ,valueBoxOutput("value8")
  ,valueBoxOutput("value9")
)
frow13 <- fluidRow(
  box(
    title = "Breakdown of Tweet Reason (RT'd Only)"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot11", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
frow14 <- fluidRow(
  box(
    title = "Breakdown of Tweet Reason by Airline (RT'd Only)"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot12", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
frow15 <- fluidRow(
  box(
    title = "Most Popular Timezones"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,width = "600px"
    ,plotOutput("plot13", height = "300px", width = "600px")
  ) ##Need a comma to add another box -> ,box()
)
airline_v <- levels(tweets$airline)
frow16 <- fluidRow(
  selectInput('xcol', 'Airline', airline_v, selected = airline_v[1])
)

##Creating the User Interface
ui <- dashboardPage(
  dashboardHeader(title = "Airline Tweets"),
  dashboardSidebar(sidebarMenu(
    ##adding the sidebar icons and titles
    menuItem("Dashboard", tabName = "dashboard", icon = icon("stats",lib='glyphicon')),
    menuItem("Time Analysis", tabName = "time", icon = icon("time", lib='glyphicon')),
    menuItem("Individual", tabName = "united", icon = icon("plane", lib='glyphicon')),
    menuItem("Retweets", tabName = "rt", icon = icon("share-alt", lib='glyphicon')),
    menuItem("Location", tabName = "map", icon = icon("map-marker", lib='glyphicon'))
  )),
  dashboardBody(
      tabItems(
        ##adding the pre-defined rows to each tabitem
        tabItem(tabName = "dashboard",
                frow1,
                frow2,
                frow3,
                frow4,
                frow10
                ),
        tabItem(tabName = "time",
                frow5,
                frow6,
                frow11
        ),
        tabItem(tabName = "united",
                frow7,
                frow16,
                frow8,
                frow9
                ),
        tabItem(tabName = "rt",
                frow12,
                frow13,
                frow14
                ),
        tabItem(tabName = "map",
                frow15
                )
      )
  )
)


##Providing the input for the dashboard
server <- function(input, output) {
  
  #some data manipulation to derive the values of KPI boxes
  max_rt <- max(neg_tweets$retweet_count)
  max_index <- which.max(prop_table$value)
  maxneg_air <- prop_table$airline[max_index]
  maxneg_value <- prop_table$value[max_index]
  most_index <- which.max(airlinecount)
  most_air <- airlinecountdf$airline[most_index]
  most_value <- airlinecountdf$value[most_index]
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    
    valueBox(
      formatC(max_rt,  big.mark=',')
      ,paste('Most Retweets')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(maxneg_value, big.mark=',')
      ,paste('Neg Prop: ', maxneg_air)
      ,icon = icon("thumbs-down",lib='glyphicon')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({ 
    valueBox(
      formatC(most_value, big.mark=',')
      ,paste('Most Tweets: ', most_air)
      ,icon = icon("chevron-up",lib='glyphicon')
      ,color = "blue")  
  })
  output$value4 <- renderValueBox({ 
    airline_select <- input$xcol
    reason_sub <- subset(dat.m, airline == airline_select)
    pop_reason_index <- which.max(reason_sub$value)
    pop_reason <- reason_sub$reason[pop_reason_index]
    pop_reason_value <- reason_sub$value[pop_reason_index]
    
    valueBox(
      formatC(pop_reason_value, big.mark=',')
      ,paste(pop_reason)
      ,icon = icon("question-sign",lib='glyphicon')
      ,color = "purple")  
  })
  output$value5 <- renderValueBox({
    airline_select <- input$xcol
    dated_sub <- subset(reduced_dated_neg, airline == airline_select)
    pop_day_index <- which.max(dated_sub$value)
    pop_day <- dated_sub$created[pop_day_index]
    pop_day_value <- dated_sub$value[pop_day_index]
    
    valueBox(
      formatC(pop_day_value, big.mark=',')
      ,paste('Most Active Date: ', pop_day)
      ,icon = icon("calendar",lib='glyphicon')
      ,color = "green")  
  })
  output$value6 <- renderValueBox({ 
    airline_select <- input$xcol
    prop_index <- which(prop_table$airline == airline_select)
    prop_airline <- prop_table$airline[prop_index]
    prop_value <- prop_table$value[prop_index]
    
    valueBox(
      formatC(prop_value, big.mark=',')
      ,paste('Negative Proportion')
      ,icon = icon("thumbs-down",lib='glyphicon')
      ,color = "blue")  
  })
  output$value7 <- renderValueBox({ 
    valueBox(
      formatC(all_retweets_sum, big.mark=',')
      ,paste('Sum of All RTs')
      ,icon = icon("thumbs-up",lib='glyphicon')
      ,color = "purple")  
  })
  output$value8 <- renderValueBox({ 
    valueBox(
      formatC(neg_retweets_sum, big.mark=',')
      ,paste('Sum of Negative RTs')
      ,icon = icon("thumbs-down",lib='glyphicon')
      ,color = "green")  
  })
  output$value9 <- renderValueBox({ 
    valueBox(
      formatC(neg_rt_prop, big.mark=',')
      ,paste('Negative RT Proportion')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")  
  })
  #creating the plotOutput content
  output$plot1 <- renderPlot({
    ggplot(airlinecountdf, aes(factor(airline), value)) + 
      geom_bar(stat="identity", position = "dodge", fill = "#FF6666") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Airline", y = "Tweets")
  })
  output$plot2 <- renderPlot({
    ggplot(prop_table, aes(factor(airline), value)) + 
      geom_bar(stat="identity", position = "dodge", fill = "#FF6666") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Airline", y = "Proportion")
  })
  output$plot3 <- renderPlot({
    ggplot(dat.grouped, aes(factor(reasons), value, fill = airline)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Negative Reason", y = "Number of Tweets")
  })
  output$plot4 <- renderPlot({
    ggplot(dat.grouped, aes(factor(airline), value, fill = reasons)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Airline", y = "Number of Tweets")
  })
  output$plot5 <- renderPlot({
    ggplot(total_dated_neg, aes(factor(created), x)) + 
      geom_bar(stat="identity", position = "dodge", fill = "#FF6666" ) + 
      scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Date", y = "Number of Tweets")
  })
  output$plot6 <- renderPlot({
    ggplot(reduced_dated_neg, aes(factor(created), value, fill = airline)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Date", y = "Number of Tweets",title = "Negative Tweets by Day")
  })
  output$plot7 <- renderPlot({
    airline_select <- input$xcol
    reason_sub <- subset(dat.m, airline == airline_select)
    reason_sub <- reason_sub[,-2]
    
    ggplot(reason_sub, aes(factor(reasons), value)) + 
      geom_bar(stat="identity", position = "dodge", fill = "#FF6666" ) + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = paste("Reason -", airline_select), y = "Number of Tweets")
  })
  output$plot8 <- renderPlot({
    airline_select <- input$xcol
    
    sel_airline_data <- subset(neg_tweets, airline == airline_select)
    levels(sel_airline_data$negativereason) <- newlevels
    sel_airline_data <- sel_airline_data[,c(4,10)]
    sel_airline_data$tweet_created <- as.Date(sel_airline_data$tweet_created)
    dated_sel_table <- table(sel_airline_data)
    sel.dated <- melt(dated_sel_table, c("reason","created"))
    
    ggplot(sel.dated, aes(factor(created), value, fill = reason)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Date", y = "Number of Tweets")
    
  })
  output$plot9 <- renderPlot({
    ggplot(dat.m, aes(x = factor(reasons), y = value, group = airline, colour = airline)) + 
      geom_line() + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Negative Reason", y = "Number of Tweets")
  })
  output$plot10 <- renderPlot({
    ggplot(reduced_dated_neg_reason, aes(factor(created), value, fill = reason)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Date", y = "Number of Tweets")
  })
  output$plot11 <- renderPlot({
    ggplot(dat.rt.grouped, aes(factor(airline), value, fill = reasons)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Airline", y = "Number of Tweets")
  })
  output$plot12 <- renderPlot({
    ggplot(dat.rt.grouped, aes(factor(reasons), value, fill = airline)) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set1") + theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Negative Reason", y = "Number of Tweets")
  })
  output$plot13 <- renderPlot({
    ggplot(tz_df_sign, aes(factor(timezone), value)) + 
      geom_bar(stat="identity", position = "dodge", fill = "#FF6666") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1.0)) +
      labs(x = "Timezone", y = "Tweets")
  })
}

##Displaying the Shiny Dashboard
shinyApp(ui, server)

##----------------------------------------------------------------------------##
##Writing the text contained in all the tweets to a corpus file
docs <- Corpus(VectorSource(tweets$text))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

##Transforming all of the following characters to spaces, to isolate words.
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
##Removing airline names
docs <- tm_map(docs, removeWords, airline_v)
##Creating a vector for further airline names to remove, as well as some acronyms/shorthand
removals <- c("united", "usairways","americanair", "southwestair","jetblue","tco","http")
docs <- tm_map(docs, removeWords, removals)

# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

##Plotting the finished wordcloud, containing 200 words
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.4, 
          colors=brewer.pal(8, "Set1"))

##----------------------------------------------------------------------------##
##Plotting the coordinates on a map, based on positive or negative sentiment
##Create a subset of data that contains coordinates only
coord_tweets <- subset(tweets, tweets$tweet_coord != "")

##Remove all those coords that are 0,0
index_00 <-which(coord_tweets[,9] == "[0.0, 0.0]")
coord_tweets <- coord_tweets[-index_00,]

##Have to split coordinates into longitude and latitude
coord_tweets$tweet_coord <- gsub('[[]', '', coord_tweets$tweet_coord)
coord_tweets$tweet_coord <- gsub('[]]', '', coord_tweets$tweet_coord)
latlon <- strsplit(coord_tweets$tweet_coord, ",")
latlon_df <- data.frame(matrix(unlist(latlon), nrow=816, byrow=T))
colnames(latlon_df) <- c("lat","lon")

## assign either a positive or negative or neutral label to the tweet
latlon_df$sent <- coord_tweets$airline_sentiment
latlon_df$reason <- coord_tweets$negativereason
latlon_df$airline <- coord_tweets$airline
latlon_df$lat <- as.character(latlon_df$lat)
latlon_df$lat <- as.numeric(latlon_df$lat)
latlon_df$lon <- as.character(latlon_df$lon)
latlon_df$lon <- as.numeric(latlon_df$lon)

colours <- character(nrow(latlon_df))
colours[latlon_df$sent == "positive"] <- "green"
colours[latlon_df$sent == "negative"] <- "red"
colours[latlon_df$sent == "neutral"] <- "blue"

# Show a circle at each position
# Unfortunately this map cannot be added to the Dashboard as it is a different plot type
leaflet(data = latlon_df) %>% addTiles() %>% addCircleMarkers(~lon, ~lat , radius = 5,fillOpacity = 0.5,opacity = 0.5, col = colours,popup = paste("Sentiment: ",latlon_df$sent, "<br>",
                                                                                                                                                   "Negative Reason: ", latlon_df$reason,"<br>",
                                                                                                                                                   "Airline: ", latlon_df$airline))

