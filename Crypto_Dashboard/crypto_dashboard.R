library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(tidyr)
library(rvest)
library(tidyverse)
library(stringr)
library(jsonlite)
library(tidyverse)
library(data.table)
library(lubridate)
library(formattable)
library(scales)
library(rlist)

#updated for IO

options(scipen = 999) #removes scientific notation

#pulling from Kraken API
get_avg_pairs <- function() {
  url <- "https://api.kraken.com/0/public/AssetPairs"
  out <- jsonlite::fromJSON(url)
  return(out)
}


# format TimeZone for coinmarket cap scraping
fixTZ = function(timeStamp){
  tmDIFF = round(as.numeric(difftime(Sys.time(),
                                     lubridate::force_tz(with_tz(Sys.time(),tz="UTC")),
                                     units = "hours")),0)
  as.POSIXct(timeStamp + hours(tmDIFF), tz= Sys.timezone())
}

# Define UI ----
ui <- fluidPage(
  #titlePanel(strong("Crypto Dashboard")),br(),
  br(),
  tabsetPanel(tabPanel('Moving Averages', br(),
  fluidRow(column(4),column(4,align="center",  wellPanel(h4("Moving Averages"),
                               h5("Select an asset to see the 50, 100, and 200 week moving averages"),
                               selectInput('avgchoice', "Select Asset", choices = c("NADA")),
                               actionButton('avgrun', 'Run Averages'), actionButton('note', 'Note'))), column(4)), br(),
  
  fluidRow(column(2),column(8, align="center",plotOutput("avggraph")), column(2)),br(), br(),
                            fluidRow(column(4),column(4,align="center",(formattableOutput('avgtable'))), column(4))),
  tabPanel('Market', br(),
           fluidRow(column(4), column(4, align="center",wellPanel(h4("Load the Market from CoinMarketCap.com"),
                                        numericInput('marketcount', "Input Number of Coins to View", value = 100),br(),
                                        actionButton("refresh", "Load Market")))), br(),
           fluidRow(column(1),column(10,dataTableOutput("market")), column(1))),
  tabPanel('Fear and Greed Index', br(),
           fluidRow(column(3), column(6, align="center", wellPanel(img(src="https://alternative.me/crypto/fear-and-greed-index.png", width="90%"), br(),
                                                                   br(),h4("Explanation from Official Site: https://alternative.me/crypto/fear-and-greed-index/"), br(),
                                                                   
                                                                   h5("The crypto market behaviour is very emotional. People tend to get greedy when the market is rising which results in FOMO (Fear of missing out). Also, people often sell their coins in irrational reaction of seeing red numbers. With our Fear and Greed Index, we try to save you from your own emotional overreactions. There are two simple assumptions:

Extreme fear can be a sign that investors are too worried. That could be a buying opportunity.
When Investors are getting too greedy, that means the market is due for a correction.
Therefore, we analyze the current sentiment of the Bitcoin market and crunch the numbers into a simple meter from 0 to 100. Zero means 'Extreme Fear', while 100 means 'Extreme Greed'."), column(3)))))
  
))

# Define server logic ----
server <- function(input, output, session) {
  
  showModal(modalDialog(h5(strong("Welcome to the Crypto Dashboard")),
                        h5("There are multiple tabs to view moving averages, the current market, and the Fear and Greed Index"),
                        h5("Data Sources: Kraken API, CoinMarketCap API, and Fear and Greed Index"),
                        h5("Contact: Taylor@bytethehand.org")))
  
  observeEvent(input$note, {
    
    showModal(modalDialog(h5(strong("The options are displayed as the asset ticker and paired currency")),br(),
                          h5("Ethereum = XETHZ USD"),
                          h5("Bitcoin = XXBTZ USD")))
    
    
  })
  
  #read in API key
  
  api_val <- read_file('API/API.txt')
  
  # Taken from this youtube channel https://www.youtube.com/watch?v=g9ItKIsCnMM&ab_channel=JasonR
  # https://github.com/jgQuantScripts/coinmarketcap/blob/main/coinmarketcapAPI.R
  PASS <- new.env()
  assign("apikey", paste(api_val), envir=PASS) #this is my personal API key --> Instructions are below
  
  #API Key Instructions
  #1. Get your API Key from https://coinmarketcap.com/api/
  #2. Get the basic plan (free) and copy your API key from the website
  #3. Open Notepad or TextEdit and paste only the API key
  #4. Name it 'API' and save it in the API folder
  #This API key allows you to pull market data
  
  getLatestListings = function(limit,fiat) #This function pulls the latest listings from coinmarket cap with the API above
  {
    # build URL
    url = paste0("https://pro-api.coinmarketcap.com/v1/cryptocurrency/listings/latest",
                 "?start=1&limit=",limit,"&convert=",fiat)
    # GET request
    pg <- httr::GET(url,httr::add_headers(`Accepts` = 'application/json',
                                          `X-CMC_PRO_API_KEY` = PASS$apikey))
    # read in content
    dt<- fromJSON(rawToChar(pg$content))
    
    # convert to data frame
    bse <- cbind(dt$data$id,
                 dt$data$name,
                 dt$data$symbol,
                 dt$data$slug,
                 dt$data$date_added,
                 dt$data$max_supply,
                 dt$data$circulating_supply,
                 dt$data$total_supply)%>%
            as.data.frame
    # format column names
    colnames(bse) = c("ID","Coin","Ticker","Slug","DateAdded","Max Supply","Circulating Supply","Total Supply")
    
    # format DateAdded
    bse$DateAdded <- as.Date(bse$DateAdded)
    
    # quote 
    qte <- dt[["data"]][["quote"]] %>% as.data.frame
    qte[[1]]$price              <- qte[[1]]$price %>% round(digits = 4)
    qte[[1]]$last_updated       <- as.POSIXct(qte[[1]]$last_updated, format="%Y-%m-%dT%H:%M:%S.000Z")
    qte[[1]]$percent_change_1h  <- round(qte[[1]]$percent_change_1h/100,4)
    qte[[1]]$percent_change_24h <- round(qte[[1]]$percent_change_24h/100,4)
    qte[[1]]$percent_change_7d  <- round(qte[[1]]$percent_change_7d/100,4)
    qte[[1]]$percent_change_30d <- round(qte[[1]]$percent_change_30d/100,4)
    qte[[1]]$percent_change_60d <- round(qte[[1]]$percent_change_60d/100,4)
    qte[[1]]$percent_change_90d <- round(qte[[1]]$percent_change_90d/100,4)
    qte[[1]]$market_cap_dominance<-round(qte[[1]]$market_cap_dominance/100,4)
    
    
    # cbind data & quotes
    df <- cbind(bse,qte)
    
    # return table
    df
  }
    
    ## run function and filter for USD options
    asset_pairs <- get_avg_pairs()[["result"]]%>%
      list.filter(grepl('USD', altname))
    
    filt_pairs <- names(asset_pairs) %>%
      as.data.frame%>%
      filter(!grepl("USDT", .))
    
    asset_names <- sub("USD", " USD", filt_pairs$.)
    
    #number each one - split for list
    #rejoin for filtering?
    
    updateSelectInput(session,'avgchoice', choices = asset_names) #works

  
  observeEvent(input$avgrun, {
    
    showModal(modalDialog("Loading Averages from Kraken", footer=NULL))
    
    
    filtchoice <- gsub(" USD", "USD", input$avgchoice)
    
    
    #pulls the data from the kraken API
    get_ohlc <- function(asset_pair,interval) {
      url <- "https://api.kraken.com/0/public/OHLC?"
      now <- as.numeric(round(Sys.time()))
      twohundweeks <- (now-(200*604800)) #200 weeks or most available if under
      parameters <- base::paste0("pair=", asset_pair, "&since=", twohundweeks,"&interval=", interval)
      out <- jsonlite::fromJSON(base::paste0(url, parameters))
      return(out)}
    
    
    
    twohund_week_pull <- get_ohlc(filtchoice, 10080)[['result']]%>%
      as.data.frame()
    
    names(twohund_week_pull) <- c("time", "Open", "High", "Low", "Close", "???", "Volume", "Trades", "End")
    
    #creates a table with averages for weeks - Need to make this an IF statement based on number of weeks present - subset week count in a base table then do the rest of the work from there
    week_cnt <- twohund_week_pull%>%
      mutate(time=as.numeric(substr(time, 1,10)))%>%
      mutate(Date=as.POSIXct(time, origin="1970-01-01", tz='EST'))%>%
      mutate_at(vars("Open", "High","Low","Close"), as.numeric)%>%
      select(Date, Open, High, Low, Close)%>%
      mutate(`Week Number`=seq.int(nrow(twohund_week_pull)))%>%
      mutate(`Week Count`=nrow(twohund_week_pull))
    
    week_count_val <- week_cnt[[1,7]]
    
    
    #could ifelse here - either 200 week or not
    if(week_count_val==200){
      twohund_week_pull <- twohund_week_pull %>%
        mutate(time=as.numeric(substr(time, 1,10)))%>%
        mutate(Date=as.POSIXct(time, origin="1970-01-01", tz='EST'))%>%
        mutate_at(vars("Open", "High","Low","Close"), as.numeric)%>%
        select(Date, Open, High, Low, Close)%>%
        mutate(`Week Number`=rev(seq.int(nrow(twohund_week_pull))))%>%
        mutate(`Week Count`=nrow(twohund_week_pull))%>%
        mutate(`200 Week Average`=mean(Close))%>%
        mutate(`50week_flag`=case_when(`Week Number`<51 ~ 1,
                                       TRUE~0))%>%
        mutate(`100week_flag`=case_when(`Week Number`<100~1,
                                        TRUE~0))%>%
        group_by(`50week_flag`)%>%
        mutate(`50 Week Average`=mean(Close))%>%
        ungroup()%>%
        group_by(`100week_flag`)%>%
        mutate(`100 Week Average`=mean(Close))
      
      #pulls the pair names and joins the api filter id with the base name - ticker - which needs to be used to filter price
      asset_pairs <- get_avg_pairs()[["result"]]%>%
        list.filter(grepl('USD', altname))
      
      asset_count <- length(asset_pairs)
      
      asset_base <- get_avg_pairs()[["result"]]%>%
        list.select(altname, base)
      
      asset_base <- rbindlist(asset_base)%>%
        filter(grepl('USD', altname))%>%
        mutate(id=1:asset_count)
      
      base_names <- names(asset_pairs)%>%
        as.data.frame()%>%
        mutate(id=1:asset_count)
      
      colnames(base_names) <- c('asset_tick', 'id')
      
      base_names <- base_names %>%
        left_join(asset_base)
      
      #pulling in price data - will need to filter here for coin - so will need actual pair
      #may need to look into getting listings for a specific coin
      ct1 <- getLatestListings(200, "USD")
      
      crypt_table <- ct1%>%
        mutate(Price=ct1$USD$price)%>%
        mutate(`Volume 24H`=ct1$USD$volume_24h)%>%
        mutate(`Volume Change 24H`=ct1$USD$percent_change_24h)%>%
        mutate(`% Change 1H`=ct1$USD$percent_change_1h)%>%
        mutate(`% Change 24H`=ct1$USD$percent_change_24h)%>%
        mutate(`% Change 7D`=ct1$USD$percent_change_7d)%>%
        mutate(`% Change 30D`=ct1$USD$percent_change_30d)%>%
        mutate(`Market Cap`=ct1$USD$market_cap)%>%
        mutate(`Market Cap Dominance`=ct1$USD$market_cap_dominance)%>%
        select(Coin, Ticker, Price)
      
      #average table to be displayed under the graph
      avg_table <- twohund_week_pull %>%
        ungroup()%>%
        filter(`50week_flag`==1 & `100week_flag`==1)%>%
        mutate(asset_tick=filtchoice)%>%
        left_join(base_names)%>%
        select(Asset=base,`50 Week Average`, `100 Week Average`, `200 Week Average`)%>%
        mutate(Asset=case_when(Asset=='XXBT'~'BTC',
                               Asset=='XETH'~'ETH',
                               TRUE~Asset))%>%
        unique()%>%
        left_join(select(crypt_table, Ticker, `Current Price`=Price), by=c('Asset'='Ticker'))%>%
        select(Asset, `Current Price`, `50 Week Average`, `100 Week Average`, `200 Week Average`)
      
      
      twohun_week_pt <- twohund_week_pull[[nrow(twohund_week_pull),8]]
      onehun_week_pt <- twohund_week_pull[[nrow(twohund_week_pull),12]]
      fiftyweek_pt <- twohund_week_pull[[nrow(twohund_week_pull),11]]
      today_pt <- twohund_week_pull[[nrow(twohund_week_pull),2]]
      opening_week <- twohund_week_pull[[1,1]] #first week close for plotting h_lines
      
      averages_graph <- ggplot() + geom_line(aes(x=twohund_week_pull$Date, y=twohund_week_pull$Close)) + geom_hline(yintercept = twohun_week_pt, color="red", size=1.25)+
        geom_hline(yintercept = onehun_week_pt, color="blue", size=1.25)+ geom_hline(yintercept = fiftyweek_pt, color="green", size=1.25) +
        geom_text(aes(opening_week, twohun_week_pt, label=paste0("$",round(twohun_week_pt,2))),vjust=-.7)+ #have to subset first date of table to plot on x axis
        geom_text(aes(opening_week, twohun_week_pt, label=paste('200 Week Average'), vjust=1.3))+
        geom_text(aes(opening_week, onehun_week_pt, label=paste0("$",round(onehun_week_pt,2))), vjust=-.7)+
        geom_text(aes(opening_week, onehun_week_pt, label=paste('100 Week Average'), vjust=1.3))+
        geom_text(aes(opening_week, fiftyweek_pt, label=paste0("$",round(fiftyweek_pt,2))), vjust=-1)+
        geom_text(aes(opening_week, fiftyweek_pt, label=paste('50 Week Average'), vjust=1.3))+
        theme_classic() + xlab('Weeks') + ylab('Weekly Closing Price')
      
      output$avggraph <- renderPlot(averages_graph)
      output$avgtable <- renderFormattable(formattable(avg_table, align='c'))
    }
    
    
    #need to fix flaggin system - ASSUMPTION IS A 200 COUNT, but NOT TRUE FROM HERE - Should be able to change flag to less than and then reverse calculate the flag
    else if(100<week_count_val & week_count_val<200){
      
      onehund_week_pull <- twohund_week_pull %>%
        mutate(time=as.numeric(substr(time, 1,10)))%>%
        mutate(Date=as.POSIXct(time, origin="1970-01-01", tz='EST'))%>%
        mutate_at(vars("Open", "High","Low","Close"), as.numeric)%>%
        select(Date, Open, High, Low, Close)%>%
        mutate(`Week Number`=rev(seq.int(nrow(twohund_week_pull))))%>%
        mutate(`Week Count`=nrow(twohund_week_pull))%>%
        mutate(`200 Week Average`=mean(Close))%>%
        mutate(`50week_flag`=case_when(`Week Number`<51 ~ 1,
                                       TRUE~0))%>%
        mutate(`100week_flag`=case_when(`Week Number`<100~1,
                                        TRUE~0))%>%
        group_by(`50week_flag`)%>%
        mutate(`50 Week Average`=mean(Close))%>%
        ungroup()%>%
        group_by(`100week_flag`)%>%
        mutate(`100 Week Average`=mean(Close))
      
      overall_pt <- onehund_week_pull[[nrow(onehund_week_pull),8]]
      onehun_week_pt <- onehund_week_pull[[nrow(onehund_week_pull),12]]
      fiftyweek_pt <- onehund_week_pull[[nrow(onehund_week_pull),11]]
      today_pt <- onehund_week_pull[[nrow(onehund_week_pull),2]]
      opening_week <- onehund_week_pull[[1,1]] #first week close for plotting h_lines
      week_total <- onehund_week_pull[[1,7]]
      
      overall_col_name <- paste(week_total, 'Week Average', sep=' ')
      overall_col_name <- enquo(overall_col_name)
      
      #pulls the pair names and joins the api filter id with the base name - ticker - which needs to be used to filter price
      asset_pairs <- get_avg_pairs()[["result"]]%>%
        list.filter(grepl('USD', altname))
      
      asset_base <- get_avg_pairs()[["result"]]%>%
        list.select(altname, base)
      
      asset_count <- length(asset_pairs)
      
      asset_base <- rbindlist(asset_base)%>%
        filter(grepl('USD', altname))%>%
        mutate(id=1:asset_count)
      
      base_names <- names(asset_pairs)%>%
        as.data.frame()%>%
        mutate(id=1:asset_count)
      
      colnames(base_names) <- c('asset_tick', 'id')
      
      base_names <- base_names %>%
        left_join(asset_base)
      
      #pulling in price data - will need to filter here for coin - so will need actual pair
      #may need to look into getting listings for a specific coin
      ct1 <- getLatestListings(200, "USD")
      
      crypt_table <- ct1%>%
        mutate(Price=ct1$USD$price)%>%
        mutate(`Volume 24H`=ct1$USD$volume_24h)%>%
        mutate(`Volume Change 24H`=ct1$USD$percent_change_24h)%>%
        mutate(`% Change 1H`=ct1$USD$percent_change_1h)%>%
        mutate(`% Change 24H`=ct1$USD$percent_change_24h)%>%
        mutate(`% Change 7D`=ct1$USD$percent_change_7d)%>%
        mutate(`% Change 30D`=ct1$USD$percent_change_30d)%>%
        mutate(`Market Cap`=ct1$USD$market_cap)%>%
        mutate(`Market Cap Dominance`=ct1$USD$market_cap_dominance)%>%
        select(Coin, Ticker, Price)
      
      #average table to be displayed under the graph
      avg_table <- onehund_week_pull %>%
        ungroup()%>%
        filter(`50week_flag`==1 & `100week_flag`==1)%>%
        mutate(asset_tick=filtchoice)%>%
        left_join(base_names)%>%
        #mutate(!!overall_col_name:=`200 Week Average`)%>%
        select(Asset=base,`50 Week Average`, `100 Week Average`,!!overall_col_name:=`200 Week Average`)%>%
        #mutate(Asset=base)%>%
        mutate(Asset=case_when(Asset=='XXBT'~'BTC',
                               Asset=='XETH'~'ETH',
                               TRUE~Asset))%>%
        unique()%>%
        left_join(select(crypt_table, Ticker, `Current Price`=Price), by=c('Asset'='Ticker'))%>%
        select(Asset, `Current Price`, `50 Week Average`, `100 Week Average`, !!overall_col_name)
      
      averages_graph <- ggplot() + geom_line(aes(x=onehund_week_pull$Date, y=onehund_week_pull$Close)) + geom_hline(yintercept = overall_pt, color="red", size=1.25)+
        geom_hline(yintercept = onehun_week_pt, color="blue", size=1.25)+ geom_hline(yintercept = fiftyweek_pt, color="green", size=1.25) +
        geom_text(aes(opening_week, overall_pt, label=paste0("$",round(overall_pt,2))),vjust=-.7)+ #have to subset first date of table to plot on x axis
        geom_text(aes(opening_week, overall_pt, label=paste(week_total, "Week Average", sep=" "), vjust=1.3))+
        geom_text(aes(opening_week, onehun_week_pt, label=paste0("$",round(onehun_week_pt,2))), vjust=-.7)+
        geom_text(aes(opening_week, onehun_week_pt, label=paste('100 Week Average'), vjust=1.3))+
        geom_text(aes(opening_week, fiftyweek_pt, label=paste0("$",round(fiftyweek_pt,2))), vjust=-.7)+
        geom_text(aes(opening_week, fiftyweek_pt, label=paste('50 Week Average'), vjust=1.3))+
        theme_classic() + xlab('Weeks') + ylab('Weekly Closing Price')
      
      output$avggraph <- renderPlot(averages_graph)
      output$avgtable <- renderFormattable(formattable(avg_table, align='c'))
    }
    
    else if(week_count_val < 100 & week_count_val>75){
      fifty_week_pull <- twohund_week_pull %>%
        mutate(time=as.numeric(substr(time, 1,10)))%>%
        mutate(Date=as.POSIXct(time, origin="1970-01-01", tz='EST'))%>%
        mutate_at(vars("Open", "High","Low","Close"), as.numeric)%>%
        select(Date, Open, High, Low, Close)%>%
        mutate(`Week Number`=rev(seq.int(nrow(twohund_week_pull))))%>%
        mutate(`Week Count`=nrow(twohund_week_pull))%>%
        mutate(`200 Week Average`=mean(Close))%>%
        mutate(`50week_flag`=case_when(`Week Number`<51 ~ 1,
                                       TRUE~0))%>%
        mutate(`100week_flag`=case_when(`Week Number`<100~1,
                                        TRUE~0))%>%
        group_by(`50week_flag`)%>%
        mutate(`50 Week Average`=mean(Close))
      
      overall_pt <- fifty_week_pull[[nrow(fifty_week_pull),8]]
      fiftyweek_pt <- fifty_week_pull[[nrow(fifty_week_pull),11]]
      today_pt <- fifty_week_pull[[nrow(fifty_week_pull),2]]
      opening_week <- fifty_week_pull[[1,1]] #first week close for plotting h_lines
      week_total <- fifty_week_pull[[1,7]]
      
      overall_col_name <- paste(week_total, 'Week Average', sep=' ')
      overall_col_name <- enquo(overall_col_name)
      
      #pulls the pair names and joins the api filter id with the base name - ticker - which needs to be used to filter price
      asset_pairs <- get_avg_pairs()[["result"]]%>%
        list.filter(grepl('USD', altname))
      
      asset_base <- get_avg_pairs()[["result"]]%>%
        list.select(altname, base)
      
      asset_count <- length(asset_pairs)
      
      asset_base <- rbindlist(asset_base)%>%
        filter(grepl('USD', altname))%>%
        mutate(id=1:asset_count)
      
      base_names <- names(asset_pairs)%>%
        as.data.frame()%>%
        mutate(id=1:asset_count)
      
      colnames(base_names) <- c('asset_tick', 'id')
      
      base_names <- base_names %>%
        left_join(asset_base)
      
      #pulling in price data - will need to filter here for coin - so will need actual pair
      #may need to look into getting listings for a specific coin
      ct1 <- getLatestListings(200, "USD")
      
      crypt_table <- ct1%>%
        mutate(Price=ct1$USD$price)%>%
        mutate(`Volume 24H`=ct1$USD$volume_24h)%>%
        mutate(`Volume Change 24H`=ct1$USD$percent_change_24h)%>%
        mutate(`% Change 1H`=ct1$USD$percent_change_1h)%>%
        mutate(`% Change 24H`=ct1$USD$percent_change_24h)%>%
        mutate(`% Change 7D`=ct1$USD$percent_change_7d)%>%
        mutate(`% Change 30D`=ct1$USD$percent_change_30d)%>%
        mutate(`Market Cap`=ct1$USD$market_cap)%>%
        mutate(`Market Cap Dominance`=ct1$USD$market_cap_dominance)%>%
        select(Coin, Ticker, Price)
      
      #average table to be displayed under the graph
      avg_table <- fifty_week_pull %>%
        ungroup()%>%
        filter(`50week_flag`==1 & `100week_flag`==1)%>%
        mutate(asset_tick=filtchoice)%>%
        left_join(base_names)%>%
        #mutate(!!overall_col_name:=`200 Week Average`)%>%
        select(Asset=base,`50 Week Average`,!!overall_col_name:=`200 Week Average`)%>%
        #mutate(Asset=base)%>%
        mutate(Asset=case_when(Asset=='XXBT'~'BTC',
                               Asset=='XETH'~'ETH',
                               TRUE~Asset))%>%
        unique()%>%
        left_join(select(crypt_table, Ticker, `Current Price`=Price), by=c('Asset'='Ticker'))%>%
        select(Asset, `Current Price`, `50 Week Average`, !!overall_col_name)
      
      averages_graph <- ggplot() + geom_line(aes(x=fifty_week_pull$Date, y=fifty_week_pull$Close)) + geom_hline(yintercept = overall_pt, color="red", size=1.25)+
        geom_hline(yintercept = fiftyweek_pt, color="green", size=1.25) +
        geom_text(aes(opening_week, overall_pt, label=paste0("$",round(overall_pt,2))),vjust=-.7)+ #have to subset first date of table to plot on x axis
        geom_text(aes(opening_week, overall_pt, label=paste(week_total, "Week Average", sep=" "), vjust=1.3))+
        geom_text(aes(opening_week, fiftyweek_pt, label=paste0("$",round(fiftyweek_pt,2))), vjust=-.7)+
        geom_text(aes(opening_week, fiftyweek_pt, label=paste('50 Week Average'), vjust=1.3))+
        theme_classic() + xlab('Weeks') + ylab('Weekly Closing Price')
      
      output$avggraph <- renderPlot(averages_graph)
      output$avgtable <- renderFormattable(formattable(avg_table, align='c'))
      
    }
    
    #dividing it up like this to try and get better separation
    else if(week_count_val <75){
      
      fifty_week_pull <- twohund_week_pull %>%
        mutate(time=as.numeric(substr(time, 1,10)))%>%
        mutate(Date=as.POSIXct(time, origin="1970-01-01", tz='EST'))%>%
        mutate_at(vars("Open", "High","Low","Close"), as.numeric)%>%
        select(Date, Open, High, Low, Close)%>%
        mutate(`Week Number`=rev(seq.int(nrow(twohund_week_pull))))%>%
        mutate(`Week Count`=nrow(twohund_week_pull))%>%
        mutate(`200 Week Average`=mean(Close))%>%
        mutate(`50week_flag`=case_when(`Week Number`<51 ~ 1,
                                       TRUE~0))%>%
        mutate(`100week_flag`=case_when(`Week Number`<100~1,
                                        TRUE~0))%>%
        group_by(`50week_flag`)%>%
        mutate(`50 Week Average`=mean(Close))
      
      overall_pt <- fifty_week_pull[[nrow(fifty_week_pull),8]]
      fiftyweek_pt <- fifty_week_pull[[nrow(fifty_week_pull),11]]
      today_pt <- fifty_week_pull[[nrow(fifty_week_pull),2]]
      opening_week <- fifty_week_pull[[1,1]] #first week close for plotting h_lines
      week_total <- fifty_week_pull[[1,7]]
      
      overall_col_name <- paste(week_total, 'Week Average', sep=' ')
      overall_col_name <- enquo(overall_col_name)
      
      #pulls the pair names and joins the api filter id with the base name - ticker - which needs to be used to filter price
      asset_pairs <- get_avg_pairs()[["result"]]%>%
        list.filter(grepl('USD', altname))
      
      asset_base <- get_avg_pairs()[["result"]]%>%
        list.select(altname, base)
      
      asset_count <- length(asset_pairs)
      
      asset_base <- rbindlist(asset_base)%>%
        filter(grepl('USD', altname))%>%
        mutate(id=1:asset_count)
      
      base_names <- names(asset_pairs)%>%
        as.data.frame()%>%
        mutate(id=1:asset_count)
      
      colnames(base_names) <- c('asset_tick', 'id')
      
      base_names <- base_names %>%
        left_join(asset_base)
      
      #pulling in price data - will need to filter here for coin - so will need actual pair
      #may need to look into getting listings for a specific coin
      ct1 <- getLatestListings(200, "USD")
      
      crypt_table <- ct1%>%
        mutate(Price=ct1$USD$price)%>%
        mutate(`Volume 24H`=ct1$USD$volume_24h)%>%
        mutate(`Volume Change 24H`=ct1$USD$percent_change_24h)%>%
        mutate(`% Change 1H`=ct1$USD$percent_change_1h)%>%
        mutate(`% Change 24H`=ct1$USD$percent_change_24h)%>%
        mutate(`% Change 7D`=ct1$USD$percent_change_7d)%>%
        mutate(`% Change 30D`=ct1$USD$percent_change_30d)%>%
        mutate(`Market Cap`=ct1$USD$market_cap)%>%
        mutate(`Market Cap Dominance`=ct1$USD$market_cap_dominance)%>%
        select(Coin, Ticker, Price)
      
      #average table to be displayed under the graph
      avg_table <- fifty_week_pull %>%
        ungroup()%>%
        filter(`50week_flag`==1 & `100week_flag`==1)%>%
        mutate(asset_tick=filtchoice)%>%
        left_join(base_names)%>%
        #mutate(!!overall_col_name:=`200 Week Average`)%>%
        select(Asset=base,!!overall_col_name:=`200 Week Average`)%>%
        #mutate(Asset=base)%>%
        mutate(Asset=case_when(Asset=='XXBT'~'BTC',
                               Asset=='XETH'~'ETH',
                               TRUE~Asset))%>%
        unique()%>%
        left_join(select(crypt_table, Ticker, `Current Price`=Price), by=c('Asset'='Ticker'))%>%
        select(Asset, `Current Price`, !!overall_col_name)
      
      averages_graph <- ggplot() + geom_line(aes(x=fifty_week_pull$Date, y=fifty_week_pull$Close)) + geom_hline(yintercept = overall_pt, color="red", size=1.25)+
        geom_text(aes(opening_week, overall_pt, label=paste0("$",round(overall_pt,2))),vjust=-.7)+ #have to subset first date of table to plot on x axis
        geom_text(aes(opening_week, overall_pt, label=paste(week_total, "Week Average", sep=" "), vjust=1.3))+
        theme_classic() + xlab('Weeks') + ylab('Weekly Closing Price')
      
      output$avggraph <- renderPlot(averages_graph)
      output$avgtable <- renderFormattable(formattable(avg_table, align='c'))
      
    }
    
    removeModal()
    
    
  })
  
  observeEvent(input$refresh, {
    
    showModal(modalDialog("Loading Market from CoinMarketCap", footer=NULL))
    
    Sys.sleep(1)
    
  ###MARKET SCRAPE - Performs each time for most recent price
  ct1 <- getLatestListings(input$marketcount, "USD")
  
  crypt_table <- ct1%>%
    mutate(Price=ct1$USD$price)%>%
    mutate(`Volume 24H`=round(ct1$USD$volume_24h,2))%>%
    mutate(`Volume Change 24H`=ct1$USD$percent_change_24h)%>%
    mutate(`% Change 1H`=ct1$USD$percent_change_1h)%>%
    mutate(`% Change 24H`=ct1$USD$percent_change_24h)%>%
    mutate(`% Change 7D`=ct1$USD$percent_change_7d)%>%
    mutate(`% Change 30D`=ct1$USD$percent_change_30d)%>%
    mutate(`Market Cap`=round(ct1$USD$market_cap,2))%>%
    mutate(`Market Cap Dominance`=ct1$USD$market_cap_dominance)%>%
    select(Coin, Ticker, Price, `Volume 24H`, `Volume Change 24H`, `% Change 1H`, `% Change 24H`, `% Change 7D`, `% Change 30D`, `Market Cap`, `Market Cap Dominance`)
  
  
  output$market <- renderDataTable(datatable(crypt_table, options = list(
    rowCallback = JS(
      "function(row, data) {",
      "var num = '$' + data[3].toString();",
      "$('td:eq(3)', row).html(num);",
      "}"
    )
  )))
  
  removeModal()
  
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)