#Packages
library(shiny)
library(leaflet)
library(dplyr)
library(shinyWidgets)
library(foreign)
library(sp)
library(sf)
library(htmltools)
library(readr)
library(tigris)
library(shinyjs)


#load in base table

supp_homi_fips <- read_rds('supp_homi_fips_20.rds')%>%
  select(ID, CNTYFIPS, State, Solved, Year, VicAge, VicSex, VicRace,OffAge, OffSex,OffRace,Weapon,county_fips)%>%
  mutate(VicAgeRange=case_when(VicAge == 0 ~ "<1",
                               VicAge>=0 & VicAge <10 ~ "1-9",
                               VicAge>=10 & VicAge <20 ~ "10-19",
                               VicAge>=20 & VicAge <30 ~ "20-29",
                               VicAge>=30 & VicAge <40 ~ "30-39",
                               VicAge>=40 & VicAge <50 ~ "40-49",
                               VicAge>=50 & VicAge <60 ~ "50-59",
                               VicAge>=60 & VicAge <70 ~ "60-69",
                               VicAge>=70 & VicAge <555 ~ "70+",
                                 VicAge==999 ~ "Unknown"))%>%
  mutate(OffAgeRange=case_when(OffAge == 0 ~ "<1",
                               OffAge>=0 & OffAge <10 ~ "1-9",
                               OffAge>=10 & OffAge <20 ~ "10-19",
                               OffAge>=20 & OffAge <30 ~ "20-29",
                               OffAge>=30 & OffAge <40 ~ "30-39",
                               OffAge>=40 & OffAge <50 ~ "40-49",
                               OffAge>=50 & OffAge <60 ~ "50-59",
                               OffAge>=60 & OffAge <70 ~ "60-69",
                               OffAge>=70 & OffAge <555 ~ "70+",
                               OffAge==999 ~ "Unknown"))

#created from the murderdata.org data and prepped in the dataprep script
all_counties <- read_rds('all_counties.rds') #From tigris to save time loading from the package every tiem
state_fips <- read_rds('state_fips.rds') #state fips list for filtering county maps - taken from census - joins for county map

#from tigris counties - as SF
#all_counties <- counties(state = NULL, class="sf" , cb=TRUE, resolution="500k", year=NULL) #class can be SP or SF

#base map
map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap)

#initial input values for years
yearmin <- min(supp_homi_fips$Year)
yearmax <- max(supp_homi_fips$Year)

#murder weapon list
weaplist <- select(supp_homi_fips, Weapon)%>%
  unique()%>%
  as.list

#victim sex list
vicsexlist <- select(supp_homi_fips, VicSex)%>%
  unique()%>%
  as.list

#victim race list
vicracelist <- select(supp_homi_fips, VicRace)%>%
  unique()%>%
  as.list

#offender sex list
offsexlist <- select(supp_homi_fips, OffSex)%>%
  unique()%>%
  as.list

#offender race list
offracelist <- select(supp_homi_fips, OffRace)%>%
  unique()%>%
  as.list

#state list
statelist <- select(state_fips, `State Name`=State_Name)%>%
  unique()%>%
  as.list

# age list 
agerange <- list("<1","1-9","10-19","20-29","30-39","40-49","50-59","60-69","70+","Unknown")

#function to put decreasing legend in leaflet
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}

#Shiny

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("murdermap", width = "100%", height="100%"),
  
  
  absolutePanel(br(),br(), br(), top=10, left=10,
                selectInput("solved","Case Outcome", choices= list("Solved"=1, "Unsolved"=0, "All"=2), selected = 2, width = "125px"),
                numericRangeInput("years","Years", value = c(yearmax, yearmax), separator = "to", width = "185px", min = yearmin, max = yearmax), 
                radioButtons("weapfilter", "Weapon Filter", choices=list("Yes"=1, "No"=2), selected = 2, inline = TRUE),
                conditionalPanel("input.weapfilter==1",
                                 selectInput("weapon","Murder Weapon", choices=(weaplist), multiple = TRUE, selected ='Handgun - pistol, revolver, etc')),
                radioButtons('stfilter', "State Filter", choices=list("Yes"=1,"No"=2), selected = 2, inline=TRUE),
                conditionalPanel("input.stfilter==1",
                                 selectInput("state", "State", choices=(statelist), multiple = TRUE, selected='Illinois')),
                radioButtons('vic_filt', "Victim Filters", choices = list("Yes"=1, "No"=2), selected = 2, inline=TRUE),
                conditionalPanel("input.vic_filt==1",
                                 selectInput("vicsex", "Victim Sex", choices=(vicsexlist), multiple = TRUE, selected = 'Female'),
                                 selectInput("vicrace", "Victim Race", choices = (vicracelist),multiple = TRUE, selected = 'American Indian or Alaskan Native'),
                                 selectInput("vicagerange", "Victim Age", choices = (agerange), multiple = TRUE, selected = "<1")),
                conditionalPanel("input.solved==1",
                                 radioButtons('off_filt', "Offender Filters", choices = list("Yes"=1, "No"=2), selected = 2, inline = TRUE),
                                 conditionalPanel("input.off_filt==1",
                                                  selectInput("offsex", "Offender Sex", choices = (offsexlist), multiple = TRUE, selected = 'Female'),
                                                  selectInput('offrace', "Offender Race", choices = (offracelist), multiple = TRUE, selected = 'American Indian or Alaskan Native'),
                                                  selectInput('offage', "Offender Age", choices = (agerange), multiple = TRUE, selected = "10-19"))),
                actionButton('runbutton', "Run"),br(), br(),
                actionButton('notes', "Show Notes")
                
  ))

server <- function(input, output, session) {
  
  
  output$murdermap <- renderLeaflet({
    leaflet()%>%
      addTiles(providers$OpenStreetMap)%>%
      setView(-98.4842,39.0119,5)
  })
  
  showModal(modalDialog(h5(strong("Welcome to the Homicide Map")),
                        h5("Please select the filters you are interested in and click the 'Run' button on the left. Each time you press the 'Run' button it will remap using the filters you selected."),
                        h5(paste("Year Range:", yearmin, "-", yearmax, sep = ' ')),
                        h5("Displayed Counts: This is a case count displayed at the county level. There may be more than one victim in each case. There may also be multiple law enforcement agencies operating within each county."),
                        h5("Data Source: murderdata.org"),
                        h5("Contact: Taylor@bytethehand.org")))
  
  #Resets the filters when radio buttons changed to no
  observeEvent(input$solved,{ if(input$solved!=1){updateRadioButtons(session, "off_filt", selected = 2)}})
  observeEvent(input$weapfilter,{ if(input$weapfilter==2){updateSelectInput(session, "weapon", selected = 'Handgun - pistol, revolver, etc')}})
  observeEvent(input$stfilter,{if(input$stfilter==2){updateSelectInput(session, "state", selected='Illinois')}})
  observeEvent(input$vic_filt,{if(input$vic_filt==2){updateSelectInput(session, 'vicsex', selected = 'Female') 
                                                     updateSelectInput(session, 'vicrace', selected = 'American Indian or Alaskan Native')
                                                     updateSelectInput(session, 'vicage', selected = '<1')}})
  observeEvent(input$off_filt,{if(input$off_filt==2){updateSelectInput(session, 'offsex', selected = 'Female')
                                                    updateSelectInput(session, 'offrace', selected = 'American Indian or Alaskan Native')
                                                    updateSelectInput(session, 'offage', selected = '10-19')}})
  #below evaluates for the yes offender, but does not include the filter unless it's solved - prevents crash, but doesn't filter since data is not there for offenders in unsolved
  
  observeEvent(input$runbutton,{
    
    showModal(modalDialog("Loading...", footer=NULL))
    if(input$stfilter==2){Sys.sleep(6)}  #if the data has to be filtered for all states - 6 second delay - otherwise 3
    else{
      Sys.sleep(3)}
    
    removeModal()
  
  map_county_allyears <- reactive({
    
    
    ######START NO WEAPON NO STATE
    ###NO WEAPON, NO STATE, NO VIC, NO OFF
    #filters based on no weapon filter and no state filter and no vic and no off filter
    if(input$solved==1 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)#grouping by county fips instead of agency - since I will be mapping by county
    }
    #unsolved, no weap filter, no state filter
    if(input$solved==0 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    #all, no weap, no state filter
    if(input$solved==2 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>% 
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    
    #No Weapon No State
    #Yes Vic No Off
    if(input$solved==1 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)#grouping by county fips instead of agency - since I will be mapping by county
    }
    #unsolved, no weap filter, no state filter
    if(input$solved==0 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    #all, no weap, no state filter
    if(input$solved==2 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>% 
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    
    #NO Weapon No State
    #No Vic Yes Off
    if(input$solved==1 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(OffSex %in% input$offsex)%>%
        filter(OffRace %in% input$offrace)%>%
        filter(OffAgeRange %in% input$offage)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)#grouping by county fips instead of agency - since I will be mapping by county
    }
    #unsolved, no weap filter, no state filter
    if(input$solved==0 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    #all, no weap, no state filter
    if(input$solved==2 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>% 
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    
    #No Weapon No State
    #Yes Vic Yes Off
    if(input$solved==1 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(OffSex %in% input$offsex)%>%
        filter(OffRace %in% input$offrace)%>%
        filter(OffAgeRange %in% input$offage)%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)#grouping by county fips instead of agency - since I will be mapping by county
    }
    #unsolved, no weap filter, no state filter
    if(input$solved==0 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    #all, no weap, no state filter
    if(input$solved==2 & input$weapfilter==2 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>% 
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    
    
    ##################START YES WEAPON NO STATE
    
    ###YES WEAPON, NO STATE, NO VIC, NO OFF
    if(input$solved==1 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    if(input$solved==0 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    if(input$solved==2 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    
    #Yes Weapon No State
    #Yes Vic No Off
    if(input$solved==1 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    if(input$solved==0 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    if(input$solved==2 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    
    #Yes Weapon No State
    #No Vic Yes Off
    if(input$solved==1 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(OffSex %in% input$offsex)%>%
        filter(OffRace %in% input$offrace)%>%
        filter(OffAgeRange %in% input$offage)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    if(input$solved==0 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    if(input$solved==2 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    
    #Yes Weapon No State
    #Yes Vic Yes Off
    if(input$solved==1 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(OffSex %in% input$offsex)%>%
        filter(OffRace %in% input$offrace)%>%
        filter(OffAgeRange %in% input$offage)%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    if(input$solved==0 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    if(input$solved==2 & input$weapfilter==1 & input$stfilter==2 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS) #grouping by county fips instead of agency - since I will be mapping by county
    }
    
    
    
    
    #############START NO WEAPON YES STATE
    ###NO WEAPON, YES STATE, NO VIC, NO OFF
    #filters based no weap filter and state filter
    if(input$solved==1 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==0 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==2 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>% 
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    
    #No Weapon Yes State
    #Yes Vic No Off
    if(input$solved==1 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==0 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==2 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>% 
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    
    #No Weapon Yes State
    #No Vic Yes Off
    if(input$solved==1 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(OffSex %in% input$offsex)%>%
        filter(OffRace %in% input$offrace)%>%
        filter(OffAgeRange %in% input$offage)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==0 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==2 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>% 
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    
    #No Weapon Yes State
    #Yes Vic Yes Off
    if(input$solved==1 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(OffSex %in% input$offsex)%>%
        filter(OffRace %in% input$offrace)%>%
        filter(OffAgeRange %in% input$offage)%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==0 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==2 & input$weapfilter==2 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>% 
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    
    
    #############START YES WEAPON YES STATE
    ###YES WEAPON, YES STATE, NO VIC, NO OFF
    #filters for weapon and state filter
    if(input$solved==1 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==0 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==2 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    
    
    ###YES WEAPON, YES STATE
    #YES VIC, NO OFF
    if(input$solved==1 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==0 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==2 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==2){
      county_counts <- supp_homi_fips %>%
        filter(Weapon %in% input$weapon)%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    
    ###YES WEAPON, YES STATE
    #NO VIC, YES OFF
    if(input$solved==1 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==1){  #filter only applies here for the solved option
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(OffSex %in% input$offsex)%>%
        filter(OffRace %in% input$offrace)%>%
        filter(OffAgeRange %in% input$offage)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==0 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==2 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==2 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    
    ###YES WEAPON, YES STATE
    #YES VIC, YES OFF
    if(input$solved==1 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==1){  #filter only applies here for the solved option
      county_counts <- supp_homi_fips %>%
        filter(Solved=="Yes")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        filter(OffSex %in% input$offsex)%>%
        filter(OffRace %in% input$offrace)%>%
        filter(OffAgeRange %in% input$offage)%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==0 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Solved=="No")%>%
        filter(Weapon %in% input$weapon)%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    if(input$solved==2 & input$weapfilter==1 & input$stfilter==1 & input$vic_filt==1 & input$off_filt==1){
      county_counts <- supp_homi_fips %>%
        filter(Weapon %in% input$weapon)%>%
        filter(VicSex %in% input$vicsex)%>%
        filter(VicRace %in% input$vicrace)%>%
        filter(VicAgeRange %in% input$vicagerange)%>%
        filter(Year >= input$years[1] & Year <= input$years[2])%>%
        mutate(count=1)%>%
        group_by(CNTYFIPS)%>% #grouping by county fips instead of agency - since I will be mapping by county
        filter(State %in% input$state)}
    
    
    #Cannot sum when all observations have been filtered our in shiny - creates dummy observation and removes later in final ifelse evaluation
    
    ifelse(nrow(county_counts>=1),
           
           county_sums <- as.data.frame(county_counts) %>%   #at least one observation in the table
             group_by(CNTYFIPS)%>%
             mutate(`County Total`=sum(count)),
           
           county_sums <- supp_homi_fips[1,]%>%   #no observations creates dummy line
             mutate(`County Total`=NA)
           
           #need some data to join on - but need to remove it or filter it out for graphing - put NA for dummy county sums
           
    )
    
    all_counties %>% #all counties is from tigris is unsolved_run_code script
      left_join(county_sums, by=c('GEOID'='county_fips'), how="left")%>%
      left_join(state_fips, by=c('STATEFP'='st_fips'))%>% #state_fips is from the census in unsolved_run_code script
      select(State_Name, CNTYFIPS, GEOID, `County Total`, geometry)%>%
      unique()%>%
      filter(!is.na(`County Total`)) #Removes all NAs that can lead to a nrow = 0 for map_countyallyears --> leads to no mapping in final ifelse evaluation
    
  })
  
  
  #the color set up
  
  cpal <- reactive({colorNumeric(
    palette = "viridis",
    domain = map_county_allyears()$`County Total`)})
  
  #observe for the leafletproxy
    
    #Putting reactive color palette in
    pal <- cpal()
    
    #if else prevents a change if the user selection filters out the whole list - Good so far - need to add dates & clear for no evaluation
    
    ifelse((nrow(map_county_allyears())!=0 & input$weapfilter==2 & input$stfilter==2 & (input$years[1]<=input$years[2]) & (input$years[1]>=yearmin) & (input$years[2]<=yearmax)) #Weapon and state filter off
           |(nrow(map_county_allyears())!=0 & input$weapfilter==1 & input$weapon!="" & input$stfilter==2 & (input$years[1]<=input$years[2]) & (input$years[1]>=yearmin) & (input$years[2]<=yearmax)) #Weapon filter yes and weapon selected and state filter no
           |(nrow(map_county_allyears())!=0 & input$stfilter==1 & input$state!="" & input$weapfilter==2 & (input$years[1]<=input$years[2]) & (input$years[1]>=yearmin) & (input$years[2]<=yearmax))  #State filter yes and state selected and weap filter no
           |(nrow(map_county_allyears())!=0 & input$weapfilter==1 & input$weapon!="" & input$stfilter==1 & input$state!="" & (input$years[1]<=input$years[2]) & (input$years[1]>=yearmin) & (input$years[2]<=yearmax))
           ,
           #YES evaluation
           
           leafletProxy("murdermap")%>%
             clearControls()%>% #removes previous legends
             clearShapes()%>% #removes the current shapes so it doesn't stack
             addTiles()%>%
             addPolygons(data=map_county_allyears(),color=~pal(map_county_allyears()$`County Total`), fillOpacity = .6, label=paste(map_county_allyears()$CNTYFIPS,map_county_allyears()$`County Total`, sep=", "))%>%
             addLegend_decreasing("bottomright", pal=pal, values=map_county_allyears()$`County Total`, title = "Homicide Count", opacity = 1, decreasing = TRUE), #%>% #legend
             #setView(-98.4842,39.0119,5), #sets focus on Kansas/US - resets view every time? Annoying or good?
           
           #NO evaluation                    
           leafletProxy("murdermap")%>%
             clearShapes()
    )
    
    
})
  
  
  
  observeEvent(input$notes,{
    
    
    showModal(modalDialog(h5(strong("Welcome to the Homicide Map")),
                          h5("Please select the filters you are interested in and click the 'Run' button on the left. Each time you press the 'Run' button it will remap using the filters you selected."),
                          h5(paste("Year Range:", yearmin, "-", yearmax, sep = ' ')),
                          h5("Displayed Counts: This is a case count displayed at the county level. There may be more than one victim in each case. There may also be multiple law enforcement agencies operating within each county."),
                          h5("Data Source: murderdata.org"),
                          h5("Contact: Taylor@bytethehand.org")))
    
    
  })
  
} #end

shinyApp(ui, server)