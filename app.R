#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(naniar)

#undebug(styleEqual)
Update_Dat <- read.csv2("Update_Data.csv",sep = ",")


Update_Dat$ISP <-  gsub(x = Update_Dat$ISP, "ceratophyllum","a")
Update_Dat$ISP <-  gsub(x = Update_Dat$ISP, "egeria","b")
Update_Dat$ISP <-  gsub(x = Update_Dat$ISP, "elodea","c")
Update_Dat$ISP <-  gsub(x = Update_Dat$ISP, "lagarosiphon","d")
Update_Dat$ISP <-  gsub(x = Update_Dat$ISP, "potamogeton_crispus","e")
Update_Dat$ISP <-  gsub(x = Update_Dat$ISP, "ranunculus_trichophyllus","f")
Update_Dat$ISP <-  gsub(x = Update_Dat$ISP, "juncus_bulbosus","g")

  
  # Note	Species
  # d	lagarosiphon
  # c	elodea
  # b	egeria
  # a	ceratophyllum
  # e	potamogeton_crispus
  # f	ranunculus_trichophyllus
  # g	juncus bulbosus
  
Update_Dat <- Update_Dat %>% mutate(TN..Trend. = case_when(
  TN..Trend. == "++" ~ "Very Likely Improving",
  TN..Trend. == "+" ~ "Likely Improving",
  TN..Trend. == "-" ~ "Likely Worsening",
  TN..Trend. == "--"   ~ "Very Likely Worsening",
  TN..Trend. == "--/--" ~ "Very Likely Worsening",
  TN..Trend. == "--/-" ~ "Very Likely Worsening",
  TN..Trend. == "TBC" ~ "TBC",
  TN..Trend. == "N/A" ~ NA,
  TRUE ~ "Indeterminate"
))

Update_Dat <- Update_Dat %>% mutate(TP..Trend. = case_when(
  TP..Trend. == "++" ~ "Very Likely Improving",
  TP..Trend. == "+" ~ "Likely Improving",
  TP..Trend. == "+/+" ~ "Likely Improving",
  TP..Trend. == "-" ~ "Likely Worsening",
  TP..Trend. == "--"   ~ "Very Likely Worsening",
  TP..Trend. == "--/--" ~ "Very Likely Worsening",
  TP..Trend. == "TBC" ~ "TBC",
  TP..Trend. == "N/A" ~ NA,
  TRUE ~ "Indeterminate"
))

Update_Dat <- Update_Dat %>% mutate(CHLA..Trend. = case_when(
  CHLA..Trend. == "++" ~ "Very Likely Improving",
  CHLA..Trend. == "+" ~ "Likely Improving",
  CHLA..Trend. == "-" ~ "Likely Worsening",
  CHLA..Trend. == "--"   ~ "Very Likely Worsening",
  CHLA..Trend. == "--/--" ~ "Very Likely Worsening",
  CHLA..Trend. == "-/+" ~ "Likely Worsening",
  CHLA..Trend. == "n/-" ~ "Likely Worsening",
  CHLA..Trend. == "-/--" ~ "Very Likely Worsening",
  CHLA..Trend. == "TBC" ~ "TBC",
  CHLA..Trend. == "N/A" ~ NA,
  CHLA..Trend. == "n/+" ~ "Indeterminate",
  TRUE ~ "Indeterminate"
))

Update_Dat <- Update_Dat %>% mutate(Clarity..Trend. = case_when(
  Clarity..Trend. == "++" ~ "Very Likely Improving",
  Clarity..Trend. == "+" ~ "Likely Improving",
  Clarity..Trend. == "-" ~ "Likely Worsening",
  Clarity..Trend. == "--"   ~ "Very Likely Worsening",
  Clarity..Trend. == "--/--" ~ "Very Likely Worsening",
  Clarity..Trend. == "-/+" ~ "Likely Worsening",
  Clarity..Trend. == "-/--" ~ "Very Likely Worsening",
  Clarity..Trend. == "--/-" ~ "Very Likely Worsening",
  Clarity..Trend. == "n/-" ~ "Likely Worsening",
  Clarity..Trend. == "n/+" ~ "Indeterminate",
  Clarity..Trend. == "TBC" ~ "TBC",
  Clarity..Trend. == "N/A" ~ NA,
  TRUE ~ "Indeterminate"
))

names(Update_Dat) <- c("Name","TLI Target","TLI (Current)","TLI (3 yr Avg)", 
                       "TN","TN (Trend)",
                       "TP","TP (Trend)",
                       "CHLA (Med)","CHLA (Max)","CHL (Trend)",
                       "Secchi (Trend)",
                       "Lake SPI","Lake SPI (Native)","Lake SPI (Invasive)",
                       "Cyano Warning","Cyano Biovolume",
                       "Swimming Water Quality",
                       "Koura Abundance","Koura (Trend)","Reason for Change (Koura)","Most Recent Survey (Koura)",
                       "Kakahi Abundance","Kakahi (Trend)","Survey Type (Kakahi)",
                       "Catfish Abundance","Catfish (Trend)",
                       "Invasive Submerged Plants","LSU")

#browser()

Update_Dat <- Update_Dat[,-29]

Update_Dat$`Koura (Trend)`<- ifelse(Update_Dat$`Koura (Trend)` == "-","Worsening",
                                 ifelse(Update_Dat$`Koura (Trend)` == "+","Improving",
                                        ifelse(Update_Dat$`Koura (Trend)` == "n","Stable",NA)))

Update_Dat$`Kakahi (Trend)`<- ifelse(Update_Dat$`Kakahi (Trend)` == "-","Worsening",
                                 ifelse(Update_Dat$`Kakahi (Trend)` == "+","Improving",
                                        ifelse(Update_Dat$`Kakahi (Trend)` == "n","Stable",NA)))

Update_Dat$`Catfish (Trend)`<- ifelse(Update_Dat$`Catfish (Trend)` == "+","Worsening",
                                   ifelse(Update_Dat$`Catfish (Trend)` == "-","Improving",
                                          ifelse(Update_Dat$`Catfish (Trend)` == "n","Stable",NA)))


Update_Dat$`Reason for Change (Koura)`<- ifelse(Update_Dat$`Reason for Change (Koura)` == "N/A",NA,Update_Dat$`Reason for Change (Koura)`)


Lake_Coords <- read.csv("Lake_Coords.csv")

#Update_Dat <-Update_Dat[,c(1,2,3,4,5,7,9,10,13,6,8,11,12,14,15,16,17,18,19)]
WQ_Att <- Update_Dat[,c("Name","TLI Target","TLI (Current)","TLI (3 yr Avg)", "TN",
                        "TP","CHLA (Med)","CHLA (Max)","TN (Trend)","TP (Trend)",
                        "CHL (Trend)","Secchi (Trend)","Cyano Warning","Cyano Biovolume","Swimming Water Quality")]

Eco_Att <- Update_Dat[,c("Name","Lake SPI","Lake SPI (Native)","Lake SPI (Invasive)","Invasive Submerged Plants","Koura Abundance","Koura (Trend)","Reason for Change (Koura)","Kakahi Abundance","Kakahi (Trend)","Catfish Abundance","Catfish (Trend)")]

TLI_Dat <- read.csv2("TLI_Table.csv",sep = ",")


TLI_Dat <- TLI_Dat %>%
  mutate(across(c(CHL:TLI),as.numeric))


#names(TLI_Dat) <- c("Lake",	"Season",	"Chl",	"SD",	"TN",	"TP",	"TLI_Sample",	"TLIx_SE",	"TLn",
#                    "TLp",	"TLs",	"TLc",	"TLI")
#TLI_Dat <- TLI_Dat[-1,]

WQ_Att <- WQ_Att %>% replace_with_na_all(condition = ~.x == "N/A") 
Eco_Att <- Eco_Att%>% replace_with_na_all(condition = ~.x == "N/A") 

library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(ggplot2)
library(plotly)
library(shinycssloaders)

WQ_Att$`TLI Target` <-  as.numeric(WQ_Att$`TLI Target`)
WQ_Att$`TLI (Current)` <-  as.numeric(WQ_Att$`TLI (Current)`)
WQ_Att$`TLI (3 yr Avg)` <-  as.numeric(WQ_Att$`TLI (3 yr Avg)`)
Update_Dat$`TLI Target` <-  as.numeric(Update_Dat$`TLI Target`)
Update_Dat$`TLI (Current)` <-  as.numeric(Update_Dat$`TLI (Current)`)
Update_Dat$`TLI (3 yr Avg)` <-  as.numeric(Update_Dat$`TLI (3 yr Avg)`)


WQ_Att <- WQ_Att %>% 

  mutate(TLI_MET = case_when(
  `TLI (Current)` <= `TLI Target` ~ 1,
  `TLI (Current)` <= (`TLI Target`+0.2) ~ 2,
  `TLI (Current)` > (`TLI Target`+0.2) ~ 3,
  TRUE ~ 4))


WQ_Att <- WQ_Att %>% 

  mutate(TLI_MET_3 = case_when(
  `TLI (3 yr Avg)` <= `TLI Target` ~ 1,
  `TLI (3 yr Avg)` <= (`TLI Target`+0.2) ~ 2,
  `TLI (3 yr Avg)` > (`TLI Target`+0.2) ~ 3,
  TRUE ~ 4))

Update_Dat <- Update_Dat %>% 

  mutate(TLI_MET = case_when(
  `TLI (Current)` <= `TLI Target` ~ 1,
  `TLI (Current)` <= (`TLI Target`+0.2) ~ 2,
  `TLI (Current)` > (`TLI Target`+0.2) ~ 3,
  TRUE ~ 4))


Update_Dat <- Update_Dat %>% 

  mutate(TLI_MET_3 = case_when(
  `TLI (3 yr Avg)` <= `TLI Target` ~ 1,
  `TLI (3 yr Avg)` <= (`TLI Target`+0.2) ~ 2,
  `TLI (3 yr Avg)` > (`TLI Target`+0.2) ~ 3,
  TRUE ~ 4))



# a custom table container
sketch_WQ = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan=2, colspan = 1, 'Lake',style = "border-right: solid 2px;"),
      th(colspan = 3, 'Trophic Level Index',style = "border-right: solid 2px;"),
      th(colspan = 4, 'National Policy Statement for Freshwater Attributes',style = "border-right: solid 2px;"),
      th(colspan = 4, 'Ten Year Trends',style = "border-right: solid 2px;"),
      th(colspan = 3, 'Contact Recreational Attributes')
    ),
    # tr(
    #   lapply(c("Name","TLI Target","TLI (Current)","TLI (3 yr Avg)", "TN",
    #            "TP","CHLA (Med)","CHLA (Max)","TN (Trend)","TP (Trend)",
    #            "CHL (Trend)","Secchi (Trend)","Cyano Warning","Cyano Biovolume","Swimming Water Quality")
    #          
    #          , th)
      
      
      
      th('TLI Target'),
      th('TLI (Current)'),
      th('TLI (3 yr Avg)',style = "border-right: solid 2px;"),
      th('TN'),
      th('TP'),
      th('CHL-A (Med)'),
      th('CHL-A (Max)',style = "border-right: solid 2px;"),
      th('TN (Trend)'),
      th('TP (Trend)'),
      th('CHL-A (Trend)'),
      th('Secchi (Trend)',style = "border-right: solid 2px;"),
      th('Cyano Warning'),
      th('Cyano Biovolume'),
      th('Swimming Water Quality')
      
      
      
      
    )
  )
)


sketch_ECOL = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, colspan = 1, 'Lake', style = "border-right: solid 2px;"),
      th(colspan = 4, 'Lake Submerged Plant Index', style = "border-right: solid 2px;"),
      th(colspan = 3, 'Koura', style = "border-right: solid 2px;"),
      th(colspan = 2, 'Kakahi', style = "border-right: solid 2px;"),
      th(colspan = 2, 'Catfish')
    ),
    
    #th('Lake',style = "border-right: solid 2px;"),
    th('Lake SPI'),
    th('Lake SPI (Native)'),
    th('Lake SPI (Invasive)'),
    th('Invasive Submerged Plants',style = "border-right: solid 2px;"),
    th('Koura Abundance'),
    th('Koura (Trend)'),
   # th('Koura (CPUE)'),
    th('Reason for Change (Koura)',style = "border-right: solid 2px;"),
    th('Kakahi Abundance'),
    th('Kakahi (Trend)',style = "border-right: solid 2px;"),
    th('Catfish Abundance'),
    th('Catfish Trend')
      )
  )
)



sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Lakes Scorecard", tabName = "Scorecard", icon = icon("info-circle")),
    menuItem("Water Quality Attributes", tabName = "Table_WQ", icon = icon("table")),
    menuItem("Ecological Attributes", tabName = "Table_ECOL", icon = icon("table")),
    menuItem("TLI Figures", tabName = "TLI", icon = icon("chart-line")),
    menuItem("Map", icon = icon("globe"), tabName = "Map"),
    br(),
    a(actionButton(inputId = "email1", label = "Send Feedback", 
                   icon = icon("envelope", lib = "font-awesome")),
      href="mailto:james.dare@boprc.govt.nz?subject=Council Scorecard Feedback")
  )
)




header <- dashboardHeader(title = "Rotorua Te Arawa Lakes Summary 2024/2025",
                          titleWidth = 450
                          )



body <- dashboardBody(
  
  tabItems(
    
    tabItem(tabName = "Scorecard",
            fluidRow(
              box(width="100%",
                  solidHeader = TRUE,
                  h2("Welcome"),
                  HTML("This prototype lakes report dashboard is intended to provide greater context around annual TLI reporting, enabling councillors and people managers to make informed decisions about lake management.<br><br>The PDF document below is available to download if you would like a hard-copy summary, otherwise use the side panel options to explore information in more detail."),
                  br(),
                  br(),
                  #htmlOutput('pdfviewer'),
                  tags$iframe(style="height:800px; width:100%", src="Lake Statistics Fact Sheet.pdf")
              )#box
            )#fluid row
            
    ),#tab item
    
    
    
    tabItem(tabName = "Table_WQ",
            fluidRow(
              box(width="100%",
                  solidHeader = TRUE,
                  h2("Water Quality Attribute Table"),
                  HTML("This table summarises all relevant water quality attribute information for the 2019/20 lake year.  Double click on column headings to sort rows according to an attribute of interest."),
                  br(),
                  br(),
                  #htmlOutput('pdfviewer'),
                  DT::dataTableOutput("example_table_WQ"),
                  HTML("* Lake Rotokakahi was unable to be sampled in 2023/24 due to access issues."),
                  br(),
                  HTML("* Trophic Level Index shading: TLI (or TLI3) less than or equal to TLI target = ‘Excellent/Blue’,  TLI (or TLI3) within 0.2 of TLI target = ‘Good/Green’; TLI (or TLI3) worse than 0.2 of TLI target = ‘Poor/Red’."),
                  br(),
                  HTML("* NPS-FM Human contact attribute based on 95 percentile E. coli over the most recent five bathing seasons.  The lowest (worst) grade is shown where lakes have more than one bathing site."),
                  br(),
                  HTML("* Lake Tarawera is not routinely monitored for cyanobacteria, however ad-hoc samples collected in response to public concern has resulted in health warnings in past seasons.")

              )#box
            )#fluid row
            
    ),#tab item
    
    tabItem(tabName = "Table_ECOL",
            fluidRow(
              box(width="100%",
                  solidHeader = TRUE,
                  h2("Ecological Attribute Table"),
                  HTML("This table summarises all relevant ecological attributes for the 2019/20 lake year.  Double click on column headings to sort rows according to an attribute of interest."),
                  br(),
                  br(),
                  #htmlOutput('pdfviewer'),
                  DT::dataTableOutput("example_table_ECOL"),

                  HTML("* Invasive Submerged Plants:&nbsp&nbsp&nbsp a) Ceratophyllum;&nbsp&nbsp    b) Egeria;&nbsp&nbsp    c) Elodea;&nbsp&nbsp    d) Lagarosiphon;&nbsp&nbsp    e) <i>Potamogeton crispus</i>;&nbsp&nbsp    f) <i>Ranunculus trichophyllus;</i>&nbsp&nbsp g) <i>Juncus Bulbosus</i>&nbsp&nbsp"),
                  br(),
                  HTML("* LakeSPI native and invasive indices refer to tables 11 and 12 in the NPS-FM."),
                  br(),
                  HTML("* LakeSPI data is based on NIWA surveys carried out between 2018 and 2024."),
                  br(),
                  HTML("* Refer to handout for details about Koura, Kakahi, and Catfish monitoring periods.")
                  )#box
            )#fluid row
            
    ),#tab item
    
    
    tabItem(tabName = "TLI",
            fluidRow(
              box(width="100%",
                  solidHeader = TRUE,
                  h2("TLI Figures"),
                  HTML("The figure below shows the TLI for each monitored lake from the year 2000 to present.  The coloured dashed lines represent the TLI target for each lake, and are colour-matched to respective TLI timeseries.<br><br>The default setting shows each lake in an individual panel, but you can change this and view all lakes on the same axis by clicking the check box.<br><br>Plots are also interactive, which means that you can see the value of individual points by hovering over them.  You can also turn individual lakes 'on' and 'off' by clicking on their name in the legend, or filter to a lake of interest by double clicking the name in the legend."),
                  br(),
                  br(),
                  checkboxInput(inputId = "allplot",label =  "Show all lakes on the same axis?", value = FALSE, width = NULL),
                  withSpinner(plotlyOutput("TLI_Plot",height = 600,width = 800),type = 4, color = "#0dc5c1", size = 1)

              )#box
            )#fluid row
            
    ),#tab item
    
    
    tabItem(tabName = "Map",
            
            fluidRow(
              box(width="100%",
                  solidHeader = TRUE,
                  h2("Attribute Map"),
                  HTML("The map below shows a spatial representation of the attributes presented in 'Table' tab.  You can change the attribute of interest via the drop-down menu.  A legend is positioned in the lower right hand corner, and note that any missing values are coloured grey"),
                  br(),
                  br(),
                  
                         selectizeInput(inputId = 'Attr_Map',label ='Select an Attribute', 
                                        choices = c(names(Update_Dat[c(3:20,23:24,26:27)])),width = 200),
                         
                  br(),
                
                  column(12,
                  leafletOutput("Map",width = 800, height=700)
                  )
                  
              )#box
            )#fluid row
        
    )#tab item
    
        )#tab items
)#dashboard body
    

ui <- dashboardPage(header,
                    sidebar,
                    body
)


server <- function(input, output) {
  

  output$TLI_Plot <- renderPlotly({
    
    Plot_Data <- TLI_Dat
    TLI_Target <- Update_Dat[,1:2]
    names(TLI_Target) <- c("Lake","Target")
    TLI_Target <-data.frame(TLI_Target)
    TLI_Target$Target <- as.numeric(TLI_Target$Target)
    Plot_Data <- merge(Plot_Data,TLI_Target,by="Lake")
                       
    Plot_Data$Date <- as.Date(paste0(as.numeric(substr(Plot_Data$Season,start = 0,stop = 4))+1,"-07-01"),tz="")
    
    if(input$allplot == TRUE){
      p <- ggplot(Plot_Data, aes(x=Date, y=TLI,colour=Lake))+
        geom_point()+geom_line()+
        theme_bw()+
        ylab("TLI (TLI Units)")+
        #scale_y_continuous(limits = c(min(Plot_Data$TLI)*1.3,max(Plot_Data$TLI)*0.9))+
        xlab(NULL)+
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
        geom_hline(data = TLI_Target, aes(yintercept = Target,colour = Lake),lty=2)
        #facet_wrap(~Lake)
      
      fig <- ggplotly(p)
      
      fig
      
      
    }else{
      
      p <- ggplot(Plot_Data, aes(x=Date, y=TLI,colour=Lake))+
        geom_point()+geom_line()+
        theme_bw()+
        ylab("TLI (TLI Units)")+
        #scale_y_continuous(limits = c(min(Plot_Data$TLI)*1.3,max(Plot_Data$TLI)*0.9))+
        xlab(NULL)+
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
        geom_hline(data = TLI_Target, aes(yintercept = Target,colour = Lake),lty=2)+
        facet_wrap(~Lake)
      
      fig <- ggplotly(p)
      
      fig
      
      
    }
    
  })
  
  ########################
  ### Download Handler ###
  ########################
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Lakes_Fact_Sheet",Sys.time(), ".docx", sep = "")
    },
    content = function(file) {
      file.copy("Lake Statistics Fact sheet.docx", file)
    }
  )
  
  ########################
  ########## MAP #########
  ########################
  
  output$Map <- renderLeaflet({
  
  current_att <- input$Attr_Map

  data=Update_Dat[,c("Name",current_att)] 
  data=merge(data,Lake_Coords,by.x="Name",by.y="Lake")

 
  if(current_att %in% c("TN (Trend)", "TP (Trend)", "CHL (Trend)","Secchi (Trend)")){
    
    mypalette <-   colorFactor(
      palette = c('#00A2E1',"#62BD19","dimgrey","#FF671F","#D32939","grey90","grey90"),
      domain = c("Very Likely Improving","Likely Improving", "Indeterminate","Likely Worsening","Very Likely Worsening","N/A","Not Monitored"),
      ordered = TRUE
    )
    
    myvalues=c("Very Likely Improving","Likely Improving", "Indeterminate","Likely Worsening","Very Likely Worsening","Not Monitored")
    myvalues <- factor(myvalues,levels=c("Very Likely Improving","Likely Improving", "Indeterminate","Likely Worsening","Very Likely Worsening","Not Monitored"))
    mytitle = "10 Yr Trend"
    
  }else if(current_att %in% c("Lake SPI (Native)","Lake SPI (Invasive)")){
    mypalette <-   colorFactor(
      palette = c('#00A2E1', '#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F','#FF671F',"grey90","grey90"),
      domain = c("A","B","B/B","C","C/C","C/B","D","D/D","C/D","N/A","Not Monitored"),
      ordered = TRUE
    )
    
    myvalues=factor(c("A","B","C","D","Not Monitored"),levels=c("A","B","C","D","Not Monitored"))
    mytitle = "NPS-FM Band"
    
  }else if(current_att %in% c("Lake SPI")){
    
    mypalette <-   colorFactor(
      palette = c('#00A2E1', '#62BD19','#FFC726','#FF671F',"grey90","grey90"),
      domain = c("Excellent", "High","Moderate","Poor","N/A","Not Monitored"),
      ordered = TRUE
    )
    
    myvalues=factor(c("Excellent", "High","Moderate","Poor","Not Monitored"),levels=c("Excellent", "High","Moderate","Poor","Not Monitored"))
    mytitle = "Lake SPI Rating"
  
    
  }else if (current_att %in% c("TLI (Current)")){
    #browser()
    data[,2] <- Update_Dat[,c("TLI_MET")]

    
    mypalette <-   colorFactor(
      palette = c("#00AAE5","#62BD19","#E36055","grey90"),
      domain = c(1,2,3,4),
      ordered = TRUE
    )
    
    
    myvalues=factor(c("Yes","No","Not Monitored"),levels = c("Yes","No","Not Monitored"))
    mytitle = "Cyanobacteria Warnings"

  }else if (current_att %in% c("TLI (3 yr Avg)")){
    
    data[,2] <- Update_Dat[,c("TLI_MET_3")]
    
    mypalette <-   colorFactor(
      palette = c("#00AAE5","#62BD19","#E36055","grey90"),
      domain = c(1,2,3,4),
      ordered = TRUE
    )
    

  }else if(current_att %in% c("Swimming Water Quality")){
    
    factor(c("Excellent","Good","Fair","Poor"),levels=c("Excellent","Good","Fair","Poor"))
           
    mypalette <-   colorFactor(
      palette = c('#00A2E1','#62BD19','#FFC726','#FF671F',"grey90","grey90"),
      domain = c("Excellent","Good","Fair","Poor","N/A","Not Monitored"),
      ordered = TRUE
    )
    

    myvalues=factor(c("Excellent","Good","Fair","Poor","Not Monitored"),levels = c("Excellent","Good","Fair","Poor","Not Monitored"))
    mytitle = "Swimming Water Quality"
    
    
  }else if(current_att %in% "Cyano Warning"){
    
    
    mypalette <-   colorFactor(
      palette = c('#00AAE5',"#E36055","grey90","grey90"),
      domain = c("No", "Yes","N/A","Not Monitored"),
      ordered = TRUE
    )
    

    myvalues=factor(c("Yes","No","Not Monitored"),levels = c("Yes","No","Not Monitored"))
    mytitle = "Cyanobacteria Warnings"
    
  }else if(current_att %in% "Cyano Biovolume"){
    
    
    mypalette <-   colorFactor(
      palette = c('#00A2E1','#00A2E1','#62BD19' ,'#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F','#FF671F',"grey90","grey90"),
      domain = c("A","A/A","A/B","B","B/B","B/C","C","C/C","C/B","D","D/D","C/D","N/A","Not Monitored"),
      ordered = TRUE
      
    )
    
    myvalues <- factor(c("A","B","C","D","Not Monitored"),levels=c("A","B","C","D","Not Monitored"))
    mytitle <- "NPS-FM Band"
    
  }else if(current_att %in% c("Koura Abundance","Kakahi Abundance")){
    mypalette <-   colorFactor(
      palette = c('#00A2E1', '#FFC726','#FF671F','#62BD19','grey90',"grey90","grey90"),
      domain = c("Abundant","Moderate","Present","Common","Absent","N/A","Not Monitored"),
      ordered = TRUE
    )
    
    myvalues=factor(c("Absent","Present","Moderate","Common","Abundant"),levels = c("Abundant","Common","Moderate","Present","Absent"))
    mytitle = "Abundance"
    
  }else if(current_att %in% c("Catfish Abundance")){
    mypalette <-   colorFactor(
      palette = c('#00A2E1', '#62BD19','#FFC726','#FFC726','#FF671F',"grey90","grey90"),
      domain = c("Absent","Present","Moderate","Common","Abundant","N/A","Not Monitored"),
      ordered = TRUE
    )
    
    myvalues=factor(c("Absent","Present","Moderate","Abundant"),levels = c("Absent","Present","Moderate","Abundant"))
    mytitle = "Abundance"
    
  }else if(current_att %in% c("Koura (Trend)", "Catfish (Trend)","Kakahi (Trend)")){
    
    #browser()
    mypalette <-   colorFactor(
      palette = c("#00AAE5", '#FDC851',"#E36055","grey90","grey90","grey90"),
      domain = c("Improving", "Stable","Worsening",NA,"Not Monitored","Not Present"),
      ordered = TRUE
    )
    
    myvalues=factor(c("Improving", "Stable","Worsening","Not Present"),levels=c("Improving", "Stable","Worsening","Not Present"))
    
    #myvalues=c("Improving", "Stable","Worsening","Not Present")
    mytitle = "Qualitative Trend"
    
  }else{
  #browser()
    mypalette <-   colorFactor(
          palette = c('#00A2E1','#00A2E1','#62BD19','#62BD19' ,'#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F','#FF671F',"grey90","grey90"),
          domain = c("A","A/A","A/B","B/A","B","B/B","B/C","C","C/C","C/B","D","D/D","C/D","N/A","Not Monitored"),
          ordered = TRUE
          
  )
    
    myvalues <- factor(c("A","B","C","D","Not Monitored"),levels=c("A","B","C","D","Not Monitored"))
    mytitle <- "NPS-FM Band"
    
  }
  

    if(current_att %in% c("TLI (Current)")){
      
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        htmlwidgets::onRender("function(el, x) {
                              L.control.zoom({ position: 'topright' }).addTo(this)
    }")%>%
    setView(lat = -38.166293,lng = 176.441507, zoom = 11)%>%
        addTiles()%>%
        addCircleMarkers(data = data[,c(3,4)],
                         label = data[,c(1)],
                         labelOptions = labelOptions(noHide = T),
                         fillColor = mypalette(data[,2]),
                         color="black",
                         stroke = TRUE,
                         fillOpacity = 0.9,
                         radius = 20)%>%
      addLegend(colors=c("#00AAE5","#62BD19","#E36055","lightgrey"), labels=c("Yes", "No (<=0.2)","No (>0.2)","Not Monitored"), opacity=0.9, title = "TLI Met", position = "bottomright")
    }else if(current_att %in% c("TLI (3 yr Avg)")){
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }")%>%
        setView(lat = -38.166293,lng = 176.441507, zoom = 11)%>%
        addTiles()%>%
        addCircleMarkers(data = data[,c(3,4)],
                         label = data[,c(1)],
                         labelOptions = labelOptions(noHide = T),
                         fillColor = mypalette(data[,2]),
                         color="black",
                         stroke = TRUE,
                         fillOpacity = 0.9,
                         radius = 20)%>%
      addLegend(colors=c("#00AAE5","#62BD19","#E36055","lightgrey"), labels=c("Yes", "No (<=0.2)","No (>0.2)","Not Monitored"), opacity=0.9, title = "TLI Met (3yr Average)", position = "bottomright")
    }else{
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        htmlwidgets::onRender("function(el, x) {
                              L.control.zoom({ position: 'topright' }).addTo(this)
    }")%>%
    setView(lat = -38.166293,lng = 176.441507, zoom = 11)%>%
        addTiles()%>%
        addCircleMarkers(data = data[,c(3,4)],
                         label = data[,c(1)],
                         labelOptions = labelOptions(noHide = T),
                         fillColor = mypalette(data[,2]),
                         color="black",
                         stroke = TRUE,
                         fillOpacity = 0.9,
                         radius = 20)%>%
      
      addLegend(pal=mypalette, values=myvalues, opacity=0.9, title = mytitle, position = "bottomright")
    }
  
  

  
      
                     
  })
  
  ###############################
  ### Water Quality Datatable ###
  ###############################
  
  output$example_table_WQ <- DT::renderDataTable({

    datatable(WQ_Att,
              rownames=FALSE,
              container=sketch_WQ,
              extensions = "FixedColumns",
              options = list(pageLength = 12, 
                             scrollX=TRUE,
                             info = FALSE,
                             #autoWidth = TRUE,
                             columnDefs = list(list(targets = c(15,16), visible = FALSE),
                                               list(width="70px",targets="_all")),
                             lengthMenu = list(c(12, -1), c("12", "All")))
                             #,
                             #fixedColumns = list(leftColumns = 2))
              )%>% 
      
      formatStyle(
    'TN',
    backgroundColor = styleEqual(c("A","A/B","B","B/B","C","C/C","C/B","D","D/D"), c('#00A2E1','#62BD19', '#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F')))%>%

      formatStyle(
        'TP',
        backgroundColor = styleEqual(c("A","B","B/B","B/C","C","C/C","C/B","D","D/D"), c('#00A2E1', '#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F')))%>%

      formatStyle(
        "CHLA (Med)",
        backgroundColor = styleEqual(c("A","B","B/B","C","C/C","C/B","C/D","D","D/D"), c('#00A2E1', '#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F','#FF671F')))%>%

      formatStyle(
        "CHLA (Max)",
        backgroundColor = styleEqual(c("A","A/A","B","B/A","B/B","C","C/C","C/B","D","D/D"), c('#00A2E1','#00A2E1', '#62BD19','#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F')))%>%

      formatStyle(
        'TLI Target',
        backgroundColor = 'lightblue')%>%

      formatStyle(
        "TLI (3 yr Avg)",
        backgroundColor = styleEqual(c("Excellent", "High","Moderate","Poor"), c('#00A2E1', '#62BD19','#FFC726','#FF671F')))%>%

    formatStyle(
      "TN (Trend)",
    backgroundColor = styleEqual(c("Very Likely Improving","Likely Improving", "Indeterminate","Likely Worsening","Very Likely Worsening"), c('#00A2E1',"#62BD19","grey","#FF671F","#D32939")))%>%

    formatStyle(
      "TP (Trend)",
      backgroundColor = styleEqual(c("Very Likely Improving","Likely Improving", "Indeterminate","Likely Worsening","Very Likely Worsening"), c('#00A2E1', '#62BD19',"grey","#FF671F","#D32939")))%>%

    formatStyle(
      "CHL (Trend)",
      backgroundColor = styleEqual(c("Very Likely Improving","Likely Improving", "Indeterminate","Likely Worsening","Very Likely Worsening"), c('#00A2E1', '#62BD19',"grey","#FF671F","#D32939")))%>%

    formatStyle(
      "Secchi (Trend)",
      backgroundColor = styleEqual(c("Very Likely Improving","Likely Improving", "Indeterminate","Likely Worsening","Very Likely Worsening"), c('#00A2E1', '#62BD19',"grey","#FF671F","#D32939")))%>%

      
      formatStyle(
        "TLI (Current)",
        valueColumns = "TLI_MET",
        backgroundColor = styleEqual(c(1,2,3,4),
                                     values=c("#00AAE5","#62BD19","#E36055","grey90")))%>%
      
      formatStyle(
        "TLI (3 yr Avg)",
        valueColumns = "TLI_MET_3",
        backgroundColor = styleEqual(c(1,2,3,4),
                                     values=c("#00AAE5","#62BD19","#E36055","grey90")))%>%
      
      formatStyle(
        "Cyano Warning",
        backgroundColor = styleEqual(c("Yes", "No"), c("#E36055", '#00AAE5')))%>%
      
      formatStyle(
        "Cyano Biovolume",
        backgroundColor = styleEqual(c("A","B","B/B","C","C/C","C/B","D","D/D","C/D"), c('#00A2E1', '#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F','#FF671F')))%>%
      
      formatStyle(
        "Swimming Water Quality",
        backgroundColor = styleEqual(c("Excellent", "Good","Fair","Poor"), c('#00A2E1', '#62BD19','#FFC726','#FF671F')))%>%
    
    formatStyle(c(1,4,8,12), `border-right` = "solid 2px")
})

  #########################
  ### Ecology Datatable ###
  #########################
  
  output$example_table_ECOL <- DT::renderDataTable({
    datatable(Eco_Att,
              rownames=FALSE,
              container=sketch_ECOL,
              extensions = "FixedColumns",
              options = list(pageLength = 12, 
                             scrollX=TRUE,
                             info = FALSE,
                             #autoWidth = TRUE,
                             columnDefs = list(
                                               list(width="70px",targets="_all")),
                             lengthMenu = list(c(12, -1), c("12", "All")))
    )%>% 
      
      formatStyle(
        "Lake SPI (Native)",
        backgroundColor = styleEqual(c("A","B","B/B","C","C/C","C/B","D","D/D"), c('#00A2E1', '#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F')))%>%
      
      formatStyle(
        "Lake SPI (Invasive)",
        backgroundColor = styleEqual(c("A","B","B/B","C","C/C","C/B","D","D/D"), c('#00A2E1', '#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F')))%>%
      
      formatStyle(
        "Lake SPI",
        backgroundColor = styleEqual(c("Excellent", "High","Moderate","Poor"), c('#00A2E1', '#62BD19','#FFC726','#FF671F')))%>%
      
      formatStyle(
        'Koura Abundance',
        backgroundColor = styleEqual(c("Abundant","Moderate","Present","Common","Absent"), c('#00A2E1', '#FFC726','#FF671F','#62BD19','grey90')))%>%
      
      formatStyle(
        'Kakahi Abundance',
        backgroundColor = styleEqual(c("Abundant","Moderate","Present","Common","Absent"), c('#00A2E1', '#FFC726','#FF671F','#62BD19','grey90')))%>%
      
      formatStyle(
        'Catfish Abundance',
        backgroundColor = styleEqual(c("Abundant","Moderate","Present","Common","Absent"), c("#D32939",'#FFC726','#FFC726', '#FFC726','#00A2E1')))%>%
      
       # formatStyle(
       #   'Koura (CPUE)',
       #   background = styleColorBar(range(as.numeric(Eco_Att$`Koura (CPUE)`)), 'lightblue'))%>%
      
      formatStyle(
        'Invasive Submerged Plants',
        backgroundColor = 'lightblue')%>%
      
      formatStyle(
        'Reason for Change (Koura)',
        backgroundColor = 'lightblue')%>%

      formatStyle(
        "Koura (Trend)",
        backgroundColor = styleEqual(c("Improving", "Stable","Worsening"), c("#00AAE5", '#FDC851',"#D32939")))%>%
      
      formatStyle(
        "Catfish (Trend)",
        backgroundColor = styleEqual(c("Improving", "Stable","Worsening"), c("#00AAE5", '#FDC851',"#D32939")))%>%
      
      formatStyle(
        "Kakahi (Trend)",
        backgroundColor = styleEqual(c("Improving", "Stable","Worsening"), c("#00AAE5", '#FDC851',"#D32939")))%>%
      
      formatStyle(
        "Lake SPI (Invasive)",
        backgroundColor = styleEqual(c("A","B","B/B","C","C/C","C/B","D","D/D"), c('#00A2E1', '#62BD19','#62BD19','#FFC726','#FFC726','#FFC726','#FF671F','#FF671F')))%>%
      
      formatStyle(
        "Lake SPI",
        backgroundColor = styleEqual(c("Excellent", "High","Moderate","Poor"), c('#00A2E1', '#62BD19','#FFC726','#FF671F')))%>%
      
      formatStyle(c(1,5,8,10), `border-right` = "solid 2px")
    
  })
  
  
}

shinyApp(ui, server)
