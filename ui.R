library(shiny)
library(plotly)

shinyUI(fluidPage(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.4.0/js/bootstrap.js"),
    tags$footer(HTML("<hr>Thanks to Meinhard Ploner for his brilliant article on 
    <a href=\"https://towardsdatascience.com/create-a-coronavirus-app-using-r-shiny-and-plotly-6a6abf66091d\">TowardsDataScience</a> 
    explaining the various parts of this application.<br> Check out the full code on 
    <a href=\"https://github.com/ssinhaonline/deep-covid19-study\">Github</a>."), align = "center", style = "position:absolute;
                                                                    bottom:0;
                                                                    width:100%;
                                                                    height:50px; /* Height of the footer */
                                                                    color: black;
                                                                    background-color: white;
                                                                    z-index: 1000;"),
    titlePanel("Case History of the Coronavirus (COVID-19)"),
    tabsetPanel(
        type = "tabs",
        tabPanel(
            "Regions", 
            sidebarPanel(
                selectizeInput("regionsCountry", label=h5("Country"), choices=NULL, width="100%"),
                selectizeInput("regionsState", label=h5("State / Province"), choices=NULL, width="100%"),
                checkboxGroupInput(
                    "regionsMetrics", label=h5("Selected Metrics"), 
                    choices=c("Confirmed", "Deaths", "Recovered"), 
                    selected=c("Confirmed", "Deaths", "Recovered"), 
                    width="100%"
            )
        ),
        mainPanel(
            tabsetPanel(
                    type = "tabs",
                    tabPanel("Daily", plotlyOutput("dailyMetrics")),
                    tabPanel("Cumulative", plotlyOutput("cumulatedMetrics"))
                    )
                )
            ),
        tabPanel("Comparisons", 
                 sidebarPanel(
                     h3(strong("Select Region 1")),
                     selectizeInput("comparisonsCountry1", label=h5("Country"), choices=NULL, width="100%"),
                     selectizeInput("comparisonsState1", label=h5("State / Province"), choices=NULL, width="100%"),
                     hr(),
                     h3(strong("Select Region 2")),
                     selectizeInput("comparisonsCountry2", label=h5("Country"), choices=NULL, width="100%"),
                     selectizeInput("comparisonsState2", label=h5("State / Province"), choices=NULL, width="100%"),
                     # actionButton("addRegion", "Add Region"),
                     # uiOutput("moreRegions"),
                     radioButtons(
                         "comparisonsMetrics", label=h5("Selected Metrics (Rolling Mean)"), 
                         choices=c("Confirmed", "Day/Day Change", "Day/Day Change %"), 
                         selected=c("Confirmed"), 
                         width="100%"
                     )
                 ),
                 mainPanel(
                     tabsetPanel(
                         type = "tabs",
                         tabPanel("Head To Head", plotlyOutput("headToHead"))
                         # tabPanel("Similarity", plotlyOutput("Similarity"))
                     )
                 )
            )
        )
    )
)