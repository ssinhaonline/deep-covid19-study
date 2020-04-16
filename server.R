library(dplyr)
library(tidyr)  
library(plotly)
library(zoo)

baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

f1 = list(family="Courier New, monospace", size=12, color="rgb(30,30,30)")

minutesSinceLastUpdate = function(fileName) {
    (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
}

loadData = function(fileName, columnName) {
    
    if(!file.exists(paste0("data/", fileName)) || minutesSinceLastUpdate(paste0("data/", fileName)) > 10) {
        data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
            select(-Lat, -Long) %>% 
            pivot_longer(-(1:2), names_to="date", values_to=columnName) %>% 
            mutate(
                date=as.Date(date, format="%m/%d/%y"),
                `Country/Region`=if_else(`Country/Region` == "", "?", `Country/Region`),
                `Province/State`=if_else(`Province/State` == "", "<all>", `Province/State`)
            )
        save(data, file=paste0("data/", fileName))  
    } else {
        load(file=paste0("data/", fileName))
    }
    return(data)
}

allData = 
    loadData(
        "time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
    inner_join(loadData(
        "time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
    inner_join(loadData(
        "time_series_covid19_recovered_global.csv","CumRecovered"))

function(input, output, session) {
    numRegions <- 2
    regionsData = reactive({
        d = allData %>%
            filter(`Country/Region` == input$regionsCountry)
        if(input$regionsState != "<all>") {
            d = d %>% 
                filter(`Province/State` == input$regionsState) 
        } else {
            d = d %>% 
                group_by(date) %>% 
                summarise(CumConfirmed = sum(CumConfirmed), 
                          CumDeaths = sum(CumDeaths), 
                          CumRecovered = sum(CumRecovered))
        }
        
        d %>%
            mutate(
                dateStr = format(date, format="%b %d, %Y"),    # Jan 20, 2020
                NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
                NewRecovered=CumRecovered - lag(CumRecovered, default=0),
                NewDeaths=CumDeaths - lag(CumDeaths, default=0)
            )
    })
    
    comparisonsData = reactive({
        d = allData %>%
            filter(`Country/Region` %in% c(input$comparisonsCountry1, input$comparisonsCountry2))
        if(input$comparisonsState1 != "<all>" || input$comparisonsState2 != "<all>") {
            if (input$comparisonsState1 != "<all>"){
                d = d %>% 
                    filter(`Province/State` == input$comparisonsState1) 
            } else {
                if (input$comparisonsState2 != "<all>"){
                    d = d %>% 
                        filter(`Province/State` == input$comparisonsState2) 
                }
            }
        }  else {
            d = d %>% 
                group_by(`Country/Region`, date) %>% 
                summarise(CumConfirmed = sum(CumConfirmed), 
                          CumDeaths = sum(CumDeaths), 
                          CumRecovered = sum(CumRecovered))
        }
        
        d %>%
            mutate(dateStr = format(date, format="%b %d, %Y"),    # Jan 20, 2020
                   NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
                   NewRecovered=CumRecovered - lag(CumRecovered, default=0),
                   NewDeaths=CumDeaths - lag(CumDeaths, default=0)) %>% 
            group_by(`Country/Region`, date) %>% 
            summarise(CumConfirmed = sum(CumConfirmed), 
                      CumDeaths = sum(CumDeaths), 
                      CumRecovered = sum(CumRecovered)) %>% 
            arrange(`Country/Region`, date) %>% 
            mutate(FirstConfirmed = first(date[CumConfirmed >= 1])) %>% 
            mutate(DaysSinceOutBreak = if_else(as.numeric(date - FirstConfirmed) >= 0, 
                                               as.numeric(date - FirstConfirmed), 
                                               -999999)) %>% 
            filter(DaysSinceOutBreak >= 0) %>% 
            mutate(RollingMeanConfirmed = rollmeanr(CumConfirmed, 2, fill = NA))
    })
    
    observeEvent(input$regionsCountry, {
        states = allData %>%
            filter(`Country/Region` == input$regionsCountry) %>% 
            pull(`Province/State`)
        states = c("<all>", sort(unique(states)))
        updateSelectInput(session, "regionsState", choices=states, selected=states[1])
    })
    
    observeEvent(input$comparisonsCountry1, {
        states = allData %>%
            filter(`Country/Region` == input$comparisonsCountry1) %>% 
            pull(`Province/State`)
        states = c("<all>", sort(unique(states)))
        updateSelectInput(session, "comparisonsState1", choices=states[1], selected=states[1])
    })
    
    observeEvent(input$comparisonsCountry2, {
        states = allData %>%
            filter(`Country/Region` == input$comparisonsCountry2) %>% 
            pull(`Province/State`)
        states = c("<all>", sort(unique(states)))
        updateSelectInput(session, "comparisonsState2", choices=states[1], selected=states[1])
    })
    
    # observeEvent(input$comparisonsState1, {
    #     if (input$comparisonsState1 == "<all>"){
    #         countries = sort(unique(allData$`Country/Region`))
    #         updateSelectInput(session, "comparisonsCountry2", 
    #                           choices = countries[!countries == input$comparisonsCountry1])
    #         states = allData %>%
    #             filter(`Country/Region` == input$comparisonsCountry2) %>% 
    #             pull(`Province/State`)
    #         states = c("<all>", sort(unique(states)))
    #         updateSelectInput(session, "comparisonsState2", choices=states, selected=states[1])
    #     }
    # })
    
    countries = sort(unique(allData$`Country/Region`))
    
    updateSelectInput(session, "regionsCountry", choices=countries, selected="China")
    updateSelectInput(session, "comparisonsCountry1", choices=countries, selected="China")
    updateSelectInput(session, "comparisonsCountry2", choices=countries, selected="Italy")
    
    # observeEvent(input$addRegion, {
    #     numRegions = numRegions + 1
    #     output$moreRegions = renderUI({
    #         # opTag = tagList()
    #         tagList(
    #             h3(strong(paste0("Select Region ", as.character(numRegions)))),
    #             selectizeInput(ns("country"), paste0("comparisonsCountry", as.character(numRegions)), label=h5("Country"), choices=NULL, width="100%"),
    #             selectizeInput(ns("state"), paste0("comparisonsState", as.character(numRegions)), label=h5("State / Province"), choices=NULL, width="100%")
    #         )
    #         # tagAppendChild(opTag, h3(strong(paste0("Select Region ", as.character(numRegions)))))
    #         # tagAppendChild(opTag, selectizeInput(paste0("comparisonsCountry", as.character(numRegions)), label=h5("Country"), choices=NULL, width="100%"))
    #         # tagAppendChild(opTag, selectizeInput(paste0("comparisonsState", as.character(numRegions)), label=h5("State / Province"), choices=NULL, width="100%"))
    #         # opTag
    #     })
    #     updateSelectInput(session, paste0("comparisonsCountry", as.character(numRegions)), choices=countries, selected="US")
    #     states = allData %>%
    #         filter(`Country/Region` == input[[paste0("comparisonsCountry", as.character(numRegions))]]) %>% 
    #         pull(`Province/State`)
    #     states = c("<all>", sort(unique(states)))
    #     updateSelectInput(session, paste0("comparisonsState", as.character(numRegions)), choices=states[1], selected=states[1])
    # })
    
    
    
    renderBarPlot = function(varPrefix, legendPrefix, yaxisTitle) {
        renderPlotly({
            regionsData = regionsData()
            plt = regionsData %>% 
                plot_ly() %>%
                config(displayModeBar=FALSE) %>%
                layout(
                    barmode='group', 
                    xaxis=list(
                        title="", tickangle=-90, type='category', ticktext=as.list(regionsData$dateStr), 
                        tickvals=as.list(regionsData$date), gridwidth=1), 
                    yaxis=list(
                        title=yaxisTitle
                    ),
                    legend=list(x=0.05, y=0.95, font=list(size=15), bgcolor='rgba(240,240,240,0.5)'),
                    font=f1
                )
            for(metric in input$regionsMetrics) 
                plt = plt %>%
                add_trace(
                    x= ~date, y=regionsData[[paste0(varPrefix, metric)]], type='bar', 
                    name=paste(legendPrefix, metric, "Cases"),
                    marker=list(
                        color=switch(metric, Deaths='rgb(200,30,30)', Recovered='rgb(30,200,30)', Confirmed='rgb(100,140,240)'),
                        line=list(color='rgb(8,48,107)', width=1.0)
                    )
                )
            plt
        })
    }
    
    output$dailyMetrics = renderBarPlot("New", legendPrefix="New", yaxisTitle="New Cases per Day")
    output$cumulatedMetrics = renderBarPlot("Cum", legendPrefix="Cumulated", yaxisTitle="Cumulated Cases")
    
    output$headToHead = renderPlotly({
        comparisonsData = comparisonsData()
        plt = comparisonsData %>% 
            plot_ly(mode = "lines",
                    type = "scatter") %>%
            config(displayModeBar=FALSE) %>%
            layout(
                xaxis=list(
                    title="Days Since Outbreak", 
                    tickangle=-90, 
                    tickvals=as.list(union(comparisonsData %>% 
                                               filter(`Country/Region` == input$comparisonsCountry1) %>%
                                               pull(DaysSinceOutBreak), 
                                           comparisonsData %>% 
                                               filter(`Country/Region` == input$comparisonsCountry2) %>% 
                                               pull(DaysSinceOutBreak))), 
                    gridwidth=1), 
                yaxis=list(
                    title="Confirmed"
                ),
                legend=list(x=0.05, y=0.95, font=list(size=15), bgcolor='rgba(240,240,240,0.5)'),
                font=f1
            )
        for(country in c(input$comparisonsCountry1, input$comparisonsCountry2)) {
            countryData = comparisonsData %>% 
            filter(`Country/Region` == country)
            
            plt = plt %>%
            add_trace(
                x = countryData %>% 
                    pull(DaysSinceOutBreak), 
                y = countryData %>% 
                    pull(RollingMeanConfirmed),
                fill = "tozeroy",
                name = country
            )
        }
        plt
    })
}