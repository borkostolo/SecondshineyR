library(DT)
library(shiny)
library(googleVis)

library(shinydashboard)
library(tidyverse)
library(leaflet)
library(maps)
library(geojsonio)
library(RColorBrewer)
library(stats)
library(shinyWidgets)




shinyServer(function(input, output){
    # show map using googleVis
    output$map <- renderGvis({
        gvisGeoChart(state_stat, "state.name", input$selected,
                     options=list(region="US", displayMode="regions", 
                                  resolution="provinces",
                                  width="auto", height="auto",colors="navy"))
    })
    
    output$map2 <- renderGvis({
        gvisGeoChart(state_stat, "state.name", input$selected,
                     options=list(region="US", displayMode="regions", 
                                  resolution="provinces",
                                  width="auto", height="auto", colors="red"))
    })
    
    # show histogram using googleVis
    output$hist <- renderGvis({
        gvisHistogram(state_stat[,input$selected, drop=FALSE])
    })
    
    # Generate barchart
    output$dfbarh <- renderGvis({
      dfbarh=data.frame(country=c("Sky PROG","Avalon","Unicorn LLC","Golden Globe"), 
                        val1=c(10,13,14,24), 
                        val2=c(23,12,32,35))

        gvisBarChart(dfbarh,
                     options=list(
          title="$",
          hAxis="{title:'Values in Dollars'}",
          colors = "['#122222']",
          legend =  "none",
          font = '14',
          width='auto', height= 200))
        
        
        
    })
 
    output$hist2 <- renderGvis({
        gvisHistogram(state_stat[,input$selected, drop=FALSE])
    })   
    output$hist3 <- renderGvis({
      gvisHistogram(state_stat[,input$selected, drop=FALSE], option=list(
                                                                         legend="{ position: 'none' }",
                                                                         colors="['grey']"))
    }) 
  
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(state_stat, rownames=FALSE) %>% 
            formatStyle(input$selected, background="blue", fontWeight='bold')
    })
    
    # show statistics using infoBox
    output$maxBox <- renderInfoBox({
        max_value <- max(state_stat[,input$selected])
        max_state <- 
            state_stat$state.name[state_stat[,input$selected] == max_value]
        infoBox(max_state, max_value, icon = icon("hand-o-up"),color="navy")
    })
    output$minBox <- renderInfoBox({
        min_value <- min(state_stat[,input$selected])
        min_state <- 
            state_stat$state.name[state_stat[,input$selected] == min_value]
        infoBox(min_state, min_value, icon = icon("hand-o-down"),color="navy")
    })
    output$avgBox <- renderInfoBox(
        infoBox(paste("AVG.", input$selected),
                mean(state_stat[,input$selected]), 
                icon = icon("calculator"),color="navy", fill = TRUE))
    bins = reactiveValues()
    labtxt = reactiveValues()

    # show statistics using Dashboard
    output$aggRes <- renderInfoBox({
        max_value <- max(c(12,34,45))
        max_state <- "Nassau County"
        infoBox(max_state, max_value, icon = icon("globe"),color="red", fill=T)
    })
        
    output$compCount <- renderInfoBox({
        max_value <- max(c(12,34,45))
        max_state <- "Top 5 companies"
        infoBox(max_state, max_value, icon = icon("chart-pie"), color="red", fill=T)
    })   
    
    output$compInd <- renderInfoBox({
        max_value <- max(c(456,34,45))
        max_state <- "Agriculture"
        infoBox(max_state, max_value, icon = icon("check"),color="red", fill=T)
    }) 
    
    # Value Boxes for the summary
    output$progressBox <- renderValueBox({
        valueBox(
            paste0(25 + input$bcount, "%"), "Progress", icon = icon("list"),
            color = "red"
        )
    })
    output$approvalBox <- renderValueBox({
        valueBox(
            "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "red"
        )
    })
    
    # Values for the Suggested TAB
    
    # Leaflet
    # r_colors <- rgb(t(col2rgb(colors()) / 255))
    # names(r_colors) <- colors()
    points <- eventReactive(input$recalc, {
        cbind(rnorm(30) * 1.8 + -78, rnorm(30) +40)
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(data = points())
    })
    
    output$mymap2 <- renderLeaflet({
      leaflet() %>% setView(lng = 260.0589, lat = 40.1801, zoom = 5) -> m
      m %>% addProviderTiles(providers$CartoDB.Positron)
    })
    
    
    


    

    
})