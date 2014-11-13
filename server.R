library(leaflet)
library(RColorBrewer)
library(maps)

shinyServer(function(input, output, session) {
    
  values <- reactiveValues(highlight = c())
  
  map <- createLeafletMap(session, "map")
  
  # Draw the given states, with or without highlighting
  drawStates <- function(stateNames, highlight = FALSE) {
    states <- map("state", stateNames, plot=FALSE, fill=TRUE)
    map$addPolygon(I(states$y), I(states$x), I(states$names),
      I(lapply(states$names, function(x) {
        x <- strsplit(x, ":")[[1]][1]
        list(fillColor = colors[[x]])
      })),
      I(list(fill=TRUE, fillOpacity=0.7, 
        stroke=TRUE, opacity=1, color="white", weight=ifelse(highlight, 4, 1)
      ))
    )
  }

  # session$onFlushed is necessary to delay the drawing of the polygons until
  # after the map is created
  session$onFlushed(once=TRUE, function() {
    # Get shapes from the maps package
    drawStates(names(density))
  })
  
  
  
  
  # input$map_shape_mouseover gets updated a lot, even if the id doesn't change.
  # We don't want to update the polygons and stateInfo except when the id
  # changes, so use values$highlight to insulate the downstream reactives (as 
  # writing to values$highlight doesn't trigger reactivity unless the new value 
  # is different than the previous value).
  observe({
    values$highlight <- input$map_shape_mouseover$id
  })
  
  
  observe({
      if(is.null(input$map_shape_click$id)) {
          values$stateSelected <- "Montana"
      }
      else {
          values$stateSelected <- simpleCap(input$map_shape_click$id)
      }
  })

  
  # Dynamically render the box in the upper-right
  output$stateInfo <- renderUI({
    if (is.null(values$highlight)) {
      return(tags$div("Hover over a state to see emissions!"))
    } else {
      # Get a properly formatted state name
      stateName <- simpleCap(input$map_shape_mouseover$id)
      return(tags$div(
    
        tags$strong(stateName),
        tags$div(states.co2[[stateName]][1,"2012"], HTML("Million Metric Tons of CO2 per Year in 2012"))
      ))
    }
  })
  


  #######shouldn't need to be changed
  lastHighlighted <- c()
  # When values$highlight changes, unhighlight the old state (if any) and
  # highlight the new state
  observe({
    if (length(lastHighlighted) > 0)
      drawStates(getStateName(lastHighlighted), FALSE)
    lastHighlighted <<- values$highlight
    
    if (is.null(values$highlight))
      return()
    
    isolate({
      drawStates(getStateName(values$highlight), TRUE)
    })
  })
  

  
  output$commSlider <- renderUI({
      tags$div(title="Adjust slider to reduce CO2 emissions from the Commercial Sector",
      sliderInput("inCommSlider", "Commercial CO2 Emissions", min = 0,
      max = states.co2[[values$stateSelected]][2,"2012"],
      value = states.co2[[values$stateSelected]][2,"2012"],
      step = 0.01, format = "#.## MMTCO2")
      )
  })
  
  output$industSlider <- renderUI({
      sliderInput("inIndustSlider", "Industrial CO2 Emissions", min = 0, max = states.co2[[values$stateSelected]][3,"2012"],
      value = states.co2[[values$stateSelected]][3,"2012"],
      step = 0.01, format = "#.## MMTCO2")
  })
  
  output$residSlider <- renderUI({
      sliderInput("inResidSlider", "Residential CO2 Emissions", min = 0, max = states.co2[[values$stateSelected]][4,"2012"],
      value = states.co2[[values$stateSelected]][4,"2012"],
      step = 0.01, format = "#.## MMTCO2")
  })
  
  output$transSlider <- renderUI({
      sliderInput("inTransSlider", "Transportation CO2 Emissions", min = 0, max = states.co2[[values$stateSelected]][5,"2012"],
      value = states.co2[[values$stateSelected]][5,"2012"],
      step = 0.01, format = "#.## MMTCO2")
  })
  
  output$elecSlider <- renderUI({
      sliderInput("inElecSlider", "Electric Power Production CO2 Emissions", min = 0, max = states.co2[[values$stateSelected]][6,"2012"],
      value = states.co2[[values$stateSelected]][6,"2012"],
      step = 0.01, format = "#.## MMTCO2")
  })



    co2.sum <- reactive({
        return(sum(input$inCommSlider, input$inIndustSlider, input$inResidSlider, input$inTransSlider, input$inElecSlider))
    })

    percent.reduc <- reactive({
        return(round(100*(1-co2.sum()/sum(states.co2[[values$stateSelected]][2:6,"2012"])), digits = 2))
    })

    mycolor <- reactive({
        if(percent.reduc() > state.goals[values$stateSelected,]){
            return("green")
        }
        else{
            return("red")
        }
    })

    output$descrip1 <- renderUI({
        helpText(paste("The EPA's Clean Power Plan requires ", values$stateSelected, " to reduce it's CO2 emissions to ", state.goals[values$stateSelected,], "% of 2012 levels by 2030. Move the sliders to reduce the emissions from each sector to reach the EPA's goal.", sep=""))
    })

    output$text1 <- renderUI({
        if(percent.reduc() < state.goals[values$stateSelected,]){
           return(tags$div(
                tags$div(tags$strong(paste("EPA Target for ", values$stateSelected, ": ", state.goals[values$stateSelected,], "%", sep="")),
                    align= "center"),
                tags$div(paste("CO2 Emissions Reduced: ", percent.reduc(), "%", sep=""),
                    align= "center",
                    style="color:red")))}
        else {
            return(tags$div(
                tags$div(tags$strong(paste("EPA Target for ", values$stateSelected, ": ", state.goals[values$stateSelected,], "%", sep="")),
                    align= "center"),
                tags$div(paste("CO2 Emissions Reduced: ", percent.reduc(), "%", sep=""),
                    align= "center",
                    style="color:green"),
                tags$div(tags$strong(tags$h4("You've reach the EPA's Goal!")), align="center")
            ))}
    })

    output$text3 <- renderUI({
        NULL
    })



    pieData <- reactive({
        changeCommSlice <- states.co2[[values$stateSelected]][2,"2012"]-input$inCommSlider
        changeIndustSlice <- states.co2[[values$stateSelected]][3,"2012"]-input$inIndustSlider
        changeResidSlice <- states.co2[[values$stateSelected]][4,"2012"]-input$inResidSlider
        changeTransSlice <- states.co2[[values$stateSelected]][5,"2012"]-input$inTransSlider
        changeElecSlice <- states.co2[[values$stateSelected]][6,"2012"]-input$inElecSlider
        
        
        dat <- c(input$inCommSlider,changeCommSlice, input$inIndustSlider, changeIndustSlice, input$inResidSlider,changeResidSlice, input$inTransSlider,changeTransSlice, input$inElecSlider,changeElecSlice)
        return(dat)
    })



    ####Creates Pie Chart To show Co2 reductions
    lbls <- c("Commercial", "Industrial", "Residential", "Transportation", "Electric Production")
    pieColors <- c("blue", "gray", "orange", "grey", "yellow", "gray", "red", "gray", "green", "gray")
    output$pieChart <- renderPlot({
        
        data <- pieData()
        
        pct <- round(data[seq(1,10,2)]/sum(data[seq(1,10,2)])*100, digits = 2)
        lbls <- paste(lbls, pct) # add percents to labels
        lbls <- paste(lbls,"%", sep="") # ad % to labels
        
        for(i in c(1,3,5,7,9)){lbls <- append(lbls, NA, after=c(i))}
        
        
        pie(data, labels = lbls, col = pieColors)
    })

    output$pieChartOut <- renderUI({
        plotOutput("pieChart")
        
    })

})
