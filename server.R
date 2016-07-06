#Load the libraries used in the choropleth
library(leaflet)
library(maps)
require(rCharts)
library(ShinyDash)
library(shinyBS)

#Set RCHART widths
options(RCHART_WIDTH = 200, RCHART_HEIGHT = 167)

# Additional function to use bootstrap elements (shiny update changed to bootstrap 3 elements)
shinybootstrap2::withBootstrap2({

# Initialize shiny session
shinyServer(function(input, output, session) {
    
    #Initiate reactive values
    values <- reactiveValues()
    
    #Create leaflet map
    map <- createLeafletMap(session, "map")
    
    #To create a new data.frame that we can input slider values into then reset to original if neccessary
    state.data2 <- state.data
    
    #get color data for drawStates function, depending on if per capita or total is selected
    getColorData <- function(x){
        if(input$btngrp1 == "perCapita"){
            return(getColorsPerCapita(state.data2["co2PerCapitaNew"], x))
        }
        if(input$btngrp1 == "total"){
            return(getColorsTotal(state.data2["totalNew"], x))
        }
    }
    
    # Draw the given states, retrieve color from getColorData() and highlight or not
    drawStates <- function(stateNames, highlight = FALSE) {
        states <- map("state", stateNames, plot=FALSE, fill=TRUE)
        map$addPolygon(I(states$y), I(states$x), I(states$names),
        I(lapply(states$names, function(x) {
            x <- strsplit(x, ":")[[1]][1]
            list(fillColor = isolate({getColorData(x)}))
        })),
        I(list(fill=TRUE, fillOpacity=0.7,
        stroke=TRUE, opacity=1, color="white", weight=ifelse(highlight, 4, 1)
        ))
        )
    }
    
    #if button group 1 changes, redraw states
    observe({
        #Set observer to depend on reactive input variable btngrp1
        input$btngrp1
        drawStates(rownames(state.data2))
    })
    
    # session$onFlushed is necessary to delay the drawing of the polygons until after the map is created
    session$onFlushed(once=TRUE, function() {
        # Get shapes from the maps package
        drawStates(rownames(state.data2))
    })
    
    #Set the state that should be highlighted to the mouseover state
    observe({
        values$highlight <- input$map_shape_mouseover$id
    })
    
    #initialize the lastHighlighted variable
    lastHighlighted <- c()
    
    #When values$highlight changes, unhighlight the old state (if any) and highlight the new state
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
    
    #Sets values$stateSelected when a state is clicked
    observe({
        if(is.null(input$map_shape_click$id)) {
            values$stateSelected <<- "Montana"
        }
        else {
            #Remove ':main' problem and caitalize state name
            values$stateSelected <<- simpleCap(getStateName(input$map_shape_click$id))
        }
    })
    
    #Set's slider values into state.data2 matrix so that values are remembered when stateSelected changes. Make observer depend on the input slider values
    observe({
        input$inCommSlider
        input$inIndustSlider
        input$inResidSlider
        input$inTransSlider
        input$inElecSlider
        
        isolate({
        
            if (length(input$inCommSlider) > 0) {
                state.data2[values$stateSelected,"commSlider"] <<- input$inCommSlider
            }
            if (length(input$inIndustSlider) > 0) {
                state.data2[values$stateSelected,"industSlider"] <<- input$inIndustSlider
            }
            if (length(input$inResidSlider) > 0) {
                state.data2[values$stateSelected,"residSlider"] <<- input$inResidSlider
            }
            if (length(input$inTransSlider) > 0) {
                state.data2[values$stateSelected,"transSlider"] <<- input$inTransSlider
            }
            if (length(input$inElecSlider) > 0) {
                state.data2[values$stateSelected,"elecSlider"] <<- input$inElecSlider
            }
            
            drawStates(rownames(state.data2))
        })
    })
    
    
    #Computes co2.sum (used in percent.reduc), percent.reduc
    observe({
        
        #create dependency upon input$reset
        input$reset
        
        #Calculate the sum of the slider values
        values$co2.sum <<- sum(input$inCommSlider, input$inIndustSlider, input$inResidSlider, input$inTransSlider, input$inElecSlider)
        
        #Calculate percent of emissions reduced for the state and country
        isolate({
            values$percent.reduc <<- round(100*(1-values$co2.sum/state.data2[values$stateSelected, 5]), digits = 2)
            
            values$USpercent.reduc <<- round(100*(1-sum(state.data2[,6:10])/sum(state.data2[,5])), digits=2)
        })
    })
    
    #Store new co2 per capita and new total
    observe({
        values$co2.sum
        
        isolate({
            state.data2[values$stateSelected, "co2PerCapitaNew"] <<- sum(state.data2[values$stateSelected,6:10])/pop[values$stateSelected,"2012"]*1e6
            state.data2[values$stateSelected, "totalNew"] <<- sum(state.data2[values$stateSelected,6:10])
        })
    })
    
    #Set new goal value and redraw states if goal changes
    observe({
        state.data2[isolate({values$stateSelected}), "newGoal"] <<- (state.data2[isolate({values$stateSelected}), "goals"] - values$percent.reduc)
        
        isolate({drawStates(values$stateSelected)})
        
    })
    
    #Create Slider Max function that takes slider max value from original states.co2 data frame
    sliderMax <- function(sector){
        return(round(states.co2[[values$stateSelected]][sector,"2012"], digits=4))
    }
    
    
    #Create each slider, make it depend on input$reset
    observe({
        input$reset
        
        isolate({
            state.data2 <<- state.data
    
            output$commSlider <- renderUI({
                sliderInput("inCommSlider", NULL, min = 0,
                max = sliderMax(2)+.000000001,
                value = state.data2[values$stateSelected,"commSlider"],
                step = 0.0000001, format = "#.## MMTCO<sub>2</sub>")
            
            })
            
            output$industSlider <- renderUI({
                sliderInput("inIndustSlider", NULL, min = 0,
                max = sliderMax(3)+.000000001,
                value = state.data2[values$stateSelected,"industSlider"],
                step = 0.0000001, format = "#.## MMTCO<sub>2</sub>")
            })
            
            output$residSlider <- renderUI({
                sliderInput("inResidSlider", NULL, min = 0,
                max = sliderMax(4)+.000000001,
                value = state.data2[values$stateSelected,"residSlider"],
                step = 0.0000001, format = "#.## MMTCO<sub>2</sub>")
            })
            
            output$transSlider <- renderUI({
                sliderInput("inTransSlider", NULL, min = 0,
                max = sliderMax(5)+.000000001,
                value = state.data2[values$stateSelected,"transSlider"],
                step = 0.0000001, format = "#.## MMTCO<sub>2</sub>")
            })
            
            output$elecSlider <- renderUI({
                
                #fixes problem that vermont has no electric emissions and errors out the slider
                if(values$stateSelected != "Vermont"){
                    sliderInput("inElecSlider", NULL,
                    min = 0, max = sliderMax(6)+.000000001,
                    value = state.data2[values$stateSelected,"elecSlider"],
                    step = 0.0000001, format = "#.## MMTCO<sub>2</sub>")
                }
                else{
                    sliderInput("inElecSlider", NULL,
                    min = 0, max = .000000001,
                    value = 0,
                    step = 0.00, format = "#.## MMTCO<sub>2</sub>")
                }
            })
            
                drawStates(rownames(state.data2))
                map$clearPopups()
            })
    })
    
    #Add popovers for each slider to give information about each emissions sector
    addPopover(session, id = "commSliderTitle", title = "<b>Commercial CO<sub>2</sub> Sources</b>", content = paste("<span style='color: black;'>Commercial emissions come primarily from fossil fuel combustion for heating and cooking. Large office buildings, warehouses, and stores are commonly heated with natural gas, a fossil fuel that emits CO<sub>2</sub> when it is combusted.", br(),br(), "Emissions from the Industrial sector can be reduced by:", "<ul>" , "<li>Increasing the energy efficiency of commercial buildings by using: </li>", "<ul>", "<li>Better insulation</li>", "<li>Passive heating</li>", "<li>Energy-efficient heating, cooling and ventilation</li>", "<li>Energy-efficient appliances</li></ul></ul></span>", sep=""), placement = "bottom", trigger = "hover")
    
    addPopover(session, id = "industSliderTitle", title = "<b>Industrial CO<sub>2</sub> Sources</b>", content = paste("<span style='color: black;'>Industrial emissions come primarily from the burning of fossil fuels in order to create power and heat for processes such as refining aluminum or running equipment. CO<sub>2</sub> emissions from Industry represent the third largest source of CO<sub>2</sub> emissions in the US.", br(),br(), "Emissions from the Industrial sector can be reduced by:", "<ul>" , "<li>Increasing energy efficiency of operations</li>", "<li>Switching to fuels that emit less CO<sub>2</sub>, i.e., using natural gas instead of coal to power machinery.</li>", "<li>Recycling, i.e., producing industrial products from materials that are recycled or renewable</li></ul></span>", sep=""), placement = "bottom", trigger = "hover")
    
    addPopover(session, id = "residSliderTitle", title = "<b>Residential CO<sub>2</sub> Sources</b>", content = paste("<span style='color: black;'>Residential emissions come primarily from fossil fuel combustion for heating and cooking. According to the <a href='http://www.eia.gov/tools/faqs/faq.cfm?id=49&t=8'>EIA</a>, in 2009 about 61% of all US households were heated with natural gas, which releases CO<sub>2</sub> when burned.", br(),br(), "Emissions from the Residential sector can be reduced by:", "<ul>" , "<li>Increasing the energy efficiency of homes by using: </li>", "<ul>", "<li>Better insulation</li>", "<li>Passive heating</li>", "<li>Energy-efficient heating, cooling and ventilation</li>", "<li>Energy-efficient appliances</li></ul></ul></span>", sep=""), placement = "bottom", trigger = "hover")
    
    addPopover(session, id = "transSliderTitle", title = "<b>Transportation CO<sub>2</sub> Sources</b>", content = paste("<span style='color: black;'>These emissions come primarily from cars and trucks as well as aircraft, ships, boats and trains. Transportation is the second largest source of CO<sub>2</sub> emissions after Electric Production.", br(),br(), "Emissions from transportation can be reduced by:", "<ul>" , "<li>Switching to alternative fuel types like biofuels, hydrogen, and electricity from renewable sources</li>", "<li>Improving fuel efficiency</li>", "<li>Improving operating practices</li>", "<li>Reducing Travel</li></ul></span>", sep=""), placement = "left", trigger = "hover")
    
    addPopover(session, id = "elecSliderTitle", title = "<b>Electricity Production CO<sub>2</sub> Sources</b>", content = paste("<span style='color: black;'>These emissions come primarily from coal and natural gas power plants. While coal accounts for about 75% of the emissions from this sector, it only represents about 39% of the electricity generated in the US.", br(),br(), "Emissions from electricity produciton can be reduced by:", "<ul>" , "<li>Increasing power plant efficiency or switching from coal to natural gas combustion</li>", "<li>Renewable energy</li>", "<li>Reducing energy demand</li>", "<li>Nuclear Energy</li>", "<li>Carbon capture technologies</li></ul></span>", sep=""), placement = "left", trigger = "hover")
    
    
    
    #Render state text displaying percent of state selected emissions reduced
    output$stateText2 <- renderUI({
        
        if(values$percent.reduc < state.data2[values$stateSelected,"goals"]){
            color1="red"}
        else {
            color1="green"}
        
        if(values$USpercent.reduc < usGoal){
            color2="red"}
        else {
            color2="green"}
        
        if(state.data2[values$stateSelected,"goals"] > 0){
            HTML(paste("<b><div style='text-align: center;'><span style='color: ",color1, "'>", values$stateSelected, " CO<sub>2</sub> Emissions Reduced: <span style='font-size: 130%;'>", values$percent.reduc, "%</span></span></div><b>", sep=""))
        }
        else{
            HTML(paste("<b><div style='text-align: center;'><span style='color: ",color1, "'>", values$stateSelected, " Target Already Reached!</span></span></div><b>", sep=""))
        }
    })
    
    #Render us text displaying percent of US emisison reduced
    output$USText2 <- renderUI({

        #Make the color change from red to green if goal is met
        if(values$percent.reduc < state.data2[values$stateSelected,"goals"]){
            color1="red"}
        else {
            color1="green"}
        
        if(values$USpercent.reduc < usGoal){
            color2="red"}
        else {
            color2="green"}
        
        HTML(paste("<b><div style='text-align: center;'><span style='color: ", color2, "'>", "Total US CO<sub>2</sub> Emissions Reduced: <span style='font-size: 130%;'>", values$USpercent.reduc, "%</span></span></div></b>", sep=""))
        
    })
    
    #Output title of pie chart. Done so that title fits and looks better
    output$pieTitle <- renderUI({
        HTML(paste("<div style='text-align: center;'><b>", values$stateSelected, " CO<sub>2</sub> Emissions Breakdown</b></div>", sep=""))
    })
    
    
    #Set values for pie chart
    observe({
        
        #Fix Vermont problem where electric emisisons are zero
        if(is.null(input$inElecSlider)){
            elecSliderValue <<- 0
        }
        else{
            elecSliderValue <<- input$inElecSlider }
        
        #Set pie changes for dull pie shapes to show how much has been reduced
        isolate({values$pieChanges <- abs(round(c(
            sliderMax(2)-input$inCommSlider,
            sliderMax(3)-input$inIndustSlider,
            sliderMax(4)-input$inResidSlider,
            sliderMax(5)-input$inTransSlider,
            sliderMax(6)- elecSliderValue), digits=2))})
        
        #Create array for pie chart
        values$data <- c(input$inCommSlider, values$pieChanges[1], input$inIndustSlider, values$pieChanges[2], input$inResidSlider, values$pieChanges[3], input$inTransSlider, values$pieChanges[4], elecSliderValue, values$pieChanges[5])
        
    })
    
    #Creates rChart piechart using values calculated above
    output$rChart <- renderChart2({
        
        
        dat <- data.frame(sectors = c("Commercial", "Commercial (Reduced)","Industrial","Commercial (Reduced)", "Residential", "Residential (Reduced)","Transportation","Transportation (Reduced)", "Electric Production","Electric Production (Reduced)"), emissions = values$data)
        
        dat2 <-  data.frame(sectors = c("a", "b", "c", "d"), emissions = c(1,4,6,3))
        
        pie = nPlot(x = "sectors", y = "emissions", data = dat, type = "pieChart")
        #pie$addParams(width = 200)
        state <- values$stateSelected
        #p2$chart(color = c('brown', 'blue', '#594c26', 'green')
        pie$chart(margin = "#! {top: 0, right: 0, bottom: 0, left: 0} !#", showLegend = F, showLabels = T, labelType = "percent", pieLabelsOutside = F, donut = T, labelThreshold = 0.00, donutRatio = .2,
            tooltipContent = "#! function(key, x, y, graph) {
                    return key + '<br>' + x + ' MMTCO<sub>2</sub>'} !#")
    
        return(pie)
    })

    #Render state guage values
    output$stateGauge <- renderGauge({
        values$percent.reduc
        reduction <- round(sum(state.data[values$stateSelected,6:10])-sum(state.data2[values$stateSelected,6:10]), digits=2)
    })

    #Render state Gauge output
    output$stateGaugeOutput <- renderUI({
        gaugeOutput("stateGauge", width=215, height=165, units="MMTCO2 Reduced", min=0, max=round(state.data2[values$stateSelected,"goals"]*sum(state.data[values$stateSelected,6:10])/100, digits=2), title=paste("Target for ", values$stateSelected, ": ", round(state.data2[values$stateSelected,"goals"], digits=2), "%", sep=""))
    })

    #Renders US gauge that shows how much of US emisions have been reduced
    observe({
        input$reset
        isolate({
            output$USGauge <- renderGauge({
                values$percent.reduc
                reduction <- round(sum(state.data[,6:10])-sum(state.data2[,6:10]), digits=2)
            })
            output$USGaugeOutput <- renderUI({
                gaugeOutput("USGauge", width=215, height=165, units="MMTCO2 Reduced", min=0, max=round(usGoal*sum(state.data[,6:10])/100, digits=2), title=paste("Target for US: 16.36%"))
            })
        })
    })

    #Create function that will render popup of emissions information
    showStatePopup <- function(stateClicked) {
        stateName <- simpleCap(getStateName(stateClicked))
        state <- map("state", stateName, plot=FALSE)
        
        a <- data.frame(lon = state$x, lat = state$y)
        a <- a[order(-a[,2], a[,1]),]
        
        content <- as.character(tagList(
            tags$h4(tags$strong(stateName)),
            #tags$h5(tags$strong("2012 Levels")),

            HTML(paste("<b>Total CO<sub>2</sub> Emissions in 2012:</b> ", state.data[stateName, 5], " Million Metric Tons of CO<sub>2</sub> (MMTCO<sub>2</sub>)", br(),
            "<b>Per Capita CO<sub>2</sub> Emissions in 2012:</b> ", state.data[stateName, 3], " Tons of CO<sub>2</sub> Per Capita",br(), "<b>CO<sub>2</sub> Reduction Target for 2025:</b>", br(),round(state.data2[values$stateSelected,"goals"], digits=2), "% of 2012 Levels",  sep=""))
            ))
        map$showPopup(a$lat[1], a$lon[1], content, stateName, options=list(
            maxWidth = "200"
        ))
    }

    # When map is clicked, show a popup with state emissions info
    clickObs <- observe({
        map$clearPopups()
        event <- input$map_shape_click
        if (is.null(event))
        return()
        
        isolate({
            showStatePopup(event$id)
        })
    })

    #Intialize window booleans, info is shown on open and exercise is hidden
    session$onFlushed(once=TRUE, function(){
        values$showInfo <<- T
        values$showExercise <<- F
    })

    #If continue button on info window is pressed, close window
    observe({
        if(length(input$continue) > 0){
            if(input$continue > 0){
                values$showInfo <<- F
            }
        }
    })

    #If info button is pressed, close info window if already open or open window if closed
    observe({
        input$info
        isolate({
            if(length(values$showInfo) > 0){
                if(values$showInfo == F){
                    values$showInfo <<- T
                }
                else{
                    values$showInfo <<- F
                }
            }
        })
    })



    #If values showInfo is true, display Info window
    observe({
        values$showInfo
        if(length(values$showInfo) > 0){
                output$helpWindow <- renderUI({
                    if(values$showInfo==T){
                        absolutePanel(
                        left = "32.5%", top = "3%", width = "35%", class = "infoBox", fixed = TRUE, draggable = TRUE,
                        tags$div(h2("State Emissions Reduction Explorer"), style='text-align: center;'),
                        HTML(paste("<span style='color: black;'><ul><li>In November 2014, President Obama <a href='http://www.whitehouse.gov/the-press-office/2014/11/11/fact-sheet-us-china-joint-announcement-climate-change-and-clean-energy-c'>announced</a> a new target to cut US greenhouse gas emissions by 26-28% below 2005 levels by 2025.</li>
                        <li>National and state emissions levels have fluctuated since 2005, the year that Obama&rsquo;s national goal is based on. Because of this, the map is based off of 2012 emissions levels, and the goals for the nation and each state reflect these changes in emissions.</li>
                        <li>Based on 2012 levels, the national goal is now 16.4% reduction by 2025 and each state has a unique emissions goal.</li>
                        <li>Emissions displayed here only include CO<sub>2</sub> emissions from fossil fuel combustion from 5 economic sectors. Other sources such as Industrial Processes, Solvents, Agriculture, Waste, and Land-Use, Land-Use Change, and Forestry were not included due to a lack of data availability.</li>
                        </ul>
                        <div style='text-align: center;'><b>Directions</b></div>
                        <ol>
                        <li>Click on a state to select it.</li>
                        <li>Move the sliders to reduce state emissions to meet the state goal.</li>
                        <li>Reduce several states&rsquo; emissions to meet the national goal.</li>
                        </ol>
                        <div style='text-align: center;'><b>Data Sources</b></div>
                        <ul>
                        <li>Population data from: <a href='https://www.census.gov/popest/data/datasets.html'>US Census Bureau</a></li>
                        <li>Emissions data from: <a href='http://epa.gov/statelocalclimate/resources/state_energyco2inv.html'>EPA</a></li>
                        </ul>
                        <div style='text-align: center;'>For more information, please contact: <a href='mailto:benjamin.poulter@montana.edu'>benjamin.poulter@montana.edu</a></div>
                        <br>
                        </span>")),
                        tags$div(actionButton("continue", label = "Continue"), style='text-align: center;')
                        )
                    }
                })
            
        }
        
    })


    observe({
        if(length(input$continue2) > 0){
            if(input$continue2 > 0){
                values$showExercise <<- F
            }
        }
        
    })

    observe({
        input$exercise
        isolate({
            if(length(values$showExercise) > 0){
                if(values$showExercise == F){
                    values$showExercise <<- T
                }
                else{
                    values$showExercise <<- F
                }
            }
        })
    })

    observe({
        values$showExercise
        if(length(values$showExercise) > 0){
            output$exerciseWindow <- renderUI({
                if(values$showExercise==T){
                    absolutePanel(
                    left = "35%", top = "10%", width = "30%", class = "infoBox", fixed = TRUE, draggable = TRUE,
                    tags$div(h2("Map Exercises"), style='text-align: center;'),
                    HTML(paste("<span style='color: black;'>
                    <ol>
                    <li>Find which state has the largest total CO2 emissions.</li>
                    <li>Which state has the highest per capita emissions and lowest?</li>
                    <li>Do states with high per capita emissions also have high total emissions?</li>
                    <li>Find a state that has high per capita and total emissions.</li>
                    <li>What states have largest commercial, industrial, residential, transportation, and electric emissions?</li>
                    <li>Which five states, when reduced completely, could meet Obama&rsquo;s emission reduction goals?</li>
                    <li>What percent National reduction could be made by closing power plants (eliminating electric power production emissions)?</li>
                    <li>What percent National reduction could be made by having zero-emission transportation?</li>
                    <li>What percent National reduction could be made by having solar or geothermal residential heating (eliminating residential emissions)?</li>
                    
                    </ol>
                    </span>")),
                    tags$div(actionButton("continue2", label = "Continue"), style='text-align: center;')
                    )
                }
            })
            
        }
        
    })

    observe({
        input$info
        input$reset
        isolate({values$showExercise <<- F})
    })

    observe({
        input$exercise
        input$reset
        isolate({values$showInfo <<- F})
    })



    output$sliderTitle <- renderUI({
        HTML(paste("<div style='text-align: center; margin-bottom: 8px;'><h4><b>", values$stateSelected," Economic Sectors<b><h4></div>", sep=""))
    })


    observe({
        updateButtonGroup(session, "btngrp1", toggle = "radio", style = "default", size = "default", disabled = FALSE, value = input$btngrp1)
    })


#for(i in 6:10){print(sum(state.data[,i])/sum(state.data[,5]))}

})

})
