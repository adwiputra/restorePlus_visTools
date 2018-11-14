library(leaflet)
library(rgdal)
library(RColorBrewer)
library(scales)
library(lattice)
library(tidyverse)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
# set.seed(100)
zipdata <- allzips[sample.int(nrow(allzips), 10000),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see
# zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {

  ## Interactive Map ###########################################
colRow <- readOGR(".", "GLOBIOM_Grid_Prov_Rev4")
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 124.0832, lat = -2.629957, zoom = 5)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  # zipsInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(zipdata[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  # 
  #   subset(zipdata,
  #     latitude >= latRng[1] & latitude <= latRng[2] &
  #       longitude >= lngRng[1] & longitude <= lngRng[2])
  # })
  
  seriesInView <- reactive({
    typeBy <- input$type
    scenBy <- input$scen
    if(typeBy != "PriFor" & typeBy != "CrpLnd")
      colorBy <- input$color else
        colorBy <- "Area"
    if(colorBy == "Area")
      unit <- "Ha" else if(colorBy == "Production")
        unit <- "Ton" else
          unit <- "Ton/Ha"
    # reshaping the table into appropriate form: year, scen, value
    plotTable <- colRowRds %>% filter(Type == paste0(typeBy) & (Scen == paste0(scenBy) | Scen == "Baseline"))
    plotTable <- plotTable %>% select(Scen, Year, colorBy) %>% group_by(Scen, Year) %>% summarize_at(colorBy, funs(sum))
    # filling the year with no data
    return(plotTable)
    # ADhere
  })

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  # output$histCentile <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  # 
  #   hist(zipsInBounds()$centile,
  #     breaks = centileBreaks,
  #     main = "SuperZIP score (visible zips)",
  #     xlab = "Percentile",
  #     xlim = range(allzips$centile),
  #     col = '#00DD00',
  #     border = 'white')
  # })

  # output$scatterCollegeIncome <- renderPlot({
  #   # If no zipcodes are in view, don't plot
  #   if (nrow(zipsInBounds()) == 0)
  #     return(NULL)
  # 
  #   print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })

  observe({
    typeIn <- as.character(input$type)
    if(typeIn == "PriFor" | typeIn == "CrpLnd")
      updateSelectInput(session, "color", "Color", choices = c("Area" = "Area"), selected = "Area") else
        updateSelectInput(session, "color", "Color", choices = c(
          "Production" = "Production",
          "Productivity" = "Productivity",
          "Area" = "Area"
        ), selected = "Production")
  })
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    typeBy <- input$type
    scenBy <- input$scen
    yearBy <- input$year
    if(typeBy != "PriFor" & typeBy != "CrpLnd")
      colorBy <- input$color else
        colorBy <- "Area"
    
    if(colorBy == "Area")
      unit <- "(Ha)" else if(colorBy == "Production")
        unit <- "(Tonnes)" else
          unit <- "(Tonnes/Ha)"
    # sizeBy <- input$size
    # dummy data AD====
    # joinTable <- read.csv("master_data_wide.csv", stringsAsFactors= FALSE)
    joinTable <- colRowRds %>% filter(Type == paste0(typeBy) & Year == yearBy & Scen == paste0(scenBy))
    joinTable <- joinTable %>% rename_at(1, ~"GRID")
    colRowDisp <- colRowShp %>% left_join(joinTable, by = "GRID")
    # colRowDisp <- colRowDisp %>% mutate(Productivity= case_when(is.na(Productivity) ~ 0, TRUE ~ Productivity))
    # if (colorBy == "superzip") {
    #   # Color and palette are treated specially in the "superzip" case, because
    #   # the values are categorical instead of continuous.
    #   colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
    #   pal <- colorFactor("viridis", colorData)
    # } else {
    #   colorData <- zipdata[[colorBy]]
    #   pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    # }

    # if (sizeBy == "superzip") {
    #   # Radius is treated specially in the "superzip" case.
    #   radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    # } else {
    #   radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    # }

    # leafletProxy("map", data = zipdata) %>%
    #   clearShapes() %>%
    #   addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
    #     stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
    #   addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
    #     layerId="colorLegend")
    # AD misc====
    colorDat <- colRowDisp %>% dplyr::select(colorBy) %>% pull()
    pale <- colorNumeric("viridis", colorDat)
    palet <- colRowDisp %>% dplyr::select(colorBy) %>% pull() %>% pale()
    # AD misc \ends----
    # , layerId = ~GRID to be added with proper ID
    if(colorBy != "Productivity"){
      leafletProxy("map", data = colRow) %>%  clearShapes() %>% addPolygons(weight = 1, smoothFactor = 0.5, layerId = ~LocID,
                                                                            opacity = 1.0, fillOpacity = 0.5,
                                                                            color = ~palet,
                                                                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                                                bringToFront = TRUE)) %>% addLegend("bottomleft", pal=pale, na.label = "N\\A", values=colorDat, title= paste0(colorBy, "<br>", unit), layerId = "colorLegend")
    } else{
      leafletProxy("map", data = colRow) %>%  clearShapes() %>% addPolygons(weight = 1, smoothFactor = 0.5, layerId = ~LocID,
                                                                            opacity = 1.0, fillOpacity = 0.5,
                                                                            color = ~palet,
                                                                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                                                bringToFront = TRUE)) %>% addLegend("bottomleft", pal=pale, na.label = "N\\A", labFormat = labelFormat(suffix = "                                               "), values=colorDat, title= paste0(colorBy, "<br>", unit), layerId = "colorLegend")
      #     labFormat = labelFormat(big.mark = ".", decimal.mark = ",")
      
    }
  })
  # leafletProxy("map", data = colRow) %>%  clearShapes() %>% addPolygons(weight = 1, smoothFactor = 0.5, layerId = ~LocID,
  #                                                                       opacity = 1.0, fillOpacity = 0.5,
  #                                                                       color = ~palet,
  #                                                                       highlightOptions = highlightOptions(color = "white", weight = 2,
  #                                                                                                           bringToFront = TRUE)) %>% addLegend("bottomleft", pal=pale, na.label = "N\\A", values=colorDat, title= paste0(colorBy, "<br>", unit), layerId = "colorLegend")
  
  
  
  # reactive values to avoid error in renderplot below====
  yAxis <- reactive({
    if(input$type == "PriFor" | input$type == "CrpLnd")
      return("Area") else
        return(input$color)
  })
  
  plotTitle <- reactive({
    expands <- c(
      "Cocoa" = "Ccoa",
      "Coffee" = "Coff",
      "Corn" = "Corn",
      "Crop Land" = "CrpLnd",
      "Primary Forest" = "PriFor",
      "Rice" = "Rice",
      "Small-holder Oil Plam" = "SOpal",
      "Company Oil Palm" = "LOpal"
    )
    plothead <- expands[which(grepl(input$type, expands))] %>% names()
    return(plothead)
  })
  
  plotUnit <- reactive({
    if(yAxis() == "Area")
      unit <- "Ha" else if(yAxis() == "Production")
        unit <- "Tonnes" else
          unit <- "Tonnes/Ha"
        return(unit)
  })
  # reactive values... \ends----
  
  
  
  output$scatterCollegeIncome <- renderPlot({
    ggplot(data = seriesInView(), aes_string("Year", paste0(yAxis()))) + geom_line(aes(color = Scen)) + theme_classic() + ggtitle(plotTitle()) + ylab(plotUnit()) + theme(legend.position= "bottom", legend.title = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.41, size = 18)) + scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)) 
    # isolate({input$color})
  })
  # observe({ # sensitive only to the adjustments made in type, scenario, and color
  #   typeBy <- input$type
  #   scenBy <- input$scen
  #   if(typeBy != "PriFor" & typeBy != "CrpLnd")
  #     colorBy <- input$color else
  #       colorBy <- "Area"
  #   
  # })
  
  # Show a popup at the given location
  # showZipcodePopup <- function(zipcode, lat, lng) {
  #   selectedZip <- allzips[allzips$zipcode == zipcode,]
  #   content <- as.character(tagList(
  #     tags$h4("Score:", as.integer(selectedZip$centile)),
  #     tags$strong(HTML(sprintf("%s, %s %s",
  #       selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
  #     ))), tags$br(),
  #     sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
  #     sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
  #     sprintf("Adult population: %s", selectedZip$adultpop)
  #   ))
  #   leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  # }
  showZipcodePopup <- function(zipcode, lat, lng, colRowDisp) {
    selectedZip <- colRowDisp[colRowDisp$LocID == zipcode,]
    content <- as.character(tagList(
      tags$h4("Province: ", paste0(capwords(selectedZip$PROVINSI, strict = TRUE))),
      # tags$strong(HTML(sprintf("%s, %s %s",
      #   selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      # ))), tags$br(),
      sprintf("Production (Tonnes): %s", prettyNum(round(selectedZip$Production, 3), big.mark = ",")), tags$br(),
      sprintf("Area (Ha): %s", prettyNum(round(selectedZip$Area, 3), big.mark = ",")), tags$br(),
      sprintf("Productivity (Tonnes/Ha): %s", prettyNum(round(selectedZip$Productivity, 4), big.mark = ","))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    typeBy <- input$type
    scenBy <- input$scen
    yearBy <- input$year
    if(typeBy != "PriFor" & typeBy != "CrpLnd")
      colorBy <- input$color else
        colorBy <- "Area"
    joinTable <- colRowRds %>% filter(Type == paste0(typeBy) & Year == yearBy & Scen == paste0(scenBy))
    joinTable <- joinTable %>% rename_at(1, ~"GRID")
    colRowDisp <- colRowShp %>% left_join(joinTable, by = "GRID")
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng, colRowDisp = colRowDisp)
    })
  })


  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
