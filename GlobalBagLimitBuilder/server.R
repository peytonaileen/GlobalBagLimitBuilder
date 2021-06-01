

shinyServer(function(input, output, session) {

    
    #-----------------------------------------
    # Read in data
    #-----------------------------------------
    ## Load the shape file to a Spatial Polygon Data Frame (SPDF) using the readOGR()     function
    myspdf = readOGR(dsn=here("GlobalBagLimitBuilder", "data", "world_border"), layer="TM_WORLD_BORDERS_SIMPL-0.3")
    
    #Load fishbase country info
    fish_master<-country()
    
    
    #-----------------------------------------
    # Create multiple pages 
    #----------------------------------------- 
    NUM_PAGES <- 5
    rv <- reactiveValues(page = 1)
    show("above1")
    show("below1")
    
    observe({
        toggleState(id = "prevBtn", condition = rv$page > 1)
        toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES)
    })
    
    navPage <- function(direction) {
        rv$page <- rv$page + direction
    }
    
    observeEvent(input$prevBtn,{
        lapply(1:NUM_PAGES, function(X) {hide(paste0("above", X)) })
        lapply(1:NUM_PAGES, function(X) {hide(paste0("below", X)) })
        navPage(-1)
        show(paste0("above", rv$page))
        if(rv$page == 2 | rv$page == 3) {
            show(paste0("below3"))
        } else {
            show(paste0("below", rv$page))
        }
    } )
    
    observeEvent(input$nextBtn, {
        lapply(1:NUM_PAGES, function(X) {hide(paste0("above", X)) })
        lapply(1:NUM_PAGES, function(X) {hide(paste0("below", X)) })
        navPage(1)
        show(paste0("above", rv$page))
        if(rv$page == 2 | rv$page == 3) {
            show(paste0("below3"))
        } else {
            show(paste0("below", rv$page))
        }
    } )

    
    #-----------------------------------------------
    #  Create sidebar steps  
    #----------------------------------------------- 
    
    #STEP ONE 
    output$step_one_conditional<-reactive({
        #!is.null(input$groupInput)
    })
    
    output$selected_groupInput <- renderText({
        #paste(input$groupInput, ";")
    })
    outputOptions(output, 'step_one_conditional', suspendWhenHidden = FALSE)
    
    #STEP TWO 
    
    output$step_two_conditional<-reactive({
        #!is.null(input$selectedSpecies)
    })
    
    output$selected_speciesInput <- renderText({
        #paste(input$selectedSpecies, ";")
    })
    outputOptions(output, 'step_two_conditional', suspendWhenHidden = FALSE)
    
    #STEP Three
    
    output$step_three_conditional<-reactive({
        #NROW(bag_proposals_data()) >0
        
    })
    
    output$current_proposals_text <- renderText({
        #paste(bag_proposals_data()$Proposal,  ";")
    })
    outputOptions(output, 'step_three_conditional', suspendWhenHidden = FALSE)
    
    #--------------------------------------------
    # Create species map to sort out a species list 
    #--------------------------------------------
    
    #Mapping
    output$mymap <- renderLeaflet({
        # Create the map data and add polygons 
        leaflet(data=myspdf) %>% 
            addTiles() %>% 
            setView(lat=10, lng=0, zoom=2.4) %>%
            addPolygons(fillColor = "green",
                        highlight = highlightOptions(weight = 5,
                                                     color = "blue",
                                                     fillOpacity = 0.7,
                                                     bringToFront = TRUE),
                        label = ~NAME,
                        layerId = ~NAME) # add a layer ID to each shape. This will be used to identify the shape clicked
        
    })
    
    # Zoom and set the view after click on state shape
    # input$mymap_shape_click will be NULL when not clicked initially to any shape
    # input$mymap_shape_click will have the ID, lat and lng corresponding to the shape clicked
    # Observe to update the view and zoom level when a shape is clicked
    observe(
        {  click = input$mymap_shape_click
        #  subset the spdf object to get the lat, lng and country name of the selected shape (Country in this case)
        sub = myspdf[myspdf$NAME==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
        lat = sub$LAT
        lng = sub$LON
        country = sub$NAME
        if(is.null(click)) ## if nothing is clicked
            return()
        else
            leafletProxy("mymap") %>%
            setView(lng = lng , lat = lat, zoom = 4) %>%
            clearMarkers() %>%
            addMarkers(lng =lng , lat = lat, popup = country)
        # using lat long from spdf will not change the view on multiple clicks on the same shape
        
        }
    )
    
    ## absolute Panel displaying country Name
    output$map_text <- renderText({
        sub = myspdf[myspdf$NAME==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
        country <- sub$NAME
        paste("Country:", country)
    })
    
    map_species_list<-reactive({
        sub = myspdf[myspdf$NAME==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
        countryIn = sub$NAME
        #Manual changes so that country names from map conform to fishbase country names
        if(length(countryIn) > 0){
            if(countryIn == "United States") countryIn<-"USA"
            if(countryIn == "United Kingdom") countryIn <-"UK"
            if(countryIn == "Marshall islands") countryIn <- "Marshall Is."
            if(countryIn == "Northern Mariana islands") countryIn <- "North Marianas"
        }
        Y<-fish_master %>%
            filter(
                country %in% countryIn,
                Status %in% c("endemic", "native", "introduced", "reintroduced")
            )
        Species<-sort(Y$Species)
        FBname<-species(Species, fields=c("FBname", "PicPreferredName"))
        data.frame(list(Species=Species, FBname=FBname$FBname, Pic=FBname$PicPreferredName))
    })
    
    output$map_species_table <- renderDT({
        datatable(map_species_list()[,1:2],
                  colnames = c("Scientific name", "FishBase common name"),
                  rownames = FALSE,
                  selection = "single",
                  options = list(pageLength = 15,
                                 scrollX = TRUE,
                                 columnDefs = list(list(className = 'dt-left', targets = 0:1)))
        )
        
    })
    map_species_table_Proxy<-dataTableProxy(session$ns('map_species_table'))
    
    observeEvent(input$map_species_table_rows_selected,{
        shinyalert(
            title = map_species_list()$FBname[input$map_species_table_rows_selected],
            imageUrl = paste0("https://www.fishbase.de/images/species/", map_species_list()$Pic[input$map_species_table_rows_selected]),
            imageWidth = 300,
            imageHeight = 300,
        )
    })
    
}) # close server 
