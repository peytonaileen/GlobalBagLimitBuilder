

shinyServer(function(input, output, session) {

    
    #-----------------------------------------
    # Read in data
    #-----------------------------------------
    ## Load the shape file to a Spatial Polygon Data Frame (SPDF) using the readOGR()     function
    myspdf = readOGR(dsn=here("GlobalBagLimitBuilder", "data", "world_border"), layer="TM_WORLD_BORDERS_SIMPL-0.3")
    
    #Load fishbase country info
    fish_master<-country()
    
    
    #Read in example creel data 
    creel_header <- read_csv(here("GlobalBagLimitBuilder","data", "example_creel", "creel_header.csv"))
    
    creel_no_header <- read_csv(here("GlobalBagLimitBuilder","data", "example_creel", "creel_no_header.csv"), col_names = FALSE)
    
    
    colorScale<-rainbow(n=100)
    #-----------------------------------------
    # Create multiple pages 
    #----------------------------------------- 
    
    NUM_PAGES <- 5
    
    rv <- reactiveValues(page = 1)
    shinyjs::show("above1")
    shinyjs::show("below1")
    
    observe({
        toggleState(id = "prevBtn", condition = rv$page > 1)
        toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES)
    })
    
    navPage <- function(direction) {
        rv$page <- rv$page + direction
    }
    
    observeEvent(input$prevBtn,{
        lapply(1:NUM_PAGES, function(X) {shinyjs::hide(paste0("above", X)) })
        lapply(1:NUM_PAGES, function(X) {shinyjs::hide(paste0("below", X)) })
        navPage(-1)
        shinyjs:: show(paste0("above", rv$page))
        if(rv$page == 2 | rv$page == 3) {
            shinyjs::show(paste0("below3"))
        } else {
            shinyjs::show(paste0("below", rv$page))
        }
    } )
    
    observeEvent(input$nextBtn, {
        lapply(1:NUM_PAGES, function(X) {shinyjs::hide(paste0("above", X)) })
        lapply(1:NUM_PAGES, function(X) {shinyjs::hide(paste0("below", X)) })
        navPage(1)
        shinyjs::show(paste0("above", rv$page))
        if(rv$page == 2 | rv$page == 3) {
            shinyjs::show(paste0("below3"))
        } else {
            shinyjs::show(paste0("below", rv$page))
        }
    } )

    
    #-----------------------------------------------
    #  Create sidebar steps  
    #----------------------------------------------- 
    
    #STEP ONE 
    output$step_one_conditional<-reactive({
        !is.null(input$speciesListMethod)
    })
    
    output$selected_groupInput <- renderText({
        paste(input$speciesListMethod)
    })
    outputOptions(output, 'step_one_conditional', suspendWhenHidden = FALSE)
    
    #STEP TWO 
    
    output$step_two_conditional<-reactive({
        !is.null(input$speciesInput)
    })
    
    output$selected_speciesInput <- renderText({
        paste(input$speciesInput, ";")
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
                  colnames = c("Species", "FishBase common name"),
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
       # print(map_species_list())
    })
    
    #--------------------------------------------
    # Load example data 
    #--------------------------------------------
    # exDtData <- reactive({
    #         
    #          
    #    
    #     
    # })
    # 
    # output$ExDT <- renderDataTable({
    #     exDtData()
    #     })
    
    
    creel <- reactive({
        if(is.null(input$customCreelData)){
            if(input$exampData == "creel_header"){
                return(creel_header)
            } else{
                return(creel_no_header)
            }
        }
        if(!is.null(input$customCreelData)){
            file <- input$customCreelData
            read_csv(file$datapath, col_names = c("Species", "Take")) %>% 
                filter(Species !="species") %>% 
                mutate(Take = as.numeric(Take))
            # if(input$header == TRUE){
            #     read_csv(file$datapath, col_names = TRUE)
            # } else {
            #     read_csv(file$datapath, col_names = c("species", "take"))
            # }
        }
    })
    
    output$creelDT <- renderDT({
        creel()
    })
    
    
    # output$textChoice <- renderText({
    #     if(is.null(creel())){
    #         "Please select a method of creating a species list"
    #     }
    # })
   speciesData <- reactive({
       if(input$speciesListMethod == "Creel"){
           return(creel())
       }
       if(input$speciesListMethod == "Map"){
           return(map_species_list())
           
       }

   })
    
    
    
    
    
    output$speciesSelectInput<- renderUI({
        
        multiInput(
                    inputId = "speciesInput",
                    label = "Select species to discuss",
                    choices = sort(unique(speciesData()$Species))
                )
        # if(input$speciesListMethod == "upCreel"){
        #     multiInput(
        #         inputId = "speciesInput", 
        #         label = "Select species to discuss", 
        #         choices = sort(unique(speciesData()$Species))
        #     )
        # }
        # if(input$speciesListMethod == "mapList"){
        #     multiInput(
        #         inputId = "speciesInput", 
        #         label = "Select species to discuss", 
        #         choices = sort(unique(speciesData()$Species))
        #     )
        #     
        # }
        
        # observeEvent(input$speciesListMethod, {
        #     updateMultiInput(session, "speciesInput")
        # })
        
        
       
    })
    
    #--------------------------------  
    #Calculate median bag size for group using data_MOD 
    #--------------------------------
    
    data_MOD <- reactive({
        req(speciesData())
        data_MOD<- speciesData() %>% 
            filter(Species %in% input$speciesInput)
        
    })
    
    
    sampleBagLimit<-reactive({
        req(input$sampleBag, data_MOD(), input$speciesListMethod == "Creel")
       
        #Median historical take
        bag_median <- median(data_MOD()$Take)
        
        #Vectors of new empirical distribution based on
        bag_size_greater<-rep(input$sampleBag, NROW(which(data_MOD()$Take > input$sampleBag))) #This is a vector of values
        bag_size_less<-data_MOD()$Take[which(data_MOD()$Take <= input$sampleBag)] #This is a vector of values
        original_bag_vector<-data_MOD()$Take
        new_bag_vector<-c(bag_size_greater,bag_size_less)
        
        #Bootstrapping analysis here so that we could convey uncertainty in the creel survey
        n = 1000#number of boostrap samples
        set.seed(1)
        bootPercentReduction<-sapply(1:n, function(x){
            sampOrig<-sample(data_MOD()$Take, size=NROW(data_MOD()$Take), replace = TRUE)
            sampGreater<-rep(input$sampleBag, NROW(which(sampOrig > input$sampleBag))) #This is a vector of values
            sampLess<-sampOrig[which(sampOrig <= input$sampleBag)] #This is a vector of values
            origSum<-sum(sampOrig)
            truncSum<-sum(c(sampGreater,sampLess))
            round(abs((truncSum-origSum)/origSum)*100, 0)
        })
        
        return(list(bag_median = bag_median, 
                    original_bag_vector = original_bag_vector,
                    new_bag_vector = new_bag_vector,
                    minPercentReduction = min(bootPercentReduction),
                    maxPercentReduction = max(bootPercentReduction)))
    })
    
    output$median_bag_info <- renderText({
        req(sampleBagLimit())
        paste("Median historical take:", sampleBagLimit()$bag_median)
    })
    
    output$pct_take <- renderText({
        req(sampleBagLimit())
        paste0("Plausible percentage reduction in take using bag limit: ", sampleBagLimit()$minPercentReduction, "%", " to ", sampleBagLimit()$maxPercentReduction, "%")
    })
    
    #Projected take historgram
    output$percentDecrease <- renderPlot({
        req(sampleBagLimit())
        
        
        ridge_decreased<- data.frame(Historical = c(sampleBagLimit()$original_bag_vector),
                                     Projected = c(sampleBagLimit()$new_bag_vector)) %>% 
            pivot_longer(cols = 1:2 ,
                         names_to = "scenario", 
                         values_to = "take")
        
        
        ridge_decreased %>% 
            ggplot(aes(x = take, fill = scenario))+
            geom_histogram( binwidth = 1, alpha = 0.5, color="#e9ecef")+
            theme_bw()+
            scale_fill_manual(values=c("#66CDAA", "orange"))+
            scale_x_continuous(limits=c(0,max(ridge_decreased$take)))+
            theme(legend.position = "none", 
                  panel.spacing = unit(0.1, "lines"), 
                  text = element_text(size = 15), 
                  plot.caption = element_text(hjust = 0.5, face = "italic"))+
            labs( title = "Projected Distribution of Take Under Sample Bag Limit",
                  y = "Frequency", 
                  x = "Take per Trip (Number of Fish)"
                  #caption = "The historical take is reflective of the distribution of take associated with the current selection. The projected take is \n reflective of the current selection under the sample bag limit. This projection assumes that all fishers previously \n taking more than the suggested bag limit, would now take the maximum amount of fish allowed."
            ) +
            facet_wrap(~scenario)
        
        
    })
    
    #Species ridgeline plot 
    
    output$speciesRidgeline <- renderPlot({
        req(input$speciesListMethod == "Creel")
        if(NROW(data_MOD()) > 0){
            
            n_obs<-data.frame(species_MOD=unique(data_MOD()$Species),
                              x = rep(max(data_MOD()$Take), NROW(unique(data_MOD()$Species))),
                              y = unique(data_MOD()$Species),
                              label = sapply(unique(data_MOD()$Species), FUN=function(x){ paste0("n=(",NROW(data_MOD()$Species[data_MOD()$Species==x]),")")})
            )
            
            data_MOD() %>% 
                arrange(desc(NROW(Species))) %>% 
                ggplot(aes(x = Take,
                           y = Species
                ))+
                geom_density_ridges(alpha = 0.5, 
                                    aes(fill = Species, 
                                        color = Species))+
                scale_x_continuous(limits=c(0,max(data_MOD()$Take)))+
                scale_color_manual(name = "species_group", values=colorScale) +    
                
                scale_fill_manual(name = "species_group", values=colorScale) +
                theme_bw()+
                theme(legend.position = "none", 
                      panel.spacing = unit(0.1, "lines"), 
                      text = element_text(size = 15), 
                      plot.caption = element_text(hjust = 0.5, face = "italic"))+
                labs( title = "Distribution of Historical Take by Species",
                      y = "Species Name", 
                      x = "Historical Take per Trip (Number of Fish)" 
                      #caption = "The number of fish taken per trip per fisher is shown by species, where n is the number of trips"
                ) +
                geom_text(data=n_obs, aes(x=x, y=y, label = label), position=position_nudge(y= .25, x = -5))
        }
    })
    
    
}) # close server 
