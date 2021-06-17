

shinyServer(function(input, output, session) {

    
    #-----------------------------------------
    # Read in data
    #-----------------------------------------
    ## Load the shape file to a Spatial Polygon Data Frame (SPDF) using the readOGR()     function
    myspdf = readOGR(dsn=here("GlobalBagLimitBuilder", "data", "world_border"), layer="TM_WORLD_BORDERS_SIMPL-0.3")
    
    #Load fishbase country info
    fish_master<-country()
    
    
    #Read in example creel data 
    creel_sin_species <- read_csv(here("GlobalBagLimitBuilder","data", "example_creel", "creel_sin_species.csv"))
    
    creel_multi_species <- read_csv(here("GlobalBagLimitBuilder","data", "example_creel", "creel_multi_species.csv"))
    
    creel_gear <- read_csv(here("GlobalBagLimitBuilder","data", "example_creel", "creel_gear.csv"))
    
    creel_groups <- read_csv(here("GlobalBagLimitBuilder","data", "example_creel", "creel_groups.csv"))
    
    
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
        NROW(bag_proposals_data()) >0
        
    })
    
    output$current_proposals_text <- renderText({
        paste(bag_proposals_data()$Proposal,  ";")
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
    
    #----------------------------------------------------------------------
    # Create data frame of the categories with "unique" identifier 
    #----------------------------------------------------------------------
    
    listCategories <- reactiveValues()
    
    createCategory <- function(name){
        
        catID <- gsub("\\.", "", format(Sys.time(), "%H%M%OS3"))  
        catBID <- paste0(catID, "rmv")
        
        listCategories[[catID]] <- list(name = name)
        
        #----------------------------------
        #UI for list of Categories
        #----------------------------------
        
        #Create/update UI of category list (see UI that makes this sortable)
        insertUI(
            selector = "#catTable",
            ui = tags$div(
                id = catID,
                `data-rank-id` = catID,
                wellPanel(style = "background-color: lightgrey; border-radius:4px; padding-top: 10px; padding-bottom: 10px;",
                          tags$table(
                              tags$tr(
                                  width = "100%",
                                  tags$td(width = "90%", div(style = "font-size:14px;",  listCategories[[catID]]$name)),
                                  tags$td(
                                      width = "10%",
                                      actionButton(catBID, "Remove option", class = "pull-right")
                                  )
                                  
                              )
                          ))
            )
        )
        
        #Create Observer for remove button and remove the category
        observeEvent(input[[catBID]], {
            removeUI(selector = paste0("#", catID))
            listCategories[[catID]] <- NULL
        }, ignoreInit = TRUE, once = TRUE)
    }
    
    observeEvent(input$addCategory, {
        
        if (input$category1 == "") {
            showModal(
                modalDialog(
                    title = "Please enter a category",
                    easyClose = TRUE,
                    footer = NULL
                )
            )
        } else {
            
            createCategory(name = input$category1)
            
            updateTextInput(session, "category1", label = "Enter another category",value = "")
            
        }
    })
    
    category_list_df <- reactive({
        x <- names(listCategories)
        cat_tmp_table <- data.frame()
        for(i in x){
            if(!is.null(listCategories[[i]])){ 
                cat_tmp_table <- data.frame(rbind(cat_tmp_table,
                                                 list(Name = listCategories[[i]]$name)))
                
            }}
        return(cat_tmp_table)
        
    })
    #--------------------------------------------
    # Load example data 
    #--------------------------------------------
    
    creel <- reactive({
        if(is.null(input$customCreelData)){
            if(input$exampData == "creel_multi_species"){
                return(creel_multi_species)
            } 
            if(input$exampData == "creel_sin_species"){
                return(creel_sin_species)
            } 
            if(input$exampData == "creel_gear"){
                return(creel_gear)
            } 
            if(input$exampData == "creel_groups"){
                return(creel_groups)
            } 
        }
        if(!is.null(input$customCreelData)){
            file <- input$customCreelData
            read_csv(file$datapath, col_names = c("Category", "Take")) %>% 
                filter(Take != "take", 
                       Take != "Take") %>% # add filter to take out any rows where take isn't a number
                #create input for user the category contained in their data
                mutate(Take = as.numeric(Take))
           
        }
    })
    
    output$creelDT <- renderDT({
        creel()
    })
    
    
   speciesData <- reactive({
       if(is.null(input$speciesListMethod)){
           showModal(modalDialog(
                   title = "Please choose a method",
                   easyClose = TRUE,
                   footer = NULL
               ))
       }
       if(input$speciesListMethod == "Creel"){
           if(is.null(input$customCreelData)){
               showModal(modalDialog(
                   title = "Please upload creel data",
                   easyClose = TRUE,
                   footer = NULL
               ))
           } else {
              return(creel()) 
           }
           
       }
       if(input$speciesListMethod == "Map"){
           
           if(is.null(input$mymap_shape_click)){
               showModal(modalDialog(
                   title = "Please select a location on the map",
                   easyClose = TRUE,
                   footer = NULL
               ))
           } else {
               map_cat<- map_species_list() %>% 
                   rename(Category = Species)
               return(map_cat)
           }
           
       }
       if(input$speciesListMethod == "Manual"){
           
           if(is.null(category_list_df())){
               showModal(modalDialog(
                   title = "Please add categories to discuss",
                   easyClose = TRUE,
                   footer = NULL
               ))
           } else {
               cat_list <- category_list_df() %>% 
                   rename(Category = Name)
               return(cat_list) #CHANGE ERROR ASSOCIATED WITH THIS 
           }
           
       }

   })
    
   # observeEvent(input$speciesListMethod,{
   #     if(input$speciesListMethod == "Map"){
   #     shinyjs::hide("below3")
   #     }
   # })
    
    observeEvent(input$test,{
        print(speciesData())
    })
    
    
    output$speciesSelectInput<- renderUI({
        if(is.null(input$speciesListMethod)){
            showModal(modalDialog(
                title = "Please choose a method",
                easyClose = TRUE,
                footer = NULL
            ))
        } else {
        multiInput(
                    inputId = "speciesInput",
                    label = "Select species to discuss",
                    choices = sort(unique(speciesData()$Category))
                )
         }
        
        
       
    })
    
    #--------------------------------  
    #Calculate median bag size for group using data_MOD 
    #--------------------------------
    
    data_MOD <- reactive({
        req(speciesData())
        data_MOD<- speciesData() %>% 
            filter(Category %in% input$speciesInput)
        
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
            
            n_obs<-data.frame(species_MOD=unique(data_MOD()$Category),
                              x = rep(max(data_MOD()$Take), NROW(unique(data_MOD()$Category))),
                              y = unique(data_MOD()$Category),
                              label = sapply(unique(data_MOD()$Category), FUN=function(x){ paste0("n=(",NROW(data_MOD()$Category[data_MOD()$Category==x]),")")})
            )
            
            data_MOD() %>% 
                arrange(desc(NROW(Category))) %>% 
                ggplot(aes(x = Take,
                           y = Category
                ))+
                geom_density_ridges(alpha = 0.5, 
                                    aes(fill = Category, 
                                        color = Category))+
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
    
    #-----------------------------------------------
    #  Retain bag proposals
    #-----------------------------------------------
    
    #of the species selected for consideration, this is set included in a given bag proposal
    output$speciesInputBag <- renderUI({
        pickerInput(
            inputId = "selectedSpeciesBag",
            label = "Select / deselect species",
            choices = input$speciesInput,
            options = list(
                `actions-box` = TRUE, 
                style = "btn-primary"), 
            multiple = TRUE
        )
    })
    
    
    #Retained proposals
    proposalList<-reactiveValues()
    proposalEval<-reactiveValues()
    
    #Function for creating new proposals
    createProposal<-function(nm, Species, proposedBag, restoredBag=FALSE, initDecision=NULL){
        
        #Input/output naming
        divID <- nm
        dtID <- paste0(divID, "DT")
        btnID <- paste0(divID, "rmv")
        
        #Proposal list
        proposalList[[divID]] <- list(Species = Species, `Proposed bag limit` = proposedBag)
        str1 <- paste(strong("Proposal:"), divID)
        str2 <- paste(strong("Species selected:"), proposalList[[divID]]$Species)
        str3 <- paste(strong("Bag limit regulations:"), proposalList[[divID]]$`Proposed bag limit`)
        
        #Insert output in UI
        insertUI(
            selector = "#proposalList_Table",
            ui = tags$div(id = divID,
                          actionButton(btnID,
                                       "Remove this proposal",
                                       class = "pull-right"),
                          uiOutput(dtID)
            )
        )
       # outputOptions(output, dtID, suspendWhenHidden = FALSE)
        
        # create a listener on the newly-created button that will remove it from the app when clicked
        observeEvent(input[[btnID]], {
            removeUI(selector = paste0("#", divID))
            proposalList[[divID]] <- NULL
            proposalEval[[divID]] <- NULL
        }, ignoreInit = TRUE, once = TRUE)
    }
    
    #Response to user click retain BagProposal
    observeEvent(input$saveBagProposal, {
        
        # handle the case when user does not provide ID
        divID <- if (input$saveBagProposal_name == "") gsub("\\.", "", format(Sys.time(), "%H%M%OS3"))
        else input$saveBagProposal_name
        
        # only create button if there is none
        if (is.null(proposalList[[divID]]) & !is.null(input$selectedSpeciesBag)) {
            
            #create new proposal
            createProposal(nm = divID,
                           Species = paste(input$selectedSpeciesBag,collapse=", "),
                           proposedBag = input$customBagText
            )
            
            #Reset name input
            updateSelectInput(session, "saveBagProposal_name",
                              selected="")
            updateSelectInput(session, "customBagText", 
                              selected ="")
            
            #Create an auto-restore point
             show_condition <- function(code) {
                 tryCatch(code,
                          condition = function(c) {
                              showModal(modalDialog(
                                  title = "Could not save or create restore point",
                                  easyClose = TRUE,
                                  footer = NULL
                             ))
                          }
                 )
             }

             name <- gsub("\\.", "", format(Sys.time(), "%H%M%OS3"))
             show_condition(
                 space_putRDS(
                     x = createRestorePoint(),
                     object = paste0("/", userName, "/", name, ".rds"),
                     bucket =  bucketBookmark
                 )
             )
            
            # otherwise, print a message to the console
        } else {
            # showModal(modalDialog(
            #     title = "Cannot save proposal",
            #     tags$p("Check the following:"),
            #     tags$ul(
            #         tags$li("At least 1 species must be selected"),
            #         tags$li("Scenario name may already be in use"),
            #     ),
            #     easyClose = TRUE,
            #     footer = NULL
            # ))
            # ERror associated with this chunk 
        }
    }) 
    
    #Converts proposalsList to a convenient data.frame
    #Called by several functions. DO NOT DELETE THIS REACTIVE
    bag_proposals_data<-reactive({
        x <- names(proposalList)
        tmp_table <- data.frame() 
        for(i in x){
            if(!is.null(proposalList[[i]])){ 
                tmp_table <- rbind(tmp_table,
                                   list(Proposal = i,
                                        Species = proposalList[[i]]$Species,
                                        Regulations = proposalList[[i]]$`Proposed bag limit`))
                
            }}
        return(tmp_table)
    })
    
    #Create table of options
    output$bag_proposals_table<- DT::renderDT({
        datatable(data.frame( bag_proposals_data()),
                  selection = "single",
                  rownames=FALSE,
                  options = list(dom = c('ltp'), 
                                 pageLength = 5, 
                                 lengthMenu = c(5,10,15,20),
                                 scrollY= "220px")
        )
    })
    bag_proposals_table_Proxy<-dataTableProxy('bag_proposals_table')
    
    
    
    
}) # close server 
