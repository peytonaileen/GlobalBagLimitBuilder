
#----------------------------------
# Creat global bag limit app  
#----------------------------------

#----------------------------------
#Attach packages  
#----------------------------------
library(bsplus)
library(shinyBS)
library(rfishbase)
library(leaflet)
library(rgdal)

library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(shinydashboard)
library(here)
library(ggplot2)
library(ggridges)
library(forcats)
library(Hmisc)
library(stringr)
library(rsconnect)
library(DT)
library(treemap)
library(shinyjs)
library(kableExtra)
library(ggplot2)
library(ggradar2)
library(gridExtra)
library(plotly)
library(aws.s3)
library(shinyalert)
library(shinybusy)
library(htmltools)


#----------------------------------
#Create the user interface:
#----------------------------------

tagList(
    add_busy_spinner(
        spin ="orbit",
        color = "orange",
        position = "top-right",
        onstart = FALSE),
    useShinyjs(),
    useShinyalert(),
    
    tags$style(".popover{max-width:50%;}"),

dashboardPage(
    skin ="black", 
    dashboardHeader(title = "Global Bag Limit Builder"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName ="about"), 
            menuItem("Create Species List", tabName = "createSpeciesList"),
            menuItem("Builder", tabName = "builder")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "about", 
                    fluidRow(
                        align = "center",
                        h1("What is a 'Bag Limit Builder'?", class="text-success"),
                        h3(
                            "This tool is designed to allow users to simply and easily design and document draft bag limit proposals"
                        ),
                        br(),
                        hr(), 
                        br(),
                        
                        h2("Step 1&2: Select species to include", class="text-warning"),
                        h4(
                            "Develop your species list! Here you will have the opportunity to select the species group(s) you would like to include. Then you will have the option to further refine the list of species you wish to include. We recommend that individual species of particular concern in regard to either fishing or conservation objectives be evaluated separately."
                        ),
                        icon("fish", "fa-6x"), icon("fish", "fa-6x"),icon("fish", "fa-6x"),
                        br(),
                        br(),
                        h2("Step 3: View historical catch data and develop bag limit proposals", class="text-warning"),
                        h4(
                            "There are fishing effort interview surveys from multiple sources across the state of Hawai'i built into the app to inform discussions. The data you will see reflects your species selections. Data is represented as take of number of fish per individual per day. You also have the option to filter by location and gear type. Addional filtering options will vary by source.  Mulitiple figures will apear reflecting your selection, in order to give an understanding of similarities and differences between the species and species groups you have selected. Ultimately, historical catch data are intended to provide a frame of reference for drafting bag limit proposals."), 
                        br(), 
                        icon("chart-area", "fa-8x"),
                        br(), 
                        h4("Now to draft a bag limit proposal. You will have an option to further subset species from your original selection, to reflect insights gained from the data and group discussion using the dropdown. Then you can describe proposed bag limits. This can be as specific as proposed limits for the group (i.e. Species X, Y, & Z at a bag limit of 10), or be more vague such as simply stating the group is of high cultural value to the community and should be treated with particular attention (i.e. Manini should be protected). Then you will be prompted to name the proposal. We recommend this name be easily identifiable and concise. Finally, when you are satisfied with your selection, press 'Save Proposal' in order to move the draft to your saved proposals. After the proposal has been saved, you are free to clear all current selections and select a new grouping."
                        ),
                        br(), 
                        icon("edit", "fa-8x"),
                        br(),
                        br(),
                        h2("Step 4: Evaluate your bag limit proposals", class="text-warning"),
                        h4(
                            "Here you will see all of your saved proposals that are ready to be evaluated. Through polling or group discussion, you can then adjust the decision criteria to reflect the strengths and weaknesses of the proposal. If it is decided that a proposal is not meeting objectives or needs to be re-evaluated completely, you can remove the proposal and it will be erased."
                        ),
                        br(),
                        icon("balance-scale-left", "fa-8x"),
                        h2("Step 5: Document your progress", class="text-warning"),
                        h4(
                            "Once you have drafted and evaluated as many proposals as you would like (or if you are finished with your session) you can save your progress by clicking the 'Download report' button. This will export a report summarizing all of the drafted proposals and their performance. "
                        ),
                        h4(
                            "At any time you can also save your session progress on the 'Save & Restore' tab. Here you can save the current session to return to at a later date with the 'Save current session' and 'Load previous session' buttons."
                        ),
                        br(),
                        icon("file-download", "fa-8x"),
                        br()
                    )), #Close about tab
            tabItem(tabName = "createSpeciesList", 
                    
                    tabBox(title = "Create Species List",
                           id = "listCreate",
                           side = "right",
                           width = "1000px",
                           
                           tabPanel("Species Map", 
                                    useShinyalert(),
                                    
                                    leafletOutput("mymap"),
                                    br(),
                                    DTOutput("map_species_table"),
                                    # Absolute panel will house the user input widgets
                                    # Div tag is used to add styling to absolute panel
                                    absolutePanel(top = 10, right = 10, fixed = FALSE,
                                                  tags$div(style = "opacity: 0.70; background: #FFFFFF; padding: 8px; ",
                                                           helpText("Welcome to the World map!"),
                                                           textOutput("map_text") # display the country name in absolute panel when shape is clicked
                                                  )
                                    )), 
                           tabPanel("Upload Creel Data", 
                                    fileInput(
                                        inputId = "customCreelData", 
                                        label = "Choose a data file", 
                                        accept = c(
                                            'text/csv',
                                            'text/comma-separated-values',
                                            '.csv')),
                                    switchInput(inputId ="header",
                                                label = "Header",
                                                onLabel = "Yes",
                                                offLabel = "No", 
                                                value = TRUE),
                                    selectInput(inputId="exampData",
                                                label="Example data",
                                                choices=c("Creel - header" = "creel_header",
                                                          "Creel - no header"= "creel_no_header")),
                                    DTOutput("creelDT")
                                    ))
                    ),
            tabItem(tabName = "builder", 
                    sidebarLayout(
                        sidebarPanel(
                            style = "background: white; border: white;",
                            width=3, 
                            conditionalPanel(
                                condition = "output.step_one_conditional", 
                                align = "center",
                                wellPanel(
                                    id = "groupStep", 
                                    style = "background-color: whitesmoke; border-color: #66CDAA; text-align:center;",
                                    height="100%",
                                    icon("check-circle", "fa-1x"), "Groups Selected:", 
                                    tags$style(".fa-check-circle{color:orange;}"),
                                    textOutput("selected_groupInput")
                                ),
                                icon("arrow-circle-down", "fa-3x"),
                                br(), 
                                br(),
                            ),
                            conditionalPanel(
                                condition = "output.step_two_conditional", 
                                align = "center",
                                wellPanel(
                                    id = "speciesStep", 
                                    style = "background-color: whitesmoke; border-color: #66CDAA; text-align:center;",
                                    height="100%",
                                    icon("check-circle", "fa-1x"), "Species Selected:", 
                                    textOutput("selected_speciesInput")
                                ),
                                icon("arrow-circle-down", "fa-3x"),
                                br(), 
                                br(),
                                
                            ), 
                            conditionalPanel(
                                condition = "output.step_three_conditional", 
                                align = "center",
                                wellPanel(
                                    id = "proposalStep", 
                                    style = "background-color: whitesmoke;
              border-color:#66CDAA;
              text-align:center;",
              height="100%",
              icon("check-circle",
                   "fa-1x"),
              "Proposal Drafts:",
              textOutput("current_proposals_text")),
              br(), 
              br()), 
              
              
              textAreaInput(inputId = "sessionNotes", 
                            label = "Notes:", 
                            placeholder = "Jot down any session notes here!",
                            rows = 10),
                        ),#Close sidebar panel
              
              
              mainPanel(
                  width=9,
                  
                  ########################################
                  #Above previous / next
                  ########################################
                  
                  shinyjs::hidden(
                      wellPanel(
                          id = "above1",
                          style = "background-color: white;",
                          "content here", 
                          br(),
                          radioGroupButtons(inputId = "speciesListMethod", 
                                               label = "Select method", 
                                               choices = c("Use uploaded creel data" = "upCreel", 
                                                           "Use map generated species list" = "mapList"))
                          # textOutput("textChoice")
                      )# close step one above 
                      
                  ),
                  
                  shinyjs::hidden(
                      wellPanel(
                          id = "above2",
                          style = "background-color: white;",
                          height="100%",
                          h3("Step 2: Select species to discuss"),
                          uiOutput("speciesSelectInput")
                      )#close above step 2
                      
                  ),
                  
                  shinyjs::hidden(
                      wellPanel(
                          id = "above3",
                          style = "background-color: white;",
                          height="100%",
                          h3("Step 3: View historical catch data and develop bag limit proposals")
                      )# close above step 3 
                  ),
                  shinyjs::hidden(
                      wellPanel(
                          id = "above4",
                          style = "background-color: white;",
                          height="100%",
                          h3("Step 4: Review and refine bag limit proposals")   
                      )#close above step 4
                  ),
                  shinyjs::hidden(
                      wellPanel(
                          id = "above5",
                          style = "background-color: white;",
                          height="100%",
                          h3("Step 5: Document your progress")
                      )# close above step 5
                  ),
                  div(
                      align="center",
                      actionButton(
                          inputId = "prevBtn",
                          label="Previous Step",
                          icon = icon('chevron-circle-left'), 
                          style="color:white;background-color: teal;border-color: white"
                      ),
                      actionButton(inputId="nextBtn",
                                   label = "Next Step", 
                                   icon = icon('chevron-circle-right'), 
                                   style="color:white;background-color: teal;border-color: white"), 
                      
                  ),
                  ########################################
                  #Below previous / next
                  ########################################
                  
                  
                  br(),
                  shinyjs::hidden(
                      wellPanel(
                          id = "below3",
                          style = "background-color: white;",
                          h1("Below 3")
                      )# close below step 3
                  ),
                  shinyjs::hidden(
                      wellPanel(
                          id = "below4",
                          style = "background-color: white;",
                          h3("Below 4")
                      )# close below step 4
                  )
              )
                    )# Close sidebar layout 
              ) # Close tab item
        )# Close tab items 
    )# Close dashboard body 
) # Close dashboard page 
) # Close Tag 