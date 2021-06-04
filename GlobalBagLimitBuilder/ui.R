
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
                        h2("Step 1: Select a method"),
                        h4("Before using the app, you first must create your species list using one of two methods. If you have data containing species and the frequenct of take, you can upload it to view data visualizations that can aid in your decision making. If you do not have data, you can create a species list through our interactive map, by selecting the country in which you're opperating and from there narrowing to the species of interest."),
                        h2("Step 2: Select species to include"),
                        h4("Develop your species list! Here you will have the opportunity to select the specific species you would like to discuss. You can revisit this at any point to continue to refine and change this grouping before creating a bag limit proposal."),
                        icon("fish", "fa-6x"), icon("fish", "fa-6x"),icon("fish", "fa-6x"),
                        br(),
                        br(),
                        h2("Step 3: View historical catch data and develop bag limit proposals"),
                        h4("Here you have the opportunity to draft bag limit proposals. This can be based on the data visualizations available if you have creel data, or can be based on discussion and other materials"), 
                        br(), 
                        icon("chart-area", "fa-8x"),
                        br(), 
                        h4("You will have an option to further subset species from your original selection, to reflect insights gained from the data and group discussion using the dropdown. Then you can describe proposed bag limits. This can be as specific as proposed limits for the group (i.e. Species X, Y, & Z at a bag limit of 10), or be more vague such as simply stating the group is of high cultural value to the community and should be treated with particular attention (i.e. Manini should be protected). Then you will be prompted to name the proposal. We recommend this name be easily identifiable and concise. Finally, when you are satisfied with your selection, press 'Save Proposal' in order to move the draft to your saved proposals. After the proposal has been saved, you are free to clear all current selections and select a new grouping."
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
                                    icon("check-circle", "fa-1x"), "Method Selected:", 
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
                          h2("Select a method of creating your species list"),
                          "Here you can choose if you'd like to upload creel data, or if you'd like to create a species list to discuss using the interactive map. If you choose to upload data, there will be visualization options that can help to make informed decisions. If you do not have data, using the interactive map is a great way to generate and capture discussion around bag limits for species in your area.",
                          br(),
                          radioGroupButtons(inputId = "speciesListMethod", 
                                               label = "Select method", 
                                               choices = c("Use uploaded creel data" = "Creel", 
                                                           "Use map generated species list" = "Map"), 
                                            selected = FALSE)
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
                          h3("Step 3: View historical catch data and develop bag limit proposals"), 
                          uiOutput("speciesInputBag"),
                          textAreaInput("customBagText", "Describe proposed bag regulations", 
                                        placeholder = "Ex) Bag limit of ten surgeon fish, no more than 2 Kole, and one Kala"),
                          textInput(
                              inputId = "saveBagProposal_name",
                              label = "Name your proposal:",
                              placeholder = "Surgeonfish bag", 
                              width = '50%'
                          ),
                          actionButton(
                              inputId = "saveBagProposal",
                              label = "Save proposal",
                              class="btn btn-primary",
                              icon = icon('paper-plane'),
                              width = '50%'
                          )
                      )
                  ),
                  shinyjs::hidden(
                      wellPanel(
                          id = "above4",
                          style = "background-color: white;",
                          height="100%",
                          h3("Step 4: Review and refine bag limit proposals"), 
                          h5("Below are the draft proposals. Here you can evaluate the proposals, and finalize them or remove them."),
                          br(),
                          DT::DTOutput("bag_proposals_table", height="300px"),
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
                          h1("Historical take patterns for selected species"),
                          numericInput(inputId = "sampleBag",
                                       label = h5("Your proposed bag limit:"),
                                       value = 10, 
                                       min = 0, 
                                       max = 50
                          ),
                          br(),
                          textOutput("pct_take"), 
                          br(), 
                          plotOutput(outputId = "percentDecrease"), 
                          br(), 
                          plotOutput(outputId = "speciesRidgeline")
                      )# close below step 3
                  ),
                  shinyjs::hidden(
                      wellPanel(
                          id = "below4",
                          style = "background-color: white;",
                          h3("Below 4"), 
                          tags$div(id = "proposalList_Table")
                      )# close below step 4
                  )
              )
                    )# Close sidebar layout 
              ) # Close tab item
        )# Close tab items 
    )# Close dashboard body 
) # Close dashboard page 
) # Close Tag 