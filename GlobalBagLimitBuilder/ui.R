
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
    dashboardHeader(title = "Bag Limit Builder"),
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
                        h1("What is a Bag Limit Builder?"),
                        h3("This tool is designed to allow users to simply and easily design and document draft bag limit proposals"),
                        br(),
                        h2("Getting started"),
                        h4("First, visit the 'Create Species List' tab. Here you can utilize one of three methods of creating a species list: manual entry, by uploading creel data, or my using a map to filter your species selection. After utilizing one of these methods, then move on to the 'Builder' tab."),
                        h2("Step 1: Select a method"),
                        h4("Here, select the method you used to create the species list."),
                        h2("Step 2: Select species to discuss and view historical catch data if you uploaded any"),
                        h4("Here you should refine your species list to those you'd like to discuss. You can revisit this at any point to continue to refine and change this grouping before creating a bag limit proposal."),
                        icon("fish", "fa-6x"), icon("fish", "fa-6x"),icon("fish", "fa-6x"),
                        br(),
                        br(),
                        h2("Step 3: Develop bag limit proposals"),
                        h4("Here you have the opportunity to draft bag limit proposals. This can be based on the data visualizations available if you have creel data, or can be based on discussion and other materials"), 
                        br(), 
                        icon("chart-area", "fa-8x"),
                        br(), 
                        h4("You will have an option to further subset species from your original selection, to reflect insights gained from the data and group discussion using the dropdown. Then you can describe proposed bag limits. This can be as specific as proposed limits for the group (i.e. Species X, Y, & Z at a bag limit of 10), or be more vague such as simply stating the group is of high cultural value to the community and should be treated with particular attention (i.e. Manini should be protected). Then you will be prompted to name the proposal. We recommend this name be easily identifiable and concise. Finally, when you are satisfied with your selection, press 'Save Proposal' in order to move the draft to your saved proposals. After the proposal has been saved, you are free to clear all current selections and select a new grouping."),
                        br(), 
                        icon("edit", "fa-8x"),
                        br(),
                        br(),
                        h2("Step 4: Evaluate your bag limit proposals"),
                        h4("Here you will see all of your saved proposals that are ready to be evaluated. Through polling or group discussion, you can then adjust the decision criteria to reflect the strengths and weaknesses of the proposal. If it is decided that a proposal is not meeting objectives or needs to be re-evaluated completely, you can remove the proposal and it will be erased."),
                        br(),
                        icon("balance-scale-left", "fa-8x"),
                        h2("Step 5: Document your progress"),
                        h4(
                            "Once you have drafted and evaluated as many proposals as you would like (or if you are finished with your session) you can save your progress by clicking the 'Download report' button. This will export a report summarizing all of the drafted proposals and their performance."),
                        h4("At any time you can also save your session progress on the 'Save & Restore' tab. Here you can save the current session to return to at a later date with the 'Save current session' and 'Load previous session' buttons."),
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
                                    "Here you have the opportunity to upload data and undersdand the potential impacts of different bag limit policies", 
                                    "The data should contain two columns: category and take. Each row in the data must represent a fishing event. The category then subsets those events by the species of fish taken, the gear with which it was caught, or another form of species group which could be fish families, trophic levels, etc. For examples of what this might look like, view example data sets below.", 
                                    bsButton(inputId = "q1", 
                                             label = "", 
                                             icon = icon("info-circle", "fa-2x"),
                                             disabled = FALSE,
                                             size = "extra-small",
                                             style = "default",
                                             class = 'pull-right'),
                                    bsPopover (id = "q1",
                                               title = "File upload",
                                               content =  HTML(paste(
                                                   "Double check to make sure data has been uploaded correctly. The file must be a CSV with only two columns. Column one should be the category of your choice. Ensure that this category matches the selected category from the drop down. The second column should be take in number of fish.")),
                                               placement = "left", 
                                               trigger = "focus", 
                                               options = list(container = "body")
                                    ),
                                    fileInput(
                                        inputId = "customCreelData", 
                                        label = "Upload a CSV file", 
                                        accept = c(
                                            'text/csv',
                                            'text/comma-separated-values',
                                            '.csv')),
                                    
                                    selectInput(inputId = "indicateCat", 
                                                label = "Indicate grouping category for creel data:", 
                                                choices = c("Gear",
                                                            "Species",
                                                            "Species group")),
                                    selectInput(inputId="exampData",
                                                label="Example data",
                                                choices=c("Creel multi species data" = "creel_multi_species",
                                                          "Creel single species data" = "creel_sin_species",
                                                          "Creel gear type data"= "creel_gear", 
                                                          "Creel species groups data"  ="creel_groups")),
                                    DTOutput("creelDT")
                                    ), 
                           tabPanel("Manual Species List", 
                                    textInput(inputId = "category1", 
                                              label = "Enter a category", 
                                              placeholder = "Category name",
                                              width="100%"), 
                                    actionButton(inputId = "addCategory", 
                                                 label = "Add Category"),
                                    br(), 
                                    br(), 
                                    tags$div(id = "catTable")))
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
                          h2("Step 1: Select a method of creating your species list"),
                          "Here you can choose if you'd like to upload creel data, or if you'd like to create a species list to discuss using the interactive map. If you choose to upload data, there will be visualization options that can help to make informed decisions. If you do not have data, using the interactive map is a great way to generate and capture discussion around bag limits for species in your area.",
                          br(),
                          radioGroupButtons(inputId = "speciesListMethod", 
                                               label = "Select method", 
                                               choices = c("Use uploaded creel data" = "Creel", 
                                                           "Use map generated species list" = "Map", 
                                                           "Use manually created list" = "Manual"), 
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
                          uiOutput("speciesSelectInput"),
                          actionButton(inputId = "test", 
                                       label = "test")
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