

dashboardPage(
    dashboardHeader(title = "Global Bag Limit Builder"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName ="about"), 
            menuItem("Builder", tabName = "builder")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "about", 
                    fluidRow()), 
            tabItem(tabName = "builder")
        )
    )
)