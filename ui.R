library(DT)
library(shiny)
library(shinydashboard)
library(leaflet)
library(shinyWidgets)

intro_str =  "Marketing has always been an industry that is heavily data related. Big firms spent millions of dollars every year on analysising their marketing data, in terms of 
finding insights and make their marketing investment wisely. Because of my marketing background, discovering insights."

intrdata_str = 'Basic data of companies: identifiers, headcount, sales trend, fields of activity.\n
Crefoport Rating: A risk score on a scale of 100 to 600, including a failure probability.
Download report: Export all company data for a particular company in pdf, xml or html format.
Owners and managers: active and deleted managers / owners registration information.
Events: Negative or positive events in the company bulletin, NAV page and other sources.
Relationship Graph: Visualization of managerial and ownership relationships.
Financial reports: financial data of the last 4 years next to each other and original documents (reports,  appendices) up to 2010.
Banking Relationships, Auditors: Details of current bank accounts and auditors.
Company Statement / Company History: Company information in the Company Statement / Company History data structure used by the Court of Registration.
Partner monitoring, client list upload: Possibility of uploading partner lists, whereby the user is continuously informed about the changes he has set up, and he can follow them through the online interface.
Marketing Database: Export lists of companies with sales-specific data after setting targeted filtering criteria. Examples of such information are the company name, contact details, and the names of the signatories.
Transfer Pricing Database: Filter and export a company database based on a transfer price-specific criteria system.'

compgroups = c("Aqua Holding","Statistic Capital Group","Finance Capitals","Securities Group")
industry   = c("DataScience","Buiding","Food","Engineering","Chemicals","Energy")
compname   = c("Sky PROG","Avalon","Unicorn LLC","Golden Globe")
opyear     = c(2010:2020)
visualtype = c("Plan","Plan-Now","Facts","Past")

shinyUI(dashboardPage(
    skin = "black",
    dashboardHeader(title = "Talentis Project",
                    dropdownMenu(type = "tasks", badgeStatus = "success",
                                 taskItem(value = 90, color = "green",
                                          "Documentation"),
                                 taskItem(value = 17, color = "aqua",
                                          "Project X"),
                                 taskItem(value = 75, color = "yellow",
                                          "Server deployment"),
                                 taskItem(value = 80, color = "red",
                                          "Overall project")
                    )                
                    ),
    dashboardSidebar(
        
        sidebarUserPanel("",image = "https://www.rstudio.com/assets/img/logo.svg"),

        sidebarMenu(
            
            menuItem("Introduction",tabName = "intr",  icon = icon("align-justify")),
            menuItem("Map",         tabName = "map",   icon = icon("map")     ),
            menuItem("Geograph",  tabName = "geo",   icon = icon("globe")),
            menuItem("Geographic", tabName = "geo2",  icon = icon("globe")),
            menuItem("Dashboard",   tabName = "dash",  icon = icon("dashboard")),
            menuItem("Summary",     tabName = "summ",  icon = icon("balance-scale")),
            menuItem("Layout One",  tabName = "sugg",  icon = icon("fist-raised")),
            menuItem("Layout Two",  tabName = "sugg2", icon = icon("fist-raised")),
            menuItem("Data",        tabName = "data",  icon = icon("database"))
            
        ),
        selectizeInput("selected",
                       "Select Item to Display",
                       choice)
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            # Tab item map
            tabItem(tabName = "map",
                    fluidRow(infoBoxOutput("maxBox"),
                             infoBoxOutput("minBox"),
                             infoBoxOutput("avgBox")),
                    fluidRow(box(htmlOutput("map"),  height = 300),
                             box(htmlOutput("hist"), height = 300))

                    
                    ),
            tabItem(tabName = "intr",
                    fluidRow(column(
                        width = 12,
                        box(title = "Introduction",
                            solidHeader = T,
                            width = NULL,
                            status = "danger",
                            id = "intro",
                            tags$h1("About The Project"),
                            tags$h3("Market analysis"),
                            tags$h4(intro_str),
                            tags$h2("The Dataset"),
                            tags$h4(intrdata_str),
                            tags$img(src = "ceginfo.jpg",width = 1000, height = 220)
                        )
                        
                    ))),
            
            # Dashboard
            tabItem(tabName = "dash", h2("Aggregated Company Content"),
                    fluidRow(infoBoxOutput("aggRes"   ),
                             infoBoxOutput("compCount"),
                             infoBoxOutput("compInd"  )),
                    fluidRow(box(title= "Search By: ",h5("Here you can see"),
                                 setSliderColor("red",c(1)),
                                 sliderInput("incSlide","Income Range",0,100,50 ),
                                 textInput("text", "Text input:"),
                                 status="danger"
                                 ),
                             box(title = "Statistics", status="danger",
                                 htmlOutput("map2")),)
                    ),
            
            tabItem(tabName = "geo", h3("Press the button 'New Points' to place companies on a map"),
                    leafletOutput("mymap"),
                    p(),
                    actionButton("recalc", "New points")
                    ),
            tabItem(tabName = "geo2",h2("Anothr Leaflet by address"),
                    leafletOutput("mymap2",width = "100%"),
                    absolutePanel(top = 120, right = 20,
                                  sliderInput("range", "Ranges", min(quakes$mag), max(quakes$mag),
                                              value = range(quakes$mag), step = 0.1
                                  ),
                                  selectInput("colors", "Color Scheme",
                                              rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                  ),
                                  checkboxInput("legend", "Show legend", TRUE)
                    )),
            
            tabItem(tabName = "summ", h3("Summary"),
                    fluidRow(
                        # A static valueBox
                        valueBox(10 * 2, "New Orders", icon = icon("credit-card"),color="red"),
                        
                        # Dynamic valueBoxes
                        valueBoxOutput("progressBox"),
                        
                        valueBoxOutput("approvalBox")
                    ),
                    fluidRow(
                        # Clicking this will increment the progress amount
                        box(width = 4, actionButton("bcount", "Increment progress"))
                    )),
            tabItem(tabName = "sugg",
                    fluidRow(
                        column(width = 3,
                               box(width = NULL,selectizeInput("select1","Company Group ", compgroups),status="primary"),
                               box(width = NULL,selectizeInput("select2","Industry Section ", industry),status="primary"),
                               box(width = NULL,selectizeInput("select3","Company ", compname),status="primary"),
                               box(width = NULL,selectizeInput("select4","Year ", opyear),status="primary"),
                               box(width = NULL,selectizeInput("select5","Visual Type ", visualtype),status="primary")
                                                             ),
                        column(width = 9,
                               fluidRow(
                                   
                                   valueBox(paste("$",1266), "Earnings", icon = icon("calendar-plus"),color="light-blue", width = 3),
                                   valueBox(paste("$",5388), "Spending", icon = icon("calendar-minus"),color="light-blue", width = 3),
                                   valueBox(paste("$",9077), "Capital", icon = icon("coins"),color="light-blue", width = 3),
                                   valueBox(paste("$",533), "Reimbursments", icon = icon("first-aid"),color="light-blue", width = 3),

                                   ),
                               fluidRow(box(title = "Year Chart",width = 12,height = 220,htmlOutput("hist3"),status="primary") ),
                               fluidRow(box(title = "Values In Dollars",width = 6,htmlOutput("dfbarh"),height = 250,status="primary"),
                                        box(title = "Section Fill",width = 6,height = 250,status="primary")),
                               )
                    )),
            tabItem(tabName = "sugg2",
                    fluidRow(
                        column(width = 3,
                               box(width = NULL,selectizeInput("select1","Company Group ", compgroups),status="primary"),
                               box(width = NULL,selectizeInput("select2","Industry Section ", industry),status="primary"),
                               box(width = NULL,selectizeInput("select3","Company ", compname),status="primary"),
                               box(width = NULL,selectizeInput("select4","Year ", opyear),status="primary"),
                               box(width = NULL,selectizeInput("select5","Visual Type ", visualtype),status="primary")
                        ),
                        column(width = 9,
                               fluidRow(
                                   
                                   valueBox(paste("$", 6353), "Earnings", icon = icon("calendar-plus"),color="light-blue", width = 3),
                                   valueBox(paste("$", 876), "Spending", icon = icon("calendar-minus"),color="light-blue", width = 3),
                                   valueBox(paste("$", 4222), "Capital", icon = icon("coins"),color="light-blue", width = 3),
                                   valueBox(paste("$", 2194), "Reimbursments", icon = icon("first-aid"),color="light-blue", width = 3),

                               ),
                               fluidRow(box(title = "Year Chart",width = 12,htmlOutput("hist2"))),
                               fluidRow(box(title = "Value Forecast",width = 4,height = 40,background = "orange",status="primary"),
                                        box(title = "Financial Control",width = 8,height = 40,background = "orange",status="primary")),
                               fluidRow(box(title = "Section Dodge",width = 4,height = 210,status="primary"),
                                        box(title = "Section Fill",width = 8,height = 210,status="primary")),
                        )
                    )),

            tabItem(tabName = "data",
                    fluidRow(box(DT::dataTableOutput("table"), status="primary",width = 12)))
        )
    )
))