library(shiny) #load the shiny package


#Define UI for the Shiny application here
shinyUI(fluidPage( # tells shiny this is the ui, fluid makes it look good regardless of browser
  
  #Application title
  titlePanel(title = "SSR Matcher"),
  tags$head(tags$style(".shiny-notification {position: fixed; top: 40% ;left: 50%; font-size: large;")),
  sidebarLayout( #where user input will be collected
    #Sidebar Panel
    sidebarPanel(("Enter your Microsatellite Data"),
                 fileInput("SSRinput", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
                 tags$hr()
                 ),
    
    
    #Main Panel 
    mainPanel(("Results"), #where results will be displayed
              tableOutput("closestmatches"),
              tableOutput("detail"),
              textOutput("no_match", container = pre)
    
  ))
))
