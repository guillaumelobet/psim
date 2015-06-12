# Guillaume Lobet - University of Liege

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("- P-Simulator -")),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      #h4("Model Assisted Root Image Analysis"),
      helpText("Guillaume Lobet, Hélène Adam, Stéphane Jouannic."),
      
      tags$hr(),      
      
      sliderInput("time_in_sim", "Time in simulation:", min=0, max=10, value=4),
      
      sliderInput("maturation", "Maturation speed:", min=1, max=3, value=1.1, step = 0.1),
      #sliderInput("maturation", "Maturation speed:", min=0, max=1, value=0.1, step = 0.1),
      
      sliderInput("benchmark1", "Formation of the rachis:", min=0, max=10, value=4),
      
      
      textInput('dir', value = "~/Desktop/Rice/", label = "Were you want to save your network"),
      checkboxInput("save", label = "Save network as a .ricepr (XML) file", value = F),
      
      #sliderInput("range", "Range:",min = 1, max = 100, value = c(20,50)),
      
      
      selectInput("display_group", label = "Display option", 
                  choices = list("Branch order" = 1, 
                                 "Branch ID" = 2,
                                 "Branch age" = 3,
                                 "Apexes" = 4
                                 ), selected = 1),
      
      checkboxInput("hierarchical", label = "Hierachical network", value = T),
      
      actionButton(inputId = "runPSim", label="Unleash P-Simulator"),

      
      
      tags$hr(),
      
      img(src = "ptrap.png", width = 200)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(     
        tabPanel("Panicule network",
                 visNetworkOutput("visnetwork"),
                value=1
        ),
        id="tabs1"
      )
    )
  )
))