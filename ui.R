# Guillaume Lobet - University of Liege
# This file contains the User Interface details for the Shiny app.


library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h1("- P-Simulator -")),
  
  # Sidebar with a Simulations outputs
  sidebarLayout(
    sidebarPanel(

      helpText("Guillaume Lobet, Hélène Adam, Thomas Harrop, Stéphane Jouannic."),
      
      tags$hr(),      
      
      # Total time of the simulation. So far, does not have any biological significance.
      sliderInput("time_in_sim", "Time in simulation:", min=0, max=15, value=4),
      
      # Maturation speed of the meristems. Can be seen as a proxi of cellular proliferation. 
      # Maturation follows an exponential rule (the more cell you have, the more you divide.)
      sliderInput("maturation", "Maturation speed:", min=1, max=5, value=1.1, step = 0.1),

      # The time during the rachis develops. During the time, primary branch are formed. 
      # When the time is passed, the rachis stops growing. At that time, the primary branches 
      # start developping (not before). REF HERE
      sliderInput("benchmark1", "Formation of the rachis:", min=0, max=10, value=4),
      
      
      # Saving the simulation results as a ricepr file, similar to the ones used by
      # P-TRAP (AL-Tam et al, 2013)
      textInput('dir', value = "~/Desktop/Rice/", label = "Were you want to save your network"),
      checkboxInput("save", label = "Save network as a .ricepr (XML) file", value = F),
      
      # Display options. The suer can choose how to color-code the 
      # simulated panicule.
      selectInput("display_group", label = "Display option", 
                  choices = list("Branch order" = 1, 
                                 "Branch ID" = 2,
                                 "Branch age" = 3,
                                 "Apexes" = 4
                                 ), selected = 1),
      
      # Display the panicule as a hierechical network
      checkboxInput("hierarchical", label = "Hierachical network", value = T),
      
      # Run the simulations
      actionButton(inputId = "runPSim", label="Unleash P-Simulator"),
      
      tags$hr(),
      
      # Just for fun, here we have a logo.
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