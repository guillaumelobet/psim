# Guillaume Lobet - University of Liege

shinyServer(
  
  function(input, output) {  
    
    #------------------------------------------------------
    # PROCESS THE DATA
    
 
    Results <- reactive({
      
      if(input$runPSim == 0){return()}
      
      # Threshold values for the capacity to branch and elongate
      branch_thrshld <- 10
      elong_thrshld <- 5
      
      # Table contaning the building of the nodes over time
      phytomeres <- data.frame(id = 1, 
                               branch_id = 1,
                               branch_id_sec = 0,
                               age = 1, 
                               parent = -1, 
                               generator = -1,
                               floral = F, 
                               time = 1, 
                               canBranch = F, 
                               canElongate = T, 
                               order = 0, 
                               maturity = input$maturation, 
                               branched = F, 
                               apex = T     # Apexes have the capacity to elongate
                               )
      
      panicule <-  data.frame(from=numeric(), 
                              to=numeric(), 
                              branch_id = numeric())
      
      # Loop over the simulation time
      for(i in 1:input$time_in_sim){
        
        # Get the total number of existing nodes
        n_nodes <- nrow(phytomeres)
        
        # Loop over the existing phytomeres
        for(j in 1:n_nodes){
          
          # Udate the different variables
          phytomeres$age[j] <- phytomeres$age[j]+1 
          phytomeres$maturity[j] <- phytomeres$maturity[j] * input$maturation    
          
          # Update the branching and elongation capabilities of the phytomere
          phytomeres$canBranch[j] <- (phytomeres$maturity[j] >= branch_thrshld && !phytomeres$branched[j] && !phytomeres$apex[j])
          phytomeres$canElongate[j] <- (phytomeres$maturity[j] >= elong_thrshld && phytomeres$apex[j])    # Can elongate if an apex and old enough
          
          
          # Are we in the develpmental phase of the rachis. If yes, only the rachis can develop.
          # If not ,the rachis stops developing and the branches take over.
          if(i > input$benchmark1 & phytomeres$order[j] == 0){
            #phytomeres$canBranch[j] <- F
            phytomeres$canElongate[j] <- F
          } 
          if(i <= input$benchmark1 & phytomeres$order[j] > 0){
            phytomeres$canBranch[j] <- F
            phytomeres$canElongate[j] <- F
          }
          
          
          # If the phytomere can elongate, it can create a new phytomere behind him 
          if(phytomeres$canElongate[j]){ 
            message(paste('time ', i,': ',j, " ELONGATE"))
            
            phytomeres$maturity[j] = input$maturation
            
            new_id = max(phytomeres$id)+1
            
            # update previosuly connected node (apical growth)
            new_parent <- phytomeres$parent[j]
            phytomeres$parent[j] <- new_id
            
            new_phyto <- data.frame(
              id = new_id, 
              branch_id = phytomeres$branch_id[j],
              branch_id_sec = 0,
              age = 1, 
              parent = new_parent, 
              generator = phytomeres$id[j], 
              floral = F, 
              time = i, 
              canBranch = T, 
              canElongate = T, 
              order = phytomeres$order[j], 
              maturity = input$maturation, 
              branched = F, 
              apex = F)
            
            phytomeres <- rbind(phytomeres, new_phyto)            
            
  
          }
          
          # Branching process. Is triggered only if the ode has the capacity to branch
          if(phytomeres$canBranch[j]){
            message(paste('time ', i,': ',j, " BRANCH"))
            phytomeres$maturity[j] = input$maturation
            
            
            # Create a new identifier (increment from the exiting ones)
            new_id = max(phytomeres$id)+1
            new_branch_id = max(phytomeres$branch_id)+1
            
            # Allow the phytomere to have a second ranch id (for the connections)
            phytomeres$branch_id_sec[j] <- new_branch_id           
            
            # Create a new phytomeres
            new_phyto <- data.frame(
              id = new_id, 
              branch_id = new_branch_id,
              branch_id_sec = 0,
              age = 1, 
              parent = phytomeres$id[j], 
              generator = phytomeres$id[j], 
              floral = F, 
              time = i, 
              canBranch = F, 
              canElongate = F, 
              order = phytomeres$order[j]+1, 
              maturity = input$maturation, 
              branched = F, 
              apex = T)
            
            # If a phytomeresmer is bracnhed, it cannot branch anymore
            phytomeres$branched[j] <- T
            
            # Update the paniculeicule and phytomeresmere tables
            phytomeres <- rbind(phytomeres, new_phyto)
          }
        }
      }
      
      # Create a table containing the connections
      # This is needed for the display of the network
      for(j in 1:nrow(phytomeres)){
        
        new_pan <- data.frame(
          from = phytomeres$parent[j],
          to = phytomeres$id[j], 
          branch_id = phytomeres$branch_id[j])
        
        panicule <- rbind(panicule, new_pan) 
        
      }
      # Create a table containing the connections
      # This is needed for the display of the network
      phytomeres$spike <- "spikelet"      
      for(j in 1:nrow(phytomeres)){
        if(nrow(panicule[panicule$from == phytomeres$id[j],]) == 2) phytomeres$spike[j] <- "branch"
      }
      
      message("------------------")
      
      # Select the display option
      phytomeres$group <- "SEG"
      phytomeres$group[phytomeres$apex] <- "APEX"
      if(input$display_group == 1) groups <- phytomeres$order
      if(input$display_group == 2) groups <- phytomeres$branch_id
      if(input$display_group == 3) groups <- phytomeres$age
      if(input$display_group == 4) groups <- phytomeres$group      
      if(input$display_group == 5) groups <- phytomeres$spike
      
      n_branch <<- length(unique(phytomeres$branch_id))
      n_spike <<- nrow(phytomeres[phytomeres$spike == "spikelet",])
      
      link <<- panicule
      nodes <<- data.frame(id = phytomeres$id, 
                          group=groups, 
                          title=paste(phytomeres$group, 
                                      "</br> Order = ", phytomeres$order,
                                      "</br> Branch = ", phytomeres$branch_id,
                                      "</br> Age = ", phytomeres$age))
      
      if(!input$hierarchical){
        network <- visNetwork(nodes, link, width = "100%", height="500", legend=T) %>% 
          visEdges(arrow = 'to') %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
          visInteraction(navigationButtons = TRUE)
      }else{      
        network <- visNetwork(nodes, panicule, width = "100%",height="500", legend=T) %>% 
          visEdges(arrow = 'to') %>%
          visHierarchicalLayout() %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
          visInteraction(navigationButtons = TRUE)
      }
      
      
      if(input$save){
        # Export the structure to a .ricepr file
        ricepr <- '<?xml version="1.0" encoding="UTF-8"?>'
        ricepr <- paste(ricepr, "<!--Rice Panicel Structure File", sep='\n')
        ricepr <- paste(ricepr, "-The xy-origin is the left top point in the screen", sep='\n')
        ricepr <- paste(ricepr, "-The graph is directed and was generated using P-Simulator", sep='\n')
        ricepr <- paste(ricepr, "-The left most 'generating' vertex is the root of the tree-->", sep='\n')
        ricepr <- paste(ricepr, '<result    signature="STRUCTURE"    imagepath="none">', sep='\n')
        ricepr <- paste(ricepr, '<graph>', sep='\n')
        ricepr <- paste(ricepr, '   <vertices>\n', sep='\n')
        for(i in 1:nrow(phytomeres)) ricepr <- paste(ricepr, '        <vertex id="',phytomeres$id[i],'" x="0" y="0" type="Fly" fixed="true"/>\n', sep='')      
        ricepr <- paste(ricepr, '   </vertices>', sep='\n')
        ricepr <- paste(ricepr, '   <edges>\n', sep='\n')
        for(i in 1:nrow(phytomeres)) ricepr <- paste(ricepr, '        <edge vertex1="',phytomeres$parent[i],'"  vertex2="',phytomeres$id[i],'" />\n', sep='')            
        ricepr <- paste(ricepr, '   </edges>', sep='\n')      
        ricepr <- paste(ricepr, '</graph>', sep='\n')
         
        
        write(ricepr, file = paste(input$dir, "/", "psim_time",input$time_in_sim, "_maturation", input$maturation, "_thrsld", input$benchmark1,".ricepr", sep=""))
      }
      
      
      return(network);
      
    })
    
    # Plot the different growth factors
    output$visnetwork <- renderVisNetwork({
      if(input$runPSim== 0){return()}
      Results()
    })
       
    
    
    #------------------------------------------------------
    ## Output a warning message
    output$caption1 <- renderUI( {
      if(input$runPSim== 0){return()}
      
      message  <- ""
      message <- paste(message, "<b>Number of branches</b> = ",n_branch)
      message <- paste(message, "</br><b>Number of spikelets</b> = ",n_spike)
      HTML(message)

    })    
    
    
    
  }
)