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
            phytomeres$canBranch[j] <- F
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
      
      
      message("------------------")
      
      # Select the display option
      phytomeres$group <- "SEG"
      phytomeres$group[phytomeres$apex] <- "APEX"
      if(input$display_group == 1) groups <- phytomeres$order
      if(input$display_group == 2) groups <- phytomeres$branch_id
      if(input$display_group == 3) groups <- phytomeres$age
      if(input$display_group == 4) groups <- phytomeres$group
      
      nodes <- data.frame(id = phytomeres$id, 
                          group=groups, 
                          title=paste(phytomeres$group, 
                                      "</br> Order = ", phytomeres$order,
                                      "</br> Branch = ", phytomeres$branch_id,
                                      "</br> Age = ", phytomeres$age))
      
      if(!input$hierarchical){
        network <- visNetwork(nodes, panicule, width = "100%", height="500", legend=T) %>% 
          visEdges(style = "arrow") %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
          visOptions(navigation = TRUE)
      }else{      
        network <- visNetwork(nodes, panicule, width = "100%",height="500", legend=T) %>% 
          visEdges(style = "arrow") %>%
          visHierarchicalLayout(layout = "direction") %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% 
          visOptions(navigation = TRUE)
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
    
    
    
    
    Results2 <- reactive({
      
      if(input$runPSim == 0){return()}

      # Threshold values for the capacity to branch and elongate
      branch_thrshld <- 10
      elong_thrshld <- 5
      
      # Table contaning the building of the nodes over time
      phytomeres <- data.frame(id=1, 
                               age=1, 
                               parent=-1, 
                               floral=F, 
                               time=1, 
                               canBranch=F, 
                               canElongate=T, 
                               order=0, 
                               maturity=input$maturation, 
                               branched=F, 
                               elongated=F,
                               apex = T)
      
      panicule <-  data.frame(from=numeric(), 
                              to=numeric(), 
                              order = numeric())
         
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
          phytomeres$canBranch[j] <- (phytomeres$maturity[j] >= branch_thrshld && !phytomeres$branched[j])
          phytomeres$canElongate[j] <- (phytomeres$maturity[j] >= elong_thrshld && !phytomeres$elongated[j])    
          
          message(paste(i, ":", j, " = branched:", phytomeres$branched[j], 
                        " / canBranch", phytomeres$canBranch[j],
                        " / maturation", phytomeres$maturity[j]))
          
          if(i > input$benchmark1 & phytomeres$order[j] == 0){
                phytomeres$canBranch[j] <- F
                phytomeres$canElongate[j] <- F
          } 
          if(i <= input$benchmark1 & phytomeres$order[j] > 0){
            phytomeres$canBranch[j] <- F
            phytomeres$canElongate[j] <- F
          }
          
          
          # If the phytomere can elongate, it can create a new phytomere behind him 
          if(phytomeres$canElongate[j]){          
            new_id = max(phytomeres$id)+1
            
            new_phyto <- data.frame(
              id=new_id, 
              age=1, 
              parent=phytomeres$id[j], 
              floral=F, 
              time=i, 
              canBranch=T, 
              canElongate=T, 
              order=phytomeres$order[j], 
              maturity=input$maturation, 
              branched=F, 
              elongated=F)
            
            new_pan <- data.frame(
              from=phytomeres$id[j], 
              to=new_id,
              order = phytomeres$order[j])
            
            # If the phytomere has been elongated it cannot elongate anymore NEED TO BE CHANGED
            phytomeres$elongated[j] = T       
            
            panicule <- rbind(panicule, new_pan)
            phytomeres <- rbind(phytomeres, new_phyto)
          }
          
          # Branching process. Is triggered only if the ode has the capacity to branch
          if(phytomeres$canBranch[j]){
            
            # Create a new identifier (increment from the exiting ones)
            new_id = max(phytomeres$id)+1
            
            # Create a new phytomeres
            new_phyto <- data.frame(
              id=new_id, 
              age=1, 
              parent=phytomeres$id[j], 
              floral=F, 
              time=i, 
              canBranch=F, 
              canElongate=F, 
              order=phytomeres$order[j]+1, 
              maturity=input$maturation, 
              branched=F, 
              elongated=F)
            
            # Create a new connection
            new_pan <- data.frame(
              from=phytomeres$id[j], 
              to=new_id, 
              order = phytomeres$order[j])

            # If a phytomeresmer is bracnhed, it cannot branch anymore
            phytomeres$branched[j] <- T
            
            # Update the paniculeicule and phytomeresmere tables
            panicule <- rbind(panicule, new_pan)
            phytomeres <- rbind(phytomeres, new_phyto)
          }
        }
        
    
      }
      

      message("------------------")
     
      groups <- phytomeres$order
      if(input$group_age) groups <- phytomeres$age
      nodes <- data.frame(id = phytomeres$id, 
                          group=groups, 
                          title=paste(phytomeres$id, ": ", phytomeres$canBranch, " / ", phytomeres$branched, " / ", phytomeres$maturity))
      
      edges <- data.frame(from = panicule$from, to = panicule$to)
      
      if(!input$hierarchical){
        network <<- visNetwork(nodes, edges, width = "100%", legend=T) %>% 
          visEdges(style = "arrow") %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
      }else{      
        network <<- visNetwork(nodes, edges, width = "100%", legend=T) %>% 
          visEdges(style = "arrow") %>%
          visHierarchicalLayout(layout = "direction") %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
      }
      
      return(network);
      
    })
  
    
    # Plot the different growth factors
    output$visnetwork <- renderVisNetwork({
      if(input$runPSim== 0){return()}
      Results()
    })
       
    
  }
)