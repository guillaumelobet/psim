
# Guillaume Lobet - University of Liege


# Global libraries
  
  if (!require("visNetwork",character.only = TRUE)){
    devtools::install_git('https://github.com/dataknowledge/visNetwork')
  }
  
  packages <- c()
  for(p in packages){
    if (!require(p,character.only = TRUE)){
      install.packages(p,dep=TRUE)
      if(!require(p,character.only = TRUE))stop("Package not found")
    }
  }
