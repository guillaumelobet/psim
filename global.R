
# Guillaume Lobet - University of Liege


# Global libraries
  packages <- c("visNetwork")

  for(p in packages){
    if (!require(p,character.only = TRUE))
    {
      install.packages(p,dep=TRUE)
      if(!require(p,character.only = TRUE)) stop("Package not found")
    }
  }
