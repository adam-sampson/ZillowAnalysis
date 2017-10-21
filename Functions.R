## Load libraries or ask to install them
loadLibraries <- function(requiredPackages = NULL) {
  for(pack in requiredPackages) {
    # If statement will either load the library or give user the option to auto-install if not available
    if(!require(pack,character.only = TRUE)) {
      # ask the user for permission to install
      if(menu(c('yes','no'),title=paste0('Package ',pack,' not found. Would you like to install?'))==1) {
        install.packages(pack)
        library(pack)
      }
    }
  }
}

