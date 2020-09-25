############################################################
## Define packages to be loaded, and if needed, installed ##
##                                                        ##    
## by robert lohne (rlohne@gmail.com / @robertlohne)      ##
############################################################

# 1 Definitions
# 1.1 Package names 
# Define the names of the packages to be used, these are stored in the packages variable, and loaded/installed as needed
packages <- c("")

# 1.1 Packages description
# 

# 2. Install packages not yet installed
# Installed packages are stored in the installed packages variable
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# 3. Packages loading
invisible(lapply(packages, library, character.only = TRUE))

