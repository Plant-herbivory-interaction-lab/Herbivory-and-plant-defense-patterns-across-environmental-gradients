library(rgee)

#These commented commands below need be ran when running this on a different computer.
#See this github page for documentation https://github.com/r-spatial/rgee

# They recommend to use other commands, 
# but I find that the reticulate package works well with python environments.

#reticulate::install_python() # make sure python is installed
#reticulate::virtualenv_create("rgee") #create a virtual python environment for rgee to use
#reticulate::use_virtualenv("rgee")
#reticulate::py_install("numpy","rgee","earthengine-api") # install some python modules

#ee_install_upgrade() 
#ee_Authenticate()

reticulate::use_virtualenv("rgee")
ee_Initialize() 
