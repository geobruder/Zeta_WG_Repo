# deploy.R (optional) _
# Quick helper to deploy to shinyapps.io (requires rsconnect account setup)
# Usage: edit account info and run source("deploy.R")
if(!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
rsconnect::setAccountInfo(name='YOUR_NAME', token='YOUR_TOKEN', secret='YOUR_SECRET')
rsconnect::deployApp('shiny')
