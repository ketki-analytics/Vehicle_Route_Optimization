####################################################################################################
## Setting up the Working Directory
setwd('C:/Users/somsubhra.g/Google Drive/eclipse/routeOptimization_R')
## Data File - Contains Address,Pincode,City,State, LBH and Slot Details
fileName = "BHI_05_04_2016.xlsx"
## GitHub Directory
githubDir = "https://raw.githubusercontent.com/somsubhra88/Vehicle_Route_Optimization/MultipleCuts/"
####################################################################################################
## Hub Lat Lng
# hubLatLng = c(77.6533668, 12.8852659) #Kudlu
# hubLatLng = c(77.315002, 28.714921, ) #Mandoli
# hubLatLng = c(88.317647, 22.739977 )  #Kolkata
hubLatLng = c(73.034974, 19.237481)   #Bhiwandi
####################################################################################################
# Shift start timing for the Vans
# shiftStart = c(750,1350) # For Kolkata, Kudlu
shiftStart = c(900,1300) # for Bhiwandi
# Average Speed for different Location
avgSpeed = 15 # For Kudlu, Bhiwandi, Mandoli
# avgSpeed = 20 # For Kolkata Only
####################################################################################################
## If you have proir knowledge that these Orders do not require OFD, then put the Tracking IDs here
notDeliveredOID = NULL
unDel = 0
####################################################################################################
source(paste(githubDir,'dataProcessing.R',sep=""))