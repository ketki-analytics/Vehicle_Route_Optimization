writeLines('---------Optimization Code---------')
load(paste('optimizationData ',unlist(strsplit(fileName,"\\."))[1],'.RData',sep=""))
library(dplyr)
source(paste(githubDir,"routeSearch.R",sep=""))
source(paste(githubDir,"detailPath.R",sep=""))
source(paste(githubDir,"TABU.R",sep=""))
source(paste(githubDir,"updateRoute.R",sep=""))
source(paste(githubDir,"getVanStartTime.R",sep=""))
writeLines('Loading of Source files are successful')
writeLines('####################################################################################################')
writeLines('Getting the Optimal Van Start Time')
vanStartTime = getVanStartTime(loadData,slotData,Q=170)
writeLines(paste('Initial Van Start Times are',paste(vanStartTime,collapse=",")))
writeLines('####################################################################################################')
# vanStartTime = c(rep(750,6),rep(1300,4))
writeLines('Initial Solution')
notFound = TRUE
counter = 0
while (notFound) {
  ##Initial Solution
  route = routeSearch(timeMatrix,loadData,slotData,vanStartTime, deliveryTime = 25,L = 7.5,Q = 130)
  writeLines(paste("No of Deliveries possible",length(unique(unlist(route)))))
  if(length(unique(unlist(route))) ==  - unDel + length(unique(colnames(timeMatrix))))
    notFound = FALSE
  ## Permutation of van Start Time
  if(length(unique(unlist(route))) <  - unDel + length(unique(colnames(timeMatrix))))
    vanStartTime = vanStartTime[sample(1:length(vanStartTime))]
  counter = counter + 1
  if(counter > 100)
    break
}
writeLines('Initial Solution has found')

## Initial Timing
path = detailPath(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25)
writeLines(paste("Total Kilometer Travelled",sum(path$t_ij)/4))
writeLines('####################################################################################################')
## Updating the Vanstart Time
writeLines('Getting the Optimal Van Start Time')
if(length(unique(slotData$Slot_Start_Time)) > 1)
  vanStartTime = updateRoute(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25,L = 7.5,Q = 130)
if(length(unique(slotData$Slot_Start_Time)) == 1)
  writeLines('Van Updation is not Required')

## Based on new Van Start Time Definig the new Route
route = routeSearch(timeMatrix,loadData,slotData,vanStartTime, deliveryTime = 25,L = 7.5,Q = 130)
## New Path
path = detailPath(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25)
writeLines(paste("Total Kilometer Travelled",sum(path$t_ij)/4))
writeLines('####################################################################################################')
## Tabu Search
writeLines('-------TABU Search---------')
finalResult = tabuSearch(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25,timeCutOff = 45,Q = 130)
finalPath = detailPath(finalResult$route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25)
writeLines(paste("Total Kilometer Travelled",sum(finalPath$t_ij)/4))
write.csv(finalPath,paste('Optimized Route ',unlist(strsplit(fileName,"\\."))[1],'.csv',sep=""),row.names = FALSE)
writeLines('End of optimization')
writeLines('####################################################################################################')
sink()
