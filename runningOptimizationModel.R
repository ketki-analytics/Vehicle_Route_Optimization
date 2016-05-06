load(paste('optimizationData ',unlist(strsplit(fileName,"\\."))[1],'.RData',sep=""))
library(dplyr)
source(paste(githubDir,"routeSearch.R",sep=""))
source(paste(githubDir,"detailPath.R",sep=""))
source(paste(githubDir,"TABU.R",sep=""))
source(paste(githubDir,"updateRoute.R",sep=""))
source(paste(githubDir,"getVanStartTime.R",sep=""))

## If you have proir knowledge that these Orders do not require OFD, then put the Tracking IDs here
notDeliveredOID = NULL
unDel = 0
vanStartTime = getVanStartTime(loadData,slotData,Q=130)
# vanStartTime = c(rep(750,6),rep(1300,4))

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

## Initial Timing
path = detailPath(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25)
writeLines(paste("Total Kilometer Travelled",sum(path$t_ij)/4))

## Updating the Vanstart Time
vanStartTime = updateRoute(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25,L = 7.5,Q = 130)

## Based on new Van Start Time Definig the new Route
route = routeSearch(timeMatrix,loadData,slotData,vanStartTime, deliveryTime = 25,L = 7.5,Q = 130)
## New Path
path = detailPath(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25)
writeLines(paste("Total Kilometer Travelled",sum(path$t_ij)/4))

## Tabu Search
finalResult = tabuSearch(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25,timeCutOff = 45)
finalPath = detailPath(finalResult$route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25)
writeLines(paste("Total Kilometer Travelled",sum(finalPath$t_ij)/4))
write.csv(finalPath,paste('Optimized Route',unlist(strsplit(fileName,"\\."))[1],'.csv',sep=""),row.names = FALSE)