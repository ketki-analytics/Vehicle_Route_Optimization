updateRoute = function(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25,L = 7.5,Q = 130)
{
  ## Taking all the Permutations of Vans Start Time
  source('uniquePerm.r')
  permMat = uniquePerm(vanStartTime)
  
	## Getting the Current Path and Total travell Time
	currentPath = detailPath(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25)
	currentTotalTime = sum(currentPath$t_ij)
	bestVanStartTime = vanStartTime
	## Looping through all the Permutation
	for(vst in 1:nrow(permMat))
	{
	  vanStartTimeNew = permMat[vst,]
		newRoute = routeSearch(timeMatrix,loadData,slotData,vanStartTime = vanStartTimeNew, deliveryTime = 25,L = 7.5,Q = 130)
		newPath = detailPath(newRoute,timeMatrix,latLngData,loadData,slotData,vanStartTime=vanStartTimeNew,deliveryTime = 25)
		newTotalTime = sum(newPath$t_ij)
		
		if(newTotalTime < currentTotalTime & NROW(currentPath) == NROW(newPath))
		{
			writeLines(paste(newTotalTime))
			route = newRoute
			currentPath = newPath
			currentTotalTime = newTotalTime
			bestVanStartTime = vanStartTimeNew
		}
	}
	
	return(bestVanStartTime)
}

