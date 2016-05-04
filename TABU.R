tabuSearch = function(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25,timeCutOff = 60)
{
	outlierOID = NULL
	chkList = NULL
	path = detailPath(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25)
	counter = 0
	repeat
	{
		counter = counter + 1
		oidTemp = path %>% filter(.,! to %in% c(chkList,'Hub')) %>%filter(.,t_ij == max(t_ij)) %>% filter(.,load == max(load))
		
		singleID = integer(0)
		if(length(which(unlist(lapply(route,length)) == 3)) != 0)
		  singleID = route[[which(unlist(lapply(route,length)) == 3)]][2]
		
		if(length(oidTemp$t_ij) == 0 & length(singleID) == 0)
			break
		
		if(length(oidTemp$t_ij) == 0 & length(singleID) != 0)
		  oidTemp = path %>% filter(.,to %in% c(chkList,singleID))
		
		if(length(oidTemp$t_ij) != 0)
		  if(oidTemp$t_ij < timeCutOff & length(singleID) != 0)
		    oidTemp = path %>% filter(.,to %in% c(chkList,singleID))
		
		if(length(oidTemp$t_ij) == 0)
		  break
		
		if(oidTemp$t_ij < timeCutOff & length(singleID) == 0)
			break
		if(counter > 200)
			break
		
		oidToMove = as.character(oidTemp$to)
		
		chkList = c(chkList,oidToMove)
		totalTime = sum(path$t_ij)
		slotAdherence = sum(path$slotAdherence)
		
		insertionRoute = NULL
		insertionOID = NULL
		minDiff = Inf
		
		for(r in setdiff(1:length(route),oidTemp$VanNo))
		{
			for(oid in 1:(length(route[[r]]) - 1))
			{
				currentDiff =  timeMatrix[route[[r]][oid],oidToMove] + timeMatrix[oidToMove,route[[r]][oid+1]] - timeMatrix[route[[r]][oid],route[[r]][oid+1]] 
				newRoute = route
				newRoute[[r]] = c(newRoute[[r]][1:oid],oidToMove,newRoute[[r]][(oid+1):length(newRoute[[r]])])
				newRoute[[oidTemp$VanNo]] = newRoute[[oidTemp$VanNo]][!newRoute[[oidTemp$VanNo]] %in% oidToMove]
				newPath = detailPath(newRoute,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25)
				if(minDiff > currentDiff & 
				   ((
				     totalTime > sum(newPath$t_ij) & 
				     sum(newPath$slotAdherence)/(length(newPath$slotAdherence)-length(route)) >= 0.9
				     ) |
				   ((length(oidTemp$t_ij) == 0 | oidTemp$t_ij < timeCutOff) & length(singleID) != 0))
				   )
				{
					insertionRoute = r
					insertionOID = route[[r]][oid]
					minDiff = currentDiff
				}
			}
		}
		if(is.null(insertionRoute))
			outlierOID = c(outlierOID,oidToMove)
		if(!is.null(insertionRoute))
		{
			insertionIndex = which(route[[insertionRoute]] == insertionOID)[1]
			route[[insertionRoute]] = c(route[[insertionRoute]][1:insertionIndex],oidToMove,route[[insertionRoute]][(insertionIndex+1):length(route[[insertionRoute]])])
			print(paste(insertionRoute,"-",insertionOID,"-",insertionIndex))
			route[[oidTemp$VanNo]] = route[[oidTemp$VanNo]][!route[[oidTemp$VanNo]] %in% oidToMove]
			if(length(unique(route[[oidTemp$VanNo]])) == 1)
			{
				route[[oidTemp$VanNo]] = NULL
				vanStartTime <<- vanStartTime[-VanNo]
			}
			path = detailPath(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25)
			
		}
	}
	return(list(route=route,outlierOID=outlierOID))
}
