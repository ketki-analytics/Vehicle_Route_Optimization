routeSearch = function(timeMatrix,loadData,slotData,deliveryTime = 17,vanStartTime,L = 12,Q = 140)
{
	m=length(vanStartTime)
	OID = setdiff(colnames(timeMatrix),notDeliveredOID)
	route = list()
	for(van in 1:m)
	{
		
		route[[van]] = OID[1]
		OIDCheckList = OID
		load = 0
		time = 0
		endTime = vanStartTime[van]
		repeat
		{
			previousScore = -9999
			lastOID = route[[van]][length(route[[van]])]
			timeVector = 100*timeMatrix[lastOID, colnames(timeMatrix) %in% setdiff(OIDCheckList[-1],lastOID)]
			nextOID = NULL
			for(oid in setdiff(OIDCheckList[-1],lastOID))
			{
				currentScore = 0
				t_ij = as.numeric(timeVector[names(timeVector) %in% oid])
				t_ij = ifelse(t_ij < 0.001,
							ifelse(data$Pincode[data$order_external_id %in% lastOID] == data$Pincode[data$order_external_id %in% oid],25,t_ij),
							t_ij)
				slotStartTime = slotData$Slot_Start_Time[slotData$order_external_id %in% oid]
				slotEndTime = slotData$Slot_End_Time[slotData$order_external_id %in% oid]
				currentLoad = loadData[loadData$order_external_id %in% oid,"Volume"]
				##Condition - 1
				if(endTime + t_ij >= slotStartTime & endTime + t_ij <= slotEndTime)
				{
					currentScore = currentScore + 200
					if(load + currentLoad <= Q)
						currentScore = currentScore + 50
					if(load + currentLoad > Q)
						currentScore = currentScore + 200/(Q-load - currentLoad)
					if(endTime + t_ij < vanStartTime[van] + 100*L)
						currentScore = currentScore + 50
					currentScore = currentScore + 1000/t_ij
				}
				##Condition - 2	
				if(endTime + t_ij + 100 >= slotStartTime & endTime + t_ij < slotStartTime)
				{
					currentScore = currentScore + 25
					if(load + currentLoad <= Q)
						currentScore = currentScore + 50
					if(load + currentLoad > Q)
						currentScore = currentScore + 200/(Q-load - currentLoad)
					if(endTime + t_ij < vanStartTime[van] + 100*L)
						currentScore = currentScore + 50
					currentScore = currentScore + 100/t_ij
					currentScore = currentScore + 100/(endTime + t_ij-slotStartTime)
				}
				##Condition - 3
				if(endTime + t_ij > slotEndTime & endTime + t_ij <= slotEndTime + 100)
				{
					currentScore = currentScore + 25
					if(load + currentLoad <= Q)
						currentScore = currentScore + 50
					if(load + currentLoad > Q)
						currentScore = currentScore + 200/(Q-load - currentLoad)
					if(endTime + t_ij < vanStartTime[van] + 100*L)
						currentScore = currentScore + 50
					currentScore = currentScore + 100/t_ij
					currentScore = currentScore - 100/(endTime + t_ij-slotEndTime)
				}
				
#				writeLines(paste(oid,"->",currentScore))
				if(currentScore > previousScore)
				{
					nextOID = oid
					previousScore = currentScore
				}
			}
			if(!is.null(nextOID))
			{
				load = load + loadData[loadData$order_external_id %in% nextOID,"Volume"]
				endTime = endTime + timeVector[names(timeVector) %in% nextOID] + deliveryTime
				route[[van]] = c(route[[van]],nextOID)
				OIDCheckList = setdiff(OIDCheckList,nextOID)
			}
			
			if(is.null(OIDCheckList)|is.null(nextOID))
			{
				endTime = endTime + timeMatrix[route[[van]][length(route[[van]])],"Hub"]
				route[[van]] = c(route[[van]],"Hub")
				break
			}	
				
			if(length(OIDCheckList) == 2)
			{
				endTime = endTime + timeMatrix[route[[van]][length(route[[van]])],OIDCheckList[2]] + deliveryTime + timeMatrix[OIDCheckList[2],"Hub"]
				load = load + loadData[loadData$order_external_id %in% OIDCheckList[2],"Volume"]
				route[[van]] = c(route[[van]],OIDCheckList[2],"Hub")
				OIDCheckList = NULL
				break
			}
			if(load >= Q | endTime > vanStartTime[van] + 100*L | is.null(OIDCheckList))
			{
				endTime = endTime + timeMatrix[route[[van]][length(route[[van]])],"Hub"]
				route[[van]] = c(route[[van]],"Hub")
				break
			}
			
		}
		OID = setdiff(OID,setdiff(route[[van]],"Hub"))
#		writeLines(paste("Van",van,"Total Travel Time",(endTime - vanStartTime[van])/100,"hour Travel End Time is", endTime,"Load is ",load,"cubic feet"))
		if(length(OID) == 1)
		{
			writeLines('NO FURTHER VAN IS REQUIRED')
			break
		}
	}
	if(length(OID) > 1)
	{
		writeLines("MORE VAN(s)ARE REQUIRED")
#		writeLines("RE-ITERATING")
#		vanStartTime <- vanStartTime[sample(1:length(vanStartTime))]
#		route = routeSearch(timeMatrix,loadData,slotData,vanStartTime = vanStartTime, deliveryTime = 25,L = 7.5,Q = 130)$route	
	}
#	return(list(route = route,vanStartTime = vanStartTime))
	return(route)
}
