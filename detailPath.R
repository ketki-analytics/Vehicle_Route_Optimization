detailPath = function(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime)
{
	m = length(vanStartTime)
	routeDetail = NULL
	for(van in 1:length(route))
	{
		vanRoute = route[[van]]
		endTime = vanStartTime[van]
		for(i in 1:(length(vanRoute)-1))
		{
			t_ij = 100*timeMatrix[vanRoute[i],vanRoute[i+1]]
			t_ij = ifelse(t_ij < 0.001,0,t_ij)
			endTime = endTime + t_ij + deliveryTime
			load = ifelse(vanRoute[i+1] != "Hub",loadData[loadData$order_external_id == vanRoute[i+1],"Volume"],0)
			routeDetail = rbind(routeDetail,
			data.frame(	VanNo = van,
						from = vanRoute[i],
						LatLng_from = paste(latLngData[latLngData$order_external_id == vanRoute[i],c("Lng","Lat")],sep=" ",collapse=","),
						to = vanRoute[i+1],
						LatLng_to = paste(latLngData[latLngData$order_external_id == vanRoute[i+1],c("Lng","Lat")],sep="",collapse=","),
						Slot_Start_Time = ifelse(vanRoute[i+1] != "Hub",slotData[slotData$order_external_id == vanRoute[i+1],"Slot_Start_Time"],0),
						Slot_End_Time = ifelse(vanRoute[i+1] != "Hub",slotData[slotData$order_external_id == vanRoute[i+1],"Slot_End_Time"],0),
						t_ij = 60*t_ij/100,
						endTime = endTime,
						load = load))
						
		}
	}
	routeDetail$endTime = 100*(routeDetail$endTime%/%100) + (60/100)*(routeDetail$endTime%%100)
	routeDetail$slotAdherence = ifelse(routeDetail$endTime - 15 >= routeDetail$Slot_Start_Time & routeDetail$endTime - 15 <= routeDetail$Slot_End_Time,1,0)
#	writeLines(paste("Slot Adherence Percentage",sum(routeDetail$slotAdherence)/(length(routeDetail$slotAdherence)- m)))
	return(routeDetail)
}

