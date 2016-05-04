splitting = function(timeMatrix,loadData,slotData,L=7.5,Q=125)
{
	OID = c(colnames(timeMatrix)[1],colnames(timeMatrix)[-1][sample(1:(length(colnames(timeMatrix))-1))])
	V = c(0,rep(Inf,NROW(timeMatrix)-1))
	P =list()
	for(i in 2:NROW(timeMatrix))
	{
		cost = 0
		load = 0
		j = i
		repeat
		{
			load = load + loadData[loadData$order_external_id == OID[i],"Volume"]
			startTime = slotData$Slot_Start_Time[slotData$order_external_id == OID[j]]
			endTime = slotData$Slot_End_Time[slotData$order_external_id == OID[j]]
			if(j == i)
				cost = max(100*timeMatrix[1,colnames(timeMatrix) == OID[j]] + 800,startTime) + 25 + 100*timeMatrix[colnames(timeMatrix) == OID[j],1]
		 	if(j != i)
		 		cost = max(cost - 100 * timeMatrix[colnames(timeMatrix) == OID[j-1],1] + 100*timeMatrix[colnames(timeMatrix) == OID[j-1],colnames(timeMatrix) == OID[j]], startTime) + 25 + 100*timeMatrix[colnames(timeMatrix) == OID[j],1]
		 	if(cost < 800 + 100*L & load < Q)
		 	{
		 		if(V[i-1] + cost < V[j])
		 		{
		 			V[j] = V[i-1] + cost
		 			P[[j]] = i-1
		 		}
				j = j + 1
		 	}

		 	if(j > NROW(timeMatrix) | cost > 800 + 100*L | load > Q)
		 		break
		}
	}
	return(list(V=V,Path=P))
}

