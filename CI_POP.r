CI_POP = function(timeMatrix,loadData,slotData,vanStartTime, deliveryTime = 25,L = 7.5,Q = 130)
{
  hourlyVanCost = 300
  
  m = length(vanStartTime)
  ## Creating empty routes starting with Kudlu Hub
  iniRoute = list()
  routeEndTime = list()
  routeLoad = list()
  for(i in 1:m)
  {
    iniRoute[[i]] = 'Hub'
    routeEndTime[[i]] = vanStartTime[i]
    routeLoad[[i]] = 0
  }

  OID = colnames(timeMatrix)[-1]
  counter = 0
  for(c in OID)
  {
    counter = counter + 1
    Slot_Start_Time = slotData[slotData$order_external_id==c,"Slot_Start_Time"]
    Slot_End_Time = slotData[slotData$order_external_id==c,"Slot_End_Time"]
    load = loadData[loadData$order_external_id == c,"Volume"]
    
    bestCost = Inf
    best_r = 1
    best_p = 1
    for(r in 1:m)
    {
      availablePosition = 1:length(iniRoute[[r]])
      for (p in availablePosition) 
      {
        cost = hourlyVanCost * (timeMatrix[iniRoute[[r]][p],c] + 
                                max(0,Slot_Start_Time - routeEndTime[[r]][p]-100*timeMatrix[iniRoute[[r]][p],c])/100 + deliveryTime)
        if(cost < bestCost & 
           max(routeEndTime[[r]][p]+100*timeMatrix[iniRoute[[r]][p],c],Slot_Start_Time) < min(vanStartTime[r] + 100*L,Slot_End_Time) &
           routeLoad[[r]] + load < Q)
        {
          best_r = r
          best_p = p
          best_cost = cost
        }
        
      }
    }
    
    if(length(iniRoute[[best_r]]) > best_p)
      iniRoute[[best_r]] = c(iniRoute[[best_r]][1:best_p],c,iniRoute[[best_r]][(best_p+1):length(iniRoute[[best_r]])])
    else
      iniRoute[[best_r]] = c(iniRoute[[best_r]],c)
    
    routeLoad[[best_r]] = routeLoad[[best_r]] + load
    for(k in 1:(length(iniRoute[[best_r]])-1))
      routeEndTime[[best_r]][k+1] = routeEndTime[[best_r]][k] + 100*timeMatrix[iniRoute[[best_r]][k],c] + deliveryTime

    
    if(counter %% 5 == 0)
    {
      iniRoute_1 = list()
      for(i in 1:m)
        iniRoute_1[[i]] = c(iniRoute[[i]],iniRoute[[i]][1])
      iniRoute_1 = tabuSearch(iniRoute_1,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25,timeCutOff = 45,Q = 130)$route

      for(i in 1:m)
      {
        if(is.null(iniRoute_1[[i]]))
          iniRoute_1[[i]] = 'Hub'
        else
          iniRoute_1[[i]] = iniRoute_1[[i]][-length(iniRoute_1[[i]])]
      }
      iniRoute = iniRoute_1
    }      
  }
  iniRoute_1 = list()
  for(i in 1:m)
    iniRoute_1[[i]] = c(iniRoute[[i]],iniRoute[[i]][1])
  return(iniRoute_1)
}