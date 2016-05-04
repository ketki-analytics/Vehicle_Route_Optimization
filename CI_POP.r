CI_POP = function()
{
  hourlyVanCost = 300
  
  m = length(vanStartTime)
  ## Creating empty routes starting with Kudlu Hub
  iniRoute = list()
  routeEndTime = list()
  routeLoad = list()
  for(i in 1:m)
  {
    iniRoute[[i]] = 'Kudlu_Hub'
    routeEndTime[[i]] = vanStartTime[i]
    routeLoad[[i]] = 0
  }

  OID = colnames(timeMatrix)[-1]
  for(c in OID)
  {
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
                                max(0,Slot_Start_Time - routeEndTime[[r]][p]+100*timeMatrix[iniRoute[[r]][p],c])/100)
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
      routeEndTime[[best_r]][k+1] = routeEndTime[[best_r]][k] + 100*timeMatrix[iniRoute[[best_r]][k],c] + 25
      
  }
  
  iniRoute4Path = list()
  for(i in 1:m)
    iniRoute4Path[[i]] = c(iniRoute[[i]],iniRoute[[i]][1])
  return(iniRoute4Path)
}