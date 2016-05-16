slotDiff = function(v)
{
  iterData_1 = 
    v %>%
    routeSearch(timeMatrix,loadData,slotData,., deliveryTime = 25,L = 7.5,Q = 130) %>%
    detailPath(.,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25) %>%
    filter(.,from == 'Hub') %>% select(.,Slot_Start_Time,t_ij)
  
  slotDiff = iterData_1$Slot_Start_Time -(100/60)*iterData_1$t_ij
 
  return(sum(slotDiff^2))
}

optmizeVanStartTime = function(route,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25,L = 7.5,Q = 130)
{
  counter = 0
  continueIteration = TRUE
  while(continueIteration)
  {
    counter = counter + 1
    newVanStartTime = vanStartTime
    iterData =
      newVanStartTime %>%
      routeSearch(timeMatrix,loadData,slotData,., deliveryTime = 25,L = 7.5,Q = 130) %>%
      detailPath(.,timeMatrix,latLngData,loadData,slotData,vanStartTime,deliveryTime = 25) %>%
      filter(.,from == 'Hub') %>% select(.,Slot_Start_Time,t_ij)
    
    newVanStartTime =  iterData$Slot_Start_Time  -(100/60)*iterData$t_ij
    
    if((slotDiff(vanStartTime)-slotDiff(newVanStartTime))/slotDiff(vanStartTime) < 0.05)
      continueIteration = FALSE
    else
      vanStartTime = newVanStartTime
    
    if(counter > 50)
      break
  }
  return(vanStartTime)
}
