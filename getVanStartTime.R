getVanStartTime = function(loadData,slotData,shiftStart=c(750,1350),Q = 125)
{
  minVans = ceiling(sum(loadData$Volume)/Q)+1
  
  slotData$shift = ifelse(slotData$Slot_Start_Time < 1300,shiftStart[1],shiftStart[2])
  aggrSlotDetails = aggregate(order_external_id ~ shift,slotData,length)
  aggrSlotDetails = aggrSlotDetails[aggrSlotDetails$shift != 0 | is.na(aggrSlotDetails$shift),]
  aggrSlotDetails$fraction = aggrSlotDetails$order_external_id/sum(aggrSlotDetails$order_external_id)
  
  vanfreq = round(minVans*aggrSlotDetails$fraction)
  if(sum(vanfreq) != minVans)
    vanfreq[1] = vanfreq[1] + (minVans-sum(vanfreq))
  
  return(sort(as.numeric(rep(aggrSlotDetails$shift,vanfreq))))
  
}