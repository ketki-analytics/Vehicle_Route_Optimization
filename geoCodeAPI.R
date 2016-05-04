point = function(addr1,addr2,pincode,city,state)
{
	library('httr')
	library('curl')
	library('dplyr')
	# Universal Variable
	key <<- "0e41de65-d2c5-4ebd-9cfb-a552dae27f3e"
	referer <<- "http://large-analytics.flipkart.com/"
	# Converting Everything into String
	addr1 = as.character(addr1)
	pincode = as.character(pincode)
	city = as.character(city)
	state = as.character(state)
	if(!addr2 %in% "" & !is.na(addr2))
	{
		addr2 = as.character(addr2)
		URL = paste("https://maps.flipkart.com/geocode?key=",key, 
					"&addr1=",addr1,"&addr2=",addr2,"&city=",city,
					"&state=",state,"&pincode=",pincode,sep="")
		URL = gsub("\\#","",URL)
	}
	else
	{
		URL = paste("https://maps.flipkart.com/geocode?key=",key, 
					"&addr1=",addr1,"&city=",city,"&state=",state,
					"&pincode=",pincode,sep="")
		URL = gsub("\\#","",URL)
	}
	R = URL %>%
		GET(.,add_headers(referer = referer)) %>%
		content(., "parsed", encoding = "UTF-8")

	if(is.null(R$results[[1]]$geometry$location))
	{
		if(!addr2 %in% "" & !is.na(addr2))
		{
			addr2 = as.character(addr2)
			URL = paste("https://maps.flipkart.com/geocode?key=",key, 
					"&addr1=",URLencode(addr1),"&addr2=",URLencode(addr2),"&city=",city,
					"&state=",state,"&pincode=",pincode,sep="")
			URL = gsub("\\#","",URL)
		}
		else
		{
			URL = paste("https://maps.flipkart.com/geocode?key=",key, 
					"&addr1=",URLencode(addr1),"&city=",city,"&state=",state,
					"&pincode=",pincode,sep="")
			URL = gsub("\\#","",URL)
		}
		
		R = URL %>%
			GET(.,add_headers(referer = referer)) %>%
			content(., "parsed", encoding = "UTF-8")
	}
	if(is.null(R$results[[1]]$geometry$location))
		print(URL)
	return(	R$results[[1]]$geometry$location)
}

time = function(point1,point2)
{
	avgSpeed = 15000
	URL = paste("https://maps.flipkart.com/api/v1/directions?point=",point1$Lat,",",point1$Lng,
			'&point=',point2$Lat,",",point2$Lng,"&key=",key,sep="")
	R = URL %>%
		GET(.,add_headers(referer = referer)) %>%
		content(., "parsed", encoding = "UTF-8")
	if(is.null(R$paths[[1]]$distance))
	  time(point1,point2)
	else
	  return( R$paths[[1]]$distance/avgSpeed)
}


