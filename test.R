Cost_ij = c(20,10,30,25,15,35)
Cost_i0 = c(0,20,25,30,40,35)
load_i = c(0,5,4,4,2,7)
V = c(0,rep(Inf,5))
P = rep(NA,6)
for(i in 2:6)
{
	cost = 0
	load = 0
	j = i
	repeat
	{
		load = load + load_i[j]
		if(i==j)
			cost = Cost_i0[j] + Cost_i0[j]
		else
			cost = cost - Cost_i0[j-1] + Cost_ij[j-1] + Cost_i0[j]
		if(cost <= 90 & load <= 10)
		{
			if(V[i-1] + cost < V[j])
			{
				V[j] = V[i-1] + cost
				P[j] = i-1
			}
			j = j + 1
		}
		if(j > 6 | cost > 90 |load > 10)
			break
	}
	
}

