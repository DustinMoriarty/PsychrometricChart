#Variables:
#T_d           %Dry Bulb Temperature (K)
#t_d           #Dry Bulb Temperature (deg C)
#T_w         Wet Bulb Temperature eK
#t_w         Wet Bulb Temperature (deg C)
#p           abosolute pressure
#p_da        Vapor Pressure (Pa)
#p_ws        wet bulb pressure at saturation
#W_ws        Humidity Ratio at Saturation
#W           Humidity Ratio
#p_w         Partial Pressure of Water Vapor In Moist Air
#gamma       Specific Humidity       
#phi         Relative Humidity
#h           Enthalpy
#description description
#v           Specific Volume e^3/k

#Load Dependencies
source("saturationpressure.r")


#Constructor Function for MAS  
mas <- function(t_d,t_w,p){

#Initiate Variables
	R = 8314.472; #J/mol/K
	x = list(
		t_d = 1,
		T_d = 1, 
		T_w = 1,        
		t_w = 1,        
		p = 1,          
		p_da = 1,       
		p_ws = 1,       
		W_ws = 1,       
		W = 1,          
		p_w = 1,        
		gamma = 1,       
		phi = 1,         
		h = 1,           
		description = "description", 
		v = 1)

#Calculate State Variables
	x$t_d = t_d
	x$t_w = t_w
	x$p = p
	x$T_d = t_d + 273.15 #Dry Bulb Temperature (deg K)
	x$T_w = t_w + 273.15 #Wet Bulb Temperature (deg K)
	x$p_da = SaturationPressure(x$T_d) #Partial Pressure of Dry Air
	x$p_ws = SaturationPressure(x$T_w);   #wet bulb pressure at saturation
	x$W_ws = 0.621945*x$p_ws/(x$p-x$p_ws);    #Humidity Ratio at Saturation	
	#Humidity Ratio
	x$W <-ifelse(x$t_d<0, 
		((2830 - 0.24*x$t_w)*x$W_ws -1.006*(x$t_d-x$t_w))/(2830 + 1.86*x$t_d-2.1*x$t_w), 
		((2501 -2.326*x$t_w)*x$W_ws - 1.006*(x$t_d-x$t_w))/(2501+1.86*x$t_d-4.186*x$t_w))
	x$p_w = x$p*x$W/(0.621945 + x$W) #Partial Pressure of Water Vapor
	x$gamma = x$W/(1+x$W) #Specific Humidity
	x$phi = x$p_w/x$p_da  #Relative Humidity
	x$h = 1.006*x$t_d+x$W*(2501+1.86*x$t_d) #Enthalpy
	x$v = R*x$T_d*(1+1.607858*x$W)/28.966/x$p #Specific Volume	
class(x) <- "mas"


return(x)
}

