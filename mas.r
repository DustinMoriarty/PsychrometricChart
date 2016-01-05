#TITLE: MAS                              # 
#AUTHOR:                                 #  
#DATE: December 8, 2015                  # 
##########################################
#Class MAS calculates and stores moist air state data given three input variables.

#Variables:
#T_d         Dry Bulb Temperature (K)
#t_d         Dry Bulb Temperature (deg C)
#T_w         Wet Bulb Temperature (K)
#t_w         Wet Bulb Temperature (deg C)
#p           abosolute pressure
#p_da        Vapor Pressure (Pa)
#p_ws        wet bulb pressure at saturation
#W_ws        Humidity Ratio at Saturation Using Wet Bulb Temperature
#W_s         Humidity Ratio at Saturation Using Dry Bulb Temperature
#W           Humidity Ratio
#p_w         Partial Pressure of Water Vapor In Moist Air
#gamma       Specific Humidity       
#phi         Relative Humidity
#h           Enthalpy
#description description
#v           Specific Volume m^3/kg

#Load Dependencies
source("saturationpressure.r")
source("humidityratio.r")
source("dewpoint.r")
source("tempwetbulb.r")

#Constructor Function for MAS  
mas <- function(t_d = NULL, t_w = NULL, p = NULL, t_dew = NULL, 
	T_d = NULL, phi = NULL, W = NULL,h=NULL,units="SI", description = "No Description"){

# Unit Conversion
if(units=="IM"){
    ##<<To Do>> add unit conversion
    stop("Imperial units not yet supported.")
    }
    else if(units!="SI"){
    stop("Invalid value for units.")
    }

#Cacluate t_d from T_d if necessary
if(is.null(t_d)&is.numeric(T_d)) t_d = T_d - 273.15

#Initiate Variables
    R = 8314.472; #J/mol/K


if(is.numeric(t_d)&is.numeric(t_w)&is.numeric(p)&is.null(t_dew)
    &is.null(phi)&is.null(W)&is.null(h)){
    #Situation 1: t_d, t_w, p
    #Calculate State Variables
    T_d = t_d + 273.15 #Dry Bulb Temperature (deg K)
    T_w = t_w + 273.15 #Wet Bulb Temperature (deg K)
    p_da = SaturationPressure(T_d) #Partial Pressure of Dry Air
    p_ws = SaturationPressure(T_w)   #wet bulb pressure at saturation
    W_ws = 0.621945*p_ws/(p-p_ws)    #Humidity Ratio at Saturation	
    #Humidity Ratio
    W = humidityratio(t_d = t_d, p = p, t_w = t_w)     
    W_s = 0.621945*p_da/(p-p_da)
    p_w = p*W/(0.621945 + W) #Partial Pressure of Water Vapor
    gamma = W/(1+W) #Specific Humidity
    phi = p_w/p_da  #Relative Humidity
    h = 1.006*t_d+W*(2501+1.86*t_d) #Enthalpy
    v = R*T_d*(1+1.607858*W)/28.966/p #Specific Volume	
    t_dew = dewpoint(W=W,p=p) #Dewpoint Temperature (deg C) 
    T_dew = t_dew +273.15 #Dewpoint Temperature (deg K)

}
else if(is.numeric(t_d)&is.null(t_w)&is.numeric(p)&is.numeric(t_dew)&
    is.null(phi)&is.null(W)&is.null(h)){
    # Situation 2:  t_d, t_dew and p
    T_d = t_d + 273.15 #Dry Bulb Temperature (deg K)
    T_dew = t_dew+273.15 #Dew Point Temperature (deg K)
    p_w = SaturationPressure(T_dew)
	W = 0.621945*p_w/(p-p_w)
    p_da = SaturationPressure(T_d) #Partial Pressure of Dry Air
    W_s <- 0.621945*p_da/(p-p_da)
    gamma = W/(1+W) #Specific Humidity
    phi = p_w/p_da  #Relative Humidity
    h = 1.006*t_d+W*(2501+1.86*t_d) #Enthalpy
    v = R*T_d*(1+1.607858*W)/28.966/p #Specific Volume	
    t_w = tempwetbulb(t_d=t_d,p=p,W=W) #Wet Bulb Temp (deg C)
    T_w = t_w + 273.15 #Wet Bulb Temp (deg K)
}
else if(is.numeric(t_d)&is.null(t_w)&is.numeric(p)&is.null(t_dew)&
    is.numeric(phi)&is.null(W)&is.null(h)){
    # Situation 3:  t_d, phi and p
    T_d = t_d + 273.15 #Dry Bulb Temperature (deg K)
    T_w = t_w + 273.15 #Wet Bulb Temperature (deg K)
    p_da = SaturationPressure(T_d) #Partial Pressure of Dry Air
    p_w = phi*p_da
    W = humidityratio(t_d = t_d, p = p, phi=phi)
    W_s <- 0.621945*p_da/(p-p_da)
    gamma = W/(1+W) #Specific Humidity
    phi = p_w/p_da  #Relative Humidity
    h = 1.006*t_d+W*(2501+1.86*t_d) #Enthalpy
    v = R*T_d*(1+1.607858*W)/28.966/p #Specific Volume	
    t_w = tempwetbulb(t_d=t_d,p=p,W=W) #Wet Bulb Temp (deg C)
    T_w = t_w + 273.15 #Wet Bulb Temp (deg K)
    t_dew = dewpoint(W=W,p=p) #Dewpoint Temperature (deg C) 
    T_dew = t_dew +273.15 #Dewpoint Temperature (deg K)
}
else if(is.numeric(t_d)&is.null(t_w)&is.numeric(p)&is.null(t_dew)&
    is.null(phi)&is.numeric(W)&is.null(h)){
    # t_d, W  and p
    t_dew = dewpoint(W=W,p=p) #Dewpoint Temperature (deg C) 
    T_dew = t_dew +273.15 #Dewpoint Temperature (deg K)
    T_d = t_d + 273.15 #Dry Bulb Temperature (deg K)
    p_w = SaturationPressure(T_dew)
    p_da = SaturationPressure(T_d) #Partial Pressure of Dry Air
    W_s <- 0.621945*p_da/(p-p_da)
    gamma = W/(1+W) #Specific Humidity
    phi = p_w/p_da  #Relative Humidity
    h = 1.006*t_d+W*(2501+1.86*t_d) #Enthalpy
    v = R*T_d*(1+1.607858*W)/28.966/p #Specific Volume	
    t_w = tempwetbulb(t_d=t_d,p=p,W=W) #Wet Bulb Temp (deg C)
    T_w = t_w + 273.15 #Wet Bulb Temp (deg K)
}
else if(is.numeric(t_d)&is.null(t_w)&is.numeric(p)&is.null(t_dew)&
    is.null(phi)&is.null(W)&is.numeric(h)){
    # t_d, h  and p
    W = humidityratio(t_d = t_d, h = h) #Humidity Ratio
    t_dew = dewpoint(W=W,p=p) #Dewpoint Temperature (deg C) 
    T_dew = t_dew +273.15 #Dewpoint Temperature (deg K)
    T_d = t_d + 273.15 #Dry Bulb Temperature (deg K)
    p_w = SaturationPressure(T_dew)
    p_da = SaturationPressure(T_d) #Partial Pressure of Dry Air
    W_s <- 0.621945*p_da/(p-p_da)
    gamma = W/(1+W) #Specific Humidity
    phi = p_w/p_da  #Relative Humidity
    v = R*T_d*(1+1.607858*W)/28.966/p #Specific Volume	
    t_w = tempwetbulb(t_d=t_d,p=p,W=W) #Wet Bulb Temp (deg C)
    T_w = t_w + 273.15 #Wet Bulb Temp (deg K)
}
else if(is.null(t_d)&is.null(t_w)&is.numeric(p)&is.null(t_dew)&
    is.null(phi)&is.numeric(W)&is.numeric(h)){
    # W, h  and p
    t_d = (h-2501*W)/(1.006+1.86*W)
    t_dew = dewpoint(W=W,p=p) #Dewpoint Temperature (deg C) 
    T_dew = t_dew +273.15 #Dewpoint Temperature (deg K)
    T_d = t_d + 273.15 #Dry Bulb Temperature (deg K)
    p_w = SaturationPressure(T_dew)
    p_da = SaturationPressure(T_d) #Partial Pressure of Dry Air
    W_s <- 0.621945*p_da/(p-p_da)
    gamma = W/(1+W) #Specific Humidity
    phi = p_w/p_da  #Relative Humidity
    v = R*T_d*(1+1.607858*W)/28.966/p #Specific Volume	
    t_w = tempwetbulb(t_d=t_d,p=p,W=W) #Wet Bulb Temp (deg C)
    T_w = t_w + 273.15 #Wet Bulb Temp (deg K)
}
else stop("Too many, too few or wrong type of arguments.")

x = list(
	t_d         = t_d        ,
	T_d         = T_d        ,
	t_w         = t_w        ,
	T_w         = T_w        ,
    t_dew       = t_dew      ,
    T_dew       = T_dew      ,
	p           = p          ,
	p_w         = p_w        ,
	W           = W          ,
	W_s         = W_s        ,
    gamma       = gamma      ,
	phi         = phi        ,
	h           = h          ,
	v           = v          ,          
	description = description
    )
class(x) <- "mas"
	
return(x)
}


