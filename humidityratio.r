humidityratio <- function(v = NULL, p = NULL, h = NULL, phi =
NULL, t_d = NULL, units = "SI"){
#Declare Variables
    W = 1
    R = 8314.472; #J/mol/K
    if(units=="IM"){
	##<<To Do>> add unit conversion
	stop("Imperial units not yet supported")
    } else if(units!="SI") 
	stop("invalid value for units")

    #Allow p to be entered as a scalar
    if(is.numeric(p)&is.numeric(t_d)&length(p)==1&length(t_d>1)){rep(p,length(t_d))}

    if(is.numeric(t_d)&is.numeric(h)&is.null(p)&is.null(v)&is.null(phi)){
	#Proceedure given t and h
	if(length(t_d)==length(h)){
	    W = (h - 1.006*t_d)/(2501+1.86*t_d)
	} else stop("arguments must be the same length")
    } else if(is.numeric(t_d)&is.null(h)&is.numeric(p)&is.numeric(v)&is.null(phi)){
	#Proceedure given t, p and v
	if(length(t_d)==length(v)){
	    W = 1/1.607858*(28.966*p*v/R/(t_d+273.15)-1)
	} else stop("arguments must be the same length")
    } else if(is.numeric(t_d)&is.null(h)&is.numeric(p)&is.null(v)&is.numeric(phi)){
	#Procedure given t, p and phi
	if(length(t_d)==length(phi)){
	    p_w =  phi*SaturationPressure(t_d+273.15)
	    W = 0.621945*p_w/(p-p_w)
	} else stop("arguments must be the same length")
    } else 
	stop("Not enough or to many arguments!")


return(W)
}
