humidityratio <- function(t_d=NULL, v = NULL, p = NULL, h = NULL, phi =
NULL, units = "SI"){
#Declare Variables
    W = 1
    R = 8314.472; #J/mol/K
    if(units=="IM"){
	##<<To Do>> add unit conversion
	stop("Imperial units not yet supported")
    } else if(units!="SI") 
	stop("invalid value for units")


    if(is.numeric(t_d)&is.numeric(h)&is.null(p)&is.null(v)&is.null(phi)){
	#Proceedure given t and h
	W = (h - 1.006*t_d)/(2501+1.86*t_d)
    } else if(is.numeric(t_d)&is.null(h)&is.numeric(p)&is.numeric(v)&is.null(phi)){
	#Proceedure given t, p and v
	W = 1/1.607858*(28.966*p*v/R/(t_d+273.15)-1)
    } else if(is.numeric(t_d)&is.null(h)&is.numeric(p)&is.null(v)&is.numeric(phi)){
	#Procedure given t, p and psi
	p_w =  phi*SaturationPressure(t_d+273.15)
	W = 0.621945*p_w/(p-p_w)
    } else 
	stop("Not enough or to many arguments!")


return(W)
}
