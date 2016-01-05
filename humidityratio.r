humidityratio <- function(v = NULL, p = NULL, h = NULL, phi =
NULL, t_d = NULL, t_w = NULL,units = "SI"){
#Declare Variables
    W = 1
    R = 8314.472; #J/mol/K
    if(units=="IM"){
	##<<To Do>> add unit conversion
	stop("Imperial units not yet supported")
    } else if(units!="SI") 
	stop("invalid value for units")

#    #Allow p to be entered as a scalar (Should no longer be necessary)
#    if(is.numeric(p)&is.numeric(t_d)&length(p)==1&length(t_d>1)){
#        rep(p,length(t_d))
#        }

    if(is.numeric(t_d)&is.numeric(h)&is.null(p)&is.null(v)&is.null(phi)&is.null(t_w)){
	    #Proceedure given t_d and h
        W = (h - 1.006*t_d)/(2501+1.86*t_d)
    } else if(is.numeric(t_d)&is.null(h)&is.numeric(p)&is.numeric(v)&is.null(phi)){
        #Proceedure given t_d, p and v
        W = 1/1.607858*(28.966*p*v/R/(t_d+273.15)-1)
    } else if(is.numeric(t_d)&is.null(h)&is.numeric(p)&is.null(v)&is.numeric(phi)&is.null(t_w)){
	#Procedure given t_d, p and phi
        p_w =  phi*SaturationPressure(t_d+273.15)
        W = 0.621945*p_w/(p-p_w)
    } else if(is.numeric(t_d)&is.null(h)&is.numeric(p)&is.null(v)&is.null(phi)&is.numeric(t_w)){
	#Procedure given t_d, p and t_w
        p_ws =  SaturationPressure(t_w+273.15)
        W_ws = 0.621945*p_ws/(p-p_ws)
        W <-ifelse(t_d<0, 
            ((2830 - 0.24*t_w)*W_ws -1.006*(t_d-t_w))/
            (2830 + 1.86*t_d-2.1*t_w), 
            ((2501 -2.326*t_w)*W_ws - 1.006*(t_d-t_w))/
            (2501+1.86*t_d-4.186*t_w))
    } else stop("Not enough or to many arguments!")
    return(W)
}
