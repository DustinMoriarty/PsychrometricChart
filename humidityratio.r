humidityratio <- function(t_d=NULL, v = NULL, p = NULL, h = NULL, units = "SI"){
#Declare Variables
    W = 1
    R = 8314.472; #J/mol/K
    if(units=="IM"){
	##<<To Do>> add unit conversion
	stop("Imperial units not yet supported")
	}
	else if(units!="SI") stop("invalid value for units")


    W <- ifelse(is.numeric(t_d)&is.numeric(h)&is.null(p)&is.null(v),
    #Proceedure given t and h
    (h - 1.006*t_d)/(2501+1.86*t_d),
    ifelse(is.numeric(t)&is.null(h)&is.numeric(p)&is.numeric(v),
    #Proceedure given t, p and v
    1/1.607858*(28.966*p*v/R/(t_d+273.15)-1),
    stop("Not enough or to many arguments!")))


return(W)
}
