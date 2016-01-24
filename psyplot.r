source("mas.r")
# See class MAS for general naming conventions based on the ASHRAE handbook

psylines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, w_min =
    0, w_max = 30/1000, nsteps = 1001, p = 101.3e3){
    #Class for the creation of lines on the plot. Default values corespond to 
    #ASHRAE chart 1
    obj <- list(
        nlines = nlines,
        t_d_max= t_d_max,
        t_d_min= t_d_min,
        w_max  = w_max,
        w_min  = w_min,
        p = p,
        nsteps = nsteps,
        x_min = NULL,
        x_max = NULL,
        x_list = NULL,
        t_d_list = NULL,
        w_list = NULL
    )

    if(is.numeric(obj$t_d_max)&is.numeric(obj$t_d_min)){

    # Create a list of dry bulb temperatures for the x-axis
	obj$t_d_list = seq(obj$t_d_min,obj$t_d_max,
        (obj$t_d_max-obj$t_d_min)/(obj$nsteps-1))
    }

    class(obj) <- "psylines"
return(obj)
}

#Classes From Psylines
philines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, 
    w_min = 0, w_max = 30/1000, nsteps = 1001, p = 101.3e3){
    obj = psylines(nlines, t_d_max, t_d_min, w_min, w_max, nsteps, p)
    class(obj)<-append("philines",class(obj))
    return(obj)
}

wlines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, 
    w_min = 0, w_max = 30/1000, nsteps = 1001, p = 101.3e3){
    obj = psylines(nlines, t_d_max, t_d_min, w_min, w_max, nsteps, p)
    class(obj)<-append("wlines",class(obj))
    return(obj)
}

vlines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, w_min =
    0, w_max = 30/1000, nsteps = 1001, p = 101.3e3){
    obj = psylines(nlines, t_d_max, t_d_min, w_min, w_max, nsteps, p)
    class(obj)<-append("vlines",class(obj))
    return(obj)
}


hlines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, w_min =
    0, w_max = 30/1000, nsteps = 1001, p = 101.3e3){
    obj = psylines(nlines, t_d_max, t_d_min, w_min, w_max, nsteps, p)
    class(obj)<-append("hlines",class(obj))
    return(obj)
}

t_dlines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, 
    w_min = 0, w_max = 30/1000, nsteps = 1001, p = 101.3e3){
    obj = psylines(nlines, t_d_max, t_d_min, w_min, w_max, nsteps, p)
    class(obj)<-append("t_dlines",class(obj))
    return(obj)
}

#Set the t_d list for a psyline object. Do not use directly. Use set_list in 
#order to set the t_d and w lists at the same type based on the object class.
set_t_d_list.psylines <- function(obj){ 
    if(is.numeric(obj$t_d_max)&is.numeric(obj$t_d_min)){
        # Create a list of dry bulb temperatures for the x-axis
        obj$t_d_list = seq(obj$t_d_min,obj$t_d_max,(obj$t_d_max-obj$t_d_min)/
            (obj$nsteps-1))
    return(obj)
    }
}

set_t_d_list <- function(obj)UseMethod("set_t_d_list",obj)

# Method to trim the lines to the bounds of the plot
trim.psylines <- function(obj){
    obj$w_list <- ifelse(
        obj$w_list>obj$w_max|
        obj$w_list>humidityratio(t_d = obj$t_d_list,
            p = rep(obj$p,obj$nsteps),phi = rep(1,obj$nsteps))|
        obj$w_list<obj$w_min,
        NA,
        obj$w_list)
    return(obj)
}

trim <- function(obj) UseMethod("trim",obj)

# set_list methods to set the lists for each type of line.
#<<TO DO:>> Finish adding steps to calculate x_min,x_max, x_list
#set_list.psylines <- function(obj,x=NULL)

set_list.philines <- function(obj,x=NULL){
    obj = set_t_d_list(obj)
    obj$x_min = 0
    obj$x_max = 1
    obj$x_list = seq(obj$x_min,obj$x_max,(obj$x_max-obj$x_min)/(obj$nlines-1))
    if(is.null(x)) x = obj$x_min
    obj$w_list = humidityratio(t_d = obj$t_d_list, p =
        rep(obj$p,obj$nsteps),phi = rep(x,obj$nsteps))
    obj=trim(obj)
    return(obj)
}

set_list.wlines <- function(obj,x=NULL){
    obj$x_min = obj$w_min
    obj$x_max = obj$w_max
    if(is.null(x)) x = obj$x_min
    obj$t_d_list = c(dewpoint(p = obj$p, W = x),obj$t_d_max)
    obj$t_d_list = ifelse(obj$t_d_list<obj$t_d_min,obj$t_d_min,obj$t_d_list)
    obj$x_list = seq(obj$x_min,obj$x_max,(obj$x_max-obj$x_min)/(obj$nlines-1))
    obj$w_list = c(x,x)
    obj=return(obj)
}

set_list.vlines <- function(obj,x=NULL){
    obj = set_t_d_list(obj)
    statemin = mas(W = obj$w_min,t_d = obj$t_d_min,p = obj$p)
    statemax = mas(W = obj$w_max,t_d = obj$t_d_max,p = obj$p)
    obj$x_min = statemin$v
    obj$x_max = statemax$v
    obj$x_list = seq(obj$x_min,obj$x_max,(obj$x_max-obj$x_min)/(obj$nlines-1))
    if(is.null(x)) x = obj$x_min
    obj$w_list = humidityratio(t_d=obj$t_d_list, p=rep(obj$p,obj$nsteps),
        v=rep(x,obj$nsteps))
    obj=trim(obj)
    return(obj)
}

set_list.t_dlines <- function(obj,x=NULL){
#Note: these lines are vertical so they need some special instructions below.
    obj$x_min = obj$t_d_min
    obj$x_max = obj$t_d_max
    obj$x_list = seq(obj$x_min,obj$x_max,(obj$x_max-obj$x_min)/(obj$nlines-1))
    if(is.null(x)) x = obj$x_min
    obj$t_d_list = c(x,x)
    W_s = humidityratio(t_d = x,p = obj$p,phi = 1)
    obj$w_list = c(obj$w_min,W_s)
    
    obj$w_list = ifelse(obj$w_list>obj$w_max,
        c(obj$w_min,obj$w_max),
        obj$w_list
    )
    return(obj)
}   

set_list.hlines <- function(obj,x=NULL){
    obj = set_t_d_list(obj)
    statemin = mas(W = obj$w_min,t_d = obj$t_d_min,p = obj$p)
    statemax = mas(W = obj$w_max,t_d = obj$t_d_max,p = obj$p)
    obj$x_min = statemin$h
    obj$x_max = statemax$h
    obj$x_list = seq(obj$x_min,obj$x_max,(obj$x_max-obj$x_min)/(obj$nlines-1))
    if(is.null(x)) x = obj$x_min
    obj$w_list = humidityratio(t_d=obj$t_d_list, h=rep(x,obj$nsteps))
    obj=trim(obj)
    return(obj)
}

set_list <- function(obj,x) UseMethod("set_list",obj)

#Draw the lines for a psylines object
draw.psylines <- function(obj){
    obj = set_list(obj)
    for (x in obj$x_list){
        obj = set_list(obj,x)
        lines(obj$t_d_list, obj$w_list*1000, type = "l")
	}	
}

draw <- function(obj) UseMethod("draw",obj)


#Function psyplot to create the psyplot class. Pass variables down to
#psylines objects. Psyplot defines all data necessary to create a 
#psychrometric plot.
psyplot <- function(phil = NULL, wl = NULL, 
    vl = NULL, t_dl = NULL, hl = NULL, main = "Psychrometric Plot", 
    t_d_max = 50, t_d_min = 0, w_min = 0, w_max = 30/1000, p = 101.3e3){
     
    if(is.null(phil    )) phil = philines()
    if(is.null(wl      )) wl   = wlines()  
    if(is.null(vl      )) vl   = vlines()  
    if(is.null(t_dl    )) t_dl = t_dlines()
    if(is.null(hl      )) hl   = hlines()

	phil$t_d_max <- wl$t_d_max <- vl$t_d_max<- t_dl$t_d_max <-hl$t_d_max <- t_d_max
	phil$t_d_min <- wl$t_d_min <- vl$t_d_min<- t_dl$t_d_min <-hl$t_d_min <- t_d_min
	phil$w_max   <- wl$w_max   <- vl$w_max  <- t_dl$w_max   <-hl$w_max   <- w_max
	phil$w_min   <- wl$w_min   <- vl$w_min  <- t_dl$w_min   <-hl$w_min   <- w_min
    phil$p       <- wl$p       <- vl$p      <- t_dl$p       <-hl$p       <- p
	obj <- list(
        phil = phil,
        wl = wl,
        vl = vl,
        t_dl = t_dl,
        hl = hl,
        main = main,
        t_d_max = t_d_max,
        t_d_min = t_d_min,
        w_min = w_min,
        w_max = w_max,
        p = p
    )
	class(obj) <- "psyplot"
	return(obj)
}

plot.psyplot <- function(obj){
    plot(NULL,xlim=c(obj$t_d_min,obj$t_d_max),
        ylim=c(obj$w_min,obj$w_max*1000),
        axes = FALSE,main = obj$main, 
        xlab = "Temperature (°C)", 
        ylab = ""
    )

    draw(obj$phil)
    draw(obj$wl)
    draw(obj$vl)
    draw(obj$t_dl)
    draw(obj$hl)
}
