source("mas.r")
# See class MAS for general naming conventions based on the ASHRAE handbook

psylines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, w_min =
0, w_max = 30, nsteps = 101, p = 101.3e3){
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
	obj$t_d_list = seq(obj$t_d_min,obj$t_d_max,(obj$t_d_max-obj$t_d_min)/(obj$nsteps-1))
    }

    class(obj) <- "psylines"
return(obj)
}

#Classes Decended From Psylines
philines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, w_min =
0, w_max = 30, nsteps = 101, p = 101.3e3){
obj = psylines(nlines, t_d_max, t_d_min, w_min, w_max, nsteps, p)
class(obj)<-"philines"
return(obj)
}

wlines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, w_min =
0, w_max = 30, nsteps = 101, p = 101.3e3){
obj = psylines(nlines, t_d_max, t_d_min, w_min, w_max, nsteps, p)
class(obj)<-"wlines"
return(obj)
}

vlines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, w_min =
0, w_max = 30, nsteps = 101, p = 101.3e3){
obj = psylines(nlines, t_d_max, t_d_min, w_min, w_max, nsteps, p)
class(obj)<-"vlines"
return(obj)
}

t_dlines <- function(nlines = 10, t_d_max = 50, t_d_min = 0, w_min =
0, w_max = 30, nsteps = 101, p = 101.3e3){
obj = psylines(nlines, t_d_max, t_d_min, w_min, w_max, nsteps, p)
class(obj)<-"t_dlines"
return(obj)
}

#Set the t_d list for a psyline object. Do not use directly. Use set_list in 
#order to set the t_d and w lists at the same type based on the object class.
set_t_d_list.psylines <- function(obj){ 
    if(is.numeric(obj$t_d_max)&is.numeric(obj$t_d_min)){
    # Create a list of dry bulb temperatures for the x-axis
	obj$t_d_list = seq(obj$t_d_min,obj$t_d_max,(obj$t_d_max-obj$t_d_min)/
        (obj$nsteps-1))
    }
}

set_t_d_list <- function(obj)UseMethod("set_t_d_list",obj)

# Method to trim the lines to the bounds of the plot
trim.psylines <- function(obj){
    obj$w_list <- ifelse(obj$w_list>obj$w_max|obj$W_list>humidityratio(t_d = 
    obj$t_d_list,p = rep(obj$p,obj$n_steps),phi = rep(1,obj$n_steps)),NA,
    obj$w_list)
}

trim <- function(obj) UseMethod("trim",obj)

# set_list methods to set the lists for each type of line.
set_list.psylines <- function(obj,x)
set_list.philines <- function(obj,x){
    set_t_d_list(obj)
    obj$w_lines = humidityratio(t_d = obj$t_d_list, p =
	rep(obj$p,obj$n_steps),phi = rep(x,obj$n_steps))
    trim(obj)
}
set_list.wlines <- function(obj,x){
    set_t_d_list(obj)
    obj$w_lines = humidityratio(t_d = obj$t_d_list, p =
	rep(obj$p,obj$n_steps),w  = rep(x,obj$n_steps))
    trim(obj)
}
set_list.vlines <- function(obj,x){
    set_t_d_list(obj)
    obj$w_lines = humidityratio(t_d = obj$t_d_list, p =
	rep(obj$p,obj$n_steps),v = rep(x,obj$n_steps))
    trim(obj)
}
set_list.t_dlines <- function(obj,x){
#Note: these lines are vertical so they need some special instructions below.
    obj$t_d_list = c(x,x)
    W_s = humidityratio(t_d = x,p = obj$p,phi = 1)
    if(w_phi>obj$w_max){
    obj$w_list = c(obj$w_min,obj$w_max)
    } else 
    obj$w_list = c(obj$w_min,w_s)
}

set_list <- function(obj) UseMethod("set_list",obj)

#Function psyplot to create the psyplot class. Pass variables down to
#psylines objects. Psyplot defines all data necessary to create a 
#psychrometric plot.
#<<TO DO:>> Function Not Currently Working.
psyplot <- function(phil = philines(), wl = wlines(), vl = vlines(), t_dl = t_dlines(),main = "Psychrometric Plot", t_d_max = 50, t_d_min = 0, w_min = 0, w_max = 30, p = 101.3e3){
phil$t_d_max <- wl$t_d_max <- vl$t_d_max<- t_dl$t_d_max <- t_d_max
phil$t_d_min <- wl$t_d_min <- vl$t_d_min<- t_dl$t_d_min <- t_d_min
phil$w_max   <- wl$w_max   <- vl$w_max  <- t_dl$w_max   <- w_max
phil$w_min   <- wl$w_min   <- vl$w_min  <- t_dl$w_min   <- w_min
phil$p       <- wl$p       <- vl$p      <- t_dl$p       <- p
obj <- list(phil = phil,wl = wl,vl = vl,t_dl = t_dl)
class(obj) <- "psyplot"
return(psyplot)
}

# Temporary Testing of Alogithems: To be refined into a consolidated 
# class structure
#psyline_chart1 = psylines(t_d_max = 50, t_d_min = 0, w_min = 0, w_max = 30)

##Create New Plot
#
## Constant Relative Humidity Lines
##<<TO DO>> Add Units in the plot labels.
#
#philine = psyline_chart1
#philine$x_list  = seq(0.1,1,0.9/((philine$nlines-1)))
#t_d_plot = philine$t_d_list
#p_plot = rep(philine$p,philine$nsteps)
#phi_plot =  rep(philine$x_list[philine$nlines],philine$nsteps)
#
#
#philine$w_list = humidityratio(t_d = t_d_plot, p = p_plot,phi =phi_plot)*1000
#plot(philine$t_d_list, philine$w_list, type = "l",
#    xlim =c(psyline_chart1$t_d_min,psyline_chart1$t_d_max),
#    ylim =c(psyline_chart1$w_min,psyline_chart1$w_max),
#    axes = FALSE,
#    ylab = '',
#    xlab = "Dry Bulb Temperature",
#    main = "Psychrometric Plot",
#)
#par(mar = c(5,5,5,5))
#
#axis(4)
#axis(1)
#mtext("Humidity Ratio",side = 4, line = 3)
#
#for (i in philine$x_list){
#    phi_plot =  rep(i,philine$nsteps)
#    philine$w_list = humidityratio(t_d = philine$t_d_list, 
#	p = philine$p,phi = phi_plot)*1000
#    lines(philine$t_d_list, philine$w_list, type = "l")
#}









