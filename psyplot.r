source("mas.r")

psylines <- function(nlines = 10, t_d_max = NULL, t_d_min = NULL, w_min =
NULL, w_max = NULL, nsteps = 101, p = 101.3e3){
    #Class for the creation of lines on the plot.
obj <- list(nlines = nlines,
    t_d_max= t_d_max,
    t_d_min= t_d_min,
    w_max  = w_max,
    w_min  = w_min,
    p = p,
    nsteps = nsteps,
    x_list = NULL,
    t_d_list = NULL,
    w_list = NULL
    )
    

    if(is.numeric(obj$t_d_max)&is.numeric(obj$t_d_min)){
	obj$t_d_list =
	seq(obj$t_d_min,obj$t_d_max,(obj$t_d_max-obj$t_d_min)/(obj$nsteps-1))
    }

    class(obj) <- "psylines"
return(obj)
}

psyline_chart1 = psylines(t_d_max = 50, t_d_min = 0, w_min = 0, w_max = 30)

#Create New Plot

# Constant Relative Humidity Lines
#<<TO DO>> Add Units in the plot labels.

philine = psyline_chart1
philine$x_list  = seq(0.1,1,0.9/((philine$nlines-1)))
t_d_plot = philine$t_d_list
p_plot = rep(philine$p,philine$nsteps)
phi_plot =  rep(philine$x_list[philine$nlines],philine$nsteps)


philine$w_list = humidityratio(t_d = t_d_plot, p = p_plot,phi =phi_plot)*1000
plot(philine$t_d_list, philine$w_list, type = "l",
    xlim =c(psyline_chart1$t_d_min,psyline_chart1$t_d_max),
    ylim =c(psyline_chart1$w_min,psyline_chart1$w_max),
    axes = FALSE,
    ylab = '',
    xlab = "Dry Bulb Temperature",
    main = "Psychrometric Plot",
)
par(mar = c(5,5,5,5))

axis(4)
axis(1)
mtext("Humidity Ratio",side = 4, line = 3)

for (i in philine$x_list){
    phi_plot =  rep(i,philine$nsteps)
    philine$w_list = humidityratio(t_d = philine$t_d_list, 
	p = philine$p,phi = phi_plot)*1000
    lines(philine$t_d_list, philine$w_list, type = "l")
}









