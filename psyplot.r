source("mas.r")

psylines <- function(nlines = 10, t_d_max = null, t_d_min = null, w_min =
    #Class for the creation of lines on the plot.
null, w_max = null, nsteps = 100){
obj <- list(nlines = nlines,
    t_d_max= t_d_max,
    t_d_min= t_d_min,
    w_max  = w_max,
    w_min  = w_min,
    nsteps = nsteps)
    class(obj) <- "psylines"
return(obj)
}

psyline_chart1 = psylines(t_d_max = 50, t_d_min = 0, w_min = 0, w_max = 30)

# Constant Relative Humidity Lines



