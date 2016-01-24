source('psyplot.r')

obj = hlines()
obj = set_t_d_list(obj)
statemin = mas(W = obj$w_min,t_d = obj$t_d_min,p = obj$p)
statemax = mas(W = obj$w_max,t_d = obj$t_d_max,p = obj$p)
obj$x_min = statemin$h
obj$x_max = statemax$h
obj$x_list = seq(obj$x_min,obj$x_max,(obj$x_max-obj$x_min)/(obj$nlines-1))
x = obj$x_list[4]
obj$w_list = humidityratio(t_d=obj$t_d_list, h=rep(x,obj$nsteps))
#obj=trim(obj)



