source("humidityratio.r")

tempwetbulb = function(t_d,p,phi=NULL,W=NULL){
    #Numeric Solution for Dry Bulb Temperature

    #Validate and preprocess arguments
    if(length(p)==1&length(t_d)>1){
        p<-rep(p,length(t_d))
    }

    if(is.null(phi)&is.numeric(W)){
        if(length(t_d)!=length(W)|(length(t_d)!=length(p)&length(p)!=1)){
            stop("arguments must be the same length")
        }
        W_0 = W
    }
    else if(is.numeric(phi)&is.null(W)){
        if(length(t_d)!=length(phi)|(length(t_d)!=length(p)&length(p)!=1)){
            stop("arguments must be the same length")
        }
        W_0 = humidityratio(t_d=t_d,phi=phi,p=p)
    }
    test = function(t_w,t_d,p,W_0){
        return(W_0-humidityratio(t_d = t_d,t_w=t_w, p=p))
    }

    #Solve for t_w for all values of t_w
    t_w = rep(NA,length(t_d))
    
    for (i in 1:length(t_d)){
        try({t_w[i] = uniroot(test,c(0,t_d[i]+1),t_d=t_d[i],p=p[i],W_0=W_0[i])$root},silent=TRUE)
    }
        
    return(t_w) 
}
