dewpoint <- function(W,p){
#Provide dewpoint based on W and p
p_w = p*W/(0.621945+W)
C14 = 6.54
C15 = 14.526
C16 = 0.7389
C17 = 0.09486
C18 = 0.4569
alpha <- log(p_w/1000)
t_dew <- C14+C15*alpha+C16*alpha^2+C17*alpha^3+C18*(p_w/1000)^0.1984
t_dew <- ifelse(t_dew<=0,6.09+12.608*alpha+0.4959*alpha^2,t_dew)
t_dew <- ifelse(t_dew<93,t_dew,NA)
return(t_dew)
}

