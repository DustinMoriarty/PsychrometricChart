#TITLE: MAS                              # 
#AUTHOR:                                 #  
#DATE: December 8, 2015                  # 
##########################################

source("mas.r")

patm = 101.1e3 #Pa

#State Variables as Single Points
A = mas(t_d=21.9,t_w=8.9 ,p=patm) 
B = mas(t_d=29.6,t_w=11.3,p=patm)
C = mas(t_d=15.3,t_w=5.6 ,p=patm)
D = mas(t_d=21.2,t_w=8.6 ,p=patm)

print("scalar use of mas")
print("A$W =")
print(A$W)
print("B$W = ")
print(B$W)
print("C$W = ")
print(C$W)
print("D$W = ")
print(D$W)


#State Variables as Vectors
t_d = c(21.9,29.6,15.3,21.2)
t_w = c(8.9, 11.3, 5.6, 8.6)
cycle = mas(t_d=t_d, t_w=t_w, p=patm)

print("use with vectors")
print("t_d = ")
print(t_d)
print("cycle$W")
print(cycle$W)


source("psyplot.r")
plot.psyplot(psyplot())
