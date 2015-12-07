source("MAS.r")

patm = 101.1e3 #Pa

#State Variables as Single Points
A = mas(21.9,8.9,patm) 
B = mas(29.6,11.3,patm)
C = mas(15.3,5.6,patm)
D = mas(21.2,8.6,patm);

#State Variables as Vectors
t_d = c(21.9,29.6,15.3,21.2)
t_w = c(8.9, 11.3, 5.6, 8.6)
cycle = mas(t_d, t_w, patm)
