library(lpSolve)

#part (a)

# Build a data frame
m0=c('Distance to SR 1', 'Distance to SR 2', 'Distance to SR 3', 'Distance to SR 4', 'Workload')
m1=c(16.16, 24.08, 24.32, 21.12, 0.1609)
m2=c(19, 26.47, 27.24, 17.33, 0.1164)
m3=c(25.29, 32.49, 33.42, 12.25, 0.1026)
m4=c(0, 7.93, 8.31, 36.12, 0.1516)
m5=c(3.07, 6.44, 7.56, 37.37, 0.0939)
m6=c(1.22, 7.51, 8.19, 36.29, 0.132)
m7=c(2.8, 10.31, 10.95, 33.5, 0.0687)
m8=c(2.87, 5.07, 5.67, 38.8, 0.093)
m9=c(3.8, 8.01, 7.41, 38.16, 0.2116)
m10=c(12.35, 4.52, 4.35, 48.27, 0.2529)
m11=c(11.11, 3.48, 2.97, 47.14, 0.0868)
m12=c(21.99, 22.02, 24.07, 39.86, 0.0828)
m13=c(8.82, 3.3, 5.36, 43.31, 0.0975)
m14=c(7.93, 0, 2.07, 43.75, 0.8177)
m15=c(9.34, 2.25,1.11, 45.43, 0.4115)
m16=c(8.31, 2.07, 0, 44.43, 0.3795)
m17=c(7.31, 2.44, 1.11, 43.43, 0.071)
m18=c(7.55, 0.75, 1.53, 43.52, 0.0427)
m19=c(11.13, 18.41, 19.26, 25.4, 0.1043)
m20=c(17.49, 23.44, 24.76, 23.21, 0.0997)
m21=c(11.03, 18.93, 19.28, 25.43, 0.1698)
m22=c(36.12, 43.75, 44.43, 0, 0.2531)

sr_matrix = matrix(c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22),nrow=22,byrow=T)

SR = as.data.frame(sr_matrix)
names(SR) = m0

#first find object function matrix
ob=c()
for (j in 1:22){
  ob=c(ob, as.vector(as.matrix(SR[j,1:4])))
}


#second,find constraints matrix

#SR assignment constraint
cm1 = matrix(0,22,88)
for(i in 1:22){
  cm1[i,((4*i-3):(4*i))]=1
}

# workload constraint
workload = c()
for(t in 1:4){
  wl = rep(0,88)
  for(m in 1:22){
    wl[4*m-4+t] = (as.vector(SR$Workload))[m] 
  }
  workload=c(workload,wl)
}

WM1 = matrix(workload,nrow = 4,byrow = T)
WM2 = matrix(workload,nrow = 4,byrow = T)

mat_final = rbind(cm1,WM1,WM2)

#then, find direction vector
direction_vector=rep("<=",time=30)
direction_vector[1:22]="=="
direction_vector[23:26]=">="


#Find rhs
r=rep(1,time=30)
r[23:26]=0.8
r[27:30]=1.2

#Solve the model
model = lp (direction = "min", objective.in = ob, const.mat=mat_final, const.dir=direction_vector, const.rhs=r, binary.vec=1:88)

#Display the solution
matrix(model$solution,nrow=22,byrow=T)

#Part (b)

rnew = rep(1,time=30)
rnew[23:26]=0.9
rnew[27:30]=1.1

#Solve the model
modelnew = lp (direction = "min", objective.in = ob, const.mat=mat_final, const.dir=direction_vector, const.rhs=rnew, binary.vec=1:88)

#Display the solution
matrix(modelnew$solution,nrow=22,byrow=T)

