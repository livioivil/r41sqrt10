# # tab=table(dat[,1:2])
# # XY=which(tab!=0,arr.ind = TRUE)
# # XY=cbind(XY,tab[XY])
# # XY[,1]=as.numeric(rownames(tab)[XY[,1]])
# # XY[,2]=as.numeric(colnames(tab)[XY[,2]])
# # 
# # symbols(XY[,1:2],circles = sqrt(XY[,3]/pi), inches=0.35, fg="white", bg="red")
# # 
# 
# XY=dat[,1:2]
# cor(XY)
# ei=eigen(cor(XY))
# rot=ei$vectors%*%diag(diag(ei$vectors^.5))
# XY=XY+array(rnorm(nrow(dat)*2)/20,
#             dim(XY))%*%t(rot)
# plot(XY)
# cor(XY)
