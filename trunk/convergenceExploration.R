
mats = AllBinaryMatrices(3, 3, 
                         include.empty.rows=FALSE, include.empty.cols=FALSE)

mat = mats[[23]]
rownames(mat) = c("left","middle","right")
colnames(mat) = c("tie","hat","mustache")
FindMatrixConvergence(mat,verbose=T)




FindMatrixConvergence(stiller.noscales,verbose=T)
FindMatrixConvergence(stiller.scales,verbose=T)
FindMatrixConvergence(stiller.noscales,verbose=T)