#!/usr/bin/env R

CheckFgCorrespondence = function() {
  mats3 = AllBinaryMatrices(3, 3, include.empty.cols=FALSE)
  for (mat in mats3) {
    fg = Lbayes(SurprisalSpeaker(mat), mat)
    fg = round(fg, 2)
    ibr = Lbayes(S(Lbayes(S0(mat), mat), mat), mat)
    ibr = round(ibr, 2)
    eq = MatrixEquality(fg, ibr)
    if(!eq) {
      print("======================================================================")
      print(mat)
      print("----------------------------------------")
      print('IBR')
      print(ibr)
      print("----------------------------------------")
      print('FG')
      print(fg)
    }
  }
}

GetFinalListener = function(seq) {
  fin  = seq[[length(seq)]]
  if (rownames(fin)[1] == rownames(seq[[1]])[1]) {
    fin = seq[[length(seq)-1]]
  }
  return(fin)
}


CheckArgmax = function() {
  mats3 = AllBinaryMatrices(3, 3, include.empty.cols=FALSE)
  for (mat in mats3) {
    star = IBR(mat)
    star = GetFinalListener(star)
    sur = SurprisalIBR(mat)
    sur = GetFinalListener(sur)
    eq = MatrixEquality(star, sur)
    if(!eq) {
      print("======================================================================")
      print(mat)
      print("----------------------------------------")
      print('IBR')
      print(star)
      print("----------------------------------------")
      print('SurprisalIBR')
      print(sur)
    }
  }
}


ViewMaxModelDepth = function(nrow, ncol, model='IBR') {
  df = ModelDepths(nrow, ncol, models=model)
  df$depth = df[, paste(model, 'Depth', sep='')]
  mx = subset(df, depth==max(depth))
  print(nrow(mx))
  for(i in 1:nrow(mx)) {
    print("======================================================================")
    print(StudyModelDepthsRow(mx[i, ]))
  }
}

    
    

  

