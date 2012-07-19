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

SpeakerMatrix = function(m, seq) {
  return(rownames(m)[1] == rownames(seq[[1]])[1])
}

GetFinalListener = function(seq) {
  fin  = seq[[length(seq)]]
  if (SpeakerMatrix(fin, seq)) {
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

MessageConvergenceCount = function(m) {
  vals = apply(m, 1, OneHotVector)
  conv = length(vals[vals == TRUE])
  return(conv)
}

FindProgressions = function(nrow, ncol) {
  mats = AllBinaryMatrices(nrow, ncol, include.empty.cols=FALSE)
  count = 0
  for (mat in mats) {
    seq = IBR(mat)
    vals = c()
    for (m in seq) {
      if (!SpeakerMatrix(m, seq)) {
        vals = c(vals, MessageConvergenceCount(m))
      }
    }
    if (length(vals) >= (2)) {
      print("======================================================================")
      print(mat)
      print(paste('Convergences', paste(vals, collapse=', ')))
      print(paste('Depth of system', length(seq)))
      count = count + 1
    }
  }
  print(paste("total matrices found:", count))
}
    

  

