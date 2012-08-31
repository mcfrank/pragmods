
source('createstims.R')

######################################################################
## Experiment 0

CreateExperimentalData(expt=matrix(
                         c(0,   0,   0,
                           0,   1,   0,
                           1,   1,   0), byrow=T, nrow=3),
                       outputDirname="E0",
                       outputFilename="Experiment0_data.csv",
                       n.subs=50,
                       levels=c(0,1),
                       target.referents=c(3,2),
                       distractor.referents=c(2,3), 
                       other.referents=c(1,1), 
                       target.features=c(1,2),
                       stims=c("face","snowman","sundae"),
                       stim.plurals=c("faces","snowmen","sundaes"),
                       stims.features=list(
                         '1'=c("hat","glasses","mustache"),
                         '2'=c("beanie","scarf","gloves"),
                         '3'=c("cherry","whipped cream","chocolate")),
                       stims.features.word=list(
                         '1'=c("a hat","glasses","a mustache"),
                         '2'=c("a beanie","a scarf","gloves"),
                         '3'=c("a cherry","whipped cream","chocolate sauce"))
                       )
                       
######################################################################
## Experiment 1

CreateExperimentalData(expt = matrix(
                         c(0,   0,   1,
                           0,   1,   1,
                           1,   1,   0), byrow=T, nrow=3),
                         outputDirname="E1",
                         outputFilename="Experiment1_data.csv",
                         n.subs=50,
                         levels=c(0,1,2),
                         target.referents=c(3,1,2),
                         distractor.referents=c(1,2,3),
                         other.referents=c(2,3,1),
                         target.features=c(1,3,2),
                         stims=c("face","snowman","sundae"),
                         stim.plurals=c("faces","snowmen","sundaes"),
                         stims.features=list(
                           "1"=c("hat","glasses","mustache"),
                           "2"=c("beanie","scarf","gloves"),
                           "3"=c("cherry","whipped cream","chocolate")),
                         stims.features.word=list(
                           "1"=c("a hat","glasses","a mustache"),
                           "2"=c("a beanie","a scarf","gloves"),
                           "3"=c("a cherry","whipped cream","chocolate sauce"))
                       )

######################################################################
## Experiment 2                       

CreateExperimentalData(expt=matrix(
                         c(0,   1,   1,
                           0,   1,   1,
                           1,   0,   1), byrow=T, nrow=3),
                       outputDirname="E2",
                       outputFilename="Experiment2_data.csv",
                       levels=c(0,1,2),
                       target.referents=c(3,1,1),
                       distractor.referents=c(1,2,2),
                       other.referents=c(2,3,3),
                       target.features=c(1,2,3),
                       stims=c("face","snowman","sundae"),
                       stim.plurals=c("faces","snowmen","sundaes"),
                       stims.features=list(
                         "1"=c("hat","glasses","mustache"),
                         "2"=c("beanie","scarf","gloves"),
                         "3"=c("cherry","whipped cream","chocolate")),
                       stims.features.word=list(
                         "1"=c("a hat","glasses","a mustache"),
                         "2"=c("a beanie","a scarf","gloves"),
                         "3"=c("a cherry","whipped cream","chocolate sauce"))
                       )

######################################################################
## Experiment 3

CreateExperimentalData(expt = matrix(
                         c(0,   0,   0,   1,
                           0,   1,   1,   1,
                           1,   0,   1,   1, 
                           1,   1,   1,   0), byrow=T, nrow=4),
                       outputDirname="E3",
                       outputFilename="Experiment3_data.csv",
                       levels=c(1,2,3),
                       target.referents=c(1,3,4),
                       distractor.referents=c(2,4,1),
                       other.referents=c(3,2,2),
                       target.features=c(4,1,3),
                       num.with.features=colSums(expt),
                       stims=c("face","snowman","sundae"),
                       stim.plurals=c("faces","snowmen","sundaes"),
                       stims.features=list(
                       "1"=c("hat","glasses","mustache","tie"),
                       "2"= c("beanie","scarf","gloves","belt"),
                       "3"=c("cherry","whipped cream","chocolate","banana")),
                       stims.features.word=list(
                       "1"=c("a hat","glasses","a mustache","a tie"),
                       "2"=c("a beanie","a scarf","gloves","a belt"),
                       "3"=c("a cherry","whipped cream","chocolate sauce","a banana"))
                       )
