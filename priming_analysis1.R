##################################################
### script to analyse priming exp 1 ##############
##################################################

source('~/Desktop/priming/priming_main.R')

#########################################
### processing the data #################
#########################################

# read in data
setwd('~/Desktop/priming/P1/data/transcribed/')
setwd('~/Desktop/priming/P3/data/transcribed/')
files <- list.files(path=getwd(), pattern="_p.csv")
data <- do.call(rbind, lapply(files, read.csv, stringsAsFactors=F))
data[, unlist(lapply(data, is.character))] <- lapply(data[, unlist(lapply(data, is.character))], factor)
data[,c(2,21)] <- lapply(data[,c(2,21)], factor)

# remove replaced participants (exp 1)
if (grepl('P1', getwd())) {
  data <- data[!data$subjectNo %in% c(8,14,18,24,28,33),] 
} else if (grepl('P3', getwd())) {
  data <- data[!data$subjectNo %in% c(16,17),]
}

data$X <- NULL

# rename some columns (to facilitate inter-experiment comparison)
rename(data, 'primeType') <- 'primeConstruction'
rename(data, 'targetVerbType') <- 'targetVerbPar'

# subset director and matcher trials
data.dir <- filter(data, role=='director')
data.match <- filter(data, role=='matcher')

data.dir$trialNoNew <- as.numeric(gsub('D', '', data.dir$trialNo))
data..dir <- data.dir[,c(1:2,23,3:22)]
data.match$trialNoNew <- as.numeric(gsub('M', '', data.match$trialNo))
data.match <- data.match[,c(1:2,23,3:22)]

# code DO utterances as 1, PO utteraces as 0, ? as NA
data.dir$isDO <- ifelse(data.dir$utteranceType=='?', 'NA',
                             ifelse(data.dir$utteranceType=='DO', 1, 0))
data.dir$isDO <- as.numeric(data.dir$isDO)
data.dir$isValid <- ifelse(data.dir$utteranceType=='?', 0, 1)

data.match$isValid <- rep(1, nrow(data.match))

# subset critical trials
data.dir.valid <- data.dir[data.dir$trialType=='critical' & data.dir$isValid==1,]

data.match.valid <- data.match[data.match$trialType=='critical',]

# mean centre variables for model fitting

data.dir.valid %<>%
  mutate(confederate = forcats::fct_relevel(confederate, c('native', 'nonnative')),
         primeConstruction = forcats::fct_relevel(primeConstruction, c('PO', 'DO')),
         targetVerbPar = forcats::fct_relevel(targetVerbPar, c('SV', 'DV'))) %>%
  mutate_at(vars(primeConstruction,targetVerbPar,confederate), .funs = funs(cod=simple_scale(.))) 

data.match.valid %<>%
  mutate(confederate = forcats::fct_relevel(confederate, c('native', 'nonnative')),
         primeConstruction = forcats::fct_relevel(primeConstruction, c('PO', 'DO')),
         targetVerbPar = forcats::fct_relevel(targetVerbPar, c('SV', 'DV'))) %>%
  mutate_at(vars(primeConstruction,targetVerbPar,confederate), .funs = funs(cod=simple_scale(.)))


#########################################
### descriptive stats ###################
#########################################

data.dir.valid %>%
  group_by(subjectNo, primeConstruction, targetVerbPar, confederate) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"pct") %>%
  group_by(confederate, primeConstruction, targetVerbPar) %>%
  summarise_each(funs(mean, se), pct)


data.match.valid %>%
  group_by(subjectNo, primeConstruction, targetVerbPar, confederate) %>%
  summarise(sum(matchCorr)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"pct") %>%
  group_by(confederate, primeConstruction, targetVerbPar) %>%
  summarise_each(funs(mean, se), pct)


#########################################
### produce some plots ##################
#########################################

## df to use for plotting dir data
data.dir.plot <- data.dir.valid %>%
  mutate(subjectNo=factor(subjectNo)) %>%
  #mutate(primeConstruction=recode(primeConstruction, PO="Prepositional Object", DO="Double Object")) %>%
  group_by(subjectNo,confederate,primeConstruction,targetVerbPar) %>%
  rename_at(vars(c("primeConstruction","targetVerbPar")), ~c("condition","colour_by")) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>% 
  # create a couple of columns with width and experiment variables because plotting function uses it
  mutate(w=0.8) %>% 
  mutate(exp=factor('Exp 1')) %>%
  rename_at(vars(starts_with("sum")), ~"percentage_y")
  
# bar plots
data.dir.plot %>% plotCPBar(T, T) + 
  xlab("Prime construction") + ylab("Percentage of DO descriptions produced")
  
# dot plots
data.dir.plot %>% plotCPDot(T) + 
  xlab("Prime construction") + ylab("Percentage of DO descriptions produced")

## df to use for plotting match data
data.match.plot <- data.match.valid %>%
  mutate(subjectNo=factor(subjectNo)) %>%
  group_by(subjectNo,confederate,primeConstruction,targetVerbPar) %>%
  summarise(sum(matchCorr)/sum(isValid)*100) %>% 
  # create a couple of columns with width and experiment variables because plotting function uses it
  mutate(w=0.8) %>% 
  mutate(exp=factor('Exp 1')) %>%
  rename_at(vars(c("primeConstruction", "targetVerbPar",starts_with('sum'))), ~c("condition", "colour_by",'percentage_y'))

# dot plots
data.match.plot %>% plotCPDot(T) +
  xlab("Prime construction") + ylab("Percentage of correct responses") +
  #ylim(0,100) +
  NULL


#########################################
### construct models ####################
#########################################

## sanity check - is there an effect of confederateID on DO production?

data.dir.valid %>%
  group_by(subjectNo, confederateID) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"percentage_DO") %>%
  ggplot() +
  aes(x=confederateID, y=percentage_DO) +
  stat_summary(fun.y=mean, geom='point', position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.3)

with(data.dir.valid, glmer(isDO~confederateID+(1|subjectNo), 
                                  family='binomial',
                                  control=glmerControl(optimizer=c("bobyqa")),
                                  contrasts=list(confederateID=contr.sum))) %>% summary()

# by confederate nativeness
for (l in levels(data.dir.valid$confederate)){
  print(summary(glmer(isDO~confederateID+
                        (1|subjectNo), 
                      filter(data.p1.dir.valid, confederate==l),
                      family='binomial',
                      control=glmerControl(optimizer=c("bobyqa")),
                      contrasts=list(confederateID=contr.sum)) ))
}

## main analysis
## likelihood of producing DO given primeConstruction (PO/DO), targetVerbPar (same/diff) and confederate (N/NN)

with(data.dir.valid, glmer(isDO~primeConstruction_cod*targetVerbPar_cod*confederate+
                                    (1+primeConstruction+targetVerbPar||subjectNo)+
                                    (1|targetVerb) +
                                    (1|confederateID), 
                                  family='binomial',
                                  control=glmerControl(optimizer=c("bobyqa")))) %>% summary()

## analysis for match trials

with(data.match.valid, glmer(matchCorr~primeConstruction_cod*targetVerbPar_cod*confederate+
                                      (1+primeConstruction+targetVerbPar|subjectNo)+
                                      (1|targetVerb)+
                                      (1|confederateID), 
                                    family='binomial', 
                                    control=glmerControl(optimizer=c('bobyqa')))) %>% summary()
