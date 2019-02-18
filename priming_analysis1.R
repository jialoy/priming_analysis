##################################################
### script to analyse priming exp 1 ##############
##################################################

source('~/Desktop/priming/priming_main.R')

#########################################
### processing the data #################
#########################################

# read in data
setwd('~/Desktop/priming/p1_data/')
files <- list.files(path=getwd(), pattern="_p.csv")
data <- do.call(rbind, lapply(files, read.csv, stringsAsFactors=F))
data[, unlist(lapply(data, is.character))] <- lapply(data[, unlist(lapply(data, is.character))], factor)
data[,c(2,21)] <- lapply(data[,c(2,21)], factor)

# remove replaced participants
data.p1 <- data[!data$subjectNo %in% c(8,14,18,24,28,33),]

data.p1$X <- NULL

# rename some columns (to facilitate inter-experiment comparison)
rename(data.p1, 'primeType') <- 'primeConstruction'
rename(data.p1, 'targetVerbType') <- 'targetVerbPar'

# subset director and matcher trials
data.p1.dir <- filter(data.p1, role=='director')
data.p1.match <- filter(data.p1, role=='matcher')

data.p1.dir$trialNoNew <- as.numeric(gsub('D', '', data.p1.dir$trialNo))
data.p1.dir <- data.p1.dir[,c(1:2,23,3:22)]
data.p1.match$trialNoNew <- as.numeric(gsub('M', '', data.p1.match$trialNo))
data.p1.match <- data.p1.match[,c(1:2,23,3:22)]

# code DO utterances as 1, PO utteraces as 0, ? as NA
data.p1.dir$isDO <- ifelse(data.p1.dir$utteranceType=='?', 'NA',
                             ifelse(data.p1.dir$utteranceType=='DO', 1, 0))
data.p1.dir$isDO <- as.numeric(data.p1.dir$isDO)
data.p1.dir$isValid <- ifelse(data.p1.dir$utteranceType=='?', 0, 1)

data.p1.match$isValid <- rep(1, nrow(data.p1.match))

# subset critical trials
data.p1.dir.valid <- data.p1.dir[data.p1.dir$trialType=='critical' & data.p1.dir$isValid==1,]

data.p1.match.valid <- data.p1.match[data.p1.match$trialType=='critical',]

# mean centre variables for model fitting

data.p1.dir.valid %<>%
  mutate(confederate = forcats::fct_relevel(confederate, c('native', 'nonnative')),
         primeConstruction = forcats::fct_relevel(primeConstruction, c('PO', 'DO')),
         targetVerbPar = forcats::fct_relevel(targetVerbPar, c('SV', 'DV'))) %>%
  mutate_at(vars(primeConstruction,targetVerbPar,confederate), .funs = funs(cod=simple_scale(.))) 

data.p1.match.valid %<>%
  mutate(confederate = forcats::fct_relevel(confederate, c('native', 'nonnative')),
         primeConstruction = forcats::fct_relevel(primeConstruction, c('PO', 'DO')),
         targetVerbPar = forcats::fct_relevel(targetVerbPar, c('SV', 'DV'))) %>%
  mutate_at(vars(primeConstruction,targetVerbPar,confederate), .funs = funs(cod=simple_scale(.)))


#########################################
### descriptive stats ###################
#########################################

data.p1.dir.valid %>%
  group_by(subjectNo, primeConstruction, targetVerbPar, confederate) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"pct") %>%
  group_by(primeConstruction, targetVerbPar, confederate) %>%
  summarise_each(funs(mean, se), pct)


data.p1.match.valid %>%
  group_by(subjectNo, primeConstruction, targetVerbPar, confederate) %>%
  summarise(sum(matchCorr)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"pct") %>%
  group_by(primeConstruction, targetVerbPar, confederate) %>%
  summarise_each(funs(mean, se), pct)


#########################################
### produce some plots ##################
#########################################

## df to use for plotting dir data
data.p1.dir.plot <- data.p1.dir.valid %>%
  mutate(subjectNo=factor(subjectNo)) %>%
  group_by(subjectNo,confederate,primeConstruction,targetVerbPar) %>%
  rename_at(vars(c("primeConstruction","targetVerbPar")), ~c("condition","colour_by")) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>% 
  # create a couple of columns with width and experiment variables because plotting function uses it
  mutate(w=0.8) %>% 
  mutate(exp=factor('Exp 1')) %>%
  rename_at(vars(starts_with("sum")), ~"percentage_y")
  
# bar plots
data.p1.dir.plot %>% plotCPBar(T, F) + 
  xlab("Prime construction") + ylab("Percentage of DO descriptions produced")
  
# dot plots
data.p1.dir.plot %>% plotCPDot(T) + 
  xlab("Prime construction") + ylab("Percentage of DO descriptions produced")

## df to use for plotting match data
data.p1.match.plot <- data.p1.match.valid %>%
  mutate(subjectNo=factor(subjectNo)) %>%
  group_by(subjectNo,confederate,primeConstruction,targetVerbPar) %>%
  summarise(sum(matchCorr)/sum(isValid)*100) %>% 
  # create a couple of columns with width and experiment variables because plotting function uses it
  mutate(w=0.8) %>% 
  mutate(exp=factor('Exp 1')) %>%
  rename_at(vars(c("primeConstruction", "targetVerbPar",starts_with('sum'))), ~c("condition", "colour_by",'percentage_y'))

# dot plots
data.p1.match.plot %>% plotCPDot(T) +
  xlab("Prime construction") + ylab("Percentage of correct responses") +
  ylim(0,100)


#########################################
### construct models ####################
#########################################

## sanity check - is there an effect of confederateID on DO production?

data.p1.dir.valid %>%
  group_by(subjectNo, confederateID) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"percentage_DO") %>%
  ggplot() +
  aes(x=confederateID, y=percentage_DO) +
  stat_summary(fun.y=mean, geom='point', position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.3)

with(data.p1.dir.valid, glmer(isDO~confederateID+(1|subjectNo), 
                                  family='binomial',
                                  control=glmerControl(optimizer=c("bobyqa")),
                                  contrasts=list(confederateID=contr.sum))) %>% summary()
  

## main analysis
## likelihood of producing DO given primeConstruction (PO/DO), targetVerbPar (same/diff) and confederate (N/NN)

with(data.p1.dir.valid, glmer(isDO~primeConstruction_cod*targetVerbPar_cod*confederate+
                                    (1+primeConstruction+targetVerbPar||subjectNo)+
                                    (1|targetVerb) +
                                    (1|confederateID), 
                                  family='binomial',
                                  control=glmerControl(optimizer=c("bobyqa")))) %>% summary()

## analysis for match trials

with(data.p1.match.valid, glmer(matchCorr~primeConstruction_cod*targetVerbPar_cod*confederate+
                                      (1+primeConstruction+targetVerbPar|subjectNo)+
                                      (1|targetVerb)+
                                      (1|confederateID), 
                                    family='binomial', 
                                    control=glmerControl(optimizer=c('bobyqa')))) %>% summary()
