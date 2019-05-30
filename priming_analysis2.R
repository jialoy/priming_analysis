##################################################
### script to analyse priming exp 2 ##############
##################################################

source('~/Desktop/priming/priming_main.R')
## also uses: forcats::fct_relevel(), ggpubr::asggplot() and tidyr::unite()

#########################################
### processing the data #################
#########################################

setwd('~/Desktop/priming/P2/data/transcribed')
datafiles = list.files(pattern="*.csv")
data = do.call(rbind, lapply(datafiles, read.csv))

data.p2.dir <- filter(data, role=='director')
data.p2.match <- filter(data, role=='matcher')

# counter for all utterances (PO/DO/o) produced
data.p2.dir$counter <- rep(1, nrow(data.p2.dir))

data.p2.match$isValid <- rep(1, nrow(data.p2.match))

# counter for all valid (PO/DO) utterances produced
data.p2.dir$isValid <- ifelse(data.p2.dir$utteranceType %in% c('PO', 'DO', 'o'), 
                             ifelse(data.p2.dir$utteranceType %in% c('PO', 'DO'), 1, 0), -1)

# counter for all DO utterances produced
data.p2.dir$isDO <- ifelse(data.p2.dir$utteranceType=='DO', 1, 0)

# subset dataframe for valid (coded as PO or DO) utterances
data.p2.dir.valid <- data.p2.dir %>%
  filter(isValid==1 & trialType=='critical')

data.p2.match.valid <- data.p2.match %>% filter(trialType=='critical')

## code variables - mean centre primeType, targetVerbType, targetVerbPar; set native to confederate reference level

data.p2.dir.valid %<>%
  mutate(confederate = forcats::fct_relevel(confederate, c('native', 'nonnative')),
         confederateID = forcats::fct_relevel(confederateID, c('CN1', 'CN2', 'CNN1', 'CNN2')),
         targetVerbPar = forcats::fct_relevel(targetVerbPar, c('SV', 'DV'))) %>%
  mutate_at(vars(primeType,targetVerbType,targetVerbPar,confederate), .funs = funs(cod=simple_scale(.)))

data.p2.dir %<>%
  mutate(confederate = forcats::fct_relevel(confederate, c('native', 'nonnative')),
         confederateID = forcats::fct_relevel(confederateID, c('CN1', 'CN2', 'CNN1', 'CNN2')),
         targetVerbPar = forcats::fct_relevel(targetVerbPar, c('SV', 'DV'))) %>%
  mutate_at(vars(primeType,targetVerbType,targetVerbPar,confederate), .funs = funs(cod=simple_scale(.))) 

data.p2.match.valid %<>%
  mutate(confederate = forcats::fct_relevel(confederate, c('native', 'nonnative')),
         targetVerbPar = forcats::fct_relevel(targetVerbPar, c('SV', 'DV'))) %>%
  mutate_at(vars(primeType,targetVerbType,targetVerbPar,confederate), .funs = funs(cod=simple_scale(.)))

#########################################
### descriptive stats ###################
#########################################

## mean pct of DO utterances produced by condition
data.p2.dir.valid %>%
  group_by(subjectNo, primeType, targetVerbType, targetVerbPar, confederate) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"pct") %>%
  group_by(primeType, targetVerbType, targetVerbPar, confederate) %>%
  summarise_each(funs(mean, se), pct)

## count by variable
data.p2.dir.valid %>%
  #group_by(subjectNo) %>%
  group_by(targetVerbType, confederate) %>%
  tally(isDO==1)

data.p2.dir.valid %>%
  group_by(confederate) %>%
  summarise(n=n_distinct(subjectNo))

## mean pct of correct match responses by condition
data.p2.match.valid %>%
  group_by(subjectNo, primeType, targetVerbType, targetVerbPar, confederate) %>%
  summarise(sum(matchCorr)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"pct") %>%
  group_by(primeType, targetVerbType, targetVerbPar, confederate) %>%
  summarise_each(funs(mean, se), pct)

#########################################
### produce some plots ##################
#########################################

## df to use for plotting dir data
data.p2.dir.plot <- data.p2.dir.valid %>%
  mutate(subjectNo=factor(subjectNo)) %>%
  mutate(targetVerbPar=forcats::fct_relevel(targetVerbPar, c('SV', 'DV'))) %>%
  mutate(confederate=forcats::fct_relevel(confederate, c('native', 'nonnative'))) %>%
  mutate(primeType=factor(primeType, labels=c("Alt", "Non-alt"))) %>%
  mutate(targetVerbType=factor(targetVerbType, labels=c("Alt", "Non-alt"))) %>%
  group_by(subjectNo,confederate,primeType,targetVerbType,targetVerbPar) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>% 
  rename("colour_by"=targetVerbPar) %>%
  rename_at(vars(starts_with("sum")), ~"percentage_y") %>%
  tidyr::unite(condition, primeType:targetVerbType, sep=" \u2013 ") %>% 
  mutate_at(vars(condition), funs(factor)) %>% 
  mutate(condition = forcats::fct_relevel(condition, 
                                          c("Alt \u2013 Alt","Alt \u2013 Non-alt","Non-alt \u2013 Alt","Non-alt \u2013 Non-alt"))) %>%
  mutate(w = ifelse(condition %in% c('Alt \u2013 Alt', 'Non-alt \u2013 Non-alt'),0.8,0.4)) %>%
  mutate(exp=factor('Exp 2'))  # create a column with exp variable because plotting function uses it

# bar plots
data.p2.dir.plot %>% plotCPBar(F,F) +
  xlab("Prime \u2013 target construction") + ylab("Percentage of DO descriptions produced")

# dot plots
data.p2.dir.plot %>% plotCPDot(T) + 
  xlab("\nPrime \u2013 target construction") + ylab("Percentage of DO descriptions produced")

## df to use for plotting match data

data.p2.match.plot <- data.p2.match.valid %>%
  mutate(subjectNo=factor(subjectNo)) %>%
  mutate(targetVerbPar=forcats::fct_relevel(targetVerbPar, c('SV', 'DV'))) %>%
  mutate(confederate=forcats::fct_relevel(confederate, c('native', 'nonnative'))) %>%
  mutate(primeType=factor(primeType, labels=c("Alt", "Non-alt"))) %>%
  mutate(targetVerbType=factor(targetVerbType, labels=c("Alt", "Non-alt"))) %>%
  group_by(subjectNo,confederate,primeType,targetVerbType,targetVerbPar) %>%
  summarise(sum(matchCorr)/sum(isValid)*100) %>% 
  tidyr::unite(condition, primeType:targetVerbType, sep=" \u2013 ") %>% 
  mutate_at(vars(condition), funs(factor)) %>% 
  mutate(condition = forcats::fct_relevel(condition, 
                                          c("Alt \u2013 Alt","Alt \u2013 Non-alt","Non-alt \u2013 Alt","Non-alt \u2013 Non-alt"))) %>%
  mutate(w = ifelse(condition %in% c('Alt \u2013 Alt', 'Non-alt \u2013 Non-alt'),0.8,0.4)) %>%
  mutate(exp=factor('Exp 2')) %>%
  rename_at(vars(c(targetVerbPar,starts_with("sum"))), ~c("colour_by","percentage_y"))

# dot plots
data.p2.match.plot %>% plotCPDot(F) + 
  xlab("\nPrime-target construction") + ylab("Percentage of correct responses") +
  #ylim(50,100) +
  NULL

###################################################################
# some other ways of visualising (not sure if these are better...)

data.p2.dir.plot %>% 
  ggplot() +
  aes(x=targetVerbPar, y=percentage_DO, fill=primeType) +
  stat_summary(fun.y=mean, geom='bar', position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.3) +
  facet_wrap(~confederate)

data.p2.dir.plot %>%
  ggplot() +
  aes(x=confederate, y=percentage_DO, fill=primeType) +
  stat_summary(fun.y=mean, geom='bar', position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.3) +
  scale_fill_manual(values=c('royalblue2', 'seagreen3')) +
  facet_grid(~primeType+targetVerbPar)


data.p2.dir.valid %>%
  mutate(subjectNo=factor(subjectNo)) %>%
  group_by(subjectNo,confederate,primeType,targetVerbType,targetVerbPar) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>%
  data.frame() %>%
  rename_(percentage_DO = names(.)[6]) %>%
  mutate(condition = factor(paste(primeType, paste(targetVerbType, targetVerbPar, sep='-'), sep='-'))) %>%
  mutate(condition = forcats::fct_relevel(condition, 
                                          c("alt-alt-SV","alt-alt-DV","alt-non-DV","non-non-SV","non-non-DV","non-alt-DV"))) %>%
  ggplot() +
  aes(x=condition, y=percentage_DO, fill=condition) +
  stat_summary(fun.y=mean, geom='bar', position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.3) +
  facet_wrap(~confederate)


#########################################
### construct models ####################
#########################################

## sanity check - is there an effect of confederateID on DO production?

data.p2.dir.valid %>%
  group_by(subjectNo, confederateID) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"percentage_DO") %>%
  ggplot() +
  aes(x=confederateID, y=percentage_DO) +
  stat_summary(fun.y=mean, geom='point', position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.3)

with(data.p2.dir.valid,
     glmer(as.factor(isDO)~confederateID+
             (1|subjectNo),
           family='binomial',
           contrasts=list(confederateID=contr.treatment))) %>% summary()


## looks like people are producing way more DOs with CNN1, yikes.

## main analysis
## likelihood of producing DO given primeType (alt/non), targetVerbType (alt/non), targetVerbPar (same/diff) and confederate (N/NN)
## primeType, targetVerbType, targetVerbPar are mean centred, confederate is dummy coded with native as baseline

with(data.p2.dir.valid, 
  glmer(as.factor(isDO)~primeType_cod*targetVerbType_cod*targetVerbPar_cod*confederate+
                                  (1+primeType+targetVerbType+targetVerbPar|subjectNo)+
                                  (1|confederateID)+
                                  (1|targetVerb), 
                                family='binomial',
                                control=glmerControl(optimizer=c("bobyqa")))) %>% summary()


# three way interaction:
data.p2.dir.valid %>%
  group_by(primeType) %>%
  tally(isDO==1)
#-> less likely to produce a DO description following non-A (ungrammatical) primes
#--> i.e. less ungrammatical priming

data.p2.dir.valid %>%
  group_by(primeType,targetVerbPar) %>%
  tally(isDO==1) %>%
  tidyr::spread(primeType,n) %>%
  mutate(count=non-alt)
#-> decreased likelihood of producing a DO description following an ungrammatical prime is smaller when prime and target verb are different (DV)
#--> i.e. less ungrammatical different-verb priming

data.p2.dir.valid %>%
  group_by(primeType,targetVerbPar,confederate) %>%
  tally(isDO==1)
#-> decreased likelihood of producing a DO description following an ungrammatical prime in DV condition is greater 
# when interacting with a nonnative compared to native confederate
data.p2.dir.valid %>%
  group_by(primeType,targetVerbPar,confederate) %>%
  tally(isDO==1) %>%
  tidyr::spread(primeType,n) %>%
  mutate(count=non-alt)
# final column (count) is the decrease in amount of DO target descriptions 
# following an ungrammatical (compared to grammatical) prime

# plot random effects to see if anything weird's going on
with(data.p2.dir.valid, 
     glmer(as.factor(isDO)~primeType_cod*targetVerbType_cod*targetVerbPar_cod*confederate+
             (1+primeType_cod+targetVerbType_cod+targetVerbPar_cod|subjectNo)+
             (1|confederateID)+
             (1|targetVerb), 
           family='binomial',
           control=glmerControl(optimizer=c("bobyqa")))) %>%
  ranef() %>%
  lattice::dotplot(condVar=TRUE)


## analysis for match trials

# note that a more complex model structure does not converge
with(data.p2.match.valid, glmer(matchCorr~primeType_cod*targetVerbType_cod*confederate+
                                  (1|subjectNo), 
                                family='binomial', 
                                control=glmerControl(optimizer=c('bobyqa')))) %>% summary()





