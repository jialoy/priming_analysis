###############################################
### load relevant libraries ###################
###############################################

source('~/Desktop/priming/priming_main.R')

# read in data

setwd('~/Desktop/priming/stevie/data/')
# read in director data
data.ps.dir <- readRDS('basicdata3.rds')
# build matcher data
datafiles <- list.files(pattern="sub*")
data.ps <- do.call(rbind, lapply(datafiles, read.csv))
data.ps.match <- data.ps %>%
  filter(role=='matcher') %>%
  mutate(isValid=1) %>%
  mutate_at(vars('subjectNo'), funs(factor(.))) %>%
  mutate(confederate = forcats::fct_relevel(confederate, c('native', 'nonnative')))



#########################################
### descriptive stats ###################
#########################################

data.ps.dir %>%
  group_by(subjectNo, primeType, confederateType) %>%
  summarise(mean(oppScore_percentage)) %>%
  rename_at(vars(starts_with("mean")), ~"pct") %>%
  group_by(primeType, confederateType) %>%
  summarise_each(funs(mean, se), pct)

## mean pct of correct match responses by condition
data.ps.match %>%
  group_by(subjectNo, primeType, confederate) %>%
  summarise(sum(matchCorr)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"pct") %>%
  group_by(primeType, confederate) %>%
  summarise_each(funs(mean, se), pct)

#########################################
### produce some plots ##################
#########################################

## df to use for plotting dir data
data.ps.dir.plot <- data.ps.dir %>% 
  group_by(subjectNo,primeType,confederateType) %>%
  summarise(mean(oppScore_percentage)) %>%
  rename_at(vars(c("primeType","confederateType",starts_with("mean"))), ~c("condition","confederate","percentage_y")) %>%
  mutate(colour_by=condition) %>%
  mutate(w=0.8) %>% 
  mutate(exp=factor('Exp S'))

# bar plots
data.ps.dir.plot %>% plotCPBar(T,F) +
  xlab("Prime type") + ylab("Noun phrase weight")

# dot plots
data.ps.dir.plot %>% plotCPDot(T) +
  xlab("Prime type") + ylab("Noun phrase weight") +
  ylim(0,100)

## df to use for plotting match data
data.ps.match.plot <- data.ps.match %>%
  group_by(subjectNo,confederate,primeType) %>%
  summarise(sum(matchCorr)/sum(isValid)*100) %>% 
  mutate(w=0.8) %>% 
  mutate(exp=factor('Exp S')) %>%
  rename_at(vars(c("primeType", starts_with('sum'))), ~c("condition", 'percentage_y')) %>%
  mutate(colour_by=condition)

# dot plots
data.ps.match.plot %>% plotCPDot(T) + 
  xlab("Prime type") + ylab("Percentage of correct responses") +
  ylim(0,100)


#########################################
### construct models ####################
#########################################

### stevie's analysis
mdl <- with(data.ps.dir, lmer(oppScore_percentage~primeType*confederateType + (1+primeType|subjectNo) + (1|descImg)))
summary(mdl)

### beta regression

# transform data so beta regression can handle 0s and 1s
data.ps.dir %<>% mutate(oppScore_prop_trans=(oppScore_prop*(nrow(data.ps.dir)-1)+0.5)/nrow(data.ps.dir))

with(data.ps.dir, betareg::betareg(oppScore_prop_trans~primeType*confederateType)) %>% summary()

with(data.ps.dir, glmmADMB::glmmadmb(oppScore_prop_trans~primeType*confederateType+(1+primeType|subjectNo)+(1|descImg),family="beta")) %>% summary()

with(data.ps.dir, glmmTMB::glmmTMB(oppScore_prop~primeType*confederateType+(1+primeType|subjectNo)+(1|descImg),
                                   weights=maxOppScore,
                                   family="binomial")) %>% summary()

### binomial GLMM with proportion as the DV, set weights to equal denominator of proportion (in this case max score)
### https://stats.stackexchange.com/questions/189115/fitting-a-binomial-glmm-glmer-to-a-response-variable-that-is-a-proportion-or-f
### https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
with(data.ps.dir, glmer(oppScore_prop~primeType*confederateType+(1+primeType|subjectNo)+(1|descImg),
                        weights=maxOppScore,
                        family="binomial")) %>% summary()

## analysis for match trials

with(data.ps.match, glmer(matchCorr~primeType*confederate+(1+primeType||subjectNo),
                          family="binomial")) %>% summary()
