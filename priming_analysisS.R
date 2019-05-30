###############################################
### load relevant libraries ###################
###############################################

source('~/Desktop/priming/priming_main.R')

# read in data

setwd('~/Desktop/priming/stevie/data/')

# read in director data
## basicdata4, _recoded2 and second_coding2 have individual opportunities within each trial expanded into a rows
## which code for whether each opportunity is an isDropped/isPronoun/isProper response
## generated from convert_for_logit.R script
data.ps.dir <- readRDS('basicdata3.rds')
data.ps.dir <- read.csv('basicdata4.csv') 

data.ps.dir <- read.csv('data_recoded.csv')
data.ps.dir <- read.csv('data_recoded2.csv')

data.ps.dir <- read.csv('second_coding.csv')
data.ps.dir <- read.csv('second_coding2.csv')

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

## df to use for plotting dir data (opportunity score)
data.ps.dir.plot <- data.ps.dir %>% 
  group_by(subjectNo,primeType,confederateType) %>%
  summarise(mean(oppScore_percentage)) %>%
  rename_at(vars(c("primeType","confederateType",starts_with("mean"))), ~c("condition","confederate","percentage_y")) %>%
  mutate(colour_by=condition,
         w=0.8,
         exp=factor('Exp S'))

# bar plots
data.ps.dir.plot %>% plotCPBar(T,F) +
  xlab("Prime type") + ylab("Noun phrase weight")

# dot plots
data.ps.dir.plot %>% plotCPDot(T) +
  xlab("Prime type") + ylab("Noun phrase weight") +
  #ylim(0,100) +
  NULL

## df to use for plotting dir data (pct of full NPs produced)
data.ps.dir.plot <- data.ps.dir %>%
  mutate(subjectNo=factor(subjectNo)) %>%
  #mutate(primeConstruction=recode(primeConstruction, PO="Prepositional Object", DO="Double Object")) %>%
  group_by(subjectNo,primeType,confederateType) %>%
  rename_at(vars(c("primeType","confederateType")), ~c("condition","confederate")) %>%
  summarise(sum(isProper)/sum(isValid)*100) %>% 
  # create a couple of columns with width and experiment variables because plotting function uses it
  mutate(w=0.8) %>% 
  mutate(exp=factor('Exp S')) %>%
  mutate(colour_by=condition) %>%
  rename_at(vars(starts_with("sum")), ~"percentage_y")

## df to use for plotting dir data (percentage of each response full/pronoun/dropped produced)
simple_pct <- function(x,y) sum(x)/sum(y)*100

data.ps.dir.plot2 <- data.ps.dir %>%
  mutate(primeType = forcats::fct_recode(primeType, "Pronoun"="pronoun","Full NP"="proper")) %>%
  group_by(subjectNo,primeType,confederateType) %>%
  summarise_at(vars(c("isDropped","isPronoun","isProper")), .funs=funs(simple_pct(.,isValid))) %>%
  # convert wide to long so nounType (dropped/pronoun/proper) is one var
  tidyr::gather(response, percentage_y, isDropped:isProper, factor_key=TRUE) %>%
  mutate(response = forcats::fct_recode(response, "Dropped"="isDropped","Pronoun"="isPronoun","Full NP"="isProper"))

data.ps.dir.plot2 %>%
  ggplot() +
  aes(x=response, y=percentage_y, fill=response) +
  stat_summary(fun.y=mean, geom='point', position=position_dodge(width=.9), size=4, shape=21, stroke=1.4,
               aes(x=response, y=percentage_y, fill=response)) +
  scale_fill_manual(values=c("orangered2","green3","dodgerblue3")) +
  stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.2, size=.8,
               aes(x=response, y=percentage_y, colour=response)) +
  scale_colour_manual(values=c("black","black","black"), guide="none") +
  facet_grid(primeType~confederateType) +
  xlab("Response") + ylab("Percentage of each noun type produced") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=15),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15),
        strip.text=element_text(size=15),
        
        panel.background=element_rect(fill="gray97", colour="gray97"),
        plot.background=element_rect(fill="transparent", colour=NA),
        legend.background = element_rect(fill = "gray97")) +
  geom_dotplot(binwidth=1.5, dotsize=1.2, stackdir="center", binaxis="y", stackratio=1.5, position="dodge", alpha=.4) +
  NULL

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
  #ylim(0,100) +
  NULL


#########################################
### construct models ####################
#########################################

### stevie's analysis
with(data.ps.dir, lmer(oppScore_percentage~primeType*confederateType + 
                         (1+primeType|subjectNo) + (1|descImg))) %>% summary()

### beta regression

# transform data so beta regression can handle 0s and 1s
data.ps.dir %<>% mutate(oppScore_prop_trans=(oppScore_prop*(nrow(data.ps.dir)-1)+0.5)/nrow(data.ps.dir))

with(data.ps.dir, betareg::betareg(oppScore_prop_trans~primeType*confederateType)) %>% summary()

with(data.ps.dir, glmmTMB::glmmTMB(oppScore_prop~primeType*confederateType+(1+primeType|subjectNo)+(1|descImg),
                                   weights=maxOppScore,
                                   family="binomial")) %>% summary()

with(data.ps.dir, glmmADMB::glmmadmb(oppScore_prop_trans~primeType*confederateType+(1+primeType|subjectNo)+(1|descImg),family="beta")) %>% summary()


### binomial GLMM with proportion as the DV, set weights to equal denominator of proportion (in this case max score)
### https://stats.stackexchange.com/questions/189115/fitting-a-binomial-glmm-glmer-to-a-response-variable-that-is-a-proportion-or-f
### https://stats.stackexchange.com/questions/233366/how-to-fit-a-mixed-model-with-response-variable-between-0-and-1
with(data.ps.dir, glmer(oppScore_prop~primeType*confederateType+(1+primeType|subjectNo)+(1|descImg),
                        weights=maxOppScore,
                        family="binomial")) %>% summary()

## sanity check - is there an effect of confederateID on DO production?

data.ps.dir %>%
  mutate(isValid=1) %>%
  group_by(subjectNo, confederateID) %>%
  summarise(sum(isProper)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"percentage_Proper") %>%
  ggplot() +
  aes(x=confederateID, y=percentage_Proper) +
  stat_summary(fun.y=mean, geom='point', position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.3) +
  #facet_wrap(~subjectNo) +
  NULL

with(data.ps.dir, glmer(isProper~confederateID+(1|subjectNo), family='binomial')) %>% summary()

### logit model (proper name vs. pronouns+dropped nouns collapsed)
with(data.ps.dir, glmer(isProper~primeType*confederateType+
                          (1+primeType|subjectNo)+
                          (1|descImg)+
                          (1|confederateID), 
                        family='binomial')) %>% summary()

### ordinal regression (nounType (proper/pronoun/dropped noun) as ordered variable)
data.ps.dir %<>%
  mutate(nounType = as.factor(ifelse(data.ps.dir$isProper==1, 'proper', 
                                     ifelse(data.ps.dir$isPronoun==1, 'pronoun', 'dropped'))))
with(data.ps.dir, ordinal::clmm(as.factor(nounType)~primeType*confederateType+
                          (1+primeType|subjectNo)+
                          (1|descImg)+(1|confederateID))) %>% summary()


## analysis for match trials

with(data.ps.match, glmer(matchCorr~primeType*confederate+(1+primeType||subjectNo),
                          family="binomial")) %>% summary()



######################################
###### second coding stuff ###########
######################################

library(irr)
data.ps.dir.first = read.csv('data_recoded.csv')
data.ps.dir.second = read.csv('second_coding.csv')

numOpp = t(cbind(select(data.ps.dir.first, 'opp'), 
                 select(data.ps.dir.second, 'opp')))
numProper = t(cbind(select(data.ps.dir.first, 'properNo'),
                    select(data.ps.dir.second, 'properNo')))
numPronoun = t(cbind(select(data.ps.dir.first, 'pronounNo'),
                     select(data.ps.dir.second, 'pronounNo')))
numDropped = t(cbind(select(data.ps.dir.first, 'droppedNo'),
                     select(data.ps.dir.second, 'droppedNo')))

dfs = sapply(c('numOpp', 'numProper', 'numPronoun', 'numDropped'), get, environment(), simplify=FALSE)

for (i in 1:length(dfs)) {
  print(names(dfs)[i])
  kripp.alpha(dfs[[i]], 'ratio') %>% print()
  cat('\n')
}

