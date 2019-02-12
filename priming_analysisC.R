##################################################
### script to compare priming experiments 1 and 2
##################################################

source('~/Desktop/priming/priming_main.R')

#########################################
### produce some plots ##################
#########################################

## build data.p1.dir.valid and data.p2.dir.valid from analysis1 and analysis2 scripts

## build combined dataset
exp1 <- data.p1.dir.valid %>%
  select(subjectNo,confederate,primeConstruction,targetVerbPar,isDO,targetVerb,confederateID,utteranceType) %>%
  mutate(subjectNo=paste(subjectNo, '_1', sep='')) %>%
  mutate(confederateID=paste(confederateID, '_1', sep='')) %>% 
  tibble::add_column(primeVerbType=factor("alt")) %>%
  tibble::add_column(targetVerbType=factor("alt")) %>%
  tibble::add_column(exp=factor("Exp 1")) %>%
  tibble::add_column(isValid=1) %>%
  select(subjectNo,targetVerb,primeConstruction,primeVerbType,targetVerbType,targetVerbPar,confederate,isDO,isValid,exp,confederateID,utteranceType) %>%
  data.frame()

exp2 <- data.p2.dir.valid %>%
  filter(trialType=='critical') %>%
  select(subjectNo,confederate,primeType,targetVerbType,targetVerbPar,isDO,targetVerb,confederateID,utteranceType) %>%
  mutate(subjectNo=paste(subjectNo, '_2', sep='')) %>%
  mutate(confederateID=paste(confederateID, '_2', sep='')) %>%
  plyr::rename(c("primeType"="primeVerbType")) %>%
  tibble::add_column(primeConstruction=factor("DO")) %>%
  tibble::add_column(exp=factor("Exp 2")) %>%
  tibble::add_column(isValid=1) %>%
  select(subjectNo,targetVerb,primeConstruction,primeVerbType,targetVerbType,targetVerbPar,confederate,isDO,isValid,exp,confederateID,utteranceType) %>%
  data.frame()

expC <- rbind(exp1, exp2) 

# df for plotting
expC_plotting<- expC %>%
  filter(!(is.na(isDO))) %>%
  filter(primeConstruction=='DO' & primeVerbType=='alt' & targetVerbType=='alt') %>%
  mutate(subjectNo=factor(subjectNo)) %>%
  group_by(subjectNo,confederate,exp) %>%
  summarise(sum(isDO)/sum(isValid) * 100) %>%
  rename_at(vars(starts_with("sum")), ~"percentage_y")


# bar plot
expC_plotting %>% plotCPCompBar()

# dot plot
expC_plotting %>% plotCPCompDot(T)


#########################################
### descriptive stats ###################
#########################################

desc.stats.C<-
  expC %>%
  group_by(subjectNo, exp, targetVerbPar, confederate) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>%
  data.frame() %>%
  rename_(prop_DO=names(.)[5])

with(desc.stats.C[desc.stats.C$exp=='Exp 1' & 
                    desc.stats.C$confederate=='native' & 
                    desc.stats.C$targetVerbPar=='SV',], 
     mean(prop_DO, na.rm=T))


#########################################
### construct models ####################
#########################################

expC %<>%
  filter(!is.na(isDO) & primeConstruction=='DO' & primeVerbType=='alt' & targetVerbType=='alt') %>%
  mutate(subjectNo=factor(subjectNo),
         confederateID=factor(confederateID)) %>%
  mutate_at(vars(exp,targetVerbPar), .funs = funs(cod=simple_scale(.))) %>%
  data.frame() 


with(expC,
     glmer(as.factor(isDO)~exp_cod*targetVerbPar_cod*confederate+
             (1+targetVerbPar_cod|subjectNo)+
             (1|targetVerb)+
             (1|confederateID),
           family='binomial',
           control=glmerControl(optimizer=c("bobyqa")))) %>% summary()

