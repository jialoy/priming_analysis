##################################################
### script to compare priming experiments 1 and 3
##################################################

source('~/Desktop/priming/priming_main.R')

## build data.p1.dir.valid and data.p3.dir.valid from analysis1 script

data.p1.dir.valid %<>% 
  mutate(exp = factor('Exp1'),
         confederate = forcats::fct_recode(confederate, 'nonnative1' = 'nonnative'),
         subjectNo = factor(stringr::str_c(subjectNo, '_1')))

data.p3.dir.valid %<>%
  mutate(exp = factor('Exp3'),
         confederate = forcats::fct_recode(confederate, 'nonnative3' = 'nonnative'),
         subjectNo = factor(stringr::str_c(subjectNo, '_3')))

expC <- rbind(data.p1.dir.valid, data.p3.dir.valid) %>%
  mutate(confederateNativeness = factor(ifelse(confederate=='native', 'native', 'nonnative')))

# df for plotting
data.dir.c.plot <- expC %>%
  group_by(subjectNo, confederate, primeConstruction, targetVerbPar, exp) %>%
  rename_at(vars(c("primeConstruction","targetVerbPar")), ~c("condition","colour_by")) %>%
  summarise(sum(isDO)/sum(isValid) * 100) %>%
  mutate(w=0.8) %>% 
  rename_at(vars(starts_with("sum")), ~"percentage_y")

# dot plot
data.dir.c.plot %>% plotCPDot(T) +
  xlab("Prime construction") + ylab("Percentage of DO descriptions produced")



expC %>%
  group_by(subjectNo, confederateID) %>%
  summarise(sum(isDO)/sum(isValid)*100) %>%
  rename_at(vars(starts_with("sum")), ~"percentage_DO") %>%
  ggplot() +
  aes(x=confederateID, y=percentage_DO) +
  stat_summary(fun.y=mean, geom='point', position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.3)


helm <- contr.helmert(3)[c(3:1), 2:1]   # set Helmert contrasts (compare level 1 to (2+3)/2 then level 2 to 3)

with(expC, glmer(isDO~primeConstruction_cod*targetVerbPar_cod*confederate+
                   (1+primeConstruction|subjectNo) +
                   (1|targetVerb), 
                 family='binomial',
                 contrasts=list(confederate=helm),
                 control=glmerControl(optimizer=c("bobyqa")))) %>% summary()

expC %>%
  filter(confederate %in% c('nonnative1', 'nonnative3')) %>% 
     glmer(isDO~primeConstruction_cod*targetVerbPar_cod*confederate+
                   (1+primeConstruction|subjectNo) +
                   (1|targetVerb), 
           
                 family='binomial',
                 control=glmerControl(optimizer=c("bobyqa"))) %>% 
  summary() %>%
  print()


data.cue.resp %>% 
  filter(study==s & experiment==exp) %>%
  glm(factor(accuracy)~cueDist, 
      data = .,
      family='binomial', 
      contrasts=list(cueDist=contr.sum(n=2))) %>% 
  summary() %>%
  print()


