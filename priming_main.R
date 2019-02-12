##############################################################
### functions we will need for priming experiments 1, 2 and S 
### mainly for plotting, plus a few miscellaneous
#############################################################

## load relevant libraries
require(dplyr)
require(magrittr)
require(ggplot2)
require(gridExtra)
require(dplyr)
require(lme4)

plotCPBar = function(data, facetByConf=F, facetBySubs=F) {
  # function to barplot director data (percentage of DO utterances)
  # data = df of DO percentage by prime construction, target verb par, confederate and subject no
  # facetByConf produces bar plot with proportions of PO/DO split by SV/DV facetted by confederate
  # facetBySubs produces bar plot with proporetions of PO/DO split by SV/DV facetted by subject
  # if both are set to True individual subject plots are grouped by confederate
  require(ggplot2)
  
  plotBase = function(data){
    basePlot = ggplot(data) +
      aes(x=condition, y=percentage_y, fill=colour_by) +
      stat_summary(fun.y=mean, geom='bar', position=position_dodge(), aes(width=w)) +
      scale_fill_manual(values=c("orange2","gray65"),
                        labels=c('Same Verb','Different Verb')) +
      stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.8), width=.2) +
      guides(fill=guide_legend(title="Target verb parity")) +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=15),
            legend.text=element_text(size=12),
            legend.title=element_text(size=15),
            legend.position='bottom',
            strip.text=element_text(size=15))
  
    if (levels(data$exp)=='Exp 1') {
      basePlot = basePlot +
        scale_x_discrete(breaks=c('PO','DO'),
                         labels=c('Prepositional Object','Double Object'))
    } else if (levels(data$exp)=='Exp 2') {
      basePlot = basePlot +
        scale_x_discrete(breaks=c('Alt-Alt','Alt-Non','Non-Alt','Non-Non'),
                         labels=c('Alt-\nAlt','Alt-\nNon','Non-\nAlt','Non-\nNon'))
    } else if (levels(data$exp)=='Exp S') {
      # drop legend if it's Stevie's data!
      basePlot = basePlot + theme(legend.position = "none")
    }
    
    return(basePlot)
  }
  
  if (facetByConf==F & facetBySubs==F){
    thePlot = plotBase(data)
    return(thePlot)
  } else if (facetByConf==T & facetBySubs==F) {
    thePlot = plotBase(data) + 
      facet_wrap(~confederate)
    return(thePlot)
  } else if (facetByConf==F & facetBySubs==T) {
    thePlot = plotBase(data) +
      facet_wrap(~subjectNo, ncol=10)
    return(thePlot)
  } else if (facetByConf==T & facetBySubs==T) {
    require(gridExtra)
    thePlot = arrangeGrob(
      plotBase(data[data$confederate=='native',]) + 
        facet_wrap(~subjectNo, ncol=10) +
        ggtitle('native'),
      plotBase(data[data$confederate=='nonnative',]) + 
        facet_wrap(~subjectNo, ncol=10) +
        ggtitle('nonnative')
    )
    ggpubr::as_ggplot(thePlot)
  }
}

plotCPDot = function(data, showPptDots=F) {
  # function to dotplot director data (percentage of DO utterances)
  # data = df of DO percentage by prime construction, target verb par, confederate and subject no
  # showPptDots shows data points for each individual participant
  require(ggplot2)
  
  thePlot = ggplot(data) +
    aes(x=condition, y=percentage_y, fill=colour_by) +
    stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.2, size=.8,
                 aes(x=condition, y=percentage_y, colour=colour_by)) +
    scale_colour_manual(values=c("black","black"), guide="none") +
    stat_summary(fun.y=mean, geom='point', position=position_dodge(width=.9), size=4, shape=21, stroke=1.4,
                 aes(x=condition, y=percentage_y, fill=colour_by)) +
    scale_fill_manual(values=c("royalblue","orange"), 
                      labels=c("Same verb","Different verb"),
                      name="Target verb parity") +
    facet_wrap(~confederate) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=15),
          legend.text=element_text(size=12),
          legend.title=element_text(size=15),
          strip.text=element_text(size=15))
  
  if (showPptDots == T) {
    thePlot = thePlot +
      geom_dotplot(binwidth=1, dotsize=1.1, stackdir="center", binaxis="y", stackratio=1.2, position="dodge", alpha=.4)
  }
  
  
  if (levels(data$exp)=='Exp S') {
    # drop legend if it's Stevie's data!
    thePlot = thePlot + theme(legend.position = "none")
  }
  
  return(thePlot)
}

plotCPCompDot = function(data, showPptDots=F) {
  # function to dotplot director data (percentage of DO utterances)
  # data = df of DO percentage by confederate and exp (1/2)
  # showPptDots shows data points for each individual participant
  require(ggplot2)
  
  thePlot = ggplot(data) +
    aes(x=confederate, y=percentage_y, fill=confederate) +
    stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.9), width=.2, size=.8,
                 aes(x=confederate, y=percentage_y, colour=confederate)) +
    scale_colour_manual(values=c("black","black"), guide="none") +
    stat_summary(fun.y=mean, geom='point', position=position_dodge(width=.9), size=4, shape=21, stroke=1.4,
                 aes(x=confederate, y=percentage_y, fill=confederate)) +
    scale_fill_manual(values=c("firebrick1","gray45")) +
    facet_wrap(~exp) +
    xlab("Confederate") + ylab("Percentage of DO descriptions produced") +
    ylim(0,60) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=15),
          legend.text=element_text(size=12),
          legend.title=element_text(size=15),
          strip.text=element_text(size=15))
  
  if (showPptDots == T) {
    thePlot = thePlot +
      geom_dotplot(binwidth=1, dotsize=1.1, stackdir="center", binaxis="y", stackratio=1.2, position="dodge", alpha=.4) +
      ylim(0,100)
  }
  
  print(thePlot)
}

plotCPCompBar = function(data) {
  # function to barplot director data (percentage of DO utterances)
  # data = df of DO percentage by confederate and exp (1/2)
  require(ggplot2)
  
  thePlot = ggplot(data) +
    aes(x=confederate, y=percentage_y, fill=confederate) +
    stat_summary(fun.y=mean, geom='bar', position=position_dodge(), aes(width=.8)) +
    scale_fill_manual(values=c('firebrick1','gray45')) +
    stat_summary(fun.data=mean_se, geom='errorbar', position=position_dodge(width=.8), width=.2) +
    facet_wrap(~exp) +
    ylab('Proportion of Double Object utterances') +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=15),
          legend.text=element_text(size=12),
          legend.title=element_text(size=15),
          strip.text=element_text(size=15))
  
  print(thePlot)
}

########################################################

# i actually don't need this anymore
"rename<-" = function(x, y, value) {
  # because i'm lazy
  names(x)[names(x) == y] <- value
  x}

# calculate SEMean
se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

# sum code binary var
simple_scale <- function(x) scale(as.numeric(x),scale=F)[,1]


