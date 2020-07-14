#######################################################################################
#
#                           Greenland Flowering project
#                             03 - Analysis
#
#######################################################################################
# Antoine Becker-Scarpitta
# July 2020

#  clean R work-space
rm(list=ls())

# Load 02 - Creation database (wihi load scripts 00 and 01)
source("Scripts/02_Creation_DB.r")


# DID
# delete K, W, plots => DONE
# add SITE in all dataset => DONE
# correct Zack Sil , Si names => DONE
# as.factor(year) => DONE
# NA in SECTION => DONE
# cas5, cas6, dry7, dry8 half no data => DONE
#  ADD plot size and divide flow numb by plot size => DONE

# TO DO 
# NUUK has a different structure NEED TO FIGURED OUT how to integrated it
# different format of data in SECTION
# Check the variation in survey period (month level)


# basic explorations
# Raw TotalFlower per sp
qplot(data= flow_tot_plot, Year, TotalFlower, geom=c('point', 'line')) +
  geom_smooth(method='lm', se=TRUE) +
  facet_wrap(~Species) +
  labs(y='Total number of flower per plot')

# Raw TotalFlower per plot
ggplot(flow_tot_plot, aes(Year, TotalFlower, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  facet_wrap(~Species) +
  labs(y='Total number of flower per plot')

# Flower per m2, at plot level
ggplot(flow_tot_plot, aes(Year, Flow_m2, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  facet_wrap(~Species) +
  labs(y='Number of flower per m2 (plot level)')


# 1 - Flower density per m2, with plot level ## -------------------------------
# CAS 
CAS <- ggplot(flow_tot_plot[flow_tot_plot$Species=="CAS",],
              aes(Year, Flow_m2, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Flow density m2') +
  ggtitle('CAS')

# DRY 
DRY <- ggplot(flow_tot_plot[flow_tot_plot$Species=="DRY",],
              aes(Year, Flow_m2, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Flow density m2') +
  ggtitle('DRY')

# PAP 
PAP <- ggplot(flow_tot_plot[flow_tot_plot$Species=="PAP",],
       aes(Year, Flow_m2, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Flow density m2') +
  ggtitle('PAP')

# SAX 
SAX <- ggplot(flow_tot_plot[flow_tot_plot$Species=="SAX",],
              aes(Year, Flow_m2, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Flow density m2') +
  ggtitle('SAX')

# SIL 
SIL <- ggplot(flow_tot_plot[flow_tot_plot$Species=="SIL",],
              aes(Year, Flow_m2, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Flow density m2') +
  ggtitle('SIL')

# gridExtra::grid.arrange(CAS, DRY, PAP, SAX, SIL, ncol=2, 
                        # top = "Flower density, m2")
### END ## ------------------------------------------------------------------




# 2 - Total Flower per plot ## -------------------------------
# CAS 
CAS_t <- ggplot(flow_tot_plot[flow_tot_plot$Species=="CAS",],
              aes(Year, TotalFlower, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Total flow nb') +
  ggtitle('CAS')

# DRY 
DRY_t <- ggplot(flow_tot_plot[flow_tot_plot$Species=="DRY",],
              aes(Year, TotalFlower, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Total flow nb') +
  ggtitle('DRY')

# PAP 
PAP_t <- ggplot(flow_tot_plot[flow_tot_plot$Species=="PAP",],
              aes(Year, TotalFlower, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Total flow nb') +
  ggtitle('PAP')

# SAX 
SAX_t <- ggplot(flow_tot_plot[flow_tot_plot$Species=="SAX",],
              aes(Year, TotalFlower, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Total flow nb') +
  ggtitle('SAX')

# SIL 
SIL_t <- ggplot(flow_tot_plot[flow_tot_plot$Species=="SIL",],
              aes(Year, TotalFlower, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Total flow nb') +
  ggtitle('SIL')

gridExtra::grid.arrange(CAS, CAS_t, DRY, DRY_t,PAP, PAP_t, SAX, SAX_t,SIL, SIL_t, ncol=2, 
                        top = "Flower density (m2)  /vs/  Total Flower number at plot level")

### END ## ------------------------------------------------------------------

