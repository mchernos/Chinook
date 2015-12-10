###################################
#
# Are Chinooks more common now (in Calgary)?
#
###################################
# M Chernos December 2015
library('dplyr')
library('lubridate')
library('ggplot2')

# Function to convert factors to numeric
f2n = function(x){as.numeric(as.character(x))}

# Read the data
yyc = read.csv('calgary_intl1.csv')

# Daily difference
yyc$Tdiff = c(NA, diff(f2n(yyc$maxT),1) ) 

# Chinook defined as:
mT = 5                  # Daily maxT > 0
mnts =  c(11,12,1,2,3)  # - Nov - March
tdif = 5                # - Tdiff > 5 C

chinooks = yyc %>%
  filter(Month %in% mnts & f2n(maxT) > mT & Tdiff > tdif) %>%
  group_by(Year) %>%
  summarize(Chinook_Days = length(Tdiff)) 


ggplot(data = chinooks, aes(x = Year, y = Chinook_Days)) + 
  geom_line(color = 'grey30') + 
  stat_smooth(data = subset(chinooks, Year > 1959), method = 'lm', color = 'red') + 
  stat_smooth(data = subset(chinooks, Year < 1960), method = 'lm', color = 'darkgreen') + 
  stat_smooth(se = F) + 
  theme_bw() + 
  labs(x = '', y = 'Chinook Days', 
       title = paste('T Diff =', tdif ,'C, Daily Max T =', mT,'C'))


# Check Significance of post-1960 linear regressions
summary(lm(data = subset(chinooks, Year > 1959), formula = Year~Chinook_Days))
