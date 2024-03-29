library(dplyr)
library(ggpubr)
library(RColorBrewer)

### we will characterize the whole dataset as well as the chosen individuals subgroup ###

# Loading data
# all patients
system('cat ../results/dict2.csv | cut -f1,2,3 -d "," > tmp.csv')
all <- read.csv('tmp.csv', header = F)
colnames(all) <- c('id', 'date', 'code')
system('rm -f tmp.csv')

# filtered patients (case 1; 2 PCR)
f <- read.csv('../results/filtered_dict.csv', header = F)
colnames(f) <- c('id', 'date', 'code')

# demographic info
dem <- read.csv('../data/info.tsv', sep = '\t', header = T)

# comorbidities/antecedents
ant <- read.csv('../data/antecedents.csv', sep = '\t', header = T)

# vaccination data
# some extra rows due to the organization of the data
vac <- read.csv('../data/patients_vaccines.csv', sep = '\t', header = T)

# hospitalized patients datasets
# diagnosis
diag <- read.csv('../data/diagnosis_h.csv', sep = '\t', header = T)
# other variables
var <- read.csv('../data/variables_h.csv', sep = '\t', header = T)

# possible variants
variants <- read.csv('../results/possible_variants.csv', sep = ',', header = F)
colnames(variants) <- c('id', 'reinfection1', 'reinfection2', 'reinfection3')

#### ALL PATIENTS ####

# Sex
prop.table(table(dem$sex))
# female - 0.6855
# male - 0.3133
# other - 0.0012
tab = prop.table(table(dem$sex))
tab = as.data.frame(tab)
tab$Var1 <- c('Other', 'Female', 'Male')

p <- ggbarplot(tab, x = 'Var1', y = 'Freq', palette = 'Accent',
          fill = 'Var1', xlab = '')
p + rremove('legend')

# Age
mean(dem$age, na.rm = T) # 46.37
median(dem$age, na.rm = T) # 43
quantile(dem$age, na.rm = T) # Q1 = 27; Q3 = 61

ggdensity(dem, x = 'age', 
          add = 'median', fill = 'turquoise', xlab = 'Age',
          ylab = 'Density')


# GMA
tab <- table(dem$gma)
tab <- as.data.frame(tab)
tab$GM <- c('Healthy', rep('Acute', 5), rep('Pregnancy/Birth', 5),
            rep('Chron. 1 sist.', 5), rep('Chron. 2-3 sist.', 5),
            rep('Chron. 4+ sist.', 5), rep('Active neoplasia', 4)
)
tab$Complexity <- c(0, rep(seq(1,5), 5), 1, 2, 3, 4)
tab$GM <- factor(tab$GM, levels = c('Healthy', 'Acute', 'Pregnancy/Birth',
                                    'Chron. 1 sist.', 'Chron. 2-3 sist.',
                                    'Chron. 4+ sist.', 'Active neoplasia'))
tab$Complexity <- as.factor(tab$Complexity)


p <- ggbarplot(tab, x = 'GM', y = 'Freq', fill = 'Complexity',
          palette = 'Reds')

p

# prevalence
tab$prev <- tab$Freq/3303
ggbarplot(tab, x = 'GM', y = 'prev', fill = 'Complexity',
          palette = 'Reds', ylab = 'Prevalence') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5))

# PCC (pacientes cronicos complejos) y MACA (enfermedad cronica avanzada)
prop.table(table(dem$pcc)) # 0.1105
prop.table(table(dem$maca)) # 0.0221

# Comorbidities

# diabetes mellitus 1
prop.table(table(ant$DM1)) # 0.0051 | prev ESP: 0.002

# diabetes mellitus 2
prop.table(table(ant$DM2)) # 0.0902 | prev ESP: 0.1 (30-89 yo)

# dislipemia
prop.table(table(ant$DISLIPEMIA)) # 0.1998 | prev ESP: 0.2 - 0.25

# alcohol
prop.table(table(ant$ALCOHOL)) # 0.0136

# obesidad
prop.table(table(ant$OBESITAT)) # 0.2113 | prev ESP: 0.16

# HTA
prop.table(table(ant$HTA)) # 0.2071 | prev ESP: 0.38

# asma
prop.table(table(ant$ASMA)) # 0.063 | prev ESP: 0.05

# tabaco
prop.table(table(ant$TABAC)) # 0.0717

# transplante de org solido
prop.table(table(ant$TRANS_ORGAN_SOLID)) # 0.003

# neoplasia maligna
prop.table(table(ant$NEOPLASIA_MALIGNA)) # 0.0327

# artritis reumatoide
prop.table(table(ant$ARTRITIS_REUMATOIDE)) # 0.0082

# VIH
prop.table(table(ant$VIH)) # 0.0018

# inmunodeficiencia primaria
prop.table(table(ant$INMUNODEF_PRIMARIES)) # 0.0003

# plotting all comorbidities together
ant2 = subset(ant, select = -c(ID_ANONIMITZAT, BARTHEL, BARTHEL_DATA) )
ant2 <- as.data.frame(mapply(sum, ant2))
ant2$COMORB <- rownames(ant2)
colnames(ant2) <- c('Freq', 'Comorb')
p <- ggbarplot(ant2, x = 'Comorb', y = 'Freq', fill = 'goldenrod')
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# same but with percentages
ant2$prev <- ant2$Freq/3303
p <- ggbarplot(ant2, x = 'Comorb', y = 'prev', fill = 'goldenrod', ylab = 'Prevalence')
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# reinfection variants profile
# removing empty rows
variants <- variants[variants$reinfection1 != '',]

# only first reinfection
variants1 <- variants[,c(1,2)]
prop.table(table(variants1$reinfection1))

tab = prop.table(table(variants1$reinfection1))
tab = as.data.frame(tab)
tab$Var1 <- factor(tab$Var1, levels = c('pre-alpha:pre-alpha', 'pre-alpha:alpha',
                                           'pre-alpha:delta', 'pre-alpha:omicron',
                                           'alpha:alpha', 'alpha:delta', 'alpha:omicron',
                                           'delta:delta', 'delta:omicron'))

p <- ggbarplot(tab, x = 'Var1', y = 'Freq', palette = 'Set3',
               fill = 'Var1', xlab = 'Variants', ylab = 'Proportion',
               title = 'Variant profiling of first reinfections')
p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + rremove('legend')

# all reinfections
allv <- variants1
colnames(allv) <- c('id', 'reinfection')

tmp <- variants[,c(1,3)]
colnames(tmp) <- c('id', 'reinfection')
tmp <- tmp[tmp$reinfection != '',]

allv <- rbind(allv, tmp)

tmp <- variants[,c(1,4)]
colnames(tmp) <- c('id', 'reinfection')
tmp <- tmp[tmp$reinfection != '',]

allv <- rbind(allv, tmp)
rm(tmp)
dim(allv)

tab = prop.table(table(allv$reinfection))
tab = as.data.frame(tab)
tab$Var1 <- factor(tab$Var1, levels = c('pre-alpha:pre-alpha', 'pre-alpha:alpha',
                                        'pre-alpha:delta', 'pre-alpha:omicron',
                                        'alpha:alpha', 'alpha:delta', 'alpha:omicron',
                                        'delta:delta', 'delta:omicron', 'omicron:omicron'))

p <- ggbarplot(tab, x = 'Var1', y = 'Freq', palette = 'Set3',
               fill = 'Var1', xlab = 'Variants', ylab = 'Proportion',
               title = 'Estimated variant profiling of all reinfections')
p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + rremove('legend')

# plotting total counts of variants
vnames = c('pre-alpha', 'alpha', 'delta', 'omicron')
vcounts = c(2856, 574, 1256, 2035) # obtained from python analysis
total_variant <- data.frame(variant = vnames, count = vcounts)

p <- ggbarplot(total_variant, x = 'variant', y = 'count', color = 'grey3', fill = 'lightgrey')
p + rremove('legend')

# hospitalization
prop.table(table(dem$hosp))


###### SELECTED PATIENTS ######
# filtering data
demf <- merge(dem, f[1], by = 'id')
names(ant)[colnames(ant) == 'ID_ANONIMITZAT'] <- 'id'
antf <- merge(ant, f[1], by = 'id')
variantsf <- merge(variants, f[1], by = 'id')
variantsf <- variantsf[, c(1,2)]
colnames(variantsf) <- c('id', 'reinfection')

# sex
prop.table(table(demf$sex))

tab = prop.table(table(demf$sex))
tab = as.data.frame(tab)

p <- ggbarplot(tab, x = 'Var1', y = 'Freq', palette = 'Accent',
               fill = 'Var1', xlab = 'Sex')
p + rremove('legend')

# age
mean(demf$age, na.rm = T) 
median(demf$age, na.rm = T) 
quantile(demf$age, na.rm = T)

ggdensity(demf, x = 'age', 
          add = 'median', fill = 'turquoise')

# GMA
tab <- table(demf$gma)
tab <- as.data.frame(tab)
tab$GM <- c('Sanos', rep('Agudos', 5), rep('Emb/Parto', 4),
            rep('Cron. 1 sist.', 5), rep('Cron. 2-3 sist.', 5),
            rep('Cron. 4+ sist.', 5), rep('Neop. activa', 4)
)
tab$complejidad <- c(0, 1:5, 1:4, rep(seq(1,5), 3), 1:4)
tab$GM <- as.factor(tab$GM)
tab$complejidad <- as.factor(tab$complejidad)

ggbarplot(tab, x = 'GM', y = 'Freq', fill = 'complejidad',
          palette = 'Reds')

# prevalence
tab$prev <- tab$Freq/1235
ggbarplot(tab, x = 'GM', y = 'prev', fill = 'complejidad',
          palette = 'Reds', ylab = 'Prevalence')

# PCC (pacientes cronicos complejos) y MACA (enfermedad cronica avanzada)
prop.table(table(demf$pcc))
prop.table(table(demf$maca))

#comorbidities
ant2f = subset(antf, select = -c(id, BARTHEL, BARTHEL_DATA) )
ant2f <- as.data.frame(mapply(sum, ant2f))
ant2f$COMORB <- rownames(ant2f)
colnames(ant2f) <- c('Freq', 'Comorb')
p <- ggbarplot(ant2f, x = 'Comorb', y = 'Freq', fill = 'goldenrod')
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# same but with percentages
ant2f$prev <- ant2f$Freq/1235
p <- ggbarplot(ant2f, x = 'Comorb', y = 'prev', fill = 'goldenrod', ylab = 'Prevalence')
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# variants
tab = prop.table(table(variantsf$reinfection))
tab = as.data.frame(tab)
tab$Var1 <- factor(tab$Var1, levels = c('pre-alpha:pre-alpha', 'pre-alpha:alpha',
                                        'pre-alpha:delta', 'pre-alpha:omicron',
                                        'alpha:alpha', 'alpha:delta', 'alpha:omicron',
                                        'delta:delta', 'delta:omicron'))

p <- ggbarplot(tab, x = 'Var1', y = 'Freq', palette = 'Set3',
               fill = 'Var1', xlab = 'Variants', ylab = 'Proportion',
               title = 'Variant profiling of reinfections')
p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + rremove('legend')

# hospitalization
prop.table(table(demf$hosp))



#### PATIENTS WITH MULTIPLE REINFECTIONS ####
multi <- variants[variants$reinfection2 != '',]
dim(multi) # 146 patients

# filtering data
demm <- merge(dem, multi[1], by = 'id')
antm <- merge(ant, multi[1], by = 'id')

# sex
prop.table(table(demm$sex)) # F: 0.73

tab = prop.table(table(demm$sex))
tab = as.data.frame(tab)

p <- ggbarplot(tab, x = 'Var1', y = 'Freq', palette = 'Accent',
               fill = 'Var1', xlab = 'Sex')
p + rremove('legend')

# age
mean(demm$age, na.rm = T) 
median(demm$age, na.rm = T) # 43
quantile(demm$age, na.rm = T)

ggdensity(demm, x = 'age', 
          add = 'median', fill = 'turquoise')

# GMA
tab <- table(demm$gma)
tab <- as.data.frame(tab)
table(tab)
tab$GM <- c('Sanos', rep('Agudos', 4), rep('Emb/Parto', 2),
            rep('Cron. 1 sist.', 5), rep('Cron. 2-3 sist.', 5),
            rep('Cron. 4+ sist.', 5), rep('Neop. activa', 2)
)
tab$complejidad <- c(0, 1:3, 5, 2, 3, rep(seq(1,5), 3), 1, 2)
tab$GM <- as.factor(tab$GM)
tab$complejidad <- as.factor(tab$complejidad)

ggbarplot(tab, x = 'GM', y = 'Freq', fill = 'complejidad',
          palette = 'Reds')

# prevalence
tab$prev <- tab$Freq/146
ggbarplot(tab, x = 'GM', y = 'prev', fill = 'complejidad',
          palette = 'Reds', ylab = 'Prevalence')

# PCC (pacientes cronicos complejos) y MACA (enfermedad cronica avanzada)
prop.table(table(demm$pcc)) # 0.14
prop.table(table(demm$maca)) # 0.02

#comorbidities
ant2m = subset(antm, select = -c(id, BARTHEL, BARTHEL_DATA) )
ant2m <- as.data.frame(mapply(sum, ant2m))
ant2m$COMORB <- rownames(ant2m)
colnames(ant2m) <- c('Freq', 'Comorb')
p <- ggbarplot(ant2m, x = 'Comorb', y = 'Freq', fill = 'goldenrod')
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# same but with percentages
ant2m$prev <- ant2m$Freq/146
p <- ggbarplot(ant2m, x = 'Comorb', y = 'prev', fill = 'goldenrod', ylab = 'Prevalence')
p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# hospitalization
prop.table(table(demm$hosp)) # 0.08

# comparing the combination of variants groups
# accounting just for the first infection
DEM <- merge(dem, variants1,  by = 'id')
table(DEM$reinfection1)
DEM %>% group_by(reinfection1) %>% 
  mutate(median = median(age, na.rm = TRUE)) %>% 
  group_by(reinfection1, median) %>% 
  summarise()

# sex
DEM %>%                       
  group_by(reinfection1) ########### TO DO ###########


# Getting demographics by GMA group (all patients)
dem$GM <- NA
dem[!is.na(dem$gma) & dem$gma == 1, 'GM'] <- 'Healthy'
dem[dem$gma %in% c(101:105), 'GM'] <- 'Acute'
dem[dem$gma %in% c(201:205), 'GM'] <- 'Pregnancy/Delivery'
dem[dem$gma %in% c(311:315), 'GM'] <- 'Chron. 1 system'
dem[dem$gma %in% c(321:325), 'GM'] <- 'Chron. 2-3 systems'
dem[dem$gma %in% c(331:335), 'GM'] <- 'Chron. 4+ systems'
dem[dem$gma %in% c(401:404), 'GM'] <- 'Active neoplasia'

prop.table(table(dem$GM))

# plotting age distribution for each GM
ggdensity(dem[!is.na(dem$GM),], x = 'age', 
          add = 'median', fill = 'GM', xlab = 'Age')

ggdensity(dem[!is.na(dem$GM),], x = 'age', 
          add = 'median', fill = 'GM', 
          facet.by = 'GM', xlab = 'Age')

# By sex
# Only 4 non-binary people so we won't represent them
ggdensity(dem[dem$sex != 'Altres',], x = 'age',
          add = 'median', fill = 'sex',
          xlab = 'Age', )

ggdensity(dem[dem$sex != 'Altres',], x = 'age',
          add = 'median', fill = 'sex',
          xlab = 'Age', facet.by = 'sex')

# Age groups
# The general age curve show three peaks so we will split these age groups:
# 0-34
# 35-70
# 70-100
ggdensity(dem[dem$sex != 'Altres',], x = 'age',
         xlab = 'Age', fill = 'turquoise') + 
  geom_vline(xintercept = 34) + 
  geom_vline(xintercept = 70)

# Plotting sex and GMA based on these age groups
# Group 1
tab = prop.table(table(dem[dem$age %in% c(0:34),'sex']))
tab = as.data.frame(tab)
p <- ggbarplot(tab, x = 'Var1', y = 'Freq', palette = 'Accent',
               fill = 'Var1', xlab = 'Sex', title = 'Individuals under 35 years old')
p + rremove('legend')

# Group 2
tab = prop.table(table(dem[dem$age %in% c(35:70),'sex']))
tab = as.data.frame(tab)
p <- ggbarplot(tab, x = 'Var1', y = 'Freq', palette = 'Accent',
               fill = 'Var1', xlab = 'Sex', title = 'Individuals between 35 and 70 years old')
p + rremove('legend')

# Group 3
tab = prop.table(table(dem[dem$age %in% c(71:100),'sex']))
tab = as.data.frame(tab)
p <- ggbarplot(tab, x = 'Var1', y = 'Freq', palette = 'Accent',
               fill = 'Var1', xlab = 'Sex', title = 'Individuals over 70 years old')
p + rremove('legend')

# Adding age group column
dem$age.group <- NA
dem[dem$age %in% c(0:34), 'age.group'] <- 1
dem[dem$age %in% c(35:70), 'age.group'] <- 2
dem[dem$age %in% c(71:100), 'age.group'] <- 3

table(dem$age.group)
# converting variables to factor
dem$age.group <- as.factor(dem$age.group)
dem$sex <- factor(dem$sex, levels = c('Dona', 'Home'))

# Fisher's test
fisher.test(dem$sex, dem$age.group) # 4.7e-07
library(rstatix)
pairwise_fisher_test(table(dem$sex, dem$age.group)) # differences btw 1-2 & 1-3

# GMA
# Group 1
tab <- table(dem[dem$age.group == 1, 'gma'])
tab <- as.data.frame(tab)
tab$GM <- c('Healthy', rep('Acute', 5), rep('Pregnancy/Birth', 5),
            rep('Chron. 1 sist.', 5), rep('Chron. 2-3 sist.', 5),
            rep('Chron. 4+ sist.', 3), 'Active neoplasia'
)
tab$Complexity <- c(0, rep(seq(1,5), 4), 1, 2, 3, 1)
tab$GM <- factor(tab$GM, levels = c('Healthy', 'Acute', 'Pregnancy/Birth',
                                   'Chron. 1 sist.', 'Chron. 2-3 sist.',
                                   'Chron. 4+ sist.', 'Active neoplasia'))
tab$Complexity <- as.factor(tab$Complexity)

ggbarplot(tab, x = 'GM', y = 'Freq', fill = 'Complexity',
               palette = 'Reds', title = 'Individuals under 35 years old')

# Group 2
tab <- table(dem[dem$age.group == 2, 'gma'])
tab <- as.data.frame(tab)
tab$GM <- c('Healthy', rep('Acute', 5), rep('Pregnancy/Birth', 4),
            rep('Chron. 1 sist.', 5), rep('Chron. 2-3 sist.', 5),
            rep('Chron. 4+ sist.', 5), rep('Active neoplasia', 3)
)
tab$Complexity <- c(0, seq(1,5), seq(1,4), rep(seq(1,5), 3), seq(1,3))
tab$GM <- factor(tab$GM, levels = c('Healthy', 'Acute', 'Pregnancy/Birth',
                                    'Chron. 1 sist.', 'Chron. 2-3 sist.',
                                    'Chron. 4+ sist.', 'Active neoplasia'))
tab$Complexity <- as.factor(tab$Complexity)

ggbarplot(tab, x = 'GM', y = 'Freq', fill = 'Complexity',
          palette = 'Reds', title = 'Individuals between 35 and 70 years old')

# Group 3
tab <- table(dem[dem$age.group == 3, 'gma'])
tab <- as.data.frame(tab)
tab$GM <- c(rep('Chron. 1 sist.', 2), rep('Chron. 2-3 sist.', 5),
            rep('Chron. 4+ sist.', 5), rep('Active neoplasia', 4)
)
tab$Complexity <- c(4, 5, rep(seq(1,5), 2), seq(1,4))
tab$GM <- factor(tab$GM, levels = c('Chron. 1 sist.', 'Chron. 2-3 sist.',
                                    'Chron. 4+ sist.', 'Active neoplasia'))
tab$Complexity <- as.factor(tab$Complexity)

ggbarplot(tab, x = 'GM', y = 'Freq', fill = 'Complexity',
          palette = 'Reds', title = 'Individuals over 70 years old')