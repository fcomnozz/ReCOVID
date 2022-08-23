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

p <- ggbarplot(tab, x = 'Var1', y = 'Freq', palette = 'Accent',
          fill = 'Var1', xlab = 'Sex')
p + rremove('legend')

# Age
mean(dem$age, na.rm = T) # 46.37
median(dem$age, na.rm = T) # 43
quantile(dem$age, na.rm = T) # Q1 = 27; Q3 = 61

ggdensity(dem, x = 'age', 
          add = 'median', fill = 'turquoise')

# GMA
tab <- table(dem$gma)
tab <- as.data.frame(tab)
tab$GM <- c('Sanos', rep('Agudos', 5), rep('Emb/Parto', 5),
            rep('Cron. 1 sist.', 5), rep('Cron. 2-3 sist.', 5),
            rep('Cron. 4+ sist.', 5), rep('Neop. activa', 4)
)
tab$complejidad <- c(0, rep(seq(1,5), 5), 1, 2, 3, 4)
tab$GM <- as.factor(tab$GM)
tab$complejidad <- as.factor(tab$complejidad)


ggbarplot(tab, x = 'GM', y = 'Freq', fill = 'complejidad',
          palette = 'Reds')

# prevalence
tab$prev <- tab$Freq/3303
ggbarplot(tab, x = 'GM', y = 'prev', fill = 'complejidad',
          palette = 'Reds', ylab = 'Prevalence')

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
               title = 'Variant profiling of all reinfections')
p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + rremove('legend')

# plotting total counts of variants
vnames = c('pre-alpha', 'alpha', 'delta', 'omicron')
vcounts = c(2856, 574, 1256, 2035) # obtained from python analysis
total_variant <- data.frame(variant = vnames, count = vcounts)

p <- ggbarplot(total_variant, x = 'variant', y = 'count', palette = 'Accent',
               fill = 'variant')
p + rremove('legend')



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
