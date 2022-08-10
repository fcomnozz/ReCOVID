library(dplyr)
library(ggpubr)

v <- read.csv('../data/variants_data.csv')

unique(v$pangolin_lineage) # 332 variants

table(v$pangolin_lineage)

# grouping into VOC/VOI
x <- v %>% 
  mutate(variant = case_when(grepl('BA', pangolin_lineage) ~ 'omicron',
                        grepl('BE', pangolin_lineage) ~ 'omicron',
                        grepl('BF', pangolin_lineage) ~ 'omicron',
                        grepl('BG', pangolin_lineage) ~ 'omicron',
                        grepl('AY', pangolin_lineage) ~ 'delta',
                        grepl('B\\.1\\.617\\.2', pangolin_lineage) ~ 'delta',
                        grepl('B\\.1\\.1\\.7', pangolin_lineage) ~ 'alpha',
                        grepl('Q', pangolin_lineage) ~ 'alpha',
                        grepl('P\\.1', pangolin_lineage) ~ 'gamma',
                        grepl('B\\.1\\.351', pangolin_lineage) ~ 'beta',
                        grepl('B\\.1\\.621', pangolin_lineage) ~ 'mu',
                        grepl('B\\.1\\.525', pangolin_lineage) ~ 'eta',
                        grepl('B\\.1\\.177', pangolin_lineage) ~ 'B.1.177',
                        pangolin_lineage == 'B.1' ~ 'B.1',
                        pangolin_lineage == 'B.1.1' ~ 'B.1.1',
                        pangolin_lineage == 'A' ~ 'original',
                        pangolin_lineage == 'B' ~ 'original',
                        TRUE ~ 'other')
  )

table(x$variant)

# less variants
x <- v %>% 
  mutate(variant = case_when(grepl('BA', pangolin_lineage) ~ 'omicron',
                             grepl('BE', pangolin_lineage) ~ 'omicron',
                             grepl('BF', pangolin_lineage) ~ 'omicron',
                             grepl('BG', pangolin_lineage) ~ 'omicron',
                             grepl('AY', pangolin_lineage) ~ 'delta',
                             grepl('B\\.1\\.617\\.2', pangolin_lineage) ~ 'delta',
                             grepl('B\\.1\\.1\\.7', pangolin_lineage) ~ 'alpha',
                             grepl('Q', pangolin_lineage) ~ 'alpha',
                             TRUE ~ 'other')
  )


sp <- ggscatter(x, x = "week_num", y = "n",
                color = "variant"
)

sp

# changin some things
y <- x
# removing week 22-52 (prediction?)
y <- y[y$week_num != '22-52',]

# changing week_num for a discrete scale from 1 to 127
v1 <- unique(y$week_num)
v2 <- c(1:length(v1))
map = setNames(v2, v1)
y$week <- map[unlist(y$week_num)]

# plotting again
sp <- ggscatter(y, x = "week", y = "n",
                color = "variant",
                palette = 'pal_lancet',
                alpha = 0.8)

sp + geom_vline(xintercept = 51, color = '#ED000099', linetype = 'dashed') + 
  geom_vline(xintercept = 71, color = '#42B54099', linetype = 'dashed') +
  geom_vline(xintercept = 96, color = '#0099B499', linetype = 'dashed')

###############################################################
# This plot suggest that the probable variants over time are:
# 2020-03-24 --> 2021-02-01 | pre-alpha variants (B.1, B.1.1, B.1.177, etc)
# 2021-02-08 --> 2021-06-21 | alpha
# 2021-06-28 --> 2021-12-13 | delta
# 2021-12-20 --> 2022-07-25 / currently | omicron
