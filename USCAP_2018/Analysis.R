library(readr)
library(dplyr)

DF <- read_csv("../Data/323_db.csv") %>% 
  filter(subtype != "giant condyloma")

# p53
x <- DF$p53_v
y <- DF$p53_d
shapiro.test(x)
shapiro.test(y)
wilcox.test(x, y, paired = TRUE)
cor.test(~ x + y, method = c("spearman"))

DF %>%
  group_by(factor(grade)) %>%
  filter(!is.na(grade)) %>% 
  summarise(
    mean_diff = mean(p53_v, na.rm = TRUE) - mean(p53_d, na.rm = TRUE),
    rho = cor(p53_v, p53_d, use = "complete.obs", method = c("spearman")),
    P = cor.test(p53_v, p53_d, method = c("spearman"))$p.value
  )

# ki67
x <- DF$ki67_v
y <- DF$ki67_d
wilcox.test(x, y, paired = TRUE)
cor.test(~ x + y, method = c("spearman"))

DF %>%
  group_by(factor(grade)) %>%
  filter(!is.na(grade)) %>% 
  summarise(
    mean_diff = mean(ki67_v, na.rm = TRUE) - mean(ki67_d, na.rm = TRUE),
    rho = cor(ki67_v, ki67_d, use = "complete.obs", method = c("spearman")),
    P = cor.test(ki67_v, ki67_d, method = c("spearman"))$p.value
  )

# cycd1
x <- DF$cycd1_v
y <- DF$cycd1_d
wilcox.test(x, y, paired = TRUE)
cor.test(~ x + y, method = c("spearman"))

DF %>%
  group_by(factor(grade)) %>%
  filter(!is.na(grade)) %>% 
  summarise(
    mean_diff = mean(cycd1_v, na.rm = TRUE) - mean(cycd1_d, na.rm = TRUE),
    rho = cor(cycd1_v, cycd1_d, use = "complete.obs", method = c("spearman")),
    P = cor.test(cycd1_v, cycd1_d, method = c("spearman"))$p.value
  )

# mdm2
x <- DF$mdm2_v
y <- DF$mdm2_d
wilcox.test(x, y, paired = TRUE)
cor.test(~ x + y, method = c("spearman"))

DF %>%
  group_by(factor(grade)) %>%
  filter(!is.na(grade)) %>% 
  summarise(
    mean_diff = mean(mdm2_v, na.rm = TRUE) - mean(mdm2_d, na.rm = TRUE),
    rho = cor(mdm2_v, mdm2_d, use = "complete.obs", method = c("spearman")),
    P = cor.test(mdm2_v, mdm2_d, method = c("spearman"))$p.value
  )
