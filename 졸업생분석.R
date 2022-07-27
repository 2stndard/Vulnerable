library(tidyverse)
library(showtext)
showtext_auto()
library(readxl)
library(clipr)

df_insu <- read_excel('D:/R/data/Vulnerable/Vulnerable.xlsx', sheet = '건보취업자', skip = 3, 
           col_names = TRUE, col_types = c(rep('text', 25), rep('numeric', 8))
           )

df_non_insu <- read_excel('D:/R/data/Vulnerable/Vulnerable.xlsx', sheet = '건보취업자외', skip = 3, 
                          col_names = TRUE, col_types = c(rep('text', 18), rep('numeric', 19)))

df_emp_all <- read_excel('D:/R/data/Vulnerable/Vulnerable1.xlsx', sheet = 'Sheet1', skip = 3, 
                         col_names = TRUE, col_types = c(rep('text', 24), rep('numeric', 27)))

glimpse(df_emp_all)

df_emp_all <- df_emp_all |>
  mutate(출신고교지역 = ifelse(출신고교지역 == '-', '미상', 출신고교지역))

df_emp_all <- df_emp_all |>
  filter(외국인유학생여부 == 'N')

df_emp_all <- df_emp_all |>
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

df_emp_all <- df_emp_all |>
  mutate_if(is.character, as.factor)


df_emp_all <- df_emp_all |>
  mutate(all_sum = `건보취업자 수 총합` + `총합계` - `외국인유학생`)

######################################################

df_emp_all |>
  group_by(장애인코드) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()

df_emp_all |>
  group_by(장애인코드) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  ggplot() +
  geom_col(aes(x = 1, y = percentage, fill = 장애인코드), position = position_dodge()) + 
  coord_polar()

######################################################

df_emp_all$학교지역 <- fct_relevel(df_emp_all$학교지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                               '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df_emp_all |>
  group_by(장애인코드, 학교지역) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()

install.packages('ggstance')


df_emp_all |>
  group_by(장애인코드, 학교지역) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  ggplot(aes(x = 학교지역, y = percentage)) + 
  annotate('rect', xmin = 0.5, xmax = 1.5, ymin = -Inf, ymax = Inf, fill = 'grey80', alpha = 0.5) +
  annotate('rect', xmin = 2.5, xmax = 3.5, ymin = -Inf, ymax = Inf, fill = 'grey80', alpha = 0.5) +
  annotate('rect', xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf, fill = 'grey80', alpha = 0.5) +
  annotate('rect', xmin = 6.5, xmax = 7.5, ymin = -Inf, ymax = Inf, fill = 'grey80', alpha = 0.5) +
  annotate('rect', xmin = 8.5, xmax = 9.5, ymin = -Inf, ymax = Inf, fill = 'grey80', alpha = 0.5) +
  annotate('rect', xmin = 10.5, xmax = 11.5, ymin = -Inf, ymax = Inf, fill = 'grey80', alpha = 0.5) +
  annotate('rect', xmin = 12.5, xmax = 13.5, ymin = -Inf, ymax = Inf, fill = 'grey80', alpha = 0.5) +
  annotate('rect', xmin = 14.5, xmax = 15.5, ymin = -Inf, ymax = Inf, fill = 'grey80', alpha = 0.5) +
  annotate('rect', xmin = 16.5, xmax = 17.5, ymin = -Inf, ymax = Inf, fill = 'grey80', alpha = 0.5) +
  geom_col(aes(fill = 장애인코드), position = 'dodge') + 
  geom_text(aes(label = scales::percent(round(percentage, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
  scale_fill_discrete(labels = c('비장애인', '장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.215)) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 지역별 분포 비율', y = '비율', fill = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom') 
  
    

######################################################

df_emp_all$설립구분 <- fct_relevel(df_emp_all$설립구분, '국립', '공립', '사립')

df_emp_all |>
  group_by(장애인코드, 설립구분) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()

df_emp_all |>
  group_by(장애인코드, 설립구분) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  ggplot(aes(x = 설립구분, y = percentage)) + 
  geom_col(aes(fill = 장애인코드), position = 'dodge') + 
  geom_text(aes(label = scales::percent(round(percentage, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
  scale_fill_discrete(labels = c('비장애인', '장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.85)) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 국공사립 대학 분포 비율', y = '비율', fill = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom') 

#######################################################

distinct(df_emp_all, 학제)

df_emp_all$학제 <- fct_relevel(df_emp_all$학제, '전문대학', '교육대학', '대학', 
                                  '기능대학', '각종대학(대학)', 
                                  '산업대학',  '일반대학원')

df_emp_all |>
  group_by(장애인코드, 학제) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()

df_emp_all |>
  group_by(장애인코드, 학제) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  ggplot(aes(x = 학제, y = percentage)) + 
  geom_col(aes(fill = 장애인코드), position = 'dodge') + 
  geom_text(aes(label = scales::percent(round(percentage, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
  scale_fill_discrete(labels = c('비장애인', '장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.62)) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 대학 종류별 분포 비율', y = '비율', fill = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom') 

#######################################################

distinct(df_emp_all, 학교졸업생규모)

df_emp_all$학교졸업생규모 <- fct_relevel(df_emp_all$학교졸업생규모, '500명미만', '500명이상-1000명미만', '1000명이상-1500명미만', 
                                  '1500명이상-2000명미만', '2000명이상-2500명미만', '2500명이상-3000명미만', 
                                  '3000명이상-3500명미만', '3500명이상-4000명미만', '4000명이상')

df_emp_all |>
  group_by(장애인코드, 학교졸업생규모) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()

df_emp_all |>
  group_by(장애인코드, 학교졸업생규모) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  ggplot(aes(x = 학교졸업생규모, y = percentage)) + 
  geom_col(aes(fill = 장애인코드), position = 'dodge') + 
  geom_text(aes(label = scales::percent(round(percentage, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
  scale_fill_discrete(labels = c('비장애인', '장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.005,0.025))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 대학 크기별 분포 비율', y = '비율', fill = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom') 

#######################################################

distinct(df_emp_all, 학과대분류)

df_emp_all$학과대분류 <- fct_relevel(df_emp_all$학과대분류, '인문계열', '사회계열', '교육계열', 
                                  '자연계열', '공학계열', '의약계열', '예체능계열')

df_emp_all |>
  group_by(장애인코드, 학과대분류) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()

df_emp_all <- df_emp_all |>
  mutate(학제과정 = case_when(
    학제 == '전문대학' ~ '전문대학과정', 
    학제 %in% c('교육대학', '대학', '기능대학', '각종대학(대학)', '산업대학') ~ '대학과정', 
    학제 == '일반대학원' ~ '대학원과정'
  ))

df_emp_all$학제과정 <- fct_relevel(df_emp_all$학제과정, '전문대학과정', '대학과정', '대학원과정') 
                                
df_emp_all |>
  group_by(장애인코드, 학제과정, 학과대분류) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()

df_emp_all |>
  group_by(장애인코드, 학제과정, 학과대분류) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  ggplot(aes(x = 학과대분류, y = percentage)) + 
  geom_col(aes(fill = 장애인코드), position = 'dodge') + 
  geom_text(aes(label = scales::percent(round(percentage, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
  scale_fill_discrete(labels = c('비장애인', '장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.005,0.025))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 학과 대계열별 분포 비율', y = '비율', fill = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom') + 
  facet_wrap(~학제과정)

#######################################################

distinct(df_emp_all, 학과중분류)
df_emp_all$학과중분류1 <- gsub('ㆍ', '/', df_emp_all$학과중분류)

df_emp_all |>
#  filter(학과대분류 == '사회계열') |>
  group_by(장애인코드, 학제과정, 학과대분류, 학과중분류) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()



df_emp_all |>
#  filter(학과대분류 == '사회계열') |>
  group_by(장애인코드, 학제과정, 학과대분류, 학과중분류1) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  ggplot(aes(x = 학과중분류1, y = percentage)) + 
  geom_col(aes(fill = 장애인코드), position = 'dodge') + 
  geom_text(aes(label = scales::percent(round(percentage, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
  scale_fill_discrete(labels = c('비장애인', '장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.005,0.025))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 학과 대계열별 분포 비율', y = '비율', fill = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom') + 
  facet_grid(학과대분류~학제과정, scale = 'free', space ='free')


#######################################################

distinct(df_emp_all, 학과소분류)


df_emp_all |>
#  filter(학과대분류 == '사회계열') |>
  group_by(장애인코드, 학과소분류) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()


#######################################################

distinct(df_emp_all, 거주지역)

df_emp_all$거주지역 <- fct_relevel(df_emp_all$거주지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                               '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '미상')

df_emp_all |>
  #  filter(학과대분류 == '사회계열') |>
  group_by(장애인코드, 거주지역) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()

df_emp_all |>
  #  filter(학과대분류 == '사회계열') |>
  group_by(장애인코드, 거주지역) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  ggplot(aes(x = 거주지역, y = percentage)) + 
  geom_col(aes(fill = 장애인코드), position = 'dodge') + 
  geom_text(aes(label = scales::percent(round(percentage, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
  scale_fill_discrete(labels = c('비장애인', '장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.005,0.025))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 거주지역별 분포 비율', y = '비율', fill = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom') 


#######################################################

distinct(df_emp_all, 거주지역)

df_emp_all$거주지역 <- fct_relevel(df_emp_all$거주지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                               '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '미상')

df_emp_all |>
  mutate(대학_거주 = case_when(
    as.character(거주지역) == as.character(학교지역) ~ 1,
    TRUE ~ 0
  )
  ) |>
  group_by(장애인코드, 대학_거주) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()

df_emp_all |>
  mutate(대학_거주 = case_when(
    as.character(거주지역) == as.character(학교지역) ~ 1,
    TRUE ~ 0
  )
  ) |> 
  group_by(장애인코드, 학교지역, 대학_거주) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |> View()
  write_clip()

#######################################################

distinct(df_emp_all, 출신고교지역)

df_emp_all$출신고교지역 <- fct_relevel(df_emp_all$출신고교지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '해외', '미상')

df_emp_all |>
  mutate(대학_거주 = case_when(
    as.character(거주지역) == as.character(학교지역) ~ 1,
    TRUE ~ 0
  )
  ) |>
  group_by(장애인코드, 대학_거주) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()

df_emp_all |>
  mutate(대학_고교 = case_when(
    as.character(출신고교지역) == as.character(학교지역) ~ 1,
    TRUE ~ 0
  )
  ) |> 
  group_by(장애인코드, 학교지역, 대학_고교) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |> 
  write_clip()


df_emp_all |>
  mutate(대학_고교 = case_when(
    as.character(출신고교지역) == as.character(학교지역) ~ '일치',
    TRUE ~ '불일치'
  )
  ) |> 
  group_by(장애인코드, 학교지역, 대학_고교) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  ggplot(aes(x = 학교지역, y = percentage)) + 
  geom_col(aes(fill = 장애인코드), position = 'dodge') + 
  geom_text(aes(label = scales::percent(round(percentage, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
  scale_fill_discrete(labels = c('비장애인', '장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.005,0.025))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 거주지역별 분포 비율', y = '비율', fill = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_grid(~대학_고교)


#######################################################

distinct(df_emp_all, `학사학위취득유예기간(학기)`)


df_emp_all |>
  mutate(유예 = case_when(
    `학사학위취득유예기간(학기)` == 0 ~ '비유예', 
    TRUE ~ '유예'
  )) |>
  #  filter(학과대분류 == '사회계열') |>
  group_by(장애인코드, 유예) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()


#######################################################

distinct(df_emp_all, 졸업평점)


df_emp_all |>
  #  filter(학과대분류 == '사회계열') |>
  group_by(장애인코드, 졸업평점) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()


