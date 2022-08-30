library(tidyverse)
library(showtext)
showtext_auto()
library(readxl)
library(clipr)

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

df_emp_all <- df_emp_all |>
  mutate(장애인코드 = ifelse(장애인코드 == 'N', '비장애인', '장애인')) |>
  mutate(장애인코드 = fct_relevel(장애인코드, '비장애인', '장애인'))


df_emp_all$학교지역 <- fct_relevel(df_emp_all$학교지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                               '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df_emp_all$설립구분 <- fct_relevel(df_emp_all$설립구분, '국립', '공립', '사립')

df_emp_all$학제 <- fct_relevel(df_emp_all$학제, '전문대학', '교육대학', '대학', 
                             '일반대학원', '기능대학', '각종대학(대학)', 
                             '산업대학')

df_emp_all$학교졸업생규모 <- fct_relevel(df_emp_all$학교졸업생규모, '500명미만', '500명이상-1000명미만', '1000명이상-1500명미만', 
                                  '1500명이상-2000명미만', '2000명이상-2500명미만', '2500명이상-3000명미만', 
                                  '3000명이상-3500명미만', '3500명이상-4000명미만', '4000명이상')

df_emp_all$학과대분류 <- fct_relevel(df_emp_all$학과대분류, '인문계열', '사회계열', '교육계열', 
                                '자연계열', '공학계열', '의약계열', '예체능계열')

df_emp_all <- df_emp_all |>
  mutate(학제과정 = case_when(
    학제 == '전문대학' ~ '전문대학과정', 
    학제 %in% c('교육대학', '대학', '기능대학', '각종대학(대학)', '산업대학') ~ '대학과정', 
    학제 == '일반대학원' ~ '대학원과정'
  ))

df_emp_all$학제과정 <- fct_relevel(df_emp_all$학제과정, '전문대학과정', '대학과정', '대학원과정') 

df_emp_all$거주지역 <- fct_relevel(df_emp_all$거주지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                               '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '미상')

df_emp_all$출신고교지역 <- fct_relevel(df_emp_all$출신고교지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '해외', '미상')


####################################################

df_emp_all |>
  group_by(장애인코드) |>
  summarise(sum = sum(all_sum)) |>
  mutate(percentage = sum/sum(sum)) |>
  write_clip()



######################################################

glimpse(df_emp_all)

df_emp_all_emp <- df_emp_all |>
  mutate(취업률분자 = `건보취업자 수 총합` + `취업자(해외취업자)` + `취업자(개인창작)` + `취업자(농림어업종사자)` + `취업자(1인창(사)업자)` + `취업자(프리랜서)`, 
         취업률분모 = `건보취업자 수 총합` + `총합계` - `진학자(국내-전문대학)` - `진학자(국내-대학)` - `진학자(국내-대학원)` - `진학자(국외-전문대학)` - `진학자(국외-대학)` - `진학자(국외-대학원)` - 입대자 - 취업불가능자 - 제외인정자 - 외국인유학생)

df_emp_all_emp |>
  group_by(장애인코드) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            해외취업자수 = sum(`취업자(해외취업자)`), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`), 
            프리랜서수 = sum(`취업자(프리랜서)`), 
            졸업자수 = sum(`건보취업자 수 총합` + `총합계`), 
            진학자수 = sum(`진학자(국내-전문대학)` + `진학자(국내-대학)` + `진학자(국내-대학원)` + `진학자(국외-전문대학)` + `진학자(국외-대학)` + `진학자(국외-대학원)`), 
            입대자수 = sum(입대자), 
            취업불가능자수 = sum(취업불가능자), 
            제외인정자수 = sum(제외인정자), 
            외국인유학생 = sum(외국인유학생),
            기타 = sum(기타), 
            미상 = sum(미상), 
            취업률 = sum(취업률분자)/sum(취업률분모)) |>
  write_clip()




df_emp_all_emp |>
  group_by(장애인코드) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
                  기타취업자수 = sum(`취업자(해외취업자)`) + sum(`취업자(농림어업종사자)`) + sum(`취업자(개인창작)`) + sum(`취업자(1인창(사)업자)`) + sum(`취업자(프리랜서)`),
#                  졸업자수 = sum(`건보취업자 수 총합` + `총합계`), 
                  진학자수 = sum(`진학자(국내-전문대학)` + `진학자(국내-대학)` + `진학자(국내-대학원)` + `진학자(국외-전문대학)` + `진학자(국외-대학)` + `진학자(국외-대학원)`),
            미취업자수 = sum(기타) + sum(미상)) |>
#  select(-외국인유학생) |>
  pivot_longer(2:5, names_to = '구분', values_to = '졸업생수') |>
  mutate(구분 = fct_relevel(구분, '건보취업자수', '기타취업자수', '미취업자수')) |> arrange(구분) |>
  group_by(장애인코드) |>
  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(pos) - 0.5 * pos) |>
  ggplot(aes(y = 졸업생수, x = 장애인코드)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = paste0(구분, ':', scales::comma(졸업생수), '명', '(', scales::percent(pos, accuracy = 0.1), ')')), size = 4) + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = '장애인과 비장애인의 졸업 후 상황 분포', subtitle = '입대자, 취업불가능자, 제외인정자, 외국인유학생 제외', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '졸업생 비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank())

## 700 * 1000

#########################################

df_emp_all_emp |>
  group_by(장애인코드) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()

df_emp_all_emp |>
  group_by(장애인코드) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                  해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                  농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                  개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                  `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                  프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                  취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  ggplot(aes(x = 장애인코드, y = 취업률)) +
  geom_col(aes(fill = 장애인코드), position = 'dodge') + 
  geom_text(aes(label = scales::percent(round(취업률, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
#  scale_fill_discrete(labels = c('비장애인', '장애인'), breaks = c('장애인', '비장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.005,0.10))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 취업률', y = '취업률', fill = '구분', x = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom')


df_emp_all_emp |>
  group_by(장애인코드) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                   해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                   농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                   개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                   `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                   프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                   취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  ggplot(aes(x = 1, y = 취업률)) +
  geom_point(aes(color = 장애인코드)) + 
  geom_line(aes(x = 1, y = 취업률))
  geom_text(aes(label = scales::percent(round(취업률, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
  #  scale_fill_discrete(labels = c('비장애인', '장애인'), breaks = c('장애인', '비장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.005,0.10))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 취업률', y = '취업률', fill = '구분', x = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom')
  
  

#########################################

df_emp_all_emp |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                  해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                  농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                  개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                  `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                  프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                  취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()


df_emp_all_emp |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                  해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                  농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                  개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                  `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                  프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                  취업률 = sum(취업률분자)/sum(취업률분모)
  )  |>
  ggplot(aes(x = 학교지역, y = 취업률)) +
  geom_col(aes(fill = 장애인코드), position = 'dodge') + 
  geom_text(aes(label = scales::percent(round(취업률, 3), accuracy = 0.1), group = 장애인코드), position = position_dodge(width = 0.9), size = 3, hjust = -0.2) +
  #  scale_fill_discrete(labels = c('비장애인', '장애인'), breaks = c('장애인', '비장애인')) + 
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.005,0.10))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 취업률', y = '취업률', fill = '구분', x = '구분') +
  theme_bw() +
  theme(legend.position = 'bottom')


df_emp_all_emp |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                   해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                   농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                   개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                   `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                   프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                   취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 학교지역, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 학교지역, xend = 학교지역)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff)) + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
#  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(1,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.05,0.05))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 지역별 취업률', y = '취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 

df_emp_all_emp |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
                  기타취업자수 = sum(`취업자(해외취업자)`) + sum(`취업자(농림어업종사자)`) + sum(`취업자(개인창작)`) + sum(`취업자(1인창(사)업자)`) + sum(`취업자(프리랜서)`),
                  #                  졸업자수 = sum(`건보취업자 수 총합` + `총합계`), 
                  진학자수 = sum(`진학자(국내-전문대학)` + `진학자(국내-대학)` + `진학자(국내-대학원)` + `진학자(국외-전문대학)` + `진학자(국외-대학)` + `진학자(국외-대학원)`),
                  미취업자수 = sum(기타) + sum(미상)) |>
  write_clip()




df_emp_all_emp |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
                  기타취업자수 = sum(`취업자(해외취업자)`) + sum(`취업자(농림어업종사자)`) + sum(`취업자(개인창작)`) + sum(`취업자(1인창(사)업자)`) + sum(`취업자(프리랜서)`),
                  #                  졸업자수 = sum(`건보취업자 수 총합` + `총합계`), 
                  진학자수 = sum(`진학자(국내-전문대학)` + `진학자(국내-대학)` + `진학자(국내-대학원)` + `진학자(국외-전문대학)` + `진학자(국외-대학)` + `진학자(국외-대학원)`),
                  미취업자수 = sum(기타) + sum(미상)) |>
  #  select(-외국인유학생) |>
  pivot_longer(3:6, names_to = '구분', values_to = '졸업생수') |>
  mutate(구분 = fct_relevel(구분, '건보취업자수', '기타취업자수', '미취업자수')) |> arrange(구분) |>
  group_by(장애인코드, 학교지역) |>
  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(pos) - 0.5 * pos) |>
  ggplot(aes(y = 졸업생수, x = 학교지역)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = scales::percent(pos, accuracy = 0.1)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = '장애인과 비장애인의 졸업 후 상황 분포', subtitle = '입대자, 취업불가능자, 제외인정자, 외국인유학생 제외', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(~장애인코드)

findoutlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

df_emp_all_emp |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(1, 2, 3) |>
  mutate(outlier = ifelse(findoutlier(건보취업자비율), 건보취업자비율, NA), 
         label = ifelse(!is.na(outlier), paste0(학교지역, ':', scales::percent(round(outlier, 2))), NA)) |>
  ggplot(aes(x = 장애인코드, y = 건보취업자비율)) +
  geom_boxplot() +
  ggrepel::geom_text_repel(aes(label=label), na.rm=TRUE, hjust=0) +
  stat_summary(geom = 'point', fun = 'mean', color = 'tomato3', shape = 4) +
  stat_summary(aes(label = scales::percent(..y..)), geom = 'text', fun.y = 'mean', color = 'tomato3', shape = 4, vjust = 1.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = '구분') +
  theme_classic()
  

#########################################

df_emp_all_emp |>
  group_by(장애인코드, 설립구분) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                  해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                  농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                  개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                  `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                  프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                  취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()

#########################################

df_emp_all_emp |>
  group_by(장애인코드, 학과대분류) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                  해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                  농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                  개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                  `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                  프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                  취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()


df_emp_all_emp |>
  filter(학제과정 == '전문대학과정') |>
  group_by(장애인코드, 학과대분류) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> select(9) |> pull() -> ttest

ttest1 <- ttest[1:7]
ttest2 <- ttest[8:14]

var.test(ttest1, ttest2)



df_emp_all_emp |>
  group_by(장애인코드, 학과대분류) |>
  summarise(건보취업 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                  해외취업 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                  농림어업종사자 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                  개인창작활동종사자 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                  `1인창사업자` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                  프리랜서 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                  취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
    select(-취업률) |>
  pivot_longer(3:8, names_to = '구분', values_to = '취업자비율') |>
  mutate(구분 = fct_relevel(구분, '건보취업', '해외취업', '농림어업종사자', '개인창작활동종사자', '1인창사업자', '프리랜서')) |> arrange(구분) |>
  group_by(장애인코드, 학과대분류) |>
#  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(취업자비율) - 0.5 * 취업자비율) |>
  ggplot(aes(y = 취업자비율, x = 학과대분류)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = if_else(취업자비율 > 0.5, scales::percent(취업자비율, accuracy = 0.1), NULL)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = '장애인과 비장애인의 학과대분류별 취업자 분포', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(~장애인코드)




df_emp_all_emp |>
  group_by(장애인코드, 학과대분류) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                   해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                   농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                   개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                   `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                   프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                   취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 학과대분류, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 학과대분류, xend = 학과대분류)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff)) + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(1,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.05,0.05))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 학과대계열별 취업률', y = '취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 


#########################################

df_emp_all_emp |>
  group_by(학제과정, 장애인코드, 학과대분류) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()


df_emp_all_emp |>
  group_by(장애인코드, 학제과정, 학과대분류) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> select(9) |> pull() -> ttest

ttest1 <- ttest[1:7]
ttest2 <- ttest[8:14]

var.test(ttest1, ttest2)



df_emp_all_emp |>
  group_by(장애인코드, 학제과정, 학과대분류) |>
  summarise(건보취업 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(-취업률) |>
  pivot_longer(4:9, names_to = '구분', values_to = '취업자비율') |>
  mutate(구분 = fct_relevel(구분, '건보취업', '해외취업', '농림어업종사자', '개인창작활동종사자', '1인창사업자', '프리랜서')) |> arrange(구분) |>
  group_by(장애인코드, 학제과정, 학과대분류) |>
  #  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(취업자비율) - 0.5 * 취업자비율) |>
  ggplot(aes(y = 취업자비율, x = 학과대분류)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = if_else(취업자비율 > 0.5, scales::percent(취업자비율, accuracy = 0.1), NULL)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(장애인코드~학제과정)




df_emp_all_emp |>
  group_by(장애인코드, 학제과정, 학과대분류) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 학과대분류, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 학과대분류, xend = 학과대분류)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff)) + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(1,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.25,0.25))) +
  coord_flip() +
  labs(y = '취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  facet_wrap(~학제과정) +
  theme(legend.position = 'bottom') 


#########################################

df_emp_all_emp <- df_emp_all_emp |>
  mutate(학제과정 = case_when(
    학제 == '전문대학' ~ '전문대학과정', 
    학제 %in% c('교육대학', '대학', '기능대학', '각종대학(대학)', '산업대학') ~ '대학과정', 
    학제 == '일반대학원' ~ '대학원과정'
  ))

df_emp_all_emp$학제과정 <- fct_relevel(df_emp_all_emp$학제과정, '전문대학과정', '대학과정', '대학원과정') 


df_emp_all_emp |>
  group_by(장애인코드, 학제과정) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                  해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                  농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                  개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                  `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                  프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                  취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()


df_emp_all_emp |>
  group_by(장애인코드, 학제과정) |>
  summarise(건보취업 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                해외취업 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                농림어업종사자 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                개인창작활동종사자 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                `1인창사업자` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                프리랜서 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(-취업률) |>
  pivot_longer(3:8, names_to = '구분', values_to = '취업자비율') |>
  mutate(구분 = fct_relevel(구분, '건보취업', '해외취업', '농림어업종사자', '개인창작활동종사자', '1인창사업자', '프리랜서')) |> arrange(구분) |>
  group_by(장애인코드, 학제과정) |>
  #  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(취업자비율) - 0.5 * 취업자비율) |>
  ggplot(aes(y = 취업자비율, x = 학제과정)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = if_else(취업자비율 > 0.5, scales::percent(취업자비율, accuracy = 0.1), NULL)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = '장애인과 비장애인의 학위과정별 취업자 분포', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(~장애인코드)




df_emp_all_emp |>
  group_by(장애인코드, 학제과정) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                   해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                   농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                   개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                   `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                   프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                   취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 학제과정, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 학제과정, xend = 학제과정)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff)) + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(0.5,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.05,0.05))) +
  coord_flip() +
  labs(y = '취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 

#######################################################

distinct(df_emp_all_emp, 거주지역)

df_emp_all_emp$거주지역 <- fct_relevel(df_emp_all$거주지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                               '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '미상')

df_emp_all_emp |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                  해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                  농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                  개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                  `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                  프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                  취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()



df_emp_all_emp |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                해외취업 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                농림어업종사자 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                개인창작활동종사자 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                `1인창사업자` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                프리랜서 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(-취업률) |>
  pivot_longer(3:8, names_to = '구분', values_to = '취업자비율') |>
  mutate(구분 = fct_relevel(구분, '건보취업', '해외취업', '농림어업종사자', '개인창작활동종사자', '1인창사업자', '프리랜서')) |> arrange(구분) |>
  group_by(장애인코드, 거주지역) |>
  #  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(취업자비율) - 0.5 * 취업자비율) |>
  ggplot(aes(y = 취업자비율, x = 거주지역)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = if_else(취업자비율 > 0.5, scales::percent(취업자비율, accuracy = 0.1), NULL)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = '장애인과 비장애인의 거주지역별 취업자 분포', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(~장애인코드)




df_emp_all_emp |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                   해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                   농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                   개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                   `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                   프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                   취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 거주지역, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 거주지역, xend = 거주지역)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff)) + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(0.5,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.025,0.025))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 거주지역별 취업률', y = '취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 


df_emp_all_emp |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(1, 2, 3) |>
  mutate(outlier = ifelse(findoutlier(건보취업자비율), 건보취업자비율, NA), 
         label = ifelse(!is.na(outlier), paste0(거주지역, ':', scales::percent(round(outlier, 2))), NA)) |>
  ggplot(aes(x = 장애인코드, y = 건보취업자비율)) +
  geom_boxplot() +
  ggrepel::geom_text_repel(aes(label=label), na.rm=TRUE, hjust=0) +
  stat_summary(geom = 'point', fun = 'mean', color = 'tomato3', shape = 4) +
  stat_summary(aes(label = scales::percent(..y..)), geom = 'text', fun.y = 'mean', color = 'tomato3', shape = 4, vjust = 1.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = '구분') +
  theme_classic()


df_emp_all_emp |>
  filter(거주지역 != '미상') |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> select(3) |> pull() -> ttest

ttest1 <- ttest[1:17]
ttest2 <- ttest[18:34]

var.test(ttest1, ttest2)

#######################################################

#######################################################

distinct(df_emp_all_emp, 출신고교지역)

df_emp_all_emp$출신고교지역 <- fct_relevel(df_emp_all$출신고교지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                                   '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '미상')

df_emp_all_emp |>
  filter(출신고교지역 != '미상') |>
  group_by(장애인코드, 출신고교지역) |>
  summarise(전체 = sum(취업률분자),
            건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()



df_emp_all_emp |>
  filter(출신고교지역 != '미상') |>
  group_by(장애인코드, 출신고교지역) |>
  summarise(건보취업 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(-취업률) |>
  pivot_longer(3:8, names_to = '구분', values_to = '취업자비율') |>
  mutate(구분 = fct_relevel(구분, '건보취업', '해외취업', '농림어업종사자', '개인창작활동종사자', '1인창사업자', '프리랜서')) |> arrange(구분) |>
  group_by(장애인코드, 출신고교지역) |>
  #  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(취업자비율) - 0.5 * 취업자비율) |>
  ggplot(aes(y = 취업자비율, x = 출신고교지역)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = if_else(취업자비율 > 0.5, scales::percent(취업자비율, accuracy = 0.1), NULL)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = '장애인과 비장애인의 출신 고교 지역별 취업자 분포', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(~장애인코드)




df_emp_all_emp |>
  filter(출신고교지역 != '미상') |>
  group_by(장애인코드, 출신고교지역) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 출신고교지역, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  filter(출신고교지역 != '미상') |>
  ggplot(aes(x = 출신고교지역, xend = 출신고교지역)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff)) + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(0.5,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.025,0.025))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 출신고교지역별 취업률', y = '취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 


df_emp_all_emp |>
  filter(출신고교지역 != '미상') |>
  group_by(장애인코드, 출신고교지역) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(1, 2, 9) |>
  mutate(outlier = ifelse(findoutlier(취업률), 취업률, NA), 
         label = ifelse(!is.na(outlier), paste0(출신고교지역, ':', scales::percent(round(outlier, 2))), NA)) |>
  ggplot(aes(x = 장애인코드, y = 취업률)) +
  geom_boxplot() +
  geom_jitter() +
  ggrepel::geom_text_repel(aes(label=label), na.rm=TRUE, hjust=0) +
  stat_summary(geom = 'point', fun = 'mean', color = 'tomato3', shape = 4) +
  stat_summary(aes(label = scales::percent(..y..)), geom = 'text', fun.y = 'mean', color = 'tomato3', shape = 4, vjust = 1.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = '구분') +
  theme_classic()



df_emp_all_emp |>
  filter(출신고교지역 != '미상') |>
  group_by(장애인코드, 출신고교지역) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> select(3) |> pull() -> ttest

ttest1 <- ttest[1:18]
ttest2 <- ttest[19:36]

var.test(ttest1, ttest2)



##################################################################

distinct(df_emp_all_emp, 출신고교지역)

df_emp_all_emp$출신고교지역 <- fct_relevel(df_emp_all_emp$출신고교지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주', '해외', '미상')

df_emp_all_emp |>
  mutate(대학_거주 = case_when(
    as.character(출신고교지역) == as.character(학교지역) ~ '일치',
    TRUE ~ '불일치'
  )
  ) |>
  group_by(장애인코드, 대학_거주) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                  해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                  농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                  개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                  `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                  프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                  취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
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




df_emp_all_emp |>
  mutate(대학_고교 = case_when(
    as.character(출신고교지역) == as.character(학교지역) ~ '일치',
    TRUE ~ '불일치'
  )
  ) |> 
  group_by(장애인코드, 대학_고교) |>
  summarise(건보취업 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                해외취업 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                농림어업종사자 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                개인창작활동종사자 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                `1인창사업자` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                프리랜서 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(-취업률) |>
  pivot_longer(3:8, names_to = '구분', values_to = '취업자비율') |>
  mutate(구분 = fct_relevel(구분, '건보취업', '해외취업', '농림어업종사자', '개인창작활동종사자', '1인창사업자', '프리랜서')) |> arrange(구분) |>
  group_by(장애인코드, 대학_고교) |>
  #  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(취업자비율) - 0.5 * 취업자비율) |>
  ggplot(aes(y = 취업자비율, x = 대학_고교)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = if_else(취업자비율 > 0.5, scales::percent(취업자비율, accuracy = 0.1), NULL)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = '장애인과 비장애인의 거주지역별 취업자 분포', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(~장애인코드)




df_emp_all_emp |>
  mutate(대학_고교 = case_when(
    as.character(출신고교지역) == as.character(학교지역) ~ '일치',
    TRUE ~ '불일치'
  )
  ) |> 
  group_by(장애인코드, 대학_고교) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                   해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                   농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                   개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                   `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                   프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                   취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 대학_고교, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 대학_고교, xend = 대학_고교)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff)) + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(0.5,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.025,0.025))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 고교-대학 지역별 취업률', y = '취업률', x = '대학과 고교 지역 일치 구분', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 



#########################################

df_emp_all_emp |>
  group_by(장애인코드, 졸업평점) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                  해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                  농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                  개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                  `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                  프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                  취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()


df_emp_all_emp |>
  group_by(장애인코드, 졸업평점) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> select(9) |> pull() -> ttest

ttest1 <- ttest[1:9]
ttest2 <- ttest[10:18]

var.test(ttest1, ttest2)


df_emp_all_emp |>
  group_by(장애인코드, 졸업평점) |>
  summarise(건보취업 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                해외취업 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                농림어업종사자 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                개인창작활동종사자 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                `1인창사업자` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                프리랜서 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(-취업률) |>
  pivot_longer(3:8, names_to = '구분', values_to = '취업자비율') |>
  mutate(구분 = fct_relevel(구분, '건보취업', '해외취업', '농림어업종사자', '개인창작활동종사자', '1인창사업자', '프리랜서')) |> arrange(구분) |>
  group_by(장애인코드, 졸업평점) |>
  #  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(취업자비율) - 0.5 * 취업자비율) |>
  ggplot(aes(y = 취업자비율, x = 졸업평점)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = if_else(취업자비율 > 0.5, scales::percent(취업자비율, accuracy = 0.1), NULL)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = '장애인과 비장애인의 졸업평점별 취업자 분포', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(~장애인코드)




df_emp_all_emp |>
  group_by(장애인코드, 졸업평점) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
                   해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
                   농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
                   개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
                   `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
                   프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
                   취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 졸업평점, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 졸업평점, xend = 졸업평점)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff), linetype = 'dashed') + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  geom_line(aes(y = 장애인, group = 1), color = 'red')+ 
  geom_line(aes(y = 비장애인, group = 1), color = 'blue')+ 
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(0.5,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.05,0.05))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 졸업평점별 취업률', y = '취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 


#########################################

df_emp_all_emp |>
  group_by(학제과정, 장애인코드, 졸업평점) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()


df_emp_all_emp |>
  group_by(장애인코드, 졸업평점) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> select(9) |> pull() -> ttest

ttest1 <- ttest[1:9]
ttest2 <- ttest[10:18]

var.test(ttest1, ttest2)


df_emp_all_emp |>
  group_by(장애인코드, 학제과정, 졸업평점) |>
  summarise(건보취업 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(-취업률) |> 
  pivot_longer(4:9, names_to = '구분', values_to = '취업자비율') |>
  mutate(구분 = fct_relevel(구분, '건보취업', '해외취업', '농림어업종사자', '개인창작활동종사자', '1인창사업자', '프리랜서')) |> arrange(구분) |>
  group_by(장애인코드, 학제과정, 졸업평점) |>
  #  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(취업자비율) - 0.5 * 취업자비율) |> 
  ggplot(aes(y = 취업자비율, x = 졸업평점)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = if_else(취업자비율 > 0.5, scales::percent(취업자비율, accuracy = 0.1), NULL)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_classic() +
  labs(title = '장애인과 비장애인의 졸업평점별 취업자 분포', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(장애인코드~학제과정)




df_emp_all_emp |>
  group_by(장애인코드, 학제과정, 졸업평점) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 학제과정, 졸업평점, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 졸업평점, xend = 졸업평점)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff), linetype = 'dashed') + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  geom_line(aes(y = 장애인, group = 1), color = 'red')+ 
  geom_line(aes(y = 비장애인, group = 1), color = 'blue')+ 
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(0.5,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.05,0.05))) +
  coord_flip() +
  labs( y = '취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_wrap(~학제과정)



#########################################

df_emp_all_emp |>
  group_by(장애인코드, 학교졸업생규모) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()

df_emp_all_emp |>
  group_by(장애인코드, 학교졸업생규모) |>
  summarise(건보취업 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(-취업률) |>
  pivot_longer(3:8, names_to = '구분', values_to = '취업자비율') |>
  mutate(구분 = fct_relevel(구분, '건보취업', '해외취업', '농림어업종사자', '개인창작활동종사자', '1인창사업자', '프리랜서')) |> arrange(구분) |>
  group_by(장애인코드, 학교졸업생규모) |>
  #  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(취업자비율) - 0.5 * 취업자비율) |>
  ggplot(aes(y = 취업자비율, x = 학교졸업생규모)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = if_else(취업자비율 > 0.5, scales::percent(취업자비율, accuracy = 0.1), NULL)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(~장애인코드)

df_emp_all_emp |>
  group_by(장애인코드, 졸업평점) |>
  summarise(건보취업 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  select(-취업률) |>
  pivot_longer(3:8, names_to = '구분', values_to = '취업자비율') |>
  mutate(구분 = fct_relevel(구분, '건보취업', '해외취업', '농림어업종사자', '개인창작활동종사자', '1인창사업자', '프리랜서')) |> arrange(구분) |>
  group_by(장애인코드, 졸업평점) |>
  #  mutate(pos = 졸업생수 / sum(졸업생수)) |>
  mutate(pos1 = cumsum(취업자비율) - 0.5 * 취업자비율) |>
  ggplot(aes(y = 취업자비율, x = 졸업평점)) +
  geom_col(aes(fill = 구분), position = position_fill(reverse = TRUE)) + 
  geom_text(aes(y = pos1, label = if_else(취업자비율 > 0.5, scales::percent(취업자비율, accuracy = 0.1), NULL)), size = 4) +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  labs(title = '장애인과 비장애인의 졸업평점별 취업자 분포', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB', y = '비율', x = '구분') +
  theme(legend.position="bottom",legend.title = element_blank()) +
  coord_flip() +
  facet_wrap(~장애인코드)


#########################################

df_emp_all_emp$학교졸업생규모 <- fct_relevel(df_emp_all_emp$학교졸업생규모, '500명미만', '500명이상-1000명미만', '1000명이상-1500명미만', 
                                      '1500명이상-2000명미만', '2000명이상-2500명미만', '2500명이상-3000명미만', 
                                      '3000명이상-3500명미만', '3500명이상-4000명미만', '4000명이상')


df_emp_all_emp |>
  group_by(장애인코드, 학교졸업생규모) |>
  summarise(대상자 = sum(취업률분자), 
    건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()

df_emp_all_emp |>
  group_by(장애인코드, 학교졸업생규모) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> select(9) |> pull() -> ttest

ttest1 <- ttest[1:9]
ttest2 <- ttest[10:18]

t.test(ttest1, ttest2)

df_emp_all_emp |>
  group_by(장애인코드, 학교졸업생규모) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 학교졸업생규모, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 학교졸업생규모, xend = 학교졸업생규모)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff), linetype = 'dashed') + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  geom_line(aes(y = 장애인, group = 1), color = 'red')+ 
  geom_line(aes(y = 비장애인, group = 1), color = 'blue')+ 
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(0.5,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.05,0.05))) +
  coord_flip() +
  labs(y = '취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 


#########################################

df_emp_all_emp <- df_emp_all_emp |>
  mutate(학제과정 = case_when(
    학제 == '전문대학' ~ '전문대학과정', 
    학제 %in% c('교육대학', '대학', '기능대학', '각종대학(대학)', '산업대학') ~ '대학과정', 
    학제 == '일반대학원' ~ '대학원과정'
  ))

df_emp_all_emp$학제과정 <- fct_relevel(df_emp_all_emp$학제과정, '전문대학과정', '대학과정', '대학원과정') 


df_emp_all_emp$학교졸업생규모 <- fct_relevel(df_emp_all_emp$학교졸업생규모, '500명미만', '500명이상-1000명미만', '1000명이상-1500명미만', 
                                      '1500명이상-2000명미만', '2000명이상-2500명미만', '2500명이상-3000명미만', 
                                      '3000명이상-3500명미만', '3500명이상-4000명미만', '4000명이상')


df_emp_all_emp |>
  group_by(장애인코드, 학제과정, 학교졸업생규모) |>
  summarise(대상자 = sum(취업률분자), 
            건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |>
  write_clip()

df_emp_all_emp |>
  filter(학제과정 == '대학과정') |>
  group_by(장애인코드, 학교졸업생규모) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자수 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자수 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자수 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자수` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서수 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> select(9) |> pull() -> ttest

ttest1 <- ttest[1:7]
ttest2 <- ttest[8:14]

var.test(ttest1, ttest2)

df_emp_all_emp |>
  group_by(장애인코드, 학제과정, 학교졸업생규모) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 학제과정, 학교졸업생규모, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 학교졸업생규모, xend = 학교졸업생규모)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff), linetype = 'dashed') + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  geom_line(aes(y = 장애인, group = 1), color = 'red')+ 
  geom_line(aes(y = 비장애인, group = 1), color = 'blue')+ 
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(0.5,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.25,0.25))) +
  coord_flip() +
  labs(y = '취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  facet_wrap(~학제과정) +
  theme(legend.position = 'bottom') 

