
#########################################

df_emp_all_emp |> glimpse()

#########################################

df_emp_all_emp |>
  filter(출신고교지역 != '미상') |>
  group_by(장애인코드, 출신고교지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |>
  write_clip()



df_emp_all_emp |>
  filter(출신고교지역 != '미상') |>
  group_by(장애인코드, 출신고교지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 출신고교지역, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 

temp$`산업분류(대분류)` <- gsub('달리 분류되지 않은', '기타', temp$`산업분류(대분류)`)


temp |>
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
  scale_x_discrete(limits = rev, expand = expansion(add = c(1,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.05,0.05))) +
  coord_flip() +
  labs(y = '취업자 비율', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 


#########################################


df_emp_all_emp |>
  mutate(근무_거주 = case_when(
    as.character(근무지역) == as.character(거주지역) ~ '일치',
    TRUE ~ '불일치'
  )
  ) |> 
  group_by(장애인코드, 거주지역, 근무_거주) |>
  summarise(건보취업자비율 = sum(`건보취업자 수 총합`) / sum(취업률분자), 
            해외취업자비율 = sum(`취업자(해외취업자)`) / sum(취업률분자), 
            농림어업종사자비율 = sum(`취업자(농림어업종사자)`) / sum(취업률분자), 
            개인창작활동종사자비율 = sum(`취업자(개인창작)`) / sum(취업률분자), 
            `1인창사업자비율` = sum(`취업자(1인창(사)업자)`) / sum(취업률분자), 
            프리랜서비율 = sum(`취업자(프리랜서)`) / sum(취업률분자), 
            취업률 = sum(취업률분자)/sum(취업률분모)
  ) |> 
  select(장애인코드, 거주지역, 근무_거주, 취업률) |>
  pivot_wider(names_from = 장애인코드, values_from = 취업률) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) -> temp 


temp |>
  ggplot(aes(x = 근무_거주, xend = 근무_거주)) + 
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
  labs( y = '취업률', x = '대학과 고교 지역 일치 구분', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  facet_wrap(~거주지역) +
  theme_bw() +
  theme(legend.position = 'bottom') 


#########################################
df_emp_all_emp$근무지역 <- fct_relevel(df_emp_all_emp$근무지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                                   '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df_emp_all_emp$학교지역 <- fct_relevel(df_emp_all_emp$학교지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                                   '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

library(treemapify)

df_emp_all_emp |>  
  filter(!is.na(근무지역), 근무지역 != '-', 장애인코드 == '장애인') |>
  group_by(장애인코드, 학교지역) |> 
  mutate(total = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학교지역, 근무지역) |>
  summarise(rate = sum(`취업자(건보가입자)`) / mean(total), 
            total = sum(`취업자(건보가입자)`)) |> 
  ggplot(aes(area = total, fill = total, label = paste0(근무지역, ':', scales::percent(rate, accuracy = 0.1)), subgroup = 학교지역)) +
  geom_treemap() +
  geom_treemap_text(colour = "white") +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "white",
                             fontface = "italic") +
  theme(legend.position = "none")

    
full_join(df_emp_all_emp |>  
    filter(!is.na(근무지역), 근무지역 != '-', 장애인코드 == '비장애인') |>
    group_by(장애인코드, 학교지역) |> 
    mutate(total = sum(`취업자(건보가입자)`)) |> 
    ungroup() |>
    group_by(장애인코드, 학교지역, 근무지역) |>
    summarise(rate = sum(`취업자(건보가입자)`) / mean(total), 
              total = sum(`취업자(건보가입자)`)), 
df_emp_all_emp |>  
  filter(!is.na(근무지역), 근무지역 != '-', 장애인코드 == '장애인') |>
  group_by(장애인코드, 학교지역) |> 
  mutate(total = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학교지역, 근무지역) |>
  summarise(rate = sum(`취업자(건보가입자)`) / mean(total), 
            total = sum(`취업자(건보가입자)`)), 
by = c('근무지역', '학교지역')
) |> write_clip()
  

#########################################
df_emp_all_emp$거주지역 <- fct_relevel(df_emp_all_emp$거주지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                                   '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

distinct(df_emp_all_emp, 거주지역)

df_emp_all_emp |>  
  filter(!is.na(거주지역), 거주지역 != '미상', !is.na(근무지역), 근무지역 != '-', 장애인코드 == '장애인') |>
  group_by(장애인코드, 거주지역) |> 
  mutate(total = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 거주지역, 근무지역) |>
  summarise(rate = sum(`취업자(건보가입자)`) / mean(total), 
            total = sum(`취업자(건보가입자)`)) |> 
  ggplot(aes(area = total, fill = total, label = paste0(근무지역, ':', scales::percent(rate, accuracy = 0.1)), subgroup = 거주지역)) +
  geom_treemap() +
  geom_treemap_text(colour = "white") +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "white",
                             fontface = "italic") +
  theme(legend.position = "none")


full_join(df_emp_all_emp |>  
            filter(!is.na(거주지역), 거주지역 != '미상', !is.na(근무지역), 근무지역 != '-', 장애인코드 == '비장애인') |>
            group_by(장애인코드, 거주지역) |> 
            mutate(total = sum(`취업자(건보가입자)`)) |> 
            ungroup() |>
            group_by(장애인코드, 거주지역, 근무지역) |>
            summarise(rate = sum(`취업자(건보가입자)`) / mean(total), 
                      total = sum(`취업자(건보가입자)`)), 
          df_emp_all_emp |>  
            filter(!is.na(거주지역), 거주지역 != '미상', !is.na(근무지역), 근무지역 != '-', 장애인코드 == '장애인') |>
            group_by(장애인코드, 거주지역) |> 
            mutate(total = sum(`취업자(건보가입자)`)) |> 
            ungroup() |>
            group_by(장애인코드, 거주지역, 근무지역) |>
            summarise(rate = sum(`취업자(건보가입자)`) / mean(total), 
                      total = sum(`취업자(건보가입자)`)), 
          by = c('거주지역', '근무지역')
) |> write_clip()


#########################################
df_emp_all_emp$거주지역 <- fct_relevel(df_emp_all_emp$거주지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                                   '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df_emp_all_emp <- df_emp_all_emp |>
  mutate(대학권역 = case_when(
    학교지역 %in% c('서울', '인천', '경기') ~ '수도권', 
    학교지역 %in% c('대구', '경북', '강원') ~ '대구경북강원',
    학교지역 %in% c('대전', '세종', '충북', '충남') ~ '충청',
    학교지역 %in% c('광주', '전북', '전남', '제주') ~ '호남제주',
    학교지역 %in% c('부산', '울산', '경남') ~ '부산울산경남'
  ))

df_emp_all_emp$대학권역 <- fct_relevel(df_emp_all_emp$대학권역, '수도권', '대구경북강원', '충청', '호남제주', '부산울산경남')

df_emp_all_emp <- df_emp_all_emp |>
  mutate(거주권역 = case_when(
    거주지역 %in% c('서울', '인천', '경기') ~ '수도권', 
    거주지역 %in% c('대구', '경북', '강원') ~ '대구경북강원',
    거주지역 %in% c('대전', '세종', '충북', '충남') ~ '충청',
    거주지역 %in% c('광주', '전북', '전남', '제주') ~ '호남제주',
    거주지역 %in% c('부산', '울산', '경남') ~ '부산울산경남'
  ))

df_emp_all_emp$거주권역 <- fct_relevel(df_emp_all_emp$거주권역, '수도권', '대구경북강원', '충청', '호남제주', '부산울산경남')

df_emp_all_emp <- df_emp_all_emp |>
  mutate(근무권역 = case_when(
    근무지역 %in% c('서울', '인천', '경기') ~ '수도권', 
    근무지역 %in% c('대구', '경북', '강원') ~ '대구경북강원',
    근무지역 %in% c('대전', '세종', '충북', '충남') ~ '충청',
    근무지역 %in% c('광주', '전북', '전남', '제주') ~ '호남제주',
    근무지역 %in% c('부산', '울산', '경남') ~ '부산울산경남'
  ))

df_emp_all_emp$근무권역 <- fct_relevel(df_emp_all_emp$근무권역, '수도권', '대구경북강원', '충청', '호남제주', '부산울산경남')


df_emp_all_emp |>  
  filter(!is.na(대학권역), !is.na(거주권역),!is.na(근무권역)) |>
  group_by(장애인코드, 대학권역) |> 
  mutate(total = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 대학권역, 근무권역) |>
  summarise(rate = sum(`취업자(건보가입자)`) / mean(total), 
            total = sum(`취업자(건보가입자)`)) |> write_clip() 



df_emp_all_emp |>  
  filter(!is.na(대학권역), !is.na(거주권역),!is.na(근무권역)) |>
  group_by(장애인코드, 대학권역) |> 
  mutate(total = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 대학권역, 근무권역) |>
  summarise(rate = sum(`취업자(건보가입자)`) / mean(total), 
            total = sum(`취업자(건보가입자)`)) |>
  filter(장애인코드 == '장애인') |>
  ggplot(aes(area = total, fill = total, label = paste0(근무권역, ':', scales::percent(rate, accuracy = 0.1)), subgroup = 대학권역)) +
  geom_treemap() +
  geom_treemap_text(colour = "white") +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "white",
                             fontface = "italic") +
  theme(legend.position = "none")



df_emp_all_emp |>  
  filter(!is.na(대학권역), !is.na(거주권역),!is.na(근무권역)) |>
  group_by(장애인코드, 거주권역) |> 
  mutate(total = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 거주권역, 근무권역) |>
  summarise(rate = sum(`취업자(건보가입자)`) / mean(total), 
            total = sum(`취업자(건보가입자)`)) |> write_clip() 



df_emp_all_emp |>  
  filter(!is.na(대학권역), !is.na(거주권역),!is.na(근무권역)) |>
  group_by(장애인코드, 거주권역) |> 
  mutate(total = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 거주권역, 근무권역) |>
  summarise(rate = sum(`취업자(건보가입자)`) / mean(total), 
            total = sum(`취업자(건보가입자)`)) |>
  filter(장애인코드 == '장애인') |>
  ggplot(aes(area = total, fill = total, label = paste0(근무권역, ':', scales::percent(rate, accuracy = 0.1)), subgroup = 거주권역)) +
  geom_treemap() +
  geom_treemap_text(colour = "white") +
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(place = "centre", grow = TRUE,
                             alpha = 0.25, colour = "white",
                             fontface = "italic") +
  theme(legend.position = "none")
#########################################

df_emp_all_emp$`산업분류(대분류)` <- gsub('달리 분류되지 않은', '기타', df_emp_all_emp$`산업분류(대분류)`)


colnames(df_emp_all_emp)[which(names(df_emp_all_emp) == '산업분류(대분류)')] <- '산업분류'

df_emp_all_emp$산업분류1 <- gsub('및', '및\n', df_emp_all_emp$산업분류)

distinct(df_emp_all_emp, 산업분류1)


full_join(
df_emp_all_emp |>
  filter(장애인코드 == '비장애인', 산업분류 != '-', !is.na(산업분류)) |>
  group_by(장애인코드, 학제과정, 산업분류) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)), 
df_emp_all_emp |>
  filter(장애인코드 == '장애인', 산업분류 != '-', !is.na(산업분류)) |>
  group_by(장애인코드, 학제과정, 산업분류) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)),
         by = c('학제과정', '산업분류')
) |> write_clip()


glimpse(df_emp_all_emp)


df_emp_all_emp |>
  filter(학제과정 == '대학원과정', 산업분류1 != '-', !is.na(산업분류1)) |>
  group_by(장애인코드, 산업분류1) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 산업분류1, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  filter(!is.na(산업분류1)) -> temp 



temp |>
  ggplot(aes(x = 산업분류1, xend = 산업분류1)) + 
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
  labs(x = '산업분류', y = '취업자 비율', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 

##############################################

df_emp_all_emp$졸업평점1 <- gsub('-', '\n~', df_emp_all_emp$졸업평점)

distinct(df_emp_all_emp, 졸업평점1)


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학제과정, 졸업평점) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)) |> 
  arrange(장애인코드, 학제과정, 졸업평점) |> write_clip()


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학제과정, 졸업평점1) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 학제과정, 졸업평점1, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  filter(!is.na(졸업평점1)) -> temp 


temp |>
  ggplot(aes(x = 졸업평점1, xend = 졸업평점1)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff), linetype = 'dashed') + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_line(aes(y = 장애인, group = 1), color = 'red')+ 
  geom_line(aes(y = 비장애인, group = 1), color = 'blue')+ 
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
  labs(y = '취업자 비율', x = '졸업평점', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  facet_wrap(~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom')


#########################################

df_emp_all_emp$임금수준 <- gsub('미안', '미만', df_emp_all_emp$임금수준)
df_emp_all_emp$임금수준 <- gsub('-', '\n~', df_emp_all_emp$임금수준)

distinct(df_emp_all_emp, 임금수준)

df_emp_all_emp |>
  filter(임금수준 != '\n~', !is.na(임금수준)) |>
  group_by(장애인코드, 학제과정, 임금수준) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |>
  mutate(임금수준 = fct_relevel(임금수준, '100만원미만', '100만원이상\n~200만원미만', '200만원이상\n~300만원미만', '300만원이상\n~400만원미만', '400만원이상')) |> arrange(장애인코드, 학제과정, 임금수준) |>
  write_clip()



df_emp_all_emp |>
  filter(임금수준 != '\n~', !is.na(임금수준)) |>
  group_by(장애인코드, 학제과정, 임금수준) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 임금수준, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  filter(!is.na(임금수준)) -> temp 


temp |>
  ggplot(aes(x = 임금수준, xend = 임금수준)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff), linetype = 'dashed') + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_line(aes(y = 장애인, group = 1), color = 'red')+ 
  geom_line(aes(y = 비장애인, group = 1), color = 'blue')+ 
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
  labs(y = '취업자 비율', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  facet_wrap(~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') 




#########################################
df_emp_all_emp$기업규모 <- gsub('/n~', '\n~', df_emp_all_emp$기업규모)

distinct(df_emp_all_emp, 기업규모)

df_emp_all_emp |>
  filter(기업규모 != '\n~', !is.na(기업규모)) |>
  group_by(장애인코드, 학제과정, 기업규모) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)) |>      
  mutate(기업규모 = fct_relevel(기업규모, '5명미만', '5명이상\n~30명미만', '30명이상\n~300명미안', '300명이상\n~1,000명미만', '1,000명이상')) |> arrange(장애인코드, 학제과정, 기업규모) |>
  write_clip()


df_emp_all_emp |>
  filter(기업규모 != '\n~', !is.na(기업규모)) |>
  group_by(장애인코드, 학제과정, 기업규모) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 학제과정, 기업규모, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  mutate(기업규모 = fct_relevel(기업규모, '5명미만', '5명이상\n~30명미만', '30명이상\n~300명미안', '300명이상\n~1,000명미만', '1,000명이상')) |> arrange(기업규모) |>
  filter(!is.na(기업규모)) -> temp 


temp |>
  ggplot(aes(x = 기업규모, xend = 기업규모)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff), linetype = 'dashed') + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_line(aes(y = 장애인, group = 1), color = 'red')+ 
  geom_line(aes(y = 비장애인, group = 1), color = 'blue')+ 
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(1,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.15,0.15))) +
  coord_flip() +
  labs(y = '취업자 비율', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  facet_wrap(~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') 


#########################################

distinct(df_emp_all_emp, 기업유형)

df_emp_all_emp |>
  filter(기업유형 != '-', !is.na(기업유형)) |>
  group_by(장애인코드, 학제과정, 기업유형) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |>
  write_clip()



df_emp_all_emp |>
  filter(기업유형 != '-', !is.na(기업유형)) |>
  group_by(장애인코드, 학제과정, 기업유형) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 기업유형, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  filter(!is.na(기업유형)) -> temp 


temp |>
  ggplot(aes(x = 기업유형, xend = 기업유형)) + 
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
  labs(y = '취업자 비율', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  facet_wrap(~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') 



#########################################
df_emp_all_emp$취업준비기간1 <- gsub('-', '\n~', df_emp_all_emp$취업준비기간)


distinct(df_emp_all_emp, 취업준비기간1)

df_emp_all_emp |>
  filter(취업준비기간 != '-', !is.na(취업준비기간)) |>
  group_by(장애인코드, 학제과정, 취업준비기간) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |>
  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> 
  arrange(장애인코드, 학제과정, 취업준비기간) |>
  write_clip()



df_emp_all_emp |>
  filter(취업준비기간1 != '\n~', !is.na(취업준비기간1)) |>
  group_by(장애인코드, 학제과정, 취업준비기간1) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 학제과정, 취업준비기간1, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  mutate(취업준비기간1 = fct_relevel(취업준비기간1, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과\n~6개월이하', '졸업후6개월초과\n~9개월이하', '졸업후9개월초과\n~12개월이하', '졸업후12개월초과\n~16개월이하', '졸업후16개월초과')) |> arrange(학제과정, 취업준비기간1) |>
  filter(!is.na(취업준비기간1)) -> temp 


temp |>
  ggplot(aes(x = 취업준비기간1, xend = 취업준비기간1)) + 
  geom_segment(aes(y = 비장애인, yend = 장애인, color = diff), linetype = 'dashed') + 
  scale_color_manual(name = '차이', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인 > 비장애인', 'blue' = '장애인 < 비장애인' )) +
  ggnewscale::new_scale_color() +
  geom_line(aes(y = 장애인, group = 1), color = 'red')+ 
  geom_line(aes(y = 비장애인, group = 1), color = 'blue')+ 
  geom_point(aes(y = 비장애인, color = 'blue', shape = '비장애인')) +
  geom_point(aes(y = 장애인, color = 'red', shape = '장애인')) +
  scale_color_manual(name = '장애구분', values = c('red' = 'red', 'blue' = 'blue'), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  scale_shape_manual(name = '장애구분', values = c('장애인' = 15, '비장애인' = 0), labels = c('red' = '장애인', 'blue' = '비장애인' ))  +
  geom_text(aes(y = 비장애인, label = scales::percent(비장애인, accuracy = 0.1)), color = 'blue', hjust = ifelse(temp$diff < 'red', -0.2, 1.2)) +
  geom_text(aes(y = 장애인, label = scales::percent(장애인, accuracy = 0.1)), color = 'red', hjust = ifelse(temp$diff < 'red', 1.2, -0.2)) +
  geom_text(aes(y = if_else(비장애인 > 장애인, 장애인 + (diff_val/2), 비장애인 + (diff_val/2)), label = scales::percent(diff_val, accuracy = 0.1, suffix = '%p')), color = if_else(temp$장애인 > temp$비장애인, 'red', 'blue'), vjust = 1.75, fontface = 'bold.italic') +
  #  geom_hline(aes(yintercept = 0), color = 'grey80') +
  scale_x_discrete(limits = rev, expand = expansion(add = c(1,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.15,0.15))) +
  coord_flip() +
  labs(y = '취업자 비율', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  facet_wrap(~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') 


#########################################
df_emp_all_emp$임금수준1 <- gsub('\n~', '-', df_emp_all_emp$임금수준)


distinct(df_emp_all_emp, 졸업평점)
distinct(df_emp_all_emp, 임금수준1)

df_emp_all_emp |>
  filter(임금수준1 != '-', !is.na(임금수준1)) |>
  group_by(장애인코드, 학제과정) |>
  mutate(건보취업자수 = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학제과정, 임금수준1, 졸업평점) |>
  summarise(비율 = sum(`취업자(건보가입자)`) / mean(건보취업자수)      
  ) |> write_clip()

df_emp_all_emp |>
  filter(임금수준 != '\n~', !is.na(임금수준)) |>
  group_by(장애인코드, 학제과정) |>
  mutate(건보취업자수 = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학제과정, 임금수준, 졸업평점) |>
  summarise(비율 = sum(`취업자(건보가입자)`) / mean(건보취업자수)      
  ) |> 
  mutate(임금수준 = fct_relevel(임금수준, '100만원미만', '100만원이상\n~200만원미만', '200만원이상\n~300만원미만', '300만원이상\n~400만원미만', '400만원이상')) |> 
  arrange(장애인코드, 학제과정, 임금수준, 졸업평점) |>
  ggplot(aes(x = 임금수준, y = 졸업평점, fill = 비율, label = 비율)) + 
  geom_tile() +
  geom_text(aes(label = scales::percent(비율, accuracy = 0.1)), color = 'white') +
  facet_grid(장애인코드~학제과정) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -90), 
        legend.position = 'none')

df_emp_all_emp |>
  filter(임금수준 != '-', !is.na(임금수준)) |>
  group_by(장애인코드, 학제과정) |>
  mutate(건보취업자수 = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학제과정, 임금수준, 졸업평점) |>
  summarise(비율 = sum(`취업자(건보가입자)`) / mean(건보취업자수)      
  ) |> 
  pivot_wider(names_from = 졸업평점, values_from = 비율) |>
  write_clip()

#########################################

distinct(df_emp_all_emp, 졸업평점)
distinct(df_emp_all_emp, 기업유형)

df_emp_all_emp |>
  filter(기업유형 != '-', !is.na(기업유형)) |>
  group_by(장애인코드, 학제과정) |>
  mutate(건보취업자수 = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학제과정, 기업유형, 졸업평점) |>
  summarise(비율 = sum(`취업자(건보가입자)`) / mean(건보취업자수)      
  ) |> 
#  mutate(임금수준 = fct_relevel(임금수준, '100만원미만', '100만원이상\n~200만원미만', '200만원이상\n~300만원미만', '300만원이상\n~400만원미만', '400만원이상')) |> 
  arrange(장애인코드, 학제과정, 기업유형, 졸업평점) |> 
  ggplot(aes(x = 기업유형, y = 졸업평점, fill = 비율, label = 비율)) + 
  geom_tile() +
  geom_text(aes(label = scales::percent(비율, accuracy = 0.1)), color = 'white', size = 3) +
  facet_grid(장애인코드~학제과정) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -90), 
        legend.position = 'none')

df_emp_all_emp |>
  filter(기업유형 != '-', !is.na(기업유형)) |>
  group_by(장애인코드, 학제과정) |>
  mutate(건보취업자수 = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학제과정, 기업유형, 졸업평점) |>
  summarise(비율 = sum(`취업자(건보가입자)`) / mean(건보취업자수)      
  ) |> 
  pivot_wider(names_from = 기업유형, values_from = 비율) |>
  write_clip()

#########################################

distinct(df_emp_all_emp, 졸업평점)
distinct(df_emp_all_emp, 취업준비기간1)

df_emp_all_emp |>
  filter(취업준비기간1 != '\n~', !is.na(취업준비기간1)) |>
  group_by(장애인코드, 학제과정) |>
  mutate(건보취업자수 = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학제과정, 취업준비기간1, 졸업평점) |>
  summarise(비율 = sum(`취업자(건보가입자)`) / mean(건보취업자수)      
  ) |> 
#  mutate(취업준비기간1 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> 
  arrange(장애인코드, 학제과정, 취업준비기간1, 졸업평점) |> 
  ggplot(aes(x = 취업준비기간1, y = 졸업평점, fill = 비율, label = 비율)) + 
  geom_tile() +
  geom_text(aes(label = scales::percent(비율, accuracy = 0.1)), color = 'white', size = 3) +
  facet_grid(장애인코드~학제과정) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -90), 
        legend.position = 'none')

df_emp_all_emp |>
  filter(취업준비기간 != '-', !is.na(취업준비기간)) |>
  group_by(장애인코드, 학제과정) |>
  mutate(건보취업자수 = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학제과정, 취업준비기간, 졸업평점) |>
  summarise(비율 = sum(`취업자(건보가입자)`) / mean(건보취업자수)      
  ) |> 
  pivot_wider(names_from = 졸업평점, values_from = 비율) |>
  write_clip()


#########################################

distinct(df_emp_all_emp, 기업유형)
distinct(df_emp_all_emp, 취업준비기간1)
  
df_emp_all_emp |>
  filter(취업준비기간1 != '\n~', !is.na(취업준비기간1)) |>
  filter(기업유형 != '-', !is.na(기업유형)) |>
  group_by(장애인코드, 학제과정) |>
  mutate(건보취업자수 = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학제과정, 취업준비기간1, 기업유형) |>
  summarise(비율 = sum(`취업자(건보가입자)`) / mean(건보취업자수)      
  ) |> 
  #  mutate(임금수준 = fct_relevel(임금수준, '100만원미만', '100만원이상\n~200만원미만', '200만원이상\n~300만원미만', '300만원이상\n~400만원미만', '400만원이상')) |> 
  arrange(장애인코드, 학제과정, 취업준비기간1, 기업유형) |> 
  ggplot(aes(x = 취업준비기간1, y = 기업유형, fill = 비율, label = 비율)) + 
  geom_tile() +
  geom_text(aes(label = scales::percent(비율, accuracy = 0.1)), color = 'white', size = 3) +
  facet_grid(장애인코드~학제과정) +
  labs(x = '취업준비기간') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -90), 
        legend.position = 'none')


#########################################

distinct(df_emp_all_emp, 임금수준)
distinct(df_emp_all_emp, 취업준비기간1)

df_emp_all_emp |>
  filter(취업준비기간1 != '\n~', !is.na(취업준비기간1)) |>
  filter(임금수준 != '\n~', !is.na(임금수준)) |>
  group_by(장애인코드, 학제과정) |>
  mutate(건보취업자수 = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학제과정, 임금수준, 취업준비기간1) |>
  summarise(비율 = sum(`취업자(건보가입자)`) / mean(건보취업자수)) |> 
  mutate(임금수준 = fct_relevel(임금수준, '100만원미만', '100만원이상\n~200만원미만', '200만원이상\n~300만원미만', '300만원이상\n~400만원미만', '400만원이상')) |> 
  arrange(장애인코드, 학제과정, 임금수준, 취업준비기간1) |> 
  ggplot(aes(x = 취업준비기간1, y = 임금수준, fill = 비율, label = 비율)) + 
  geom_tile() +
  geom_text(aes(label = scales::percent(비율, accuracy = 0.1)), color = 'white', size = 3) +
  facet_grid(장애인코드~학제과정) +
  labs(x = '취업준비기간') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = -90), 
        legend.position = 'none')



df_emp_all_emp |>
  filter(취업준비기간 != '-', !is.na(취업준비기간)) |>
  filter(임금수준 != '\n~', !is.na(임금수준)) |>
  group_by(장애인코드, 학제과정) |>
  mutate(건보취업자수 = sum(`취업자(건보가입자)`)) |> 
  ungroup() |>
  group_by(장애인코드, 학제과정, 임금수준, 취업준비기간) |>
  summarise(비율 = sum(`취업자(건보가입자)`) / mean(건보취업자수)) |> 
  pivot_wider(names_from = 임금수준, values_from = 비율) |>
  write_clip()





#########################################

df_emp_all_emp$근무지역 <- fct_relevel(df_emp_all$근무지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                                   '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df_emp_all_emp |>
  distinct(근무지역)


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 근무지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |>
  write_clip()


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 근무지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 근무지역, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  filter(!is.na(근무지역)) -> temp 


temp |>
  ggplot(aes(x = 근무지역, xend = 근무지역)) + 
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
  labs(title = '비장애인과 장애인의 근무지역별 취업자 비율', y = '취업자 비율', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') 



####################################################


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 기업유형, 근무지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 기업유형, 근무지역, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  filter(!is.na(근무지역)) -> temp 


temp |>
  ggplot(aes(x = 근무지역, xend = 근무지역)) + 
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
  labs(title = '비장애인과 장애인의 근무지역별 취업자 비율', y = '취업자 비율', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom') +
  facet_wrap(~기업유형)





##############################################

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> write_clip()


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 학교지역, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  filter(!is.na(학교지역)) -> temp 


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
  labs(title = '비장애인과 장애인의 학교지역별 취업자 비율', y = '취업자 비율', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom')



##############################################

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학과대분류) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> write_clip()


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학과대분류) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 학과대분류, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  filter(!is.na(학과대분류)) -> temp 


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
  labs(title = '비장애인과 장애인의 학과대분류별 취업자 비율', y = '취업자 비율', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom')



##############################################

df_emp_all_emp |>
  mutate(학제과정 = case_when(
    학제 == '전문대학' ~ '전문대학과정', 
    학제 %in% c('교육대학', '대학', '기능대학', '각종대학(대학)', '산업대학') ~ '대학과정', 
    학제 == '일반대학원' ~ '대학원과정'
  )) |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학제과정) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> write_clip()


df_emp_all_emp |>
  mutate(학제과정 = case_when(
    학제 == '전문대학' ~ '전문대학과정', 
    학제 %in% c('교육대학', '대학', '기능대학', '각종대학(대학)', '산업대학') ~ '대학과정', 
    학제 == '일반대학원' ~ '대학원과정'
  )) |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학제과정) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 학제과정, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
  mutate(학제과정 = fct_relevel(학제과정, '전문대학과정', '대학과정', '대학원과정')) |> arrange(학제과정) |>
  filter(!is.na(학제과정)) -> temp 


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
  labs(title = '비장애인과 장애인의 학제과정별 취업자 비율', y = '취업자 비율', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom')



##############################################

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> write_clip()


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |> 
  select(장애인코드, 거주지역, 비율) |>
  pivot_wider(names_from = 장애인코드, values_from = 비율) %>%
  mutate(diff = ifelse(비장애인 - 장애인 < 0, 'red', 'blue'), 
         diff_val = abs(비장애인 - 장애인)
  ) |>
#  mutate(학제과정 = fct_relevel(학제과정, '전문대학과정', '대학과정', '대학원과정')) |> arrange(학제과정) |>
  filter(!is.na(거주지역)) -> temp 


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
  scale_x_discrete(limits = rev, expand = expansion(add = c(1,0.5))) +
  scale_y_continuous(labels = scales::percent, expand = expansion(add = c(0.05,0.05))) +
  coord_flip() +
  labs(title = '비장애인과 장애인의 거주지역별 취업자 비율', y = '취업자 비율', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB') +
  theme_bw() +
  theme(legend.position = 'bottom')
