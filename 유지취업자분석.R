
#########################################

df_emp_all_emp |> 
  View()

df_emp_all_emp |> 
  glimpse()


#######################################3

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(학교지역)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~학교지역, ncol = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학교지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(학교지역)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 학교지역, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
#  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~차수, ncol = 2) +
  theme_bw() +
  theme(legend.position = 'bottom', 
        axis.text.x = element_text(angle = -90)) +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')

#######################################3
glimpse(df_emp_all_emp)

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(대학권역, 장애인코드) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 대학권역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(대학권역)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent, limits = c(0.5, 0.95)) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~대학권역, ncol = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#######################################3

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(거주지역)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~거주지역, ncol = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#######################################3
glimpse(df_emp_all_emp)

df_emp_all_emp |>
  filter(!is.na(거주권역)) |>
  group_by(거주권역, 장애인코드) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 거주권역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(거주권역)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent, limits = c(0.667, 1)) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~거주권역, ncol = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#######################################3
df_emp_all_emp$근무지역 <- fct_relevel(df_emp_all_emp$근무지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                                   '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df_emp_all_emp |>
  filter(!is.na(근무지역), 근무지역 != '-') |>
  group_by(장애인코드, 근무지역) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |> write_clip()

df_emp_all_emp |>
  filter(!is.na(근무지역), 근무지역 != '-') |>
  group_by(장애인코드, 근무지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(근무지역)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~근무지역, ncol = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#######################################3
glimpse(df_emp_all_emp)


df_emp_all_emp |>
  filter(!is.na(근무권역), 근무권역 != '-') |>
  group_by(근무권역, 장애인코드) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |> write_clip()

df_emp_all_emp |>
  filter(!is.na(근무권역), 근무권역 != '-') |>
  group_by(장애인코드, 근무권역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(근무권역)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent, limits = c(0.622, 1)) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~근무권역, ncol = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#######################################3

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학제과정) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학제과정) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(학제과정)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')



#######################################3

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학과대분류) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학과대분류) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(학과대분류)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~학과대분류) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#######################################3

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학제과정, 학과대분류) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학제과정, 학과대분류) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(4:7, names_to = '차수') |>
  filter(!is.na(학과대분류)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_grid(학과대분류~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')



#################################################

df_emp_all_emp |>
  filter(!is.na(`산업분류`), 산업분류 != '-') |>
  group_by(장애인코드, `산업분류`) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
#            `유지취업자1차` = sum(`1차유지자수`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
#            `유지취업자2차` = sum(`2차유지자수`), 
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
#            `유지취업자3차` = sum(`3차유지자수`), 
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
#            `유지취업자4차` = sum(`4차유지자수`), 
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()



df_emp_all_emp |>
  filter(!is.na(`산업분류`), 산업분류 != '-') |>
  group_by(장애인코드, `산업분류`) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(`산업분류`)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~`산업분류`, ncol = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')
  



#################################################

df_emp_all_emp |>
  filter(학제과정 == '대학원과정') |>
  filter(!is.na(`산업분류`), 산업분류 != '-') |>
  group_by(장애인코드, `산업분류`) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            #            `유지취업자1차` = sum(`1차유지자수`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            #            `유지취업자2차` = sum(`2차유지자수`), 
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            #            `유지취업자3차` = sum(`3차유지자수`), 
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            #            `유지취업자4차` = sum(`4차유지자수`), 
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()



df_emp_all_emp |>
  filter(학제과정 == '대학원과정') |>
  filter(!is.na(`산업분류`), 산업분류 != '-') |>
  group_by(장애인코드, `산업분류`) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(`산업분류`)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~`산업분류`, ncol = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')



#########################################

df_emp_all_emp |>
  filter(임금수준1 != '-', !is.na(임금수준1)) |>
  group_by(장애인코드, 임금수준1) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()

df_emp_all_emp |>
  filter(임금수준 != '\n~') |>
  group_by(장애인코드, 임금수준) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(임금수준)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~임금수준) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs( y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#########################################

df_emp_all_emp |>
  filter(임금수준1 != '-', !is.na(임금수준1)) |>
  group_by(장애인코드, 학제과정, 임금수준1) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()

df_emp_all_emp |>
  filter(임금수준 != '\n~', !is.na(임금수준)) |>
  group_by(장애인코드, 학제과정, 임금수준) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(4:7, names_to = '차수') |>
  filter(!is.na(임금수준)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_grid(임금수준~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs( y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')



#########################################
df_emp_all_emp$기업규모1 <- gsub('\n~', '-', df_emp_all_emp$기업규모)
df_emp_all_emp <- df_emp_all_emp |>
  mutate(기업규모1 = fct_relevel(기업규모1, '5명미만', '5명이상-30명미만', '30명이상-300명미안', '300명이상-1,000명미만', '1,000명이상'))

df_emp_all_emp <- df_emp_all_emp |>
  mutate(기업규모 = fct_relevel(기업규모, '5명미만', '5명이상\n~30명미만', '30명이상\n~300명미안', '300명이상\n~1,000명미만', '1,000명이상'))


distinct(df_emp_all_emp, 기업규모)


df_emp_all_emp |>
  filter(기업규모1 != '-', !is.na(기업규모1)) |>
  group_by(장애인코드, 기업규모1) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()

df_emp_all_emp |>
  filter(기업규모 != '\n~') |>
  group_by(장애인코드, 기업규모) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(기업규모)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~기업규모) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs( y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#########################################

df_emp_all_emp |>
  filter(기업규모1 != '-', !is.na(기업규모1)) |>
  group_by(장애인코드, 학제과정, 기업규모1) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()

df_emp_all_emp |>
  filter(임금수준 != '\n~', !is.na(기업규모)) |>
  group_by(장애인코드, 학제과정, 기업규모) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(4:7, names_to = '차수') |>
  filter(!is.na(기업규모)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_grid(기업규모~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs( y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')




#########################################
distinct(df_emp_all_emp, 기업유형)


df_emp_all_emp |>
  filter(기업유형 != '-', !is.na(기업유형)) |>
  group_by(장애인코드, 기업유형) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()

df_emp_all_emp |>
  filter(기업유형 != '-', !is.na(기업유형)) |>
  group_by(장애인코드, 기업유형) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(기업유형)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~기업유형) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs( y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#########################################

df_emp_all_emp |>
  filter(기업유형 != '-', !is.na(기업유형)) |>
  group_by(장애인코드, 학제과정, 기업유형) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()

df_emp_all_emp |>
  filter(기업유형 != '-', !is.na(기업유형)) |>
  group_by(장애인코드, 학제과정, 기업유형) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(4:7, names_to = '차수') |>
  filter(!is.na(기업유형)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_grid(기업유형~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs( y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')




#########################################
distinct(df_emp_all_emp, 졸업평점)
distinct(df_emp_all_emp, 졸업평점1)

df_emp_all_emp |>
  group_by(장애인코드, 졸업평점) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()

df_emp_all_emp |>
  group_by(장애인코드, 졸업평점) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(졸업평점)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~졸업평점) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs( y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#########################################

df_emp_all_emp |>
  group_by(장애인코드, 학제과정, 졸업평점) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()

df_emp_all_emp |>
  group_by(장애인코드, 학제과정, 졸업평점1) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(4:7, names_to = '차수') |>
  filter(!is.na(졸업평점1)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_grid(졸업평점1~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs( y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')





#########################################
distinct(df_emp_all_emp, 취업준비기간1)
distinct(df_emp_all_emp, 취업준비기간)

df_emp_all_emp <- df_emp_all_emp |>
mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과'))
  
df_emp_all_emp <- df_emp_all_emp |>
  mutate(취업준비기간1 = fct_relevel(취업준비기간1, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과\n~6개월이하', '졸업후6개월초과\n~9개월이하', '졸업후9개월초과\n~12개월이하', '졸업후12개월초과\n~16개월이하', '졸업후16개월초과'))

df_emp_all_emp |>
  filter(취업준비기간 != '-', !is.na(취업준비기간)) |>
  group_by(장애인코드, 취업준비기간) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()

df_emp_all_emp |>
  filter(취업준비기간1 != '\n~', !is.na(취업준비기간1)) |>
  group_by(장애인코드, 취업준비기간1) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(취업준비기간1)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_wrap(~취업준비기간1) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs( y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#########################################

df_emp_all_emp |>
  filter(취업준비기간 != '-', !is.na(취업준비기간)) |>
  group_by(장애인코드, 학제과정, 취업준비기간) |>
  summarise(건보취업자수 = sum(`건보취업자 수 총합`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  write_clip()

df_emp_all_emp |>
  filter(취업준비기간1 != '\n~', !is.na(취업준비기간1)) |>
  group_by(장애인코드, 학제과정, 취업준비기간1) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
            `유지취업율1차`= sum(`1차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율2차`= sum(`2차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율3차`= sum(`3차유지자수`) / sum(`건보취업자 수 총합`),
            `유지취업율4차`= sum(`4차유지자수`) / sum(`건보취업자 수 총합`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(4:7, names_to = '차수') |>
  filter(!is.na(취업준비기간1)) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(name = '구분', values = c('비장애인' = 'blue', '장애인' = 'red')) +
  facet_grid(취업준비기간1~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs( y = '유지취업률', fill = '구분', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')





















#########################################

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 기업규모) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  write_clip()


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 기업규모) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(기업규모)) |>
  mutate(기업규모 = fct_relevel(기업규모, '5명미만', '5명이상-30명미만', '30명이상-300명미안', '300명이상-1,000명미만', '1,000명이상')) |> arrange(기업규모) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~기업규모) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(title = '비장애인과 장애인의 기업규모별 유지취업률', y = '유지취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')



#########################################

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 취업준비기간) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  write_clip()



df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 취업준비기간) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(취업준비기간)) |>
  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~취업준비기간) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(title = '비장애인과 장애인의 기업규모별 유지취업률', y = '유지취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')


#########################################

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 기업유형) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  write_clip()


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 기업유형) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(기업유형)) |>
#  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~기업유형) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(title = '비장애인과 장애인의 기업유형별 유지취업률', y = '유지취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')

#########################################

df_emp_all_emp$근무지역 <- fct_relevel(df_emp_all$근무지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', 
                                   '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df_emp_all_emp |>
  distinct(근무지역)


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 근무지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  write_clip()


df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 근무지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(근무지역)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~근무지역) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(title = '비장애인과 장애인의 근무지역별 유지취업률', y = '유지취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')



#######################################3

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 거주지역) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(거주지역)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~거주지역) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(title = '비장애인과 장애인의 거주지역별 유지취업률', y = '유지취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')




#######################################3

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 졸업평점) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 졸업평점) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(졸업평점)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~졸업평점) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(title = '비장애인과 장애인의 졸업평점별 유지취업률', y = '유지취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')







#######################################3

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학과대분류) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학과대분류) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(학과대분류)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~학과대분류) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(title = '비장애인과 장애인의 학과대분류별 유지취업률', y = '유지취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')



#######################################3

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학제과정) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |> write_clip()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 학제과정) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`), 
                  `유지취업율1차`= sum(`1차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율2차`= sum(`2차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율3차`= sum(`3차유지자수`) / sum(`취업자(건보가입자)`),
                  `유지취업율4차`= sum(`4차유지자수`) / sum(`취업자(건보가입자)`)
  ) |>
  select(-건보취업자수) |>
  pivot_longer(3:6, names_to = '차수') |>
  filter(!is.na(학제과정)) |>
  #  mutate(취업준비기간 = fct_relevel(취업준비기간, '입학전', '졸업전', '졸업후3개월이하', '졸업후3개월초과-6개월이하', '졸업후6개월초과-9개월이하', '졸업후9개월초과-12개월이하', '졸업후12개월초과-16개월이하', '졸업후16개월초과')) |> arrange(취업준비기간) |>
  ggplot(aes(x = 차수, y = value)) +
  geom_line(aes(group = 장애인코드, color = 장애인코드)) +
  geom_point(aes(color = 장애인코드)) +
  scale_x_discrete(labels = c('1차', '2차', '3차', '4차')) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~학제과정) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  labs(title = '비장애인과 장애인의 학제과정별 유지취업률', y = '유지취업률', fill = '구분', subtitle = '대상 : 2020년 전체 고등교육기관 졸업생 중 건보취업자 ', caption = '데이터 출처 : 한국교육개발원 고등교육기관 취업통계 DB')
