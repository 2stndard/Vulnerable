
#########################################

df_emp_all_emp |> View()

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, `산업분류(대분류)`) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |>
  write_clip()



#########################################

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 임금수준) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |>
  write_clip()


#########################################

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 기업규모) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |>
  write_clip()


#########################################

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 취업준비기간) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |>
  write_clip()


#########################################

df_emp_all_emp |>
  #  filter(is.na(`산업분류(대분류)`) == FALSE) |>
  group_by(장애인코드, 기업유형) |>
  summarise(건보취업자수 = sum(`취업자(건보가입자)`)) |>
  mutate(비율 = 건보취업자수 / sum(건보취업자수)      
  ) |>
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
