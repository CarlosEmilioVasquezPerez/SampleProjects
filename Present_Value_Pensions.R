#librerias
library(readxl)
library(lubridate)
library(tidyverse)
library(dplyr)

#This Script will calculate de Present Value Amount of Pensions Obligations based on IFRS 19
#It is required to have a database with the following structure: id, date of birth, pension amount, pension motive, gender, type of pension (annuity, temporary)
#It is required to structure life tables based in mortality risk only

#life table
tables = "your tables"
tables$age1 <- seq.int(nrow(tables))-1

#active pension table
retired <- "your table"
retired['age'] <- as.integer((valuation_date - as.Date(retired$birthdate))/365.25)

#Define demographic, financial and actuarial assumptions:
valuation_date = as.Date('2020-12-31')
pension_amount_increase <- 0.002
annual_pension_frecuency <- 12
discount_rate <- 0.09
n_discount_rate <- (((discount_rate+1)^(1/12))-1)*12 #nominal discount rate
mortality_table = 'table'
inicio_lx = 100000

#survival assumptions:
spouse_pct = 0.5
spouse_age_dif = 3
son_pct = 0.4
son_age_dif_m = 30
son_age_dif_f = 30

dist_son = 0.2
dist_spouse = 0.6
dist_parent = 0.2

#Projected Unit Credit
pv_df <- data.frame()

for (c in 1:nrow(retired)) {

df1 <- subset(retired, NUM == c)
df1_a = cbind(df1
              ,son_birthdate = df1$birthdate %m+% years(ifelse(df1$gender=='F',son_age_dif_f,son_age_dif_m))
              ,son_gender = ifelse(df1$gender=='F','M','F')
              ,spouse_birthdate = df1$birthdate %m+% years(ifelse(df1$gender=='F',-spouse_age_dif,spouse_age_dif))
              ,spouse_gender = ifelse(df1$gender=='F','M','F')
              ,parent_birthdate = df1$birthdate %m+% years(ifelse(df1$gender=='F',-son_age_dif_m,-son_age_dif_f))
              ,parent_gender = ifelse(df1$gender=='F','M','F')
)

mortality_principal = gsub(" ","",paste('qx_',df1$gender,"_",mortality_table), fixed = TRUE)
mortality_survival = gsub(" ","",paste('qx_',df1_a$spouse_gender,"_",mortality_table), fixed = TRUE)

single_table0 <- tables %>% select(age1, paste0(mortality_principal))
single_table1 <- tables %>% select(age1, paste0(mortality_survival))

df2 <- rbind(df1, df1[rep(1, 120), ])
df2$NUM <- seq.int(nrow(df2))

df3 = cbind(df2
            #principal
            ,age1 = df2$age + df2$NUM -1
            ,year_count = df2$NUM-1
            ,pension_increment = (1+pension_amount_increase)^((df2$NUM)-1)
            ,pension_amount = (df2$amount * annual_pension_frecuency) * (1+pension_amount_increase)^((df2$NUM)-1)
            #survival
            ,son_age = as.integer((valuation_date - as.Date(df2$son_birthdate))/365.25) + df2$NUM -1
            ,spouse_age = as.integer((valuation_date - as.Date(df2$spouse_birthdate))/365.25) + df2$NUM -1
            ,parent_age = as.integer((valuation_date - as.Date(df2$parent_birthdate))/365.25) + df2$NUM -1
            )

df4 = merge(x = df3, y = single_table0, by = "age1", all.x = TRUE)
names(df4)[length(names(df4))]<-"principal_mortality" 
df5 = merge(x = df4, y = single_table1, by.x = "son_age", by.y = "age1", all.x = TRUE)
names(df5)[length(names(df5))]<-"son_mortality" 
df6 = merge(x = df5, y = single_table1, by.x = "spouse_age", by.y = "age1", all.x = TRUE)
names(df6)[length(names(df6))]<-"spouse_mortality" 
df7 = merge(x = df6, y = single_table1, by.x = "parent_age", by.y = "age1", all.x = TRUE)
names(df7)[length(names(df7))]<-"parent_mortality" 

y_principal <- c(df7$principal_mortality)
y_son <- c(df7$son_mortality)
y_spouse <- c(df7$spouse_mortality)
y_parent <- c(df7$parent_mortality)

lx_principal <- vector(mode = 'numeric', length = nrow(df7))
lx_son <- vector(mode = 'numeric', length = nrow(df7))
lx_spouse <- vector(mode = 'numeric', length = nrow(df7))
lx_parent <- vector(mode = 'numeric', length = nrow(df7))

lx <- vector(mode = 'numeric', length = nrow(df5))

for (i in seq_along(lx_principal)) {
  if (i == 1) {
    lx_principal[[i]] <- inicio_lx                    
    lx_son[[i]] <- inicio_lx
    lx_spouse[[i]] <- inicio_lx
    lx_parent[[i]] <- inicio_lx
  }
  if (i > 1) {
    lx_principal[[i]] <- (lx_principal[[i-1]] - lx_principal[[i-1]] * y_principal[[i-1]]) 
    lx_son[[i]] <- (lx_son[[i-1]] - lx_son[[i-1]] * y_son[[i-1]])    
    lx_spouse[[i]] <- (lx_spouse[[i-1]] - lx_spouse[[i-1]] * y_spouse[[i-1]])    
    lx_parent[[i]] <- (lx_parent[[i-1]] - lx_parent[[i-1]] * y_parent[[i-1]])    
  }
}

df8 = cbind(df7
            ,lx_principal
            ,lx_son
            ,lx_spouse
            ,lx_parent
            ,p_principal = lx_principal/inicio_lx
            ,p_son = lx_son/inicio_lx
            ,p_spouse = lx_spouse/inicio_lx
            ,p_parent = lx_parent/inicio_lx
            ,vx = (1+n_discount_rate/12)^-(df7$year_count*12+6)
            ,Dx_son = lx_son * df7$pension_increment * (1+n_discount_rate/12)^-(df7$year_count*12+6)
            ,Dx_spouse = lx_spouse * df7$pension_increment * (1+n_discount_rate/12)^-(df7$year_count*12+6)
            ,Dx_parent = lx_parent * df7$pension_increment * (1+n_discount_rate/12)^-(df7$year_count*12+6)
)

df9 = cbind(df8
            ,Nx_son = rev(cumsum(rev(replace_na(df8$Dx_son,0))))
            ,Nx_spuse = rev(cumsum(rev(replace_na(df8$Dx_spouse,0))))
            ,Nx_parent = rev(cumsum(rev(replace_na(df8$Dx_parent,0))))
)


df10 = cbind(df9
             ,pv_principal = df9$p_principal*df9$vx*df9$pension_amount
             
             ,pv_son = ifelse(df9$son_age>18,0, 
                              ifelse(df9$survival == 1
                                     , df9$p_son * df9$p_principal * df9$principal_mortality * (df9$Nx_son/df9$Dx_son) * df9$pension_amount * df9$vx * dist_son * son_pct
                                     , df9$p_son * df9$p_principal * df9$principal_mortality *  df9$pension_amount * df9$vx * dist_son * son_pct))
             
             ,pv_spouse = ifelse(df9$survival == 1
                                 , df9$p_spouse * df9$p_principal * df9$principal_mortality * (df9$Nx_spouse/df9$Dx_spouse) * df9$pension_amount * df9$vx * dist_spouse * spouse_pct
                                 , df9$p_spouse * df9$p_principal * df9$principal_mortality *  df9$pension_amount * df9$vx * dist_spouse * spouse_pct)
             
             ,pv_parent = ifelse(df9$survival == 1
                                 , df9$p_parent * df9$p_principal * df9$principal_mortality * (df9$Nx_parent/df9$Dx_parent) * df9$pension_amount * df9$vx * dist_spouse * son_pct
                                 , df9$p_parent * df9$p_principal * df9$principal_mortality *  df9$pension_amount * df9$vx * dist_parent * son_pct)
             
            )

df11 = cbind(id = c
             , pv_principal = sum(na.omit(df10$pv_principal))
             , pv_son = sum(na.omit(df10$pv_son))
             , pv_spouse = sum(na.omit(df10$pv_spouse))
             , pv_parent = sum(na.omit(df10$pv_parent))
             , pv_total = sum(na.omit(df10$pv_principal)) + sum(na.omit(df10$pv_son)) + sum(na.omit(df10$pv_spouse)) + sum(na.omit(df10$pv_parent))
)

pv_df = rbind(pv_df, df11) #Final Table

}

View(pv_df)
