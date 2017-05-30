#change of opinion towards homosexual relations throughout the years

gss <- gss %>% group_by(year) %>% mutate(homo_sex = as.character(homosex), obs_years = n())
dist_homo <- gss %>% filter(!is.na(homosex), homo_sex=="Always Wrong")
new <- dist_homo %>% group_by(year, obs_years) %>% summarise(cnt = n())  
new <- new %>% group_by(year) %>% summarise(prop = cnt/obs_years)

ggplot(new, aes(year, prop)) + geom_line() + geom_point() +
  labs(title = "Shift of negative opinion towards homosexual relationships throughout the years",
       x="Year", y="% People against homosexual relationships")

new_ht <- gss %>% filter(year == 2000 | year == 2012) %>% select(year, homosex)
new_ht <- new_ht %>% filter(!is.na(homosex))
new_ht$homosex <- factor(new_ht$homosex)

#Contingency Table
table <- table(new_ht$year,new_ht$homosex)

#Performing the Chi-Square test of independence
chisq.test(table)
