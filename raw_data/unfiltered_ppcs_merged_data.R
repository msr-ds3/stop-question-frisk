load('merged_ppcs.RData')
library("zoo")
merge <- rbind(ppcs_1996_cleaned, ppcs_1999_cleaned, ppcs2002_polished, ppcs2005_polished, ppcs_2008_cleaned, ppcs_2011)


merge1 <- merge%>%
group_by(year) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = count, group = 1)) + 
  geom_point() + 
  geom_line() + 
  xlab("Year") + 
  ylab("Number of Incidents") + 
  scale_y_continuous(labels = comma)+
  theme( axis.text.x = element_text(size=10, angle = 40, vjust=.5) )

merge1 + scale_x_continuous(name="Year", breaks = c(1996,1999,2002, 2005, 2008, 2011)) +
  theme_bw(base_size = 11) + scale_y_continuous(name="Number of Incidents", limits=c(0, 100000))

"1996","1999","2002", "2005", "2008", "2011"

  