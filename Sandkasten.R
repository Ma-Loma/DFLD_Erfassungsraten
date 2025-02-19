txtGr<-12

stat %>%
  ggplot(aes(x = Fangradius, y = Leq, color = DEN)) +
  geom_vline(aes(xintercept = praef), color = "black") +
  geom_line() +
  geom_point() +
  facet_grid(cols = vars(StatNam), rows = vars(WTC))+
  theme(axis.text.x = element_text(size=txtGr, angle=90),
        axis.text.y = element_text(size=txtGr),
        axis.title = element_text(size=txtGr+2))
#ggsave("export/DFLD_Erfassungsraten_files/figure-docx/test.png")

stat2<-full_data %>%
  left_join(Erfassungsdauern) %>%
  mutate(Dauer=NULL,Gewichtsklasse=if_else(WTC=="H","Heavy","Medium+Light+NA",missing="Medium+Light+NA")) %>% 
  left_join(DauernInIntervallenDF)%>% 
  group_by(StatNam, Fangradius,DEN,Gewichtsklasse) %>%
  summarise(
    n1 = sum(StatusNr == 0),
    n2 = n(),
    Erfassungsquote = scales::percent(n1 / n2),
    Anfang = first(Anfang),
    Ende = first(Ende),
    Dauer = first(Dauer),
    Leq = dauerschallpegel(SEL, Dauer)
  )


stat2<-praef_Fangradius %>%
  mutate(praef = Fangradius, .keep = "unused") %>%
  right_join(stat2)
stat2 %>%
  ggplot(aes(x = Fangradius, y = Leq, color = DEN)) +
  geom_vline(aes(xintercept = praef), color = "black") +
  geom_line() +
  geom_point() +
  facet_grid(cols = vars(StatNam), rows = vars(Gewichtsklasse))+
  theme(axis.text.x = element_text(size=txtGr, angle=90),
        axis.text.y = element_text(size=txtGr),
        axis.title = element_text(size=txtGr+2))
ggsave("export/DFLD_Erfassungsraten_files/figure-docx/Leq.png")

stat2 %>%
  mutate(Erfassung=n1/n2) %>% 
  pivot_longer(c(n1,n2,Erfassung),names_to="Wert") %>% 
  ggplot(aes(x = Fangradius, y = value, color = DEN)) +
  geom_vline(aes(xintercept = praef), color = "black") +
  geom_line() +
  geom_point() +
  facet_grid(cols = vars(StatNam), rows = vars(Wert,Gewichtsklasse),scales = "free")+
  theme(axis.text.x = element_text(size=txtGr, angle=90),
        axis.text.y = element_text(size=txtGr),
        axis.title = element_text(size=txtGr+2))
ggsave("export/DFLD_Erfassungsraten_files/figure-docx/n1n2.png")

stat2 %>% 
  filter(DEN=="D") %>% 
  pivot_wider(names_from = Gewichtsklasse,values_from = c(n1,n2,Erfassungsquote,Leq)) %>% 
  qflextable()



praef_data<-praef_data %>%
  mutate(eventID=paste(StatNr,Fangradius,Lfd,FlugNr,AbAnDatetime,sep="_"))


full_data<-full_data %>%
  mutate(eventID=paste(StatNr,Fangradius,Lfd,FlugNr,AbAnDatetime,sep="_"))

full_data<-praef_Fangradius %>%
  mutate(praef = Fangradius, .keep = "unused") %>%
  right_join(full_data)

overlap_df <- full_data %>%
  group_by(StatNam, Fangradius) %>%  # Group by relevant columns
#  group_by(StatNam) %>% 
  arrange(tsAnfDatetime, .by_group = TRUE) %>%  # Ensure correct order
  mutate(prev_End = lag(tsEndDatetime),
         prev_ID = lag(eventID))%>%
  filter(!is.na(prev_End) & tsAnfDatetime < prev_End)  # Overlap condition
  #select(Event1 = prev_ID, Event2 = eventID, Start1 = lag(tsAnfDatetime), 
  #       End1 = prev_End, Start2 = tsAnfDatetime, End2 = tsEndDatetime)
  
prob_flg <-
  overlap_df %>%
  ungroup() %>%
  select(prev_ID, eventID) %>%
  unlist() %>%
  unique()

komisch <-
  full_data %>%
  ungroup() %>%
  filter(eventID %in% prob_flg) %>%
  arrange(tsAnfDatetime, .by_group = TRUE)

#komisch %>% 
full_data %>% 
  mutate(istErkannt=if_else(StatusNr==0,"n1","n2-n1")) %>% 
  group_by(StatNam,istErkannt,DEN) %>% 
  summarize(n=n()) %>%
  pivot_wider(names_from=c(istErkannt,DEN),values_from = n,values_fill = 0)
  
erk_gleichz_tab<-full_data %>% 
  filter(praef==Fangradius) %>% 
  mutate(istErkannt=if_else(StatusNr==0,"erkannt","unerkannt")) %>%
  mutate(istGleichzeitig=if_else(eventID %in% prob_flg,"gleichzeitig","einzeln")) %>% 
  group_by(StatNam,istErkannt,istGleichzeitig,Fangradius) %>% 
  summarize(n=n()) %>%
  pivot_wider(names_from=c(istErkannt,istGleichzeitig),values_from = n,values_fill = 0) %>% 
  qflextable()
erk_gleichz_tab
erk_gleichz_tab %>% 
  save_as_image("export/DFLD_Erfassungsraten_files/figure-docx/Erkannt_Gleichzeitig_Tabelle.png")



komisch %>%
  filter(praef==Fangradius) %>% 
  slice_head(n = 8) %>%
  ggplot(aes(
    y = factor(Piste),
    x = tsAnfDatetime,
    xend = tsEndDatetime
  )) +
  geom_segment(aes(yend = factor(Piste),
                   color = factor(ATD)),
               linewidth = 2) +
  labs(x = "Zeiten über Schwelle", y = "AbAnZeit", title = "Ereignisintervalle") +
  facet_wrap(vars(StatNam),scales="free_x")
ggsave("export/DFLD_Erfassungsraten_files/figure-docx/Doppelerfassung Beispiele.png")