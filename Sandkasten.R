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
ggsave("export/DFLD_Erfassungsraten_files/figure-docx/test.png")

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

