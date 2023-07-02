
fontfamily = "Arial"
windowsFonts("Arial" = windowsFont("Arial"))

theme0 <- theme(
  #panel.grid.minor = element_line(size = 0.1, linetype = 2,colour = "grey75"),panel.grid.major = element_line(size = 0.1, linetype = 2,colour = "grey75"),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.border = element_rect(colour = "black", size=1),
  text = element_text(family= fontfamily, size = 15),
  axis.text.y = element_text(angle = 0, color = "black", size = 15, margin = margin(r = 10)),
  axis.text.x = element_text(angle = 0, color = "black", size = 15, margin = margin(t = 10), vjust= 0.5),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  #axis.ticks = element_line(linetype = 1,size = 0.5),
  #axis.ticks.length = unit(-0.1, "cm"),
  #axis.text.y.right =  element_blank(),  axis.title.y.right = element_blank(),
  axis.text.x.top =  element_blank(),  axis.title.x.top = element_blank(),
  strip.background = element_rect(fill="grey95"),
  strip.text = element_text(size = 16),
  #plot.title = element_text(hjust = 0.5,margin=margin(0,0,15,0)),
  plot.margin = margin(t = 10, r = 15, b = 10, l = 10) #panel.spacing = unit(1, "lines"),
)

theme_leg <- theme(legend.position="right", legend.justification = "center",
                   #legend.position=c(.1,0.7),
                   #legend.title = element_blank(),
                   legend.key.size = unit(1.5, "cm"),
                   legend.key.height=unit(1.5,"line"),
                   legend.spacing.x = unit(1, 'cm'), #legend.spacing.y = unit(5, 'cm'),
                   legend.text = element_text(margin = margin(l = -25,t=0, b=0), size = 15),
                   legend.box.margin=margin(-10, 10,-8,10),
                   legend.background = element_blank())


library(dplyr)
library(ggplot2)

GCAMDATA_FAOSTAT_ProdArea_195Regs_271Prod160AreaItems_1973to2020 %>%
  gather_years() %>% filter(item == "Soybeans", year >= 2000) -> df

df %>% filter(area %in% c(#"Brazil",
                          "United States of America")) %>%
  mutate(area = c("USA")) %>%
  select(-area_code) %>%
  bind_rows(
    df %>% group_by_at(vars(-value, -area, -area_code)) %>%
      summarise(value = sum(value)) %>% mutate(area = "World")
  ) -> df1

df1 %>% filter(element == "Production") %>% mutate(value = value / 1000) %>%
  ggplot() +
  geom_hline(yintercept = 300, linetype = 5, color = "grey80", size = 0.7) +
  geom_hline(yintercept = 200, linetype = 5, color = "grey80", size = 0.7) +
  geom_hline(yintercept = 100, linetype = 5, color = "grey80", size = 0.7) +
  geom_line(aes(x = year, y = value, color = area), size = 1.5) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 380), expand = c(0, 0)) +
  labs(x = "Year", y = "Million tonnes", color = "Region") +
  theme_bw()  + theme0 + theme_leg -> p
ggsave("SoyProd.png", p, width = 7, height = 6)

df1 %>% filter(element == "Area harvested") %>% mutate(value = value / 1000) %>%
  ggplot() +
  geom_hline(yintercept = 40, linetype = 5, color = "grey80", size = 0.7) +
  geom_hline(yintercept = 80, linetype = 5, color = "grey80", size = 0.7) +
  geom_hline(yintercept = 120, linetype = 5, color = "grey80", size = 0.7) +
  geom_line(aes(x = year, y = value, color = area), size = 1.5) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(0, 158), expand = c(0, 0)) +
  labs(x = "Year", y = "Million hectares", color = "Region") +
  theme_bw()  + theme0 + theme_leg-> p;p

ggsave("SoyArea.png", p, width = 7, height = 6)


df1 %>% select(-unit) %>%
  spread(element, value) %>% mutate(value = Production / `Area harvested`) %>%
  ggplot() +
  geom_hline(yintercept = 3.5, linetype = 5, color = "grey80", size = 0.7) +
  geom_hline(yintercept = 3, linetype = 5, color = "grey80", size = 0.7) +
  geom_hline(yintercept = 2.5, linetype = 5, color = "grey80", size = 0.7) +
  geom_line(aes(x = year, y = value, color = area), size = 1.5) +
  scale_color_brewer(palette = "Set1") +
  scale_y_continuous(limits = c(2, 3.75), expand = c(0, 0)) +
  labs(x = "Year", y = "Tonne per hectare", color = "Region") +
  theme_bw() + theme0 + theme_leg -> p;p

ggsave("SoyYield.png", p, width = 7, height = 6)

# prod

GCAM_AgLU_SUA_APE_1973_2019 %>%
  filter(GCAM_commodity == "Soybean", year >= 2000) %>%
  mutate(value = value / 1000)


