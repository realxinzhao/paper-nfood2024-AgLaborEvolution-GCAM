
GCAM_AgLU_SUA_APE_1973_2019 <-
  GCAM_APE_before2010 %>%
  bind_rows(GCAM_APE_after2010) %>%
  mutate(unit = "1000 tonnes") ->
  GCAM_AgLU_SUA_APE_1973_2019

GCAM_AgLU_SUA_APE_1973_2019 %>% saveRDS("GCAM_AgLU_SUA_APE_1973_2019.RDS")

GCAM_AgLU_SUA_APE_1973_2019 <-
  GCAM_APE_before2010 %>%
  bind_rows(GCAM_APE_after2010) %>%
  mutate(unit = "1000 tonnes") %>%
  # clean and aggregate elements not using
  filter(!element %in% c("Regional demand", "Regional supply")) %>%
  mutate(element = replace(element,
                           element %in% c("Processed", "Seed", "Residuals"),
                           "Other uses")) %>%
  dplyr::group_by_at(dplyr::vars(-value)) %>%
  summarise(value = sum(value), .groups = "drop")

library(ggplot2)



LIVECOMM <- c("Beef",  "Dairy", "Pork", "Poultry", "SheepGoat", "OtherMeat_Fish")
CROP1COMM <- c( "Corn", "Rice", "OtherGrain", "Wheat", "RootTuber")
CROP2COMM <- c("Soybean", "OilPalm", "OilCrop", "FiberCrop" )
CROP3COMM <- c("Vegetables", "Fruits", "SugarCrop", "NutsSeeds", "Legumes", "MiscCrop")

GCAM_AgLU_SUA_APE_1973_2019 %>%
  filter(year >= 2010) %>%
  mutate(value = value / 1000) %>%
  spread(element, value) %>%
  transmute(region, GCAM_commodity, year, `Closing stocks`,
            GrossUse = (`Opening stocks` + Production - Export),
            StockToUseRatio = `Closing stocks`/GrossUse ) %>%
  mutate(COMM = if_else(GCAM_commodity %in% LIVECOMM, "Meat", "Other"),
         COMM = if_else(GCAM_commodity %in% CROP1COMM, "Staple", COMM),
         COMM = if_else(GCAM_commodity %in% CROP2COMM, "Oil", COMM),
         COMM = if_else(GCAM_commodity %in% CROP3COMM, "Other", COMM)) ->
  df_storage


df_storage %>%
  mutate(year = as.character(year)) %>%
  filter(COMM == "Staple") %>%
  ggplot() + facet_wrap(~region, scales = "free_y") +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "blue") +
  geom_abline(slope = .25, intercept = 0, , linetype = 2, color = "blue") +
  geom_point(aes(x = GrossUse, y = `Closing stocks`, group = GCAM_commodity, color = GCAM_commodity)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() -> p

ggsave("StU_Ratio_staples1.png", p, width = 12, height = 8)



df_storage %>%
  mutate(year = as.character(year)) %>%
  ggplot() + facet_wrap(~COMM) +
  geom_line(aes(x = year, y = StockToUseRatio, group = GCAM_commodity, color = GCAM_commodity)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.05, 1.8), labels = scales::percent) +
  theme_bw()


df_storage %>%
  mutate(year = as.character(year)) %>%
  filter(COMM == "Staple") %>%
  ggplot() + facet_wrap(~region, scales = "free_y") +
  geom_line(aes(x = year, y = StockToUseRatio, group = GCAM_commodity, color = GCAM_commodity)) +
  scale_y_continuous(expand = c(0, 0), #limits = c(-0.05, 1.8),
                     labels = scales::percent) +
  theme_bw()


df_storage %>% filter(region == "USA") %>%
  mutate(year = as.character(year)) %>%
  filter(COMM == "Staple") %>%
  ggplot() + facet_wrap(~GCAM_commodity, scales = "fixed") +
  geom_line(aes(x = year, y = StockToUseRatio, group = GCAM_commodity, color = GCAM_commodity)) +
  scale_y_continuous(expand = c(0, 0), #limits = c(-0.05, 1.8),
                     labels = scales::percent) +
  theme_bw() +
  labs(title = "USA stock to use ratio") +
  theme(axis.text.x = element_text(angle = 90)) -> p
ggsave("US_StU_Ratio_staples.png", p, width = 8, height = 5)

df_storage %>% filter(region == "USA") %>%
  mutate(year = as.character(year)) %>%
  filter(COMM == "Staple") %>%
  ggplot() + facet_wrap(~GCAM_commodity, scales = "fixed") +
  geom_abline(slope = .1, intercept = 0, linetype = 2, color = "blue") +
  geom_abline(slope = .2, intercept = 0, , linetype = 2, color = "blue") +
  geom_abline(slope = .3, intercept = 0, , linetype = 2, color = "blue") +
  geom_point(aes(x = GrossUse, y = `Closing stocks`, group = GCAM_commodity, color = GCAM_commodity)) +
  #geom_line(aes(x = year, y = StockToUseRatio, group = GCAM_commodity, color = GCAM_commodity)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_bw() +
  labs(title = "USA stock vs. use ratio") +
  theme(axis.text.x = element_text(angle = 90)) -> p; p
ggsave("US_StU_Ratio_staples1.png", p, width = 8, height = 5)


