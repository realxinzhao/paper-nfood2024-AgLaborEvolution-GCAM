# Compare other use and stock variation

load_from_cache("L100.ag_Storage_Mt_R_C_Y") %>% first -> L100.ag_Storage_Mt_R_C_Y
L100.ag_Storage_Mt_R_C_Y %>% #filter(GCAM_commodity == "Corn") %>%
  spread(element, value) %>%
  mutate(StockIncrease = `Closing stocks` - `Opening stocks`) %>%
  filter(year == 2015)

load_from_cache("L109.ag_ALL_Mt_R_C_Y") %>% first -> L109.ag_ALL_Mt_R_C_Y


load_from_cache("common/GCAM_region_names")  %>% first   -> GCAM_region_names



L100.ag_Storage_Mt_R_C_Y %>% #filter(GCAM_commodity == "Corn") %>%
  spread(element, value) %>%
  mutate(StockIncrease = `Closing stocks` - `Opening stocks`) %>%
  filter(year == 2015) %>%
  left_join(GCAM_region_names) %>%
  left_join(
    L109.ag_ALL_Mt_R_C_Y %>%   left_join(GCAM_region_names)
  ) -> df

df %>% gather(element, value, -region, -GCAM_commodity, -GCAM_region_ID, -year) ->
  df1

df1 %>% filter(grepl("stocks|Other|Stock", element)) %>%
  filter(GCAM_commodity == "Corn") %>%
  ggplot() +
  geom_hline(yintercept = 0) +
  facet_wrap(~region, scales = "free_y") +
  geom_bar(aes(x = element, y = value, fill = element), stat = "identity", color = "black") +
  labs(x = "Element", y = "Mt") +
  theme_bw() + theme0 + theme_leg  +
  theme(axis.text.x = element_text(angle = 90)) -> p

ggsave("StockVar_Other_region.png", p, width = 20, height = 14)


df1 %>% filter(grepl("stocks|Other|Stock", element)) %>%
  filter(GCAM_commodity == "Corn") %>%
  group_by_at(vars(-value, -region, -GCAM_region_ID)) %>% summarise(value = sum(value)) %>%
  ggplot() + #facet_wrap(~region, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = element, y = value, fill = element), stat = "identity", color = "black") +
  labs(x = "Element", y = "Mt") +
  theme_bw() + theme0 + theme_leg  +
  theme(axis.text.x = element_text(angle = 90)) -> p;p

ggsave("StockVar_Other_world.png", p, width = 10, height = 7)


df1 %>% filter(!grepl("Supply", element)) %>%
  filter(GCAM_commodity == "Corn") %>%
  group_by_at(vars(-value, -region, -GCAM_region_ID)) %>% summarise(value = sum(value)) %>%
  ggplot() + facet_wrap(~GCAM_commodity, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = element, y = value, fill = element), stat = "identity", color = "black") +
  labs(x = "Element", y = "Mt") +
  theme_bw() + theme0 + theme_leg  +
  theme(axis.text.x = element_text(angle = 90)) -> p;p

ggsave("StockVar_Other_Allworld.png", p, width = 10, height = 7)

df1 %>% filter(!grepl("Supply", element)) %>%
  filter(GCAM_commodity %in% c("Corn", "Rice", "Wheat", "Soybean")) %>%
  group_by_at(vars(-value, -region, -GCAM_region_ID)) %>% summarise(value = sum(value)) %>%
  ggplot() + facet_wrap(~GCAM_commodity, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = element, y = value, fill = element), stat = "identity", color = "black") +
  labs(x = "Element", y = "Mt") +
  theme_bw() + theme0 + theme_leg  +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") -> p;p

ggsave("StockVar_Other_Allworld_4crop.png", p, width = 10, height = 7)

df1 %>% filter(!grepl("Supply", element)) %>%
  filter(!GCAM_commodity %in% c("Corn", "Rice", "Wheat", "Soybean",
                                "Beef", "Dairy", "Pork", "OtherMeat_Fish", "Poultry", "SheepGoat")) %>%
  group_by_at(vars(-value, -region, -GCAM_region_ID)) %>% summarise(value = sum(value)) %>%
  ggplot() + facet_wrap(~GCAM_commodity, scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = element, y = value, fill = element), stat = "identity", color = "black") +
  labs(x = "Element", y = "Mt") +
  theme_bw() + theme0 + theme_leg  +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") -> p;p

ggsave("StockVar_Other_Allworld_Othercrop.png", p, width = 15, height = 11)




