# Figures for paper


library(tidyverse)
library(ggthemes)



######################
# facet bar plots

## VMT
VMT <- read_csv("VMT_method_site.csv") %>%
  group_by(Site) %>%
  mutate(max_site = max(`Daily VMT`, na.rm = TRUE)) %>%
  mutate(max_site_method = case_when(is.na(`Daily VMT`) ~ "zz",
                                     max_site == `Daily VMT` ~ Method,
                                     TRUE ~ "zz")) %>%
  mutate(max_site_method = min(max_site_method)) %>%
  mutate(min_site = min(`Daily VMT`, na.rm = TRUE)) %>%
  mutate(min_site_method = case_when(is.na(`Daily VMT`) ~ "zz",
                                     min_site == `Daily VMT` ~ Method,
                                     TRUE ~ "zz")) %>%
  mutate(min_site_method = min(min_site_method)) %>%
  mutate(`Estimated Daily VKT\n(percent of highest estimate)` = 
           `Daily VMT` / max_site)%>%
  mutate(min_pct_max = min_site / max_site) %>%
  mutate(real = mean(`Daily VMT`, na.rm = TRUE)) %>%
  mutate(error = (`Daily VMT` - real)*2 / (real + `Daily VMT`)) %>%
  mutate(error = ifelse(error == 0, NA, error)) %>%
  mutate(min_error = min(abs(error), na.rm = TRUE)) %>%
  mutate(min_error_method = case_when(is.na(error) ~ "zz",
                                      min_error == abs(error) ~ Method,
                                      TRUE ~ "zz")) %>%
  mutate(min_error_method = min(min_error_method)) %>%
  mutate(line_error_min = ifelse(error > 0, 0, error),
         line_error_max = ifelse(error < 0, 0, error)) %>%
  mutate(set = case_when(Site == "CA-1A" ~ "Bay Area site",
                         Site == "CA-1B" ~ "Bay Area site",
                         Site == "CA-1C" ~ "Bay Area site",
                         Site == "CA-2" ~ "Bay Area site",
                         TRUE ~ "Utah site"))

VMT_notes <- VMT %>%
  summarize(across(c(3,4,5,6,11,12), first)) %>%
  mutate(label = paste("Maximum = ", 
                       formatC(max_site, 
                               digits = 0, 
                               big.mark = ",",
                               format = "f"), 
                       "\n(", 
                       max_site_method, 
                       ")\nMinimum = ",
                       formatC(min_site, 
                               digits = 0, 
                               big.mark = ",",
                               format = "f"), 
                       "\n(", 
                       min_site_method, ")", 
                       sep = ""))

VMT_plot <-ggplot(VMT, aes(Method, 
                `Estimated Daily VKT\n(percent of highest estimate)`)) +
  geom_point() +
  geom_linerange(aes(x = Method, ymin = 0, 
                     ymax = `Estimated Daily VKT\n(percent of highest estimate)`)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "25%", "50%", 
                                "75%", "100%")) +
  facet_wrap(~Site, nrow=2)  +
 #  geom_text(x = 12, y = 1.6, hjust = 1, vjust = 1, 
#             size = 2.5,
#             aes(label = label), data = VMT_notes) + 
   theme_clean() +
   theme(axis.text.x = element_text(angle = 90, 
                                    hjust=1, 
                                    size = 6)) 

VMT_plot
  
error_plot <-ggplot(VMT, aes(Method, 
                             error)) +
    geom_point() +
    geom_linerange(aes(x = Method, ymin = line_error_min, 
                       ymax = line_error_max)) +
  scale_y_continuous(limits = c(-1.5, 1.5),
                     breaks = breaks <- seq(-1.5, 1.5, by = 0.25),
                     labels = paste0(breaks*100, "%"),
                     name = "Symmetrical error,\nrelative to the average of all methods") +
    facet_wrap(~Site, nrow=2)  +
    #  geom_text(x = 12, y = 1.6, hjust = 1, vjust = 1, 
    #             size = 2.5,
    #             aes(label = label), data = VMT_notes) + 
    theme_clean() +
    theme(axis.text.x = element_text(angle = 90, 
                                     hjust=1, 
                                     size = 6))
  
error_plot

png(file="VMT.png",
    width=6, height=5, units = "in", res = 300)
VMT_plot
dev.off()

png(file="error.png",
    width=6, height=5, units = "in", res = 300)
error_plot
dev.off()

VMT_error_plot <-ggplot(VMT[!grepl("LBS", VMT$Method),], aes(Method, 
                           `error`)) +
  geom_bar(stat = "identity") +
  #scale_y_continuous(limits = c(-1, 1)) +
                   #  breaks = c(0, 0.25, 0.5, 0.75, 1),
                  #   labels = c("0", "25%", "50%", 
                   #             "75%", "100%")) +
  facet_wrap(~Site, nrow=2) +
  #geom_text(x = 12, y = 1.6, hjust = 1, vjust = 1, 
  #          size = 2.5,
  #          aes(label = label), data = VMT_notes) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust=1, 
                                   size = 6)) 

png(file="VMT_error.png",
    width=6, height=5, units = "in", res = 300)
VMT_error_plot
dev.off()

## Trip Gen
TripGen <- read_csv("TripGen.csv") %>%
  group_by(Site) %>%
  mutate(max_site = max(TripGen, na.rm = TRUE)) %>%
  mutate(max_site_method = case_when(is.na(TripGen) ~ "zz",
                                     max_site == TripGen ~ Method,
                                     TRUE ~ "zz")) %>%
  mutate(max_site_method = min(max_site_method)) %>%
  mutate(min_site = min(TripGen, na.rm = TRUE)) %>%
  mutate(min_site_method = case_when(is.na(TripGen) ~ "zz",
                                     min_site == TripGen ~ Method,
                                     TRUE ~ "zz")) %>%
  mutate(min_site_method = min(min_site_method)) %>%
  mutate(`Estimated Daily Trips\n(percent of highest estimate)` = 
           TripGen / max_site) %>%
  mutate(min_pct_max = min_site / max_site) %>%
  mutate(real = max(ifelse(Method == "LBS", TripGen, 0))) %>%
  mutate(error = (TripGen - real) / real) %>%
  mutate(error = ifelse(error == 0, NA, error)) %>%
  mutate(min_error = min(abs(error), na.rm = TRUE)) %>%
  mutate(min_error_method = case_when(is.na(error) ~ "zz",
                                      min_error == abs(error) ~ Method,
                                      TRUE ~ "zz")) %>%
  mutate(min_error_method = min(min_error_method))

TG_notes <- TripGen %>%
  summarize(across(c(3,4,5,6,11,12), first)) %>%
  mutate(label = paste("Maximum = ", 
                       formatC(max_site, 
                               digits = 0, 
                               big.mark = ",",
                               format = "f"), 
                       "\n(", 
                       max_site_method, 
                       ")\nMinimum = ",
                       formatC(min_site, 
                               digits = 0, 
                               big.mark = ",",
                               format = "f"), 
                       "\n(", 
                       min_site_method, ")", 
                       sep = ""))

TG_plot <-ggplot(TripGen, aes(Method, 
                               `Estimated Daily Trips\n(percent of highest estimate)`)) +
  geom_point() +
  geom_linerange(aes(x = Method, ymin = 0, 
                     ymax = `Estimated Daily Trips\n(percent of highest estimate)`)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "25%", "50%", 
                                "75%", "100%")) +
  facet_wrap(~Site, nrow=2) +

  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust=1, 
                                   size = 6)) 

TG_plot

png(file="trips.png",
    width=6, height=5, units = "in", res = 300)
TG_plot
dev.off()

## Trip Length
length <- read_csv("tripLength.csv") %>%
  group_by(Site) %>%
  mutate(max_site = max(TripLength, na.rm = TRUE)) %>%
  mutate(max_site_method = case_when(is.na(TripLength) ~ "zz",
                                     max_site == TripLength ~ Method,
                                     TRUE ~ "zz")) %>%
  mutate(max_site_method = min(max_site_method)) %>%
  mutate(min_site = min(TripLength, na.rm = TRUE)) %>%
  mutate(min_site_method = case_when(is.na(TripLength) ~ "zz",
                                     min_site == TripLength ~ Method,
                                     TRUE ~ "zz")) %>%
  mutate(min_site_method = min(min_site_method)) %>%
  mutate(`Estimated Average Trip Length\n(percent of highest estimate)` = 
           TripLength / max_site) %>%
  mutate(min_pct_max = min_site / max_site)%>%
  mutate(real = max(ifelse(Method == "LBS", TripLength, 0))) %>%
  mutate(error = (TripLength - real) / real) %>%
  mutate(error = ifelse(error == 0, NA, error)) %>%
  mutate(min_error = min(abs(error), na.rm = TRUE)) %>%
  mutate(min_error_method = case_when(is.na(error) ~ "zz",
                                      min_error == abs(error) ~ Method,
                                      TRUE ~ "zz")) %>%
  mutate(min_error_method = min(min_error_method))

len_notes <- length %>%
  summarize(across(c(3,4,5,6,11,12), first)) %>%
  mutate(label = paste("Maximum = ", 
                       formatC(max_site, 
                               digits = 0, 
                               big.mark = ",",
                               format = "f"), 
                       "\n(", 
                       max_site_method, 
                       ")\nMinimum = ",
                       formatC(min_site, 
                               digits = 0, 
                               big.mark = ",",
                               format = "f"), 
                       "\n(", 
                       min_site_method, ")", 
                       sep = ""))

length_plot <-ggplot(length, aes(Method, 
              `Estimated Average Trip Length\n(percent of highest estimate)`)) +
  geom_point() +
  geom_linerange(aes(x = Method, ymin = 0, 
                     ymax = `Estimated Average Trip Length\n(percent of highest estimate)`)) +
  scale_y_continuous(limits = c(0, 1.6),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", "25%", "50%", 
                                "75%", "100%")) +
  facet_wrap(~Site, nrow=2) +
  geom_text(x = 3, y = 1.6, hjust = 1, vjust = 1, 
            size = 2.5,
            aes(label = label), data = len_notes) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust=1, 
                                   size = 6)) 

length_plot

png(file="length.png",
    width=6, height=5, units = "in", res = 300)
length_plot
dev.off()

## Trip Length - by site
length2 <- length %>%
  ungroup() %>%
  group_by(Method) %>%
  mutate(max_method = max(TripLength, na.rm = TRUE)) %>%
  mutate(max_method_site = case_when(is.na(TripLength) ~ "zz",
                                     max_method == TripLength ~ Site,
                                     TRUE ~ "zz")) %>%
  mutate(max_method_site = min(max_method_site)) %>%
  mutate(min_method = min(TripLength, na.rm = TRUE)) %>%
  mutate(min_method_site = case_when(is.na(TripLength) ~ "zz",
                                     min_method == TripLength ~ Site,
                                     TRUE ~ "zz")) %>%
  mutate(min_method_site = min(min_method_site)) %>%
  mutate(`Estimated Average Trip Length\n(percent of highest estimate)` = 
           TripLength / max_site) %>%
  mutate(min_pct_max = min_method / max_method)


length_plot2 <-ggplot(length2, aes(Site, 
                                 TripLength)) +
  geom_point() +
  geom_linerange(aes(x = Site, ymin = 0, 
                     ymax = TripLength)) +
  scale_y_continuous(name = "Estimated Average Trip Length\n(kilometers)",
                     breaks = breaks <- seq(0, 70 / 1.609344, by = 10 / 1.609344),
                     labels = breaks * 1.609344) +
  facet_wrap(~Method, nrow=1) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust=1, 
                                   size = 6)) 

length_plot2

png(file="length2.png",
    width=4, height=3, units = "in", res = 300)
length_plot2
dev.off()

TripGenSummary <- TripGen %>%
  group_by(Site) %>%
  summarise(avg = mean(TripGen, na.rm = TRUE),
            min = min(TripGen, na.rm = TRUE),
            max = max(TripGen, na.rm = TRUE)) 

TripGenSummary

TripLenSummary <- length %>%
  group_by(Site) %>%
  summarise(avg = mean(TripLength, na.rm = TRUE),
            min = min(TripLength, na.rm = TRUE),
            max = max(TripLength, na.rm = TRUE))%>%
  mutate(avg = avg * 1.609344,
         min = min * 1.609344,
         max = max * 1.609344)

TripLenSummary

vmtSummary <- VMT %>%
  group_by(Site) %>%
  summarise(avg = mean(`Daily VMT`, na.rm = TRUE),
            min = min(`Daily VMT`, na.rm = TRUE),
            max = max(`Daily VMT`, na.rm = TRUE)) %>%
  mutate(avg = avg * 1.609344,
         min = min * 1.609344,
         max = max * 1.609344)

vmtSummary

error_summary_all <- VMT %>%
  left_join(vmtSummary) %>%
  group_by(Method) %>%
  summarise(min_error = min(error, na.rm = TRUE),
            max_error = max(error, na.rm = TRUE),
            mean_error = mean(error, na.rm = TRUE)) %>%
  mutate(range = max_error - min_error)

error_summary_all

error_plot <-ggplot(error_summary_all) +
  geom_linerange(aes(x = Method, 
                     ymin = min_error, 
                     ymax = max_error,
                     lty = "Range")) +
   geom_point(data = VMT, position = "jitter",
             aes(x = Method, y = error, color = set)) +
  geom_point(aes(y = mean_error, x = Method,
                 shape = "Average across sites"),
             size = 4.5) +

  scale_y_continuous(limits = c(-1.5, 1.5),
                     breaks = breaks <- seq(-1.5, 1.5, by = 0.25),
                     labels = paste0(breaks*100, "%"),
                     name = 
                       "Symmetrical error,\nrelative to the average of all methods") +
  scale_color_manual(values = c("gray", "black")) +
  scale_shape(solid = FALSE) +
  geom_abline(intercept = 0, slope = 0, lty = "dashed") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust=1),
        legend.background = element_blank(),
        legend.title = element_blank()) 

error_plot

png(file="error_avg.png",
    width=6, height=5, units = "in", res = 300)
error_plot
dev.off()