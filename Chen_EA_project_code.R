data <- read.csv("Seagrass_data.csv")

library(MASS)
#Standardised abundance negative bionomial 
model <- glm.nb(Standardised.abundance ~ Site + Sample.type + Dilution, data = data)
summary_df <- summary(model)
summary(model)

#Standardised richness negative bionomial
model2 <- glm.nb(Standardised.richness ~ Site + Sample.type + Dilution, data = data)
summary_df <- summary(model2)
summary(model2)

#Shannon lm model
model3 <- lm(Shannon ~ Site + Sample.type + Dilution, data = data)
summary(model3)


#Abundance box polt
plot <- ggplot(data, aes(x = Site, y = Standardised.abundance)) +
  # add colour
  geom_boxplot(aes(fill = Site), position = position_dodge2(width = 0.3), outlier.shape = NA) +
  # add Dilution
  geom_point(aes(shape = as.factor(Dilution), color = Site), position = position_dodge2(width = 0.3), size=3) +
  labs(title = '',
       x = 'Site', y = 'Standardised abundance') +
  scale_shape_manual(values=c(16, 17, 18)) +  
  facet_grid(~ Sample.type, scales = "free_x", space = "free_x") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position="bottom",  
        strip.background = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold")) 

print(plot)


#Shannon box plot
plot <- ggplot(data, aes(x = Site, y = Shannon)) +
  # add colour
  geom_boxplot(aes(fill = Site), position = position_dodge2(width = 0.3), outlier.shape = NA) +
  # add Dilution
  geom_point(aes(shape = as.factor(Dilution), color = Site), position = position_dodge2(width = 0.3), size=3) +
  labs(title = '',
       x = 'Site', y = 'Shannon') +
  scale_shape_manual(values=c(16, 17, 18)) +  
  facet_grid(~ Sample.type, scales = "free_x", space = "free_x") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position="bottom",  
        strip.background = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold")) 

print(plot)

#Richness box plot
plot <- ggplot(data, aes(x = Site, y = Standardised.richness)) +
  # add colour
  geom_boxplot(aes(fill = Site), position = position_dodge2(width = 0.3), outlier.shape = NA) +
  # add Dilution
  geom_point(aes(shape = as.factor(Dilution), color = Site), position = position_dodge2(width = 0.3), size=3) +
  labs(title = '',
       x = 'Site', y = 'Standardised richness') +
  scale_shape_manual(values=c(16, 17, 18)) +  
  facet_grid(~ Sample.type, scales = "free_x", space = "free_x") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),  
        legend.position="bottom",  
        strip.background = element_blank(), 
        strip.text.x = element_text(size = 12, face = "bold")) 

print(plot)

