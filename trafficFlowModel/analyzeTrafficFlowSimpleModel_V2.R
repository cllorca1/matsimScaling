#get all_all data frame

all = read.csv(file =  "c:/models/file.csv")
all$exponent = as.character(all$exponent)

all = all %>% filter(capacity == 1250)


labs = c("L = 100 m", "L = 500 m", "L = 1,000 m", "L = 5,000 m","L = 10,000 m" )
names(labs) = c(100,500,1000,5000,10000)

labs2 = c("C = 1,000 veh/h" ,"C = 1500 veh/h", "C=1600 veh/h","C=1800 veh/h", "C = 2,000 veh/h", "C = 2,500 veh/h" )
names(labs2) = c(500,750, 800, 900, 1000,1250)

all$exponent[all$exponent=="withHoles2.00-1.00"] = 1
all$exponent[all$exponent=="withHoles2.00-.95"] = 0.95
all$exponent[all$exponent=="withHoles2.00-.90"] = 0.90 
all$exponent[all$exponent=="withHoles2.00-.85"] = 0.85
all$exponent[all$exponent=="withHoles2.00-.80"] = 0.80
all$exponent[all$exponent=="withHoles2.00-.75"] = 0.75
all$exponent[all$exponent=="withHoles2.00-.70"] = 0.70
all$exponent[all$exponent=="withHoles2.00-.65"] = 0.65
all$exponent[all$exponent=="withHoles1.50-1.00"] = 1
all$exponent[all$exponent=="withHoles1.50-.95"] = 0.95
all$exponent[all$exponent=="withHoles1.50-.90"] = 0.90 
all$exponent[all$exponent=="withHoles1.50-.85"] = 0.85
all$exponent[all$exponent=="withHoles1.50-.80"] = 0.80
all$exponent[all$exponent=="withHoles1.50-.75"] = 0.75
all$exponent[all$exponent=="withHoles1.50-.70"] = 0.70
all$exponent[all$exponent=="withHoles1.50-.65"] = 0.65
all$exponent[all$exponent=="withHoles1.00-1.00"] = 1
all$exponent[all$exponent=="withHoles1.00-.95"] = 0.95
all$exponent[all$exponent=="withHoles1.00-.90"] = 0.90 
all$exponent[all$exponent=="withHoles1.00-.85"] = 0.85
all$exponent[all$exponent=="withHoles1.00-.80"] = 0.80
all$exponent[all$exponent=="withHoles1.00-.75"] = 0.75
all$exponent[all$exponent=="withHoles1.00-.70"] = 0.70
all$exponent[all$exponent=="withHoles1.00-.65"] = 0.65


all$factor = paste(all$factor * 100, "% of capacity", sep = "")


ggplot(all, aes(x=scale, y= length / tt*3.6, color = as.factor(exponent), group = as.factor(exponent))) +
  geom_path(size = 1) +
  geom_point(shape = 21, size = 3, fill = "white") +
  facet_grid(factor~length, labeller = labeller(length = labs, capacity = labs2)) +
  xlab("Scaling factor (%)") + 
  ylab("Average speed (km/h)") +
  theme_bw() + theme(legend.position = "bottom") +
  labs(color = "Exponent of the storage capacity factor") + 
  scale_color_brewer(palette = "Spectral")
