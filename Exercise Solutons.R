# Auhtor: Tanay Mukherjee

# Que 1
library(dplyr)
library(GGally)

# Load the data
salary <- read.csv("C:\\Users\\its_t\\Documents\\CUNY Fall 2019\\9750 - Software Tools and Techniques_Data Science\\HW2\\salary.csv")

View(salary)

levels(salary$Education)
salary$Education = factor(salary$Education, levels = c("Low", "Medium", "High"))

ggplot(salary) +
  aes(x = salary$Salary, y = salary$Anxiety, color = salary$Education) +
  geom_point(alpha = 0.6)+geom_smooth(method = "lm")+
  facet_grid(salary$Education ~ .) + xlab("Salary") + ylab("Anxiety") +
  ggtitle("Relationship between salaries, anxiety, and education levels")

library(GGally)
ggcorr(salary, label = TRUE)

ggpairs(salary)

# no relation between anxiety and salary  








#----
# Que 2
# Load the data
ir_nyc <- read.csv("C:\\Users\\its_t\\Documents\\CUNY Fall 2019\\9750 - Software Tools and Techniques_Data Science\\HW2\\nyc.csv")
View(ir_nyc)

ir_nyc <- ir_nyc %>% select(3:7)
ggpairs(ir_nyc)

ggcorr(ir_nyc, label = TRUE, digits = 2)

# ggcorr(
#   data = ir_nyc,
#   cor_matrix = cor(dt[, -1], use = "pairwise")
# )



# 3.	Find 2 examples of cheap restaurants that have relatively good food and 2 examples
# of expensive restaurants that have relatively bad food. 

# ir_nyc %>% arrange(Price, -Food) %>% head(n=2)
# ir_nyc %>% arrange(-Price, Food) %>% head(n=2)

summary(ir_nyc)

# Mean Price = 43
# Mean Food Rating = 21

#aes(col = ir_nyc$Restaurant)
# + geom_text_repel(data = ir_nyc, aes(label = model))

ir_nyc_filter <- ir_nyc %>% filter()

# theme_set(theme_bw()) 
# ir_plot <- ggplot(ir_nyc, aes(x=ir_nyc$Price, y = ir_nyc$Food)) +
#   geom_point() + 
#   labs(subtitle="Relationship between Foor rating and Price", y="Food", x="Price")
# plot(ir_plot)


library(ggalt)
library(ggrepel)

new <- ir_nyc %>% mutate(diff = Food - Price)
new %>% arrange(new$diff) %>%  head(2)
new %>% arrange(-new$diff) %>%  head(2)


highlight <- ir_nyc %>% subset(Price < 25 & Food > 20 | Price  <35 & Food > 22.5
                               | Price > 64 & Food < 20 | Price > 40 & Food < 17)

h1 <- highlight %>% filter(row_number()==1)
h2 <- highlight %>% filter(row_number()==2)
h3 <- highlight %>% filter(row_number()==3)
h4 <- highlight %>% filter(row_number()==4)



theme_set(theme_bw()) 
  ggplot(ir_nyc, aes(x=ir_nyc$Price, y = ir_nyc$Food)) + geom_point() +
  labs(subtitle="Relationship between Foor rating and Price", y="Food Ratings", x="Price") +
  geom_vline(xintercept = mean(ir_nyc$Price), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(ir_nyc$Food), color = "green", linetype = "dashed") +
  geom_text(aes(x=41.5, label="\n Line for Mean Price", y=24), colour="red", angle=90, text=element_text(size=5)) +
  geom_text(aes(x=22, label="\n Line for Mean Food Ratings", y=20.5), colour="green", angle=360, text=element_text(size=5)) +
  geom_text(aes(x=22, label="\n Line for Mean Food Ratings", y=20.5), colour="green", angle=360, text=element_text(size=5)) +
  geom_text_repel(data = highlight, aes(label=Restaurant, x=Price,y=Food, color = "blue"))+
  geom_encircle(aes(x=Price,y=Food),data=h1,color="red", size=2,expand=1/100000, s_shape=0.9) +
  geom_encircle(aes(x=Price,y=Food),data=h2,color="red", size=2,expand=1/100000, s_shape=0.9) +  
  geom_encircle(aes(x=Price,y=Food),data=h3,color="red", size=2,expand=1/100000, s_shape=0.9) +
  geom_encircle(aes(x=Price,y=Food),data=h4,color="red", size=2,expand=1/100000, s_shape=0.9) 

# 4.	Suppose you're going on a date and want to use the information in this dataset
# to pick where to go. Assume your budget is at most $40.
# Assuming that you can get a table anywhere you want, where would you go and why? 
  
# ir_date <- ir_nyc %>% filter(Price <= 40) %>% 
#   group_by(Restaurant,Price,Food,Decor,Service) %>%
#   arrange(desc(Price,Food,Decor,Service))


ir_date <- ir_nyc %>% filter(Price <= 40) %>% select(3:6)
ir_date <- cbind(ir_date, Total = rowSums(ir_date)) 



# colourCount <- length(unique(ir_nyc$Decor))
# colourCount
# getPalette <- colorRampPalette(brewer.pal(8, "Set1"))(colourCount)
# getPalette

ggplot(ir_nyc, aes(x=Price, y = Food)) +
  geom_point(aes(col = Decor, size = Service)) + 
  geom_text_repel(data = subset(ir_date, Restaurant== "Rughetta"), aes(label=Restaurant, x=Price,y=Food))
  
  


library(sqldf)

sqldf("select Restaurant from ( select *, dense_rank() over 
(partition by Price, Food, Decor, Service order by Food desc, Decor desc, Service desc)rn from ir_date) where rn=1")

# 5.	Create a figure that displays the relationship between
# price, food, decor, service,and the East / West indicator.
# Your figure can contain more than one plot / facet / panel.
# Make sure that the labels and the title are interpretable. 
# Interpret in detail the relationships that you see.


# library(ggExtra)
# 
# theme_set(theme_bw()) 
# ir_plot <- ggplot(ir_nyc, aes(x=ir_nyc$Price, y = ir_nyc$Food)) +
#   geom_point(aes(col = Decor, size = Service)) +
#   geom_smooth(method="loess", se=F) + facet_grid(ir_nyc$East ~ .) +
#   labs(subtitle="Relationship between all variables", 
#        y="Price", 
#        x="Food", 
#        title="Scatterplot")
# plot(ir_plot)







colourCount <- length(unique(ir_nyc$Decor))
colourCount
getPalette <- colorRampPalette(brewer.pal(8, "Set1"))(colourCount)
getPalette

theme_set(theme_bw()) 
ir_plot <- ggplot(ir_nyc, aes(x=ir_nyc$Price, y = ir_nyc$Food)) +
  geom_point(aes(col = as.factor(Decor), size = Service)) +
  geom_smooth(method="loess", se=F) + facet_grid(ir_nyc$East ~ .) +
  scale_color_manual(values = getPalette) +
  labs(subtitle="Relationship between all variables", 
       y="Food", 
       x="Price", 
       title="Scatterplot") +
  scale_color_manual(values = getPalette)
plot(ir_plot)





#----
# Que 3
# Load the data

idd <- read.table("http://users.stat.ufl.edu/~winner/data/interfaith.dat", header = FALSE)
names(idd)[1:5] <- c("SEC", "Religion", "Gender", "Interfaith dating", "Count")

names <- c(1:4)
idd[,names] <- lapply(idd[,names],factor)
str(idd)

levels(idd$SEC) = c("Low", "Middle", "High")
levels(idd$Religion) = c("Protestant", "Catholic")
levels(idd$Gender) = c("Male", "Female")
levels(idd$`Interfaith dating`) = c("Yes", "No")
str(idd)




# Create a figure that shows the relationship between 
# socioeconomic class, religion, gender, and the indicator of interfaith dating .
# Your figure can contain more than one plot / facet / panel.
# Interpret in detail the relationships that you see in the plots.
# Make sure that the labels and the title are interpretable. 



# ggplot(idd) +
#   aes(x = idd$SEC, y = idd$Count, color = idd$Religion) +
#   geom_point(alpha = 0.6)+geom_smooth(method = "lm")+
#   facet_grid(idd$Gender ~ .) + facet_grid(idd$`Interfaith dating` ~ .)+
#   xlab("SEC") + ylab("Frequency") + ggtitle("Interfaith Data Analysis")
# 
# 
# colourCount <- length(unique(idd))
# colourCount
# getPalette <- colorRampPalette(brewer.pal(8, "Set1"))(colourCount)
# getPalette
# 
# 
# scale_color_manual(values = getPalette) +
#   
# theme_set(theme_bw()) 
# idd_plot <- ggplot(idd, aes(x=idd$SEC, y = idd$Count)) +
#   geom_point(aes(col = idd$Religion, size = idd$Gender)) +
#   geom_smooth(method="loess", se=F) + facet_grid(idd$`Interfaith dating` ~ .) +
#     labs(subtitle="Relationship between all variables", 
#        y="Food", 
#        x="Price", 
#        title="Scatterplot") +
#   scale_color_manual(values = getPalette)
# plot(idd_plot)

ggplot(idd) +
  aes(x= SEC, y = Count, fill = `Interfaith dating`, labels = TRUE) +
  geom_col(position = "dodge") +
  facet_grid(Gender ~ Religion) +
  ggtitle("Relationship between different factors involved in Interfaith dating")+
  xlab("Socioeconomic Class") + ylab("Frequency") +
  geom_text(aes(label = Count, Count = Count + 0.05), position = position_dodge(0.9),vjust = 0)

