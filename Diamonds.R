str(diamonds)
regss <-lm(price ~ cut + clarity + color, data=diamonds,contrasts=list(color="contr.treatment", cut="contr.treatment", clarity="contr.treatment"))
summary(regss)
table(fitted.values(regss)==Agility)

plot(q1$price, col="navy",
     +      type="l", xlab = "Regression variables", ylab = "Price", main = "Price: Obderved and Fitted\nfor Quartile 1")

lines(fitted.values(r1), col="red",
        +       lty="dashed")

legend( 100,40,legend=c("Observed", "Fitted"),
          +         lty=c("solid", "dashed"), col=c("navy", "red"))
par(mfrow = c(2, 3))
plot(r1, which = 1:6)

facet_wrap(~C)

predict(regss, list(carat = 2, cut = "Fair"))
# Carat and Price

#without log
p <- ggplot(diamonds, aes(carat, price, color = clarity))
# with LOG
p <- ggplot(diamonds, aes(log(carat), log(price), color = clarity))
# scatter plot
# scatter plot
p + geom_point(alpha = .5) + geom_smooth(method = "lm") +  ggtitle("Relationship between carat and price") +
  xlab("Carat") + ylab("Price in $") + ylim(0, 20000)
cor(carat, price) 
#0.9659137

p <- ggplot(diamonds, aes(carat))

## Cut


p <- ggplot(diamonds, aes(factor(cut), fill = color))
p + geom_bar() + ggtitle("Distribution of Cuts of Diamonds") + xlab("Cut of Diamonds") + ylab("Frequency")

p <- ggplot(diamonds, aes(factor(cut), price))
p + geom_boxplot()

# mean of price of cut
diamonds %>%
  +     group_by(cut) %>%
  +     dplyr::summarize(Mean = mean(price, na.rm=TRUE))
#easier
by(price, cut, summary)
#mean of carat of cut
diamonds %>%
  +     group_by(cut) %>%
  +     dplyr::summarize(Mean = mean(carat, na.rm=TRUE))


#clarity
c <- diamonds[carat > .4 & carat < 1.19, ]
p <- ggplot(c, aes(factor(c$clarity), c$price))
p + geom_boxplot()

p <- ggplot(c, aes(factor(c$clarity), c$price))
p + geom_boxplot()
c <- diamonds[carat > 0 & carat < .4, ]

c <- diamonds[carat > .7 & carat < 1.04, ]
p <- ggplot(c, aes(factor(c$clarity), c$price))
p + geom_boxplot()

## exporting summary s1
s1 <- summary(r1)
s1 <- as.data.frame(s1)
out <- capture.output(s1)
cat("s1", out, file = "s1.doc", sep = "\n", append = TRUE)
