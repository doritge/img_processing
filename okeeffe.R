library(tidyverse)
library(imager)

#############################
#
#  Color transformation
#
###########################
bi <- load.image("images/black_iris.jpg")
bi_df <- bi %>%
  as.data.frame() %>%
  mutate(cc = factor(cc,labels=c('R','G','B'))) %>%
  group_by(cc) %>%
  mutate(cd = ecdf(value)(value) * 3.5) %>%
  ungroup

#### Color density analysis
p_density <- bi_df %>%
  ggplot() +
  geom_density(aes(value, fill = cc, color = cc), alpha = 0.1, size = 0.75) +
  scale_fill_manual(name = "channel",
                    values = c("R" = "red", "G" = "green", "B" = "blue")) +
  scale_color_manual(name = "channel",
                     values = c("R" = "red", "G" = "green", "B" = "blue")) +
  scale_y_continuous(sec.axis = sec_axis(~./3.5, name = "cumulative distribution")) +
  theme(legend.position = "none")
#ggsave("output/density.png", p_density)

p_cumulative <- p_density +
  geom_line(aes(value, cd, color = cc), size = 0.75, linetype = "dashed")
#ggsave("output/cumulative.png", p_cumulative)

#### Red enhancement
R(bi) <- as.cimg(ecdf(R(bi))(R(bi)),dim=dim(R(bi)))

#save.image(bi, "output/bf_red.jpg", quality = 1)

###########################
#
#  Edge detection
#
###########################
aw <- load.image("images/abstract_wave.jpg")

wave_mask <- aw %>%
  isoblur(1) %>%
  grayscale() %>%
  imgradient("xy") %>%
  enorm() %>%
  threshold("80%")
wave <- as.cimg(wave_mask)
wave[which(wave_mask)] <- 0.9

#save.image(wave, "output/wave.jpg", quality = 1)

###########################
#
#  Masking
#
###########################
drawing <- load.image("images/drawing_xii.jpg")

mask <- drawing %>%
  isoblur(7) %>%
  grayscale() %>%
  threshold("25%")

m_drawing <- drawing
alpha <-  0.7
R(m_drawing)[which(mask)] <- alpha + (1 - alpha) * R(m_drawing)[which(mask)] 
G(m_drawing)[which(mask)] <- (1 - alpha) * G(m_drawing)[which(mask)]
B(m_drawing)[which(mask)] <- (1 - alpha) * B(m_drawing)[which(mask)]

save.image(m_drawing, "output/m_drawing.jpg", quality = 1)
