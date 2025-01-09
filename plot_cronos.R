library("tidyverse")
library("ggrepel")
library("scales")
library("grid")
library("gridExtra")
library("patchwork")
library("stringr")
library("lemon")
library("magrittr")

cro_sites <- read_csv("data/qpyr/cronos_sites.csv")

cro_sites <- cro_sites |>
  mutate(
    site =
      case_when(
        site == "CA_Low" ~ "CA-Low",
        site == "CA_High" ~ "CA-High",
        site == "SJ" ~ "SJ",
      )
  )

# peores sequias
dy <- read_csv("data/qpyr/severe_spei12.csv")
dy <- dy |> arrange(desc(d_duration))
# solamente aquellos mayor de 2 meses
dys <- dy|>filter(d_duration > 2) |> as.data.frame()
dys <- dys[,'maxyear']


colores_den <- c("#56B4E9","#8B0A50","#CD5C5C")

# cores
plot_bai <- cro_sites |>
  filter(nseries > 5) |>
  filter(site %in% c("CA-High", "SJ")) |>
  ggplot(aes(x=year, y=bai_mean/100, colour=site)) +

  # add ribbon for the period 1925 - 1947
  geom_rect(aes(xmin = 1925, xmax = 1950,
                ymin = -Inf, ymax = 1),
            fill = "#CDC673", colour = NA, alpha = 0.01) +
  geom_vline(xintercept = c(1913, 1931, 1995, 1999, 2012, 2005), size = 0.6, color = "black") +
  theme_bw() + ylab('BAI') +
  geom_ribbon(aes(ymin = (bai_mean - bai_se)/100,
                  ymax = (bai_mean + bai_se)/100,
                  fill=site), alpha=.4, colour=NA) +
  geom_line() +
  geom_point(size=.8) +
  scale_colour_manual(values = colores_den) +
  scale_fill_manual(values = colores_den) +
  ylab(expression(paste("Incremento Área Basal (", cm^2, año^-1, ")", sep=''))) +
  xlab("Año") +
  # scale_x_continuous(breaks = seq(1815, 2020, by=10), limits=c(1815,2020), sec.axis = dup_axis()) +
  theme(panel.grid = element_blank(),
        axis.text =element_text(size=7),
        axis.title = element_text(size=8),
        legend.position = "none",
        strip.background = element_blank(),
        axis.title.x.top = element_blank(),
        axis.text.x.top = element_blank())

ggsave("output/cronos.jpg", plot = plot_bai, width=25, height=9, dpi=300, units="cm")




# saveImage(plot_bai_s, path="./man/figures/svg/", dpi=300, w=25, h=14, fig= "fig_4", u = "cm")



