

# libraries --------------------------------------------------------------------
library(patchwork)


# combine ----------------------------------------------------------------------
plot_Gender / plot_Race + plot_layout(guides = 'collect')


# save -------------------------------------------------------------------------
ggsave(
  "out/plot_combined.png", 
  width = 8, 
  height = 8
)
