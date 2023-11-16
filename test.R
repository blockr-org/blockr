devtools::load_all()
library(blockr.data)

stack <- new_stack(
  demo_data_block,
  select_block,
  plot_block
)

serialise(stack)
