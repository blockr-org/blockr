

# combine_plots Block Start ======================================
      new_combine_plots_block <- function(data, ...){
      
      blockr::new_block(
        name = 'combine_plots_block',
        expr = quote({
	combine_plots(plotlist = .(plotlist),
						plotgrid.args = .(plotgrid.args),
						annotation.args = .(annotation.args),
						guides = .(guides),
						... = .(...))
}),
        fields = list(
	plotlist = list(value = character(0)),
					plotgrid.args = list(value = character(0)),
					annotation.args = list(value = character(0)),
					guides = blockr::new_string_field('collect'),
					... = list(value = character(0))
),
        class = c('data_block', 'combine_plots_block')
      )
      }
      
      combine_plots_block <- function(data, ...) {
        initialize_block(new_combine_plots_block(data, ...), data)
      }
      
      blockr::register_block(
        combine_plots_block,
        'combine_plots',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'combine_plots_block')
      )
# Block End ======================================
      
      

# extract_caption Block Start ======================================
      new_extract_caption_block <- function(data, ...){
      
      blockr::new_block(
        name = 'extract_caption_block',
        expr = quote({
	extract_caption(p = .(p))
}),
        fields = list(
	p = list(value = character(0))
),
        class = c('data_block', 'extract_caption_block')
      )
      }
      
      extract_caption_block <- function(data, ...) {
        initialize_block(new_extract_caption_block(data, ...), data)
      }
      
      blockr::register_block(
        extract_caption_block,
        'extract_caption',
        'A block',
        input = 'plot',
        output = 'data.frame',
        classes = c('data_block', 'extract_caption_block')
      )
# Block End ======================================
      
      

# extract_stats Block Start ======================================
      new_extract_stats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'extract_stats_block',
        expr = quote({
	extract_stats(p = .(p),
						... = .(...))
}),
        fields = list(
	p = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'extract_stats_block')
      )
      }
      
      extract_stats_block <- function(data, ...) {
        initialize_block(new_extract_stats_block(data, ...), data)
      }
      
      blockr::register_block(
        extract_stats_block,
        'extract_stats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'extract_stats_block')
      )
# Block End ======================================
      
      

# extract_subtitle Block Start ======================================
      new_extract_subtitle_block <- function(data, ...){
      
      blockr::new_block(
        name = 'extract_subtitle_block',
        expr = quote({
	extract_subtitle(p = .(p))
}),
        fields = list(
	p = list(value = character(0))
),
        class = c('data_block', 'extract_subtitle_block')
      )
      }
      
      extract_subtitle_block <- function(data, ...) {
        initialize_block(new_extract_subtitle_block(data, ...), data)
      }
      
      blockr::register_block(
        extract_subtitle_block,
        'extract_subtitle',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'extract_subtitle_block')
      )
# Block End ======================================
      
      

# ggbarstats Block Start ======================================
new_ggbarstats_block <- function(data, ...){
      
     all_cols <- colnames(data)
  
      blockr::new_block(
        name = 'ggbarstats_block',
        expr = quote({
	        ggbarstats(data,
						x = .(x),
						y = .(y),
						counts = .(counts),
						type = .(type),
						paired = .(paired),
						results.subtitle = .(results.subtitle),
						label = .(label),
						label.args = .(label.args),
						sample.size.label.args = .(sample.size.label.args),
						k = .(k),
						proportion.test = .(proportion.test),
						perc.k = .(perc.k),
						bf.message = .(bf.message),
						ratio = .(ratio),
						conf.level = .(conf.level),
						sampling.plan = .(sampling.plan),
						fixed.margin = .(fixed.margin),
						prior.concentration = .(prior.concentration),
						title = .(title),
						subtitle = .(subtitle),
						caption = .(caption),
						legend.title = .(legend.title),
						xlab = .(xlab),
						ylab = .(ylab),
						ggtheme = .(ggtheme),
						package = .(package),
						palette = .(palette),
						ggplot.component = .(ggplot.component)
}),
        fields = list(

					x = list(value = character(0)),
					y = list(value = character(0)),
					counts = list(value = character(0)),
					type = blockr::new_string_field('parametric'),
					paired = blockr::new_switch_field(FALSE),
					results.subtitle = blockr::new_switch_field(TRUE),
					label = blockr::new_string_field('percentage'),
					label.args = list(value = character(0)),
					sample.size.label.args = list(value = character(0)),
					k = list(value = character(0)),
					proportion.test = list(value = character(0)),
					perc.k = list(value = character(0)),
					bf.message = blockr::new_switch_field(TRUE),
					ratio = list(value = character(0)),
					conf.level = blockr::new_numeric_field(0.95),
					sampling.plan = blockr::new_string_field('indepMulti'),
					fixed.margin = blockr::new_string_field('rows'),
					prior.concentration = blockr::new_numeric_field(1),
					title = list(value = character(0)),
					subtitle = list(value = character(0)),
					caption = list(value = character(0)),
					legend.title = list(value = character(0)),
					xlab = list(value = character(0)),
					ylab = list(value = character(0)),
					ggtheme = list(value = character(0)),
					package = blockr::new_string_field('RColorBrewer'),
					palette = blockr::new_string_field('Dark2'),
					ggplot.component = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'ggbarstats_block')
      )
      }
      
      ggbarstats_block <- function(data, ...) {
        initialize_block(new_ggbarstats_block(data, ...), data)
      }
      
      blockr::register_block(
        ggbarstats_block,
        'ggbarstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'ggbarstats_block')
      )
# Block End ======================================
      
      

# ggbetweenstats Block Start ======================================
      new_ggbetweenstats_block <- function(data, ...){
      
        all_cols <- colnames(data)
      blockr::new_block(
        name = 'ggbetweenstats_block',
        expr = quote({
	ggbetweenstats(data,
						x = .(x),
						y = .(y),
						type = .(type),
						pairwise.display = .(pairwise.display),
						p.adjust.method = .(p.adjust.method),
						effsize.type = .(effsize.type),
						bf.prior = .(bf.prior),
						bf.message = .(bf.message),
						results.subtitle = .(results.subtitle),
						xlab = .(xlab),
						ylab = .(ylab),
						caption = .(caption),
						title = .(title),
						subtitle = .(subtitle),
						k = .(k),
						var.equal = .(var.equal),
						conf.level = .(conf.level),
						nboot = .(nboot),
						tr = .(tr),
						centrality.plotting = .(centrality.plotting),
						centrality.type = .(centrality.type),
						centrality.point.args = .(centrality.point.args),
						centrality.label.args = .(centrality.label.args),
						point.args = .(point.args),
						boxplot.args = .(boxplot.args),
						violin.args = .(violin.args),
						ggsignif.args = .(ggsignif.args),
						ggtheme = .(ggtheme),
						package = .(package),
						palette = .(palette),
						ggplot.component = .(ggplot.component),
						... = .(...))
}),
        fields = list(
	data = list(value = character(0)),
					x = list(value = character(0)),
					y = list(value = character(0)),
					type = blockr::new_string_field('parametric'),
					pairwise.display = blockr::new_string_field('significant'),
					p.adjust.method = blockr::new_string_field('holm'),
					effsize.type = blockr::new_string_field('unbiased'),
					bf.prior = blockr::new_numeric_field(0.707),
					bf.message = blockr::new_switch_field(TRUE),
					results.subtitle = blockr::new_switch_field(TRUE),
					xlab = list(value = character(0)),
					ylab = list(value = character(0)),
					caption = list(value = character(0)),
					title = list(value = character(0)),
					subtitle = list(value = character(0)),
					k = list(value = character(0)),
					var.equal = blockr::new_switch_field(FALSE),
					conf.level = blockr::new_numeric_field(0.95),
					nboot = list(value = character(0)),
					tr = blockr::new_numeric_field(0.2),
					centrality.plotting = blockr::new_switch_field(TRUE),
					centrality.type = list(value = character(0)),
					centrality.point.args = list(value = character(0)),
					centrality.label.args = list(value = character(0)),
					point.args = list(value = character(0)),
					boxplot.args = list(value = character(0)),
					violin.args = list(value = character(0)),
					ggsignif.args = list(value = character(0)),
					ggtheme = list(value = character(0)),
					package = blockr::new_string_field('RColorBrewer'),
					palette = blockr::new_string_field('Dark2'),
					ggplot.component = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'ggbetweenstats_block')
      )
      }
      
      ggbetweenstats_block <- function(data, ...) {
        initialize_block(new_ggbetweenstats_block(data, ...), data)
      }
      
      blockr::register_block(
        ggbetweenstats_block,
        'ggbetweenstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'ggbetweenstats_block')
      )
# Block End ======================================
      
      

# ggcoefstats Block Start ======================================
      new_ggcoefstats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'ggcoefstats_block',
        expr = quote({
	ggcoefstats(x = .(x),
						statistic = .(statistic),
						conf.int = .(conf.int),
						conf.level = .(conf.level),
						k = .(k),
						exclude.intercept = .(exclude.intercept),
						effectsize.type = .(effectsize.type),
						meta.analytic.effect = .(meta.analytic.effect),
						meta.type = .(meta.type),
						bf.message = .(bf.message),
						sort = .(sort),
						xlab = .(xlab),
						ylab = .(ylab),
						title = .(title),
						subtitle = .(subtitle),
						caption = .(caption),
						only.significant = .(only.significant),
						point.args = .(point.args),
						errorbar.args = .(errorbar.args),
						vline = .(vline),
						vline.args = .(vline.args),
						stats.labels = .(stats.labels),
						stats.label.color = .(stats.label.color),
						stats.label.args = .(stats.label.args),
						package = .(package),
						palette = .(palette),
						ggtheme = .(ggtheme),
						... = .(...))
}),
        fields = list(
	x = list(value = character(0)),
					statistic = list(value = character(0)),
					conf.int = blockr::new_switch_field(TRUE),
					conf.level = blockr::new_numeric_field(0.95),
					k = list(value = character(0)),
					exclude.intercept = blockr::new_switch_field(FALSE),
					effectsize.type = blockr::new_string_field('eta'),
					meta.analytic.effect = blockr::new_switch_field(FALSE),
					meta.type = blockr::new_string_field('parametric'),
					bf.message = blockr::new_switch_field(TRUE),
					sort = blockr::new_string_field('none'),
					xlab = list(value = character(0)),
					ylab = list(value = character(0)),
					title = list(value = character(0)),
					subtitle = list(value = character(0)),
					caption = list(value = character(0)),
					only.significant = blockr::new_switch_field(FALSE),
					point.args = list(value = character(0)),
					errorbar.args = list(value = character(0)),
					vline = blockr::new_switch_field(TRUE),
					vline.args = list(value = character(0)),
					stats.labels = blockr::new_switch_field(TRUE),
					stats.label.color = list(value = character(0)),
					stats.label.args = list(value = character(0)),
					package = blockr::new_string_field('RColorBrewer'),
					palette = blockr::new_string_field('Dark2'),
					ggtheme = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'ggcoefstats_block')
      )
      }
      
      ggcoefstats_block <- function(data, ...) {
        initialize_block(new_ggcoefstats_block(data, ...), data)
      }
      
      blockr::register_block(
        ggcoefstats_block,
        'ggcoefstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'ggcoefstats_block')
      )
# Block End ======================================
      
      

# ggcorrmat Block Start ======================================
      new_ggcorrmat_block <- function(data, ...){
      
      blockr::new_block(
        name = 'ggcorrmat_block',
        expr = quote({
	ggcorrmat(data = .(data),
						cor.vars = .(cor.vars),
						cor.vars.names = .(cor.vars.names),
						matrix.type = .(matrix.type),
						type = .(type),
						tr = .(tr),
						partial = .(partial),
						k = .(k),
						sig.level = .(sig.level),
						conf.level = .(conf.level),
						bf.prior = .(bf.prior),
						p.adjust.method = .(p.adjust.method),
						pch = .(pch),
						ggcorrplot.args = .(ggcorrplot.args),
						package = .(package),
						palette = .(palette),
						colors = .(colors),
						ggtheme = .(ggtheme),
						ggplot.component = .(ggplot.component),
						title = .(title),
						subtitle = .(subtitle),
						caption = .(caption),
						... = .(...))
}),
        fields = list(
	data = list(value = character(0)),
					cor.vars = list(value = character(0)),
					cor.vars.names = list(value = character(0)),
					matrix.type = blockr::new_string_field('upper'),
					type = blockr::new_string_field('parametric'),
					tr = blockr::new_numeric_field(0.2),
					partial = blockr::new_switch_field(FALSE),
					k = list(value = character(0)),
					sig.level = blockr::new_numeric_field(0.05),
					conf.level = blockr::new_numeric_field(0.95),
					bf.prior = blockr::new_numeric_field(0.707),
					p.adjust.method = blockr::new_string_field('holm'),
					pch = blockr::new_string_field('cross'),
					ggcorrplot.args = list(value = character(0)),
					package = blockr::new_string_field('RColorBrewer'),
					palette = blockr::new_string_field('Dark2'),
					colors = list(value = character(0)),
					ggtheme = list(value = character(0)),
					ggplot.component = list(value = character(0)),
					title = list(value = character(0)),
					subtitle = list(value = character(0)),
					caption = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'ggcorrmat_block')
      )
      }
      
      ggcorrmat_block <- function(data, ...) {
        initialize_block(new_ggcorrmat_block(data, ...), data)
      }
      
      blockr::register_block(
        ggcorrmat_block,
        'ggcorrmat',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'ggcorrmat_block')
      )
# Block End ======================================
      
      

# ggdotplotstats Block Start ======================================
      new_ggdotplotstats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'ggdotplotstats_block',
        expr = quote({
	ggdotplotstats(data = .(data),
						x = .(x),
						y = .(y),
						xlab = .(xlab),
						ylab = .(ylab),
						title = .(title),
						subtitle = .(subtitle),
						caption = .(caption),
						type = .(type),
						test.value = .(test.value),
						bf.prior = .(bf.prior),
						bf.message = .(bf.message),
						effsize.type = .(effsize.type),
						conf.level = .(conf.level),
						tr = .(tr),
						k = .(k),
						results.subtitle = .(results.subtitle),
						point.args = .(point.args),
						centrality.plotting = .(centrality.plotting),
						centrality.type = .(centrality.type),
						centrality.line.args = .(centrality.line.args),
						ggplot.component = .(ggplot.component),
						ggtheme = .(ggtheme),
						... = .(...))
}),
        fields = list(
	data = list(value = character(0)),
					x = list(value = character(0)),
					y = list(value = character(0)),
					xlab = list(value = character(0)),
					ylab = list(value = character(0)),
					title = list(value = character(0)),
					subtitle = list(value = character(0)),
					caption = list(value = character(0)),
					type = blockr::new_string_field('parametric'),
					test.value = blockr::new_numeric_field(0),
					bf.prior = blockr::new_numeric_field(0.707),
					bf.message = blockr::new_switch_field(TRUE),
					effsize.type = blockr::new_string_field('g'),
					conf.level = blockr::new_numeric_field(0.95),
					tr = blockr::new_numeric_field(0.2),
					k = list(value = character(0)),
					results.subtitle = blockr::new_switch_field(TRUE),
					point.args = list(value = character(0)),
					centrality.plotting = blockr::new_switch_field(TRUE),
					centrality.type = list(value = character(0)),
					centrality.line.args = list(value = character(0)),
					ggplot.component = list(value = character(0)),
					ggtheme = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'ggdotplotstats_block')
      )
      }
      
      ggdotplotstats_block <- function(data, ...) {
        initialize_block(new_ggdotplotstats_block(data, ...), data)
      }
      
      blockr::register_block(
        ggdotplotstats_block,
        'ggdotplotstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'ggdotplotstats_block')
      )
# Block End ======================================
      
      

# gghistostats Block Start ======================================
      new_gghistostats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'gghistostats_block',
        expr = quote({
	gghistostats(data = .(data),
						x = .(x),
						binwidth = .(binwidth),
						xlab = .(xlab),
						title = .(title),
						subtitle = .(subtitle),
						caption = .(caption),
						type = .(type),
						test.value = .(test.value),
						bf.prior = .(bf.prior),
						bf.message = .(bf.message),
						effsize.type = .(effsize.type),
						conf.level = .(conf.level),
						tr = .(tr),
						k = .(k),
						ggtheme = .(ggtheme),
						results.subtitle = .(results.subtitle),
						bin.args = .(bin.args),
						centrality.plotting = .(centrality.plotting),
						centrality.type = .(centrality.type),
						centrality.line.args = .(centrality.line.args),
						normal.curve = .(normal.curve),
						normal.curve.args = .(normal.curve.args),
						ggplot.component = .(ggplot.component),
						... = .(...))
}),
        fields = list(
	data = list(value = character(0)),
					x = list(value = character(0)),
					binwidth = list(value = character(0)),
					xlab = list(value = character(0)),
					title = list(value = character(0)),
					subtitle = list(value = character(0)),
					caption = list(value = character(0)),
					type = blockr::new_string_field('parametric'),
					test.value = blockr::new_numeric_field(0),
					bf.prior = blockr::new_numeric_field(0.707),
					bf.message = blockr::new_switch_field(TRUE),
					effsize.type = blockr::new_string_field('g'),
					conf.level = blockr::new_numeric_field(0.95),
					tr = blockr::new_numeric_field(0.2),
					k = list(value = character(0)),
					ggtheme = list(value = character(0)),
					results.subtitle = blockr::new_switch_field(TRUE),
					bin.args = list(value = character(0)),
					centrality.plotting = blockr::new_switch_field(TRUE),
					centrality.type = list(value = character(0)),
					centrality.line.args = list(value = character(0)),
					normal.curve = blockr::new_switch_field(FALSE),
					normal.curve.args = list(value = character(0)),
					ggplot.component = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'gghistostats_block')
      )
      }
      
      gghistostats_block <- function(data, ...) {
        initialize_block(new_gghistostats_block(data, ...), data)
      }
      
      blockr::register_block(
        gghistostats_block,
        'gghistostats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'gghistostats_block')
      )
# Block End ======================================
      
      

# ggpiestats Block Start ======================================
      new_ggpiestats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'ggpiestats_block',
        expr = quote({
	ggpiestats(data = .(data),
						x = .(x),
						y = .(y),
						counts = .(counts),
						type = .(type),
						paired = .(paired),
						results.subtitle = .(results.subtitle),
						label = .(label),
						label.args = .(label.args),
						label.repel = .(label.repel),
						k = .(k),
						proportion.test = .(proportion.test),
						perc.k = .(perc.k),
						bf.message = .(bf.message),
						ratio = .(ratio),
						conf.level = .(conf.level),
						sampling.plan = .(sampling.plan),
						fixed.margin = .(fixed.margin),
						prior.concentration = .(prior.concentration),
						title = .(title),
						subtitle = .(subtitle),
						caption = .(caption),
						legend.title = .(legend.title),
						ggtheme = .(ggtheme),
						package = .(package),
						palette = .(palette),
						ggplot.component = .(ggplot.component),
						... = .(...))
}),
        fields = list(
	data = list(value = character(0)),
					x = list(value = character(0)),
					y = list(value = character(0)),
					counts = list(value = character(0)),
					type = blockr::new_string_field('parametric'),
					paired = blockr::new_switch_field(FALSE),
					results.subtitle = blockr::new_switch_field(TRUE),
					label = blockr::new_string_field('percentage'),
					label.args = list(value = character(0)),
					label.repel = blockr::new_switch_field(FALSE),
					k = list(value = character(0)),
					proportion.test = list(value = character(0)),
					perc.k = list(value = character(0)),
					bf.message = blockr::new_switch_field(TRUE),
					ratio = list(value = character(0)),
					conf.level = blockr::new_numeric_field(0.95),
					sampling.plan = blockr::new_string_field('indepMulti'),
					fixed.margin = blockr::new_string_field('rows'),
					prior.concentration = blockr::new_numeric_field(1),
					title = list(value = character(0)),
					subtitle = list(value = character(0)),
					caption = list(value = character(0)),
					legend.title = list(value = character(0)),
					ggtheme = list(value = character(0)),
					package = blockr::new_string_field('RColorBrewer'),
					palette = blockr::new_string_field('Dark2'),
					ggplot.component = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'ggpiestats_block')
      )
      }
      
      ggpiestats_block <- function(data, ...) {
        initialize_block(new_ggpiestats_block(data, ...), data)
      }
      
      blockr::register_block(
        ggpiestats_block,
        'ggpiestats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'ggpiestats_block')
      )
# Block End ======================================
      
      

# ggscatterstats Block Start ======================================
      new_ggscatterstats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'ggscatterstats_block',
        expr = quote({
	ggscatterstats(data = .(data),
						x = .(x),
						y = .(y),
						type = .(type),
						conf.level = .(conf.level),
						bf.prior = .(bf.prior),
						bf.message = .(bf.message),
						tr = .(tr),
						k = .(k),
						results.subtitle = .(results.subtitle),
						label.var = .(label.var),
						label.expression = .(label.expression),
						marginal = .(marginal),
						point.args = .(point.args),
						point.width.jitter = .(point.width.jitter),
						point.height.jitter = .(point.height.jitter),
						point.label.args = .(point.label.args),
						smooth.line.args = .(smooth.line.args),
						xsidehistogram.args = .(xsidehistogram.args),
						ysidehistogram.args = .(ysidehistogram.args),
						xlab = .(xlab),
						ylab = .(ylab),
						title = .(title),
						subtitle = .(subtitle),
						caption = .(caption),
						ggtheme = .(ggtheme),
						ggplot.component = .(ggplot.component),
						... = .(...))
}),
        fields = list(
	data = list(value = character(0)),
					x = list(value = character(0)),
					y = list(value = character(0)),
					type = blockr::new_string_field('parametric'),
					conf.level = blockr::new_numeric_field(0.95),
					bf.prior = blockr::new_numeric_field(0.707),
					bf.message = blockr::new_switch_field(TRUE),
					tr = blockr::new_numeric_field(0.2),
					k = list(value = character(0)),
					results.subtitle = blockr::new_switch_field(TRUE),
					label.var = list(value = character(0)),
					label.expression = list(value = character(0)),
					marginal = blockr::new_switch_field(TRUE),
					point.args = list(value = character(0)),
					point.width.jitter = blockr::new_numeric_field(0),
					point.height.jitter = blockr::new_numeric_field(0),
					point.label.args = list(value = character(0)),
					smooth.line.args = list(value = character(0)),
					xsidehistogram.args = list(value = character(0)),
					ysidehistogram.args = list(value = character(0)),
					xlab = list(value = character(0)),
					ylab = list(value = character(0)),
					title = list(value = character(0)),
					subtitle = list(value = character(0)),
					caption = list(value = character(0)),
					ggtheme = list(value = character(0)),
					ggplot.component = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'ggscatterstats_block')
      )
      }
      
      ggscatterstats_block <- function(data, ...) {
        initialize_block(new_ggscatterstats_block(data, ...), data)
      }
      
      blockr::register_block(
        ggscatterstats_block,
        'ggscatterstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'ggscatterstats_block')
      )
# Block End ======================================
      
      

# ggwithinstats Block Start ======================================
      new_ggwithinstats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'ggwithinstats_block',
        expr = quote({
	ggwithinstats(data = .(data),
						x = .(x),
						y = .(y),
						type = .(type),
						pairwise.display = .(pairwise.display),
						p.adjust.method = .(p.adjust.method),
						effsize.type = .(effsize.type),
						bf.prior = .(bf.prior),
						bf.message = .(bf.message),
						results.subtitle = .(results.subtitle),
						xlab = .(xlab),
						ylab = .(ylab),
						caption = .(caption),
						title = .(title),
						subtitle = .(subtitle),
						k = .(k),
						conf.level = .(conf.level),
						nboot = .(nboot),
						tr = .(tr),
						centrality.plotting = .(centrality.plotting),
						centrality.type = .(centrality.type),
						centrality.point.args = .(centrality.point.args),
						centrality.label.args = .(centrality.label.args),
						centrality.path = .(centrality.path),
						centrality.path.args = .(centrality.path.args),
						point.args = .(point.args),
						point.path = .(point.path),
						point.path.args = .(point.path.args),
						boxplot.args = .(boxplot.args),
						violin.args = .(violin.args),
						ggsignif.args = .(ggsignif.args),
						ggtheme = .(ggtheme),
						package = .(package),
						palette = .(palette),
						ggplot.component = .(ggplot.component),
						... = .(...))
}),
        fields = list(
	data = list(value = character(0)),
					x = list(value = character(0)),
					y = list(value = character(0)),
					type = blockr::new_string_field('parametric'),
					pairwise.display = blockr::new_string_field('significant'),
					p.adjust.method = blockr::new_string_field('holm'),
					effsize.type = blockr::new_string_field('unbiased'),
					bf.prior = blockr::new_numeric_field(0.707),
					bf.message = blockr::new_switch_field(TRUE),
					results.subtitle = blockr::new_switch_field(TRUE),
					xlab = list(value = character(0)),
					ylab = list(value = character(0)),
					caption = list(value = character(0)),
					title = list(value = character(0)),
					subtitle = list(value = character(0)),
					k = list(value = character(0)),
					conf.level = blockr::new_numeric_field(0.95),
					nboot = list(value = character(0)),
					tr = blockr::new_numeric_field(0.2),
					centrality.plotting = blockr::new_switch_field(TRUE),
					centrality.type = list(value = character(0)),
					centrality.point.args = list(value = character(0)),
					centrality.label.args = list(value = character(0)),
					centrality.path = blockr::new_switch_field(TRUE),
					centrality.path.args = list(value = character(0)),
					point.args = list(value = character(0)),
					point.path = blockr::new_switch_field(TRUE),
					point.path.args = list(value = character(0)),
					boxplot.args = list(value = character(0)),
					violin.args = list(value = character(0)),
					ggsignif.args = list(value = character(0)),
					ggtheme = list(value = character(0)),
					package = blockr::new_string_field('RColorBrewer'),
					palette = blockr::new_string_field('Dark2'),
					ggplot.component = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'ggwithinstats_block')
      )
      }
      
      ggwithinstats_block <- function(data, ...) {
        initialize_block(new_ggwithinstats_block(data, ...), data)
      }
      
      blockr::register_block(
        ggwithinstats_block,
        'ggwithinstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'ggwithinstats_block')
      )
# Block End ======================================
      
      

# grouped_ggbarstats Block Start ======================================
      new_grouped_ggbarstats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'grouped_ggbarstats_block',
        expr = quote({
	grouped_ggbarstats(data = .(data),
						... = .(...),
						grouping.var = .(grouping.var),
						plotgrid.args = .(plotgrid.args),
						annotation.args = .(annotation.args))
}),
        fields = list(
	data = list(value = character(0)),
					... = list(value = character(0)),
					grouping.var = list(value = character(0)),
					plotgrid.args = list(value = character(0)),
					annotation.args = list(value = character(0))
),
        class = c('data_block', 'grouped_ggbarstats_block')
      )
      }
      
      grouped_ggbarstats_block <- function(data, ...) {
        initialize_block(new_grouped_ggbarstats_block(data, ...), data)
      }
      
      blockr::register_block(
        grouped_ggbarstats_block,
        'grouped_ggbarstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'grouped_ggbarstats_block')
      )
# Block End ======================================
      
      

# grouped_ggbetweenstats Block Start ======================================
      new_grouped_ggbetweenstats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'grouped_ggbetweenstats_block',
        expr = quote({
	grouped_ggbetweenstats(data = .(data),
						... = .(...),
						grouping.var = .(grouping.var),
						plotgrid.args = .(plotgrid.args),
						annotation.args = .(annotation.args))
}),
        fields = list(
	data = list(value = character(0)),
					... = list(value = character(0)),
					grouping.var = list(value = character(0)),
					plotgrid.args = list(value = character(0)),
					annotation.args = list(value = character(0))
),
        class = c('data_block', 'grouped_ggbetweenstats_block')
      )
      }
      
      grouped_ggbetweenstats_block <- function(data, ...) {
        initialize_block(new_grouped_ggbetweenstats_block(data, ...), data)
      }
      
      blockr::register_block(
        grouped_ggbetweenstats_block,
        'grouped_ggbetweenstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'grouped_ggbetweenstats_block')
      )
# Block End ======================================
      
      

# grouped_ggcorrmat Block Start ======================================
      new_grouped_ggcorrmat_block <- function(data, ...){
      
      blockr::new_block(
        name = 'grouped_ggcorrmat_block',
        expr = quote({
	grouped_ggcorrmat(data = .(data),
						... = .(...),
						grouping.var = .(grouping.var),
						plotgrid.args = .(plotgrid.args),
						annotation.args = .(annotation.args))
}),
        fields = list(
	data = list(value = character(0)),
					... = list(value = character(0)),
					grouping.var = list(value = character(0)),
					plotgrid.args = list(value = character(0)),
					annotation.args = list(value = character(0))
),
        class = c('data_block', 'grouped_ggcorrmat_block')
      )
      }
      
      grouped_ggcorrmat_block <- function(data, ...) {
        initialize_block(new_grouped_ggcorrmat_block(data, ...), data)
      }
      
      blockr::register_block(
        grouped_ggcorrmat_block,
        'grouped_ggcorrmat',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'grouped_ggcorrmat_block')
      )
# Block End ======================================
      
      

# grouped_ggdotplotstats Block Start ======================================
      new_grouped_ggdotplotstats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'grouped_ggdotplotstats_block',
        expr = quote({
	grouped_ggdotplotstats(data = .(data),
						... = .(...),
						grouping.var = .(grouping.var),
						plotgrid.args = .(plotgrid.args),
						annotation.args = .(annotation.args))
}),
        fields = list(
	data = list(value = character(0)),
					... = list(value = character(0)),
					grouping.var = list(value = character(0)),
					plotgrid.args = list(value = character(0)),
					annotation.args = list(value = character(0))
),
        class = c('data_block', 'grouped_ggdotplotstats_block')
      )
      }
      
      grouped_ggdotplotstats_block <- function(data, ...) {
        initialize_block(new_grouped_ggdotplotstats_block(data, ...), data)
      }
      
      blockr::register_block(
        grouped_ggdotplotstats_block,
        'grouped_ggdotplotstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'grouped_ggdotplotstats_block')
      )
# Block End ======================================
      
      

# grouped_gghistostats Block Start ======================================
      new_grouped_gghistostats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'grouped_gghistostats_block',
        expr = quote({
	grouped_gghistostats(data = .(data),
						x = .(x),
						grouping.var = .(grouping.var),
						binwidth = .(binwidth),
						plotgrid.args = .(plotgrid.args),
						annotation.args = .(annotation.args),
						... = .(...))
}),
        fields = list(
	data = list(value = character(0)),
					x = list(value = character(0)),
					grouping.var = list(value = character(0)),
					binwidth = list(value = character(0)),
					plotgrid.args = list(value = character(0)),
					annotation.args = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'grouped_gghistostats_block')
      )
      }
      
      grouped_gghistostats_block <- function(data, ...) {
        initialize_block(new_grouped_gghistostats_block(data, ...), data)
      }
      
      blockr::register_block(
        grouped_gghistostats_block,
        'grouped_gghistostats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'grouped_gghistostats_block')
      )
# Block End ======================================
      
      

# grouped_ggpiestats Block Start ======================================
      new_grouped_ggpiestats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'grouped_ggpiestats_block',
        expr = quote({
	grouped_ggpiestats(data = .(data),
						... = .(...),
						grouping.var = .(grouping.var),
						plotgrid.args = .(plotgrid.args),
						annotation.args = .(annotation.args))
}),
        fields = list(
	data = list(value = character(0)),
					... = list(value = character(0)),
					grouping.var = list(value = character(0)),
					plotgrid.args = list(value = character(0)),
					annotation.args = list(value = character(0))
),
        class = c('data_block', 'grouped_ggpiestats_block')
      )
      }
      
      grouped_ggpiestats_block <- function(data, ...) {
        initialize_block(new_grouped_ggpiestats_block(data, ...), data)
      }
      
      blockr::register_block(
        grouped_ggpiestats_block,
        'grouped_ggpiestats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'grouped_ggpiestats_block')
      )
# Block End ======================================
      
      

# grouped_ggscatterstats Block Start ======================================
      new_grouped_ggscatterstats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'grouped_ggscatterstats_block',
        expr = quote({
	grouped_ggscatterstats(data = .(data),
						... = .(...),
						grouping.var = .(grouping.var),
						plotgrid.args = .(plotgrid.args),
						annotation.args = .(annotation.args))
}),
        fields = list(
	data = list(value = character(0)),
					... = list(value = character(0)),
					grouping.var = list(value = character(0)),
					plotgrid.args = list(value = character(0)),
					annotation.args = list(value = character(0))
),
        class = c('data_block', 'grouped_ggscatterstats_block')
      )
      }
      
      grouped_ggscatterstats_block <- function(data, ...) {
        initialize_block(new_grouped_ggscatterstats_block(data, ...), data)
      }
      
      blockr::register_block(
        grouped_ggscatterstats_block,
        'grouped_ggscatterstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'grouped_ggscatterstats_block')
      )
# Block End ======================================
      
      

# grouped_ggwithinstats Block Start ======================================
      new_grouped_ggwithinstats_block <- function(data, ...){
      
      blockr::new_block(
        name = 'grouped_ggwithinstats_block',
        expr = quote({
	grouped_ggwithinstats(data = .(data),
						... = .(...),
						grouping.var = .(grouping.var),
						plotgrid.args = .(plotgrid.args),
						annotation.args = .(annotation.args))
}),
        fields = list(
	data = list(value = character(0)),
					... = list(value = character(0)),
					grouping.var = list(value = character(0)),
					plotgrid.args = list(value = character(0)),
					annotation.args = list(value = character(0))
),
        class = c('data_block', 'grouped_ggwithinstats_block')
      )
      }
      
      grouped_ggwithinstats_block <- function(data, ...) {
        initialize_block(new_grouped_ggwithinstats_block(data, ...), data)
      }
      
      blockr::register_block(
        grouped_ggwithinstats_block,
        'grouped_ggwithinstats',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'grouped_ggwithinstats_block')
      )
# Block End ======================================
      
      

# iris_long Block Start ======================================
      new_iris_long_block <- function(data, ...){
      
      blockr::new_block(
        name = 'iris_long_block',
        expr = quote({
	iris_long( = .())
}),
        fields = list(
	 = 
),
        class = c('data_block', 'iris_long_block')
      )
      }
      
      iris_long_block <- function(data, ...) {
        initialize_block(new_iris_long_block(data, ...), data)
      }
      
      blockr::register_block(
        iris_long_block,
        'iris_long',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'iris_long_block')
      )
# Block End ======================================
      
      

# movies_long Block Start ======================================
      new_movies_long_block <- function(data, ...){
      
      blockr::new_block(
        name = 'movies_long_block',
        expr = quote({
	movies_long( = .())
}),
        fields = list(
	 = 
),
        class = c('data_block', 'movies_long_block')
      )
      }
      
      movies_long_block <- function(data, ...) {
        initialize_block(new_movies_long_block(data, ...), data)
      }
      
      blockr::register_block(
        movies_long_block,
        'movies_long',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'movies_long_block')
      )
# Block End ======================================
      
      

# pairwise_comparisons Block Start ======================================
      new_pairwise_comparisons_block <- function(data, ...){
      
      blockr::new_block(
        name = 'pairwise_comparisons_block',
        expr = quote({
	pairwise_comparisons(data = .(data),
						x = .(x),
						y = .(y),
						subject.id = .(subject.id),
						type = .(type),
						paired = .(paired),
						var.equal = .(var.equal),
						tr = .(tr),
						bf.prior = .(bf.prior),
						p.adjust.method = .(p.adjust.method),
						k = .(k),
						... = .(...))
}),
        fields = list(
	data = list(value = character(0)),
					x = list(value = character(0)),
					y = list(value = character(0)),
					subject.id = list(value = character(0)),
					type = blockr::new_string_field('parametric'),
					paired = blockr::new_switch_field(FALSE),
					var.equal = blockr::new_switch_field(FALSE),
					tr = blockr::new_numeric_field(0.2),
					bf.prior = blockr::new_numeric_field(0.707),
					p.adjust.method = blockr::new_string_field('holm'),
					k = list(value = character(0)),
					... = list(value = character(0))
),
        class = c('data_block', 'pairwise_comparisons_block')
      )
      }
      
      pairwise_comparisons_block <- function(data, ...) {
        initialize_block(new_pairwise_comparisons_block(data, ...), data)
      }
      
      blockr::register_block(
        pairwise_comparisons_block,
        'pairwise_comparisons',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'pairwise_comparisons_block')
      )
# Block End ======================================
      
      

# theme_ggstatsplot Block Start ======================================
      new_theme_ggstatsplot_block <- function(data, ...){
      
      blockr::new_block(
        name = 'theme_ggstatsplot_block',
        expr = quote({
	theme_ggstatsplot( = .())
}),
        fields = list(
	 = 
),
        class = c('data_block', 'theme_ggstatsplot_block')
      )
      }
      
      theme_ggstatsplot_block <- function(data, ...) {
        initialize_block(new_theme_ggstatsplot_block(data, ...), data)
      }
      
      blockr::register_block(
        theme_ggstatsplot_block,
        'theme_ggstatsplot',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'theme_ggstatsplot_block')
      )
# Block End ======================================
      
      

# Titanic_full Block Start ======================================
      new_Titanic_full_block <- function(data, ...){
      
      blockr::new_block(
        name = 'Titanic_full_block',
        expr = quote({
	Titanic_full( = .())
}),
        fields = list(
	 = 
),
        class = c('data_block', 'Titanic_full_block')
      )
      }
      
      Titanic_full_block <- function(data, ...) {
        initialize_block(new_Titanic_full_block(data, ...), data)
      }
      
      blockr::register_block(
        Titanic_full_block,
        'Titanic_full',
        'A block',
        input = 'data.frame',
        output = 'data.frame',
        classes = c('data_block', 'Titanic_full_block')
      )
# Block End ======================================
      
      
