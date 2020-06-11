do_state_tasks <- function(oldest_active_sites, ...) {
  # create 1_fetch/tmp directory
  if(!dir.exists('1_fetch/tmp')) dir.create('1_fetch/tmp')

  # Call split inventory function
  split_inventory(summary_file='1_fetch/tmp/state_splits.yml', sites_info=oldest_active_sites)

  # Define task table rows
  # TODO: DEFINE A VECTOR OF TASK NAMES HERE
  task_names <- oldest_active_sites$state_cd

  # Define task table columns
  download_step <- create_task_step(
    step_name = 'download',
    # TODO: Make target names like "WI_data"
    target_name = function(task_name, step_name, ...) {
      sprintf('%s_data', task_name)
    },
    # TODO: Make commands that call get_site_data()
    command = function(task_name, step_name, ...) {
      sprintf("get_site_data(state_info_file='1_fetch/tmp/inventory_%s.tsv', parameter=parameter)", task_name)
    }
  )

  plot_step <- create_task_step(
    step_name = 'plot',
    # make target names like "3_visualize/out/timeseries_WI.png"
    target_name =  function(task_name, ...) {
      sprintf('3_visualize/out/timeseries_%s.png', task_name)
    },
    # make commands that call plot_site_data()
    command = function(target_name, steps, ...) {
      sprintf("plot_site_data(out_file=target_name, site_data=%s, parameter=parameter)", steps[['download']]$target_name)
    }
  )

  tally_step <- create_task_step(
    step_name = 'tally',
    # make target names like "WI_tally"
    target_name = function(task_name, step_name, ...) {
      sprintf("%s_%s", task_name, step_name)
    },
    # make commands that call tally_site_obs()
    command = function(target_name, steps, ...) {
      sprintf("tally_site_obs(site_data=%s)", steps[['download']]$target_name)
    }
  )

  # Return test results to the parent remake file
  # Create the task plan
  task_plan <- create_task_plan(
    task_names = task_names,
    task_steps = list(download_step, plot_step, tally_step),
    final_steps = c('tally', 'plot'),
    add_complete = FALSE)
  # Create the task remakefile
  create_task_makefile(
    # TODO: ADD ARGUMENTS HERE
    task_plan = task_plan,
    makefile = '123_state_tasks.yml',
    include = c('remake.yml'),
    packages = c("tidyverse", "dataRetrieval", "lubridate"),
    sources = c(...),
    final_targets = c('obs_tallies', '3_visualize/out/timeseries_plots.yml'),
    finalize_funs = c('combine_obs_tallies', 'summarize_timeseries_plots'),
    as_promises = TRUE,
    tickquote_combinee_objects = TRUE)

  # Build the tasks
  obs_tallies <- scmake('obs_tallies_promise', remake_file='123_state_tasks.yml')
  scmake('timeseries_plots.yml_promise', remake_file='123_state_tasks.yml')
  timeseries_plots_info <- yaml::yaml.load_file('3_visualize/out/timeseries_plots.yml') %>%
    tibble::enframe(name = 'filename', value = 'hash') %>%
    mutate(hash = purrr::map_chr(hash, `[[`, 1))

  # Return nothing to the parent remake file
  return(list(obs_tallies=obs_tallies, timeseries_plots_info=timeseries_plots_info))
}

split_inventory <- function(summary_file, sites_info) {
  # create empty vector to store file names
  file_names = c()
  # Loop over each row in oldest_active_sites to...
  for (row in 1:nrow(sites_info)) {
    # pull each row
    site_info <- sites_info[row,]
    # save each row to a file
    file_name <- sprintf("1_fetch/tmp/inventory_%s.tsv", sites_info[row,'state_cd'])
    readr::write_tsv(site_info, file_name)
    # store file name in vector of file names
    file_names <- append(file_names,file_name)
  }
  # sort file names alphabetically
  file_names <- sort(file_names)
  # write a summary file to the path given by summary_file
  sc_indicate(
    ind_file = summary_file,
    data_file = file_names
  )
}

combine_obs_tallies <- function(...) {
  # filter to just those arguments that are tibbles (because the only step
  # outputs that are tibbles are the tallies)
  dots <- list(...)
  tally_dots <- dots[purrr::map_lgl(dots, is_tibble)]
  # bind the tibbles together
  bind_rows(tally_dots)
}

summarize_timeseries_plots <- function(ind_file, ...) {
  # filter to just those arguments that are character strings (because the only
  # step outputs that are characters are the plot filenames)
  dots <- list(...)
  plot_dots <- dots[purrr::map_lgl(dots, is.character)]
  do.call(combine_to_ind, c(list(ind_file), plot_dots))
}
