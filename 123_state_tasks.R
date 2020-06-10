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

  # Return test results to the parent remake file
  # Create the task plan
  task_plan <- create_task_plan(
    task_names = task_names,
    task_steps = list(download_step),
    add_complete = FALSE)
  # Create the task remakefile
  create_task_makefile(
    # TODO: ADD ARGUMENTS HERE
    task_plan = task_plan,
    makefile = '123_state_tasks.yml',
    include = c('remake.yml'),
    packages = c("tidyverse", "dataRetrieval"),
    sources = c(...),
    tickquote_combinee_objects = FALSE,
    finalize_funs = c())

  # Build the tasks
  scmake('123_state_tasks', remake_file='123_state_tasks.yml')

  # Return nothing to the parent remake file
  return()
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
