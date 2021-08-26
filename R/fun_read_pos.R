
read_data = function(path = ".", gen_start = 50, gen_end = 250, gen_step = 10) {
  # list generation files
  files = glue("{path}/{str_pad(seq({gen_start}, {gen_end}, {gen_step}),\\
               5, '0', side = 'left')}_pos.csv")
  
  data = lapply(files, data.table::fread)
  data = data.table::rbindlist(data)
}
