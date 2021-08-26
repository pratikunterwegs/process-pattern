
read_data = function(path = ".", gen_start = 50, gen_end = 250, gen_step = 10,
                     gen_add = 249) {
  # list generation files
  files = glue("{path}/{str_pad(c(seq({gen_start}, {gen_end}, {gen_step}), {gen_add}),\\
               5, '0', side = 'left')}_pos.csv")
  
  data = lapply(files, data.table::fread)
  data = data.table::rbindlist(data)
}
