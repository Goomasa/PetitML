let rec cui()=print_string "# "; let line=read_line() in 
  if line="quit" then exit 0
  else print_string line; print_newline(); cui();;