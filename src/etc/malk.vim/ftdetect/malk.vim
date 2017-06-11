au BufRead,BufNewFile *.malk setf malk
au BufRead,BufNewFile *.malk call MalkLoad()
"au TextChanged,TextChangedI  *.malk call MalkEdit()

