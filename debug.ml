let verbosity = ref 0
let colorize = ref false

let set_verbosity n = verbosity := n
let have_colors b = colorize := b

let print formatter l format =
  if l <= !verbosity
  then (
    Format.kfprintf
      (fun fmt -> Format.fprintf fmt "@.")
      formatter
      format)
  else Format.ifprintf formatter format 

let p l format = print Format.std_formatter l format
let err l format = print Format.err_formatter l format

let f level func =
  if level <= !verbosity then
    func ()

let fs level funcs = List.iter (f level) funcs
  
let ansi_red = "\x1b[0;31m"
let ansi_green = "\x1b[0;32m"
let ansi_yellow = "\x1b[0;33m"
let ansi_blue = "\x1b[0;34m"
let ansi_magenta = "\x1b[0;35m"
let ansi_bred = "\x1b[1;31m"
let ansi_bgreen = "\x1b[1;32m"
let ansi_byellow = "\x1b[1;33m"
let ansi_bblue = "\x1b[1;34m"
let ansi_bmagenta = "\x1b[1;35m"
let ansi_normal = "\x1b[0m"

let c color format =
  let b = Buffer.create 10 in
  let to_b = Format.formatter_of_buffer b in
  if !colorize then Format.pp_print_string to_b color;
  Format.kfprintf
    (fun fmt ->
      Format.fprintf fmt "%s@?"
        (if !colorize then ansi_normal else "");
      Buffer.contents b)
    to_b
    format

let red fmt = c ansi_red fmt
let green fmt = c ansi_green fmt
let yellow fmt = c ansi_yellow fmt
let blue fmt = c ansi_blue fmt
let magenta fmt = c ansi_magenta fmt
let bred fmt = c ansi_bred fmt
let bgreen fmt = c ansi_bgreen fmt
let byellow fmt = c ansi_byellow fmt
let bblue fmt = c ansi_bblue fmt
let bmagenta fmt = c ansi_bmagenta fmt

let p0 f = p 0 f
let p1 f = p 1 f
let f0 = f 0
let f1 = f 1
