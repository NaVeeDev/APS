open Format

let sep_cma fmt () = fprintf fmt ", "

let pp_lst_cma p = pp_print_list ~pp_sep:sep_cma p
