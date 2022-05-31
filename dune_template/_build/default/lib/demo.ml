
let homepage = ref "gemini://gemini.circumlunar.space/";;




let print_homepage str = 
    match str with 
    | str -> homepage := str; "homepage changed!"



