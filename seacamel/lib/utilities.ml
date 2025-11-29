let is_digit c =
    c >= '0' && c <= '9'

let is_alpha c =
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z')

let is_alnum c =
    is_alpha c || is_digit c
