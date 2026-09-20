; extends

; Keep JSX names above inherited OCaml captures and LSP semantic tokens.
((jsx_tag) @tag
  (#set! priority 130))

; Only unqualified lowercase names are built-in tags.
((jsx_tag
  . (value_name) .) @tag.builtin
  (#set! priority 131))

((jsx_prop_name) @tag.attribute
  (#set! priority 130))

(jsx_prop
  "=" @operator)

(jsx_children_spread
  "..." @operator)

; Scope delimiters to JSX so OCaml comparisons retain their own highlighting.
[
  (jsx_element_opening
    ["<" ">"] @tag.delimiter)
  (jsx_element_closing
    ["</" ">"] @tag.delimiter)
  (jsx_element_self_closing
    ["<" "/>"] @tag.delimiter)
]
