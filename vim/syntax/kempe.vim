scriptencoding utf-8

if exists('b:current_syntax')
    finish
endif

syntax match kempeSymbol "--"
syntax match kempeSymbol "->"

syntax match kempeIdentifier "\v[a-z][a-zA-Z0-9]*"
syntax match kempeType "\v[A-Z][a-zA-Z0-9]*"

syntax match kempeExternal "\".*\""

syntax match kempeNum "\v[0-9]+"
syntax match kempeNum "\v0x[0-9a-z]+"
syntax match kempeNum "\v0x[0-9a-z]+u"
syntax match kempeNum "\v[0-9]+i8"

syntax match kempeKeyword "type"
syntax match kempeKeyword "case"
syntax match kempeKeyword "import"
syntax match kempeKeyword "$"

syntax match kempeType "Int"
syntax match kempeType "Word"
syntax match kempeType "Int8"
syntax match kempeType "Ptr"

syntax match kempeComment "\v;.*$" contains=@Spell

syntax match kempeIdentifier "$cfun"
syntax match kempeSymbol "cabi"
syntax match kempeSymbol "kabi"
syntax match kempePragma "%foreign"

highlight link kempeComment Comment
highlight link kempeKeyword Keyword
highlight link kempeOperator Keyword
highlight link kempeNum Number
highlight link kempeIdentifier Identifier
highlight link kempeType Type
highlight link kempeSymbol Special
highlight link kempeExternal String
highlight link kempePragma Type

let b:current_syntax = 'kempe'
