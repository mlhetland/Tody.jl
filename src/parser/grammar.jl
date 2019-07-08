name         = r"[^\W\d]\w*"                           ^Na
number       = r"(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?"  ^Nu
str          = r"\"([^\"\\]|\\[\"\\])*\""              ^St
operator     = r"[!#%&*\-./:?@\p{S}]+"

literal      = name | number | str

unit()       = literal | group | block

arguments()  = binaries & ( "," & arguments | ϵ )

call         = (( "(" & ")" ) |  "(" & arguments & ")" )
indexing     =                   "[" & arguments & "]"

operand      = unit &̲ ( call ^C | indexing ^I | ϵ )
operand     ^= combine

unary        = operator ^Un &̲ operand

expression() = ( unary | operand ) &̲
               ( nbsp &̲ operator ^Op &̲ sp &̲ expression
               |        operator ^Op &̲    expression | ϵ )

binaries     = expression ^E

separator    = ";" | eol

line()       = binaries &̲ nbspᵋ &̲ (             line  | ϵ )
lines()      = line ^Ln &̲ nbspᵋ &̲ ( separator & lines | ϵ )

group        =                 "(" & line  ^G & ")"
block        = ( "{" & "}" ) | "{" & lines ^B & "}"

program      = bof & lines ^B & eof
program     ^= cleanup
