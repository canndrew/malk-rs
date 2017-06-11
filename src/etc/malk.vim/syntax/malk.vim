"syn match malkIdent "[a-zA-Z_][a-zA-Z_0-9]*" containedin=malkBinders
syn match malkKeywords "Rec'*" nextgroup=malkIdent skipwhite
syn match malkBinders "let" nextgroup=malkIdent skipwhite
syn match malkBinders "rec" nextgroup=malkIdent skipwhite
syn match malkNamedTypes "Type'*"
syn match malkNamedTypes "Level'*"

syn match malkEqualityTerm "=="
syn match malkEqualityType "#="

syn match malkLineComment "//.*$"
syn region malkStringLiteral start=/\v"/ skip=/\v\\./ end=/\v"/
syn region malkInsideTerm matchgroup=malkStructTerm start="{" end="}" fold contains=ALLBUT,malkBindType
syn region malkInsideType matchgroup=malkStructType start="#{" end="}" fold contains=ALLBUT,malkBindTerm
syn region malkInsideTerm matchgroup=malkEnumTerm start="\[" end="\]" fold contains=ALLBUT,malkBindType
syn region malkInsideType matchgroup=malkEnumType start="#\[" end="\]" fold contains=ALLBUT,malkBindTerm
"syn region malkInsidePattern matchgroup=malkStructPattern start="{" end="}" fold contains=ALL nextgroup=malkFuncTerm

syn match malkFuncTerm "=>"
syn match malkFuncType "->"

syn match malkBindTerm "[a-zA-Z_][a-zA-Z_0-9]*[\s]*="he=e-1 containedin=malkInsideTerm
syn match malkBindType "[a-zA-Z_][a-zA-Z_0-9]*[\s]*:"he=e-1 containedin=malkInsideType

syn match malkNumber "\<\d\+\(\(u\|i\)\(\d\+\|big\|m\)\)\?"
syn match malkNamedTypes "\(U\|I\)\(\d\+\|big\|m\)"

"hi malkStructPattern ctermfg=cyan
hi malkBinders ctermfg=yellow
hi malkKeywords ctermfg=yellow
hi malkNumber ctermfg=red
hi malkStringLiteral ctermfg=red
hi malkStructTerm ctermfg=green
hi malkStructType ctermfg=magenta
hi malkEnumTerm ctermfg=green
hi malkEnumType ctermfg=magenta
hi malkFuncTerm ctermfg=yellow cterm=bold
hi malkFuncType ctermfg=magenta cterm=bold
hi malkEqualityTerm ctermfg=lightblue cterm=bold
hi malkEqualityType ctermfg=magenta cterm=bold
hi malkNamedTypes ctermfg=magenta
hi malkLineComment ctermfg=blue
"hi malkIdent ctermfg=lightgrey
"hi malkBindTerm ctermfg=lightblue
"hi malkBindType ctermfg=lightblue cterm=bold

