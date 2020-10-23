setlocal commentstring=;\ %s

set smarttab

setl shiftwidth=4

" use kc as a checker
let g:syntastic_kempe_checkers = [ 'kc' ]
