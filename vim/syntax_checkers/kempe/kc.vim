if exists('g:loaded_syntastic_kempe_kc_checker')
    finish
endif
let g:loaded_syntastic_kempe_kc_checker = 1

let g:syntastic_kempe_kc_exec = 'kc'

function! SyntaxCheckers_kempe_kc_GetLocList() dict
    let makeprg = self.makeprgBuild({
                \ 'exe': self.getExec(),
                \ 'args': 'typecheck',
                \ 'fname': shellescape(expand('%') )})

    let errorformat =
        \ 'kc: %m'

    let loclist = SyntasticMake({
            \ 'makeprg': makeprg,
            \ 'errorformat': errorformat })

    return loclist

endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'kempe',
    \ 'name': 'kc' })
