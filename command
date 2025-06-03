let s:so_save = &g:so | let s:siso_save = &g:siso | setg so=0 siso=0 | setl so=-1 siso=-1
argglobal
if bufexists(fnamemodify("~/Documents/tofl/cwd.lisp", ":p")) | buffer ~/Documents/tofl/cwd.lisp | else | edit ~/Documents/tofl/cwd.lisp | endif
if &buftype ==# 'terminal'
  silent file ~/Documents/tofl/cwd.lisp
endif
setlocal fdm=manual
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=0
setlocal fml=1
setlocal fdn=20
setlocal fen
silent! normal! zE
9,17fold
19,27fold
29,29fold
29,33fold
29,33fold
59,63fold
let &fdl = &fdl
29
normal! zo
29
normal! zo
29
normal! zo
29
normal! zc
let s:l = 56 - ((20 * winheight(0) + 16) / 32)
if s:l < 1 | let s:l = 1 | endif
keepjumps exe s:l
normal! zt
keepjumps 56
normal! 039|
let &g:so = s:so_save | let &g:siso = s:siso_save
set hlsearch
nohlsearch
doautoall SessionLoadPost
" vim: set ft=vim :
