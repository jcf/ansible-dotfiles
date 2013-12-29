set background=dark

if has("gui_running")
  colorscheme wombat
else
  colorscheme wombat256mod
endif

" Use the same background color for signs as we do for line numbers. This
" makes signs and line numbers look like one nice big column.
hi! link SignColumn LineNr

" Use a more subtle color to highlight whitespace when using 'listchars'.
hi! SpecialKey gui=NONE guifg=#113245 guibg=#002b36

hi! ColorColumn guibg=black ctermbg=black

" Use the same background colour behind text, and non-text. This removes the
" two different background colours you get out-of-the-box.
hi! Normal ctermbg=NONE
hi! NonText ctermbg=NONE
hi! link NonText Normal
