" Vim Syntax File
" Language:     Gemspec for Bundler, Ruby
" Creator:      Tatsuhiro Ujihisa <ujihisa at gmail com>
" Last Change:  2010 Jan 02
if version < 600
    syntax clear
endif


runtime syntax/ruby.vim
syntax case match
syntax keyword gemfileKeywords gem source bundle_path bin_path disable_system_gems disable_rubygems
highlight link gemfileKeywords Define
