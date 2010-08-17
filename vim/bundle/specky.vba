" Vimball Archiver by Charles E. Campbell, Jr., Ph.D.
UseVimball
finish
syntax/rdoc.vim	[[[1
52
"
" specky: syntax highlighting for 'rdoc' output
" $Id: rdoc.vim 46 2008-08-12 16:24:10Z mahlon $


" Separator lines
"
syntax match rdocSpeckyLines /^------\+\s\?/ contains=rdocSpeckyTarget,rdocSpeckyMultiples
highlight link rdocSpeckyLines Comment

" The class/method that we're reading
"
syntax match rdocSpeckyTarget / .\+/hs=s+1 contained
highlight link rdocSpeckyTarget Underlined

" When there are multiple matches to choose from.
" This is only output by fri.
"
syntax match rdocSpeckyMultiples / Multiple choices/hs=s+1 contained
highlight link rdocSpeckyMultiples WarningMsg

" Secondary headers
"
syntax region rdocSpeckyHeader start="^\S\+.\+:$\n-\+" end="-$" keepend contains=rdocSpeckyHeaderLine
highlight link rdocSpeckyHeader Question

" Secondary header lines
"
syntax match rdocSpeckyHeaderLine /^-\+$/ contained 
highlight link rdocSpeckyHeaderLine NonText

" Remove the formatting characters from the display
"
highlight link rdocSpeckySpecials NonText

" _word_ --> italic
"
syntax region rdocSpeckyItalic matchgroup=rdocSpeckySpecials start=" _" end="_"
highlight link rdocSpeckyItalic Special

" *word* --> bold
"
syntax region rdocSpeckBold matchgroup=rdocSpeckySpecials start=" \*" end="\*"
highlight link rdocSpeckyBold Constant

" +word+ --> typewriter
"
syntax region rdocSpeckyType matchgroup=rdocSpeckySpecials start=" +" end="+"
highlight link rdocSpeckyType Identifier

let b:current_syntax = "rdoc"

syntax/specrun.vim	[[[1
75
"
" specky: syntax highlighting for the 'spec' script output
" $Id: specrun.vim 67 2009-04-20 00:57:06Z mahlon $
"

if has("folding")
  setlocal foldmethod=syntax
endif

" Command line as it was called, inserted by Specky
"
syntax match specSpeckyCmd /^Output of: .*/
highlight link specSpeckyCmd Question
"syntax match WarningMsg /\.\./

" Plain output block (...P..F...)
"
syntax region specPlain start="^[\.PF]\+" end="^$" contains=specFailedPlain,specPendingPlain
highlight link specPlain MoreMsg

" Passed specs (specdoc output)
"
syntax match specPassed /^- .*/ contains=specFailed,specPending
highlight link specPassed MoreMsg

" Pending specs (specdoc output)
"
syntax match specPending /.*PENDING: .*)$/ contained
highlight link specPending Function
"
" (Plain output)
syntax match specPendingPlain /P/ contained
highlight link specPendingPlain Function

" Failed specs (specdoc output)
"
syntax match specFailed /.*\(FAILED\|ERROR\) - \d\+)/ contained
highlight link specFailed WarningMsg
"
" (Plain output)
syntax match specFailedPlain /F/ contained
highlight link specFailedPlain WarningMsg

" Warning details
"
syntax region specFailedDetails start="^\d\+)" end="^$" fold
highlight link specFailedDetails WarningMsg

" Pending specs
"
syntax region specPendingDetails start="^Pending:" end="^$" fold
highlight link specPendingDetails Function

" Timing information
"
syntax match specTimedRun /^Finished in.*/
highlight link specTimedRun Question

" Status summary
"
syntax match specExamplesTotal /^\d\+ examples, .\+/ contains=specTotalFailed,specNoFailures,specTotalPending
highlight link specExamplesTotal Special
"
syntax match specTotalFailed /\d\+ failure\%[s]/ contained
highlight link specTotalFailed WarningMsg
"
syntax match specTotalPending /\d pending/ contained
highlight link specTotalPending Function
"
syntax match specNoFailures /0 failures/ contained
highlight link specNoFailures MoreMsg


let b:current_syntax = "specrun"

syntax/rspec.vim	[[[1
30
"
" specky: syntax highlighting for rspec files.
" $Id: rspec.vim 70 2009-06-01 14:33:54Z mahlon $
"

runtime! syntax/ruby.vim
unlet b:current_syntax

syntax keyword rspecGroupMethods describe it
highlight link rspecGroupMethods Type

syntax keyword rspecBeforeAndAfter after after_suite_parts append_after append_before before before_suite_parts prepend_after prepend_before
highlight link rspecBeforeAndAfter Statement

syntax keyword rspecMocks mock stub
highlight link rspecMocks Constant

syntax keyword rspecMockMethods and_raise and_return and_throw and_yield build_child called_max_times expected_args invoke matches
highlight link rspecMockMethods Function

syntax keyword rspecKeywords should should_not should_not_receive should_receive
highlight link rspecKeywords Constant

syntax keyword rspecMatchers be_a be_a_kind_of be_an be_an_instance_of be_close be_false be_instance_of be_kind_of be_nil be_true change eql equal exist expect have have_at_least have_at_most have_exactly include match matcher raise_error respond_to satisfy throw_symbol to to_not wrap_expectation
highlight link rspecMatchers Function

syntax keyword rspecMessageExpectation advise any_number_of_times at_least at_most exactly expected_messages_received generate_error ignoring_args matches_at_least_count matches_at_most_count matches_exact_count matches_name_but_not_args negative_expectation_for never once ordered similar_messages times twice verify_messages_received with
highlight link rspecMessageExpectation Function

let b:current_syntax = "rspec"
ftdetect/rspec.vim	[[[1
1
au BufRead,BufNewFile *_spec.rb set filetype=rspec
ftplugin/rspec.vim	[[[1
7
if exists("b:did_ftplugin")
    finish
endif

" Behave just like Ruby
runtime! ftplugin/ruby.vim

doc/specky.txt	[[[1
333
*specky.txt* Last change: $Id: specky.txt 67 2009-04-20 00:57:06Z mahlon $

                VIM REFERENCE MANUAL    by Mahlon E. Smith


                                  specky!

A Plugin for testing Ruby code with RSpec -- and more                 *specky*
==============================================================================
CONTENTS                                                      *SpeckyContents*


    1) Intro........................................|SpeckyIntro|
    2) Functionality................................|SpeckyFunctionality|
    3) Enabling Specky..............................|SpeckyVimrcExample|
    4) Configuration................................|SpeckyOptions|
        4.1) Create text banners....................|g:speckyBannerKey|
        4.2) Cycling quote styles...................|g:speckyQuoteSwitcherKey|
        4.3) Display ruby documentation.............|g:speckyRunRdocKey|
        4.4) Toggle editing between spec and code...|g:speckySpecSwitcherKey|
        4.5) Run specs for the current buffer.......|g:speckyRunSpecKey|
        4.6) Modify the default spec command........|g:speckyRunSpecCmd|
        4.7) Modify the default rdoc command........|g:speckyRunRdocCmd|
        4.8) Alter new window behavior..............|g:speckyWindowType|
    5) Author.......................................|SpeckyAuthor|
    6) License......................................|SpeckyLicense|



==============================================================================
1. INTRO                                                         *SpeckyIntro*


Specky is primarily a small collection of functions to help make behavioral
testing streamlined and easy when working with ruby and rspec.

Specky secondarily includes a couple of conveniences to make your everyday
programming tasks smooooth and pleasurable.



==============================================================================
2. FUNCTIONALITY                                         *SpeckyFunctionality*


Okay then, what does it do?

By default?  Nothing but syntax highlighting unless you are comfortable using
the menus.  I decided the easiest way to cherry pick the functionality that
you'd like was to enable them via key bindings.  By doing this, Specky won't
make assumptions about your current environment, and won't stomp on anything
you don't want it to.

    Specky won't do -anything- with your environment until you enable ~
    the key bindings!! ~

After you've configured your bindings, here are some of the things you can
now do with a single key stroke:
>
    - Switch back and forth from code to testing spec 

    - Run the spec, with results going to a new, syntax highlighted buffer 

    - Jump quickly to spec failures and failure detail 
        - 'e' and 'r' to move back and forth on each failed assertion, 
        - 'E' to jump details for it. 
        - '<C-e>' to "forget" the currently selected failed assertion
        - 'q' to close the spec output buffer. 

    - View rdoc of the word under the cursor

    - Dynamically switch string types for the word under the cursor
      (double quoted, quoted, symbol)

    - Make lovely and quick comment banners for ruby code.

Specky also includes a "snippets" file that can be used with the Snipmate
plugin by Michael Sanders <msanders42+vim@gmail.com>. (Minimum version 0.74.)

	http://www.vim.org/scripts/script.php?script_id=2540

==============================================================================
3. ENABLING-SPECKY                                        *SpeckyVimrcExample*


Here's what my config looks like. >

    let g:speckyBannerKey = "<C-S>b"
    let g:speckyQuoteSwitcherKey = "<C-S>'"
    let g:speckyRunRdocKey = "<C-S>r"
    let g:speckySpecSwitcherKey = "<C-S>x"
    let g:speckyRunSpecKey = "<C-S>s"
    let g:speckyRunSpecCmd = "spec -fs -r loadpath.rb"
    let g:speckyRunRdocCmd = "fri -L -f plain"
    let g:speckyWindowType = 2


With these bindings, all specky commands start with <ctrl-s> ("s" for
specky!), followed by a mnemonic function to run:

    b ----> Banner creation ~
    ' ----> Quote cycling ~
    r ----> run Rdoc ~
    x ----> code and spec eXchange ~
    s ----> run Spec ~

Of course, <ctrl-s> is a "suspend" signal for most terminals, so these
bindings are meant for a |gui| environment, such as gvim.  Your mileage (and
tastes) will doubtlessly vary.  Do what you will.  I won't judge you.



==============================================================================
4. CONFIGURATION-OPTIONS                                       *SpeckyOptions*


Here are all of the available configuration options.

Please note that you must set binding variables:

    |g:speckyBannerKey|
    |g:speckyQuoteSwitcherKey|
    |g:speckyRunRdocKey|
    |g:speckySpecSwitcherKey|
    |g:speckyRunSpecKey|

...in order to enable the respective specky functionality.  See
|SpeckyVimrcExample| for details. Any other options are entirely optional.
Put these into your |vimrc|, or wherever else you enjoy storing this kind of
stuff.



------------------------------------------------------------------------------
4.1                                                        *g:speckyBannerKey*


Setting this binding enables comment banner creation.

This is purely a convenience routine, and a stylistic one at that.  I prefer
large advertising of what "area" of code you are in, along with other
miscellaneous labels for humans to read.  If this isn't how you roll, then by
all means, don't enable this binding!  :)

As an example -- you can just type:

	instance methods ~

Then hit the keystroke.  It will magically turn into: >

  ########################################################################
  ### I N S T A N C E   M E T H O D S
  ########################################################################

With all those saved extra keystrokes this might provide you per banner over
the years, your RSI-free hands will thank you.  And the total time savings!!
Oh man, what are you going to DO with all of that extra free time?
The possibilities are staggering.



------------------------------------------------------------------------------
4.2                                                 *g:speckyQuoteSwitcherKey*


Setting this binding enables quote "style switching".

If you aren't in ruby mode, this just changes the word under the cursor
back and forth from double quoting to single quoting.

    string -> "string" -> 'string' -> "string" ... ~

In ruby mode, symbols are also put into the rotation.

    "string" -> 'string' -> :string -> "string" ... ~

Note that quote cycling only works with a |word|.



------------------------------------------------------------------------------
4.3                                                       *g:speckyRunRdocKey*


Setting this enables the display of rdoc documentation for the current
word under the cursor.  For lookups with multiple matches, you can continue
using this binding to "drill down" to the desired documentation.
         


------------------------------------------------------------------------------
4.4                                                  *g:speckySpecSwitcherKey*


Setting this enables spec to code switching, and visa versa.

Switching uses path searching instead of reliance on directory structure in
your project.  The idea here is that you'd |:chdir| into your project
directory.  Spec files just need to end in '_spec.rb', which is a common
convention.

    aRubyClass.rb ---> aRubyClass_spec.rb~
 
Because it leaves respective buffers open, you can essentially think of this
as a quick toggle between code and tests.



------------------------------------------------------------------------------
4.5                                                       *g:speckyRunSpecKey*


Setting this variable runs "spec" on the current buffer.

All output is sent to a syntax highlighted scratch buffer. This new window is
re-used for each spec run.  You can quickly "jump" to assertion failures and
their associated details with the following keys:

        e and r ~
            Move forward and backward through the failed assertions.

        E~
            While on a failure line, jump to the details of the failure.

        <C-e> ~
            "Forget" the last found failed assertion, and start over at the
            beginning of the list. (ie, the next 'e' keystroke will select
            error #1.)

        q ~
            Closes the spec output buffer. 


Normally, you'd only want to perform this keystroke while in a spec file
buffer.  If specky thinks you are in code, rather than a buffer (as indicated
by the lack of a "_spec.rb" file naming convention) then it will attempt to
switch to the spec before running the command.



------------------------------------------------------------------------------
4.6                                                       *g:speckyRunSpecCmd*


This is the program, with flags, that the current file is sent to when
executing the |g:speckyRunSpecKey| keybinding.

A common addition is to include an "-r" flag for sucking in local libraries
necessary for testing your project.  The spec "plain" output format is
supported too, though less useful.

    Default: ~
        spec -fs



------------------------------------------------------------------------------
4.7                                                       *g:speckyRunRdocCmd*


If you prefer an rdoc display program other than "ri", you can set it
with this variable.  "fri -L -f plain" is always a nice choice, for example.

    Default: ~
        ri



------------------------------------------------------------------------------
4.8                                                       *g:speckyWindowType*


For both spec and rdoc commands, this variable controls the behavior of the
newly generated window.

	Default: ~
		0
		
	0 ~
		Create a new tabbed window
	1 ~
		Split the current window horizontally
	2 ~
		Split the current window vertically


==============================================================================
5. AUTHOR                                                       *SpeckyAuthor*


Specky was written by Mahlon E. Smith.

    mahlon@martini.nu ~
    http://www.martini.nu/ 



==============================================================================
6. LICENSE                                                     *SpeckyLicense*


Specky is distributed under the BSD license.
    http://www.opensource.org/licenses/bsd-license.php
>
    Copyright (c) 2008, Mahlon E. Smith <mahlon@martini.nu>
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

        * Redistributions of source code must retain the above copyright
          notice, this list of conditions and the following disclaimer.

        * Redistributions in binary form must reproduce the above copyright
          notice, this list of conditions and the following disclaimer in the
          documentation and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
    TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



vim: set noet nosta sw=4 ts=4 ft=help :
plugin/specky.vim	[[[1
347
" vim: set noet nosta sw=4 ts=4 fdm=marker :
"
" Specky!
" Mahlon E. Smith <mahlon@martini.nu>
" $Id: specky.vim 69 2009-04-20 05:06:53Z mahlon $
"


" Hook up the functions to the user supplied key bindings. {{{
"
if exists( 'g:speckySpecSwitcherKey' )
	execute 'map ' . g:speckySpecSwitcherKey . ' :call <SID>SpecSwitcher()<CR>'
"	map &g:speckySpecSwitcherKey <SID>SpecSwitcher()
endif

if exists( 'g:speckyQuoteSwitcherKey' )
	execute 'map ' . g:speckyQuoteSwitcherKey . ' :call <SID>QuoteSwitcher()<CR>'
endif

if exists( 'g:speckyBannerKey' )
	execute 'map ' . g:speckyBannerKey . ' :call <SID>MakeBanner()<CR>'
endif

if exists( 'g:speckyRunSpecKey' )
	execute 'map ' . g:speckyRunSpecKey . ' :call <SID>RunSpec()<CR>'
endif

if exists( 'g:speckyRunRdocKey' )
	execute 'map ' . g:speckyRunRdocKey . ' :call <SID>RunRdoc()<CR>'
endif

if exists( 'specky_loaded' )
	finish
endif
let specky_loaded = '$Rev: 92 $'


"}}}
" Menu configuration {{{
"
let s:menuloc = '&Plugin.&specky'
execute 'menu ' . s:menuloc . '.&Jump\ to\ code/spec :call <SID>SpecSwitcher()<CR>'
execute 'menu ' . s:menuloc . '.Run\ &spec :call <SID>RunSpec()<CR>'
execute 'menu ' . s:menuloc . '.&RDoc\ lookup :call <SID>RunRdoc()<CR>'
execute 'menu ' . s:menuloc . '.Rotate\ &quote\ style :call <SID>QuoteSwitcher()<CR>'
execute 'menu ' . s:menuloc . '.Make\ a\ &banner :call <SID>MakeBanner()<CR>'


" }}}
" SpecSwitcher() {{{
"
" When in ruby code or an rspec BDD file, try and search recursively through
" the filesystem (within the current working directory) to find the
" respectively matching file.  (code to spec, spec to code.)
"
" This operates under the assumption that you've used chdir() to put vim into
" the top level directory of your project.
"
function! <SID>SpecSwitcher()

	" If we aren't in a ruby or rspec file then we probably don't care
	" too much about this function.
	"
	if &ft != 'ruby' && &ft != 'rspec'
		call s:err( "Not currently in ruby or rspec mode." )
		return
	endif

	" Ensure that we can always search recursively for files to open.
	"
	let l:orig_path = &path
	set path=**

	" Get the current buffer name, and determine if it is a spec file.
	"
	" /tmp/something/whatever/rubycode.rb ---> rubycode.rb
	" A requisite of the specfiles is that they match to the class/code file,
	" this emulates the eigenclass stuff, but doesn't require the same
	" directory structures.
	"
	" rubycode.rb ---> rubycode_spec.rb
	" 
	let l:filename     = matchstr( bufname('%'), '[0-9A-Za-z_.-]*$' )
	let l:is_spec_file = match( l:filename, '_spec.rb$' ) == -1 ? 0 : 1

	if l:is_spec_file
		let l:other_file = substitute( l:filename, '_spec\.rb$', '\.rb', '' )
	else
		let l:other_file = substitute( l:filename, '\.rb$', '_spec\.rb', '' )
	endif

	let l:bufnum = bufnr( l:other_file )
	if l:bufnum == -1
		" The file isn't currently open, so let's search for it.
		execute 'find ' . l:other_file
	else
		" We've already got an open buffer with this file, just go to it.
		execute 'buffer' . l:bufnum
	endif

	" Restore the original path.
	"
	execute 'set path=' . l:orig_path
endfunction


" }}}
" QuoteSwitcher() {{{
"
" Wrap the word under the cursor in quotes.  If in ruby mode,
" cycle between quoting styles and symbols.
"
" variable -> "variable" -> 'variable' -> :variable
"
function! <SID>QuoteSwitcher()
	let l:type = strpart( expand("<cWORD>"), 0, 1 )
	let l:word = expand("<cword>")

	if l:type == '"'
		" Double quote to single
		"
		execute ":normal viWc'" . l:word . "'"

	elseif l:type == "'"
		if &ft == 'ruby' || &ft == 'rspec'
			" Single quote to symbol
			"
			execute ':normal viWc:' . l:word
		else
			" Single quote to double
			"
			execute ':normal viWc"' . l:word . '"'
		end

	else
		" Whatever to double quote
		"
		execute ':normal viWc"' . l:word . '"'
	endif

	" Move the cursor back into the cl:word
	"
	call cursor( 0, getpos('.')[2] - 1 )
endfunction


" }}}
" MakeBanner() {{{
"
" Create a quick banner from the current line's text.
"
function! <SID>MakeBanner()
	let l:banner_text = toupper(join( split( getline('.'), '\zs' ), ' ' ))
	let l:banner_text = substitute( l:banner_text, '^\s\+', '', '' )
	let l:sep = repeat( '#', &textwidth == 0 ? 72 : &textwidth )
	let l:line = line('.')

	call setline( l:line, l:sep )
 	call append( l:line, [ '### ' . l:banner_text, l:sep ] )
	execute 'normal 3=='
	call cursor( l:line + 3, 0 )
endfunction


" }}}
" RunSpec() {{{
"
" Run this function while in a spec file to run the specs within vim.
"
function! <SID>RunSpec()

	" If we're in the code instead of the spec, try and switch
	" before running tests.
	"
	let l:filename     = matchstr( bufname('%'), '[0-9A-Za-z_.-]*$' )
	let l:is_spec_file = match( l:filename, '_spec.rb$' ) == -1 ? 0 : 1
	if ( ! l:is_spec_file )
		silent call <SID>SpecSwitcher()
	endif

	let l:spec   = bufname('%')
	let l:buf    = 'specky:specrun'
	let l:bufnum = bufnr( l:buf )

	" Squash the old buffer, if it exists.
	"
	if buflisted( l:buf )
		execute 'bd! ' . l:buf
	endif

	execute <SID>NewWindowCmd() . l:buf
	setlocal buftype=nofile bufhidden=delete noswapfile filetype=specrun
	set foldtext='--'.getline(v:foldstart).v:folddashes

	" Set up some convenient keybindings.
	"
	nnoremap <silent> <buffer> q :close<CR>
	nnoremap <silent> <buffer> e :call <SID>FindSpecError(1)<CR>
	nnoremap <silent> <buffer> r :call <SID>FindSpecError(-1)<CR>
	nnoremap <silent> <buffer> E :call <SID>FindSpecError(0)<CR>
	nnoremap <silent> <buffer> <C-e> :let b:err_line=1<CR>

	" Default cmd for spec
	"
	if !exists( 'g:speckyRunSpecCmd' )
		let g:speckyRunSpecCmd = 'spec -fs'
	endif

	" Call spec and gather up the output
	"
	let l:cmd    =  g:speckyRunSpecCmd . ' ' . l:spec
	let l:output = system( l:cmd )
	call append( 0, split( l:output, "\n" ) )
	call append( 0, '' )
	call append( 0, 'Output of: ' . l:cmd  )
	normal gg

	" Lockdown the buffer
	"
	setlocal nomodifiable
endfunction


" }}}
" RunRdoc() {{{
"
" Get documentation for the word under the cursor.
"
function! <SID>RunRdoc()

	" If we aren't in a ruby file (specs are ruby-mode too) then we probably
	" don't care too much about this function.
	"
	if ( &ft != 'ruby' && &ft != 'rdoc' && &ft != 'rspec' )
		call s:err( "Not currently in a rubyish-mode." )
		return
	endif

	" Set defaults
	"
	if !exists( 'g:speckyRunRdocCmd' )
		let g:speckyRunRdocCmd = 'ri'
	endif

	let l:buf     = 'specky:rdoc'
	let l:bufname = bufname('%')

	if ( match( l:bufname, l:buf ) != -1 )
		" Already in the rdoc buffer.  This allows us to lookup
		" something like Kernel#require.
		"
		let l:word = expand('<cWORD>')
	else
		" Not in the rdoc buffer.  This allows us to lookup
		" something like 'each' in some_hash.each { ... }
		"
		let l:word = expand('<cword>')
	endif

	" Squash the old buffer, if it exists.
	"
	if buflisted( l:buf )
		execute 'bd! ' . l:buf
	endif

	" With multiple matches, strip the comams from the cWORD.
	"
	let l:word = substitute( l:word, ',', '', 'eg' )

	execute <SID>NewWindowCmd() . l:buf
	setlocal buftype=nofile bufhidden=delete noswapfile filetype=rdoc
	nnoremap <silent> <buffer> q :close<CR>

	" Call the documentation and gather up the output
	"
	let l:cmd    = g:speckyRunRdocCmd . ' ' . l:word
	let l:output = system( l:cmd )
	call append( 0, split( l:output, "\n" ) )
	execute 'normal gg'

	" Lockdown the buffer
	"
	execute 'setlocal nomodifiable'
endfunction


" }}}
" FindSpecError( detail ) {{{
"
" Convenience searches for jumping to spec failures.
"
function! <SID>FindSpecError( detail )

	let l:err_str = '(FAILED\|ERROR - \d\+)$'

	if ( a:detail == 0 )
		" Find the detailed failure text for the current failure line,
		" and unfold it.
		"
		let l:orig_so = &so
		set so=100
		call search('^' . matchstr(getline('.'),'\d\+)$') )
		if has('folding')
			silent! normal za
		endif
		execute 'set so=' . l:orig_so

	else
		" Find the 'regular' failure line
		"
		if exists( 'b:err_line' )
			call cursor( b:err_line, a:detail == -1 ? 1 : strlen(getline(b:err_line)) )
		endif
		call search( l:err_str, a:detail == -1 ? 'b' : '' )
		let b:err_line = line('.')
		nohl

	endif
endfunction

" }}}
" NewWindowCmd() {{{
"
" Return the stringified command for a new window, based on user preferences.
"
function! <SID>NewWindowCmd()
	if ( ! exists('g:speckyWindowType' ) )
		return 'tabnew '
	endif

	if ( g:speckyWindowType == 1 )
		return 'new '
	elseif ( g:speckyWindowType == 2 )
		return 'vert new '
	else
		return 'tabnew '
	endif
endfunction

" }}}
" s:err( msg ) "{{{
" Notify of problems in a consistent fashion.
"
function! s:err( msg )
	echohl WarningMsg|echomsg 'specky: ' . a:msg|echohl None
endfunction " }}}

snippets/rspec.snippets	[[[1
168
#
# specky: snippet file for rspec, to be used with the quite excellent
# 'snipmate' Vim plugin by Michael Sanders <msanders42+vim@gmail.com>.
# http://www.vim.org/scripts/script.php?script_id=2540
#
# $Id: rspec.snippets 70 2009-06-01 14:33:54Z mahlon $
#

snippet .and and_raise()
	.and_raise( ${1:exception}.new("${2:message}") )
snippet .and and_return { }
	.and_return { ${1} }
snippet .and and_return()
	.and_return( ${1:value} )
snippet .and and_throw()
	.and_throw( ${1:sym} )
snippet .and and_yield()
	.and_yield( ${1:values} )
snippet .at at_least()
	.at_least( ${1:n} ).times
snippet .at at_most()
	.at_most( ${1:n} ).times
snippet .on
	.once
snippet .tw
	.twice
snippet .any
	.any_number_of_times
snippet des Describe subject
	describe "${1:subject}" do
		${2}
	end
snippet des Describe Type
	describe ${1:Type} do
		${2}
	end
snippet des Describe Type, description
	describe ${1:Type}, "${2:description}" do
		${3}
	end
snippet des Describe a shared group
	describe "${1:A Shared Thing}", :shared => true do
		${2}
	end
snippet it it block
	it "${1:should do something}" do
		${2}
	end
snippet it it (pending)
	it "${1:does something}"${2}
snippet .ex
	.exactly( ${1:n} ).times
snippet .w
	.with( ${1:args} )${2}
snippet con
	context "${1:context}" do
		${2}
	end
snippet mock
	${1:var} = mock( "${2:mock_name}"${3:, :null_object => true} )
	${4}
snippet st
	stub!( :${1:expectation} ).with( ${2:args} ).and_return( ${3} )
snippet bef Before each test
	before( :each ) do
		${1}
	end
snippet bef Before all tests
	before( :all ) do
		${1}
	end
snippet aft After each test
	after( :each ) do
		${1}
	end
snippet aft After all tests
	after( :all ) do
		${1}
	end
snippet sh=
	${1:target}.should == ${2:value}
	${3}
snippet shn=
	${1:target}.should_not == ${2:value}
	${3}
snippet she
	${1:target}.should equal( ${2:value} )
	${3}
snippet shne
	${1:target}.should_not equal( ${2:value} )
	${3}
snippet shm Should contain
	${1:target}.should =~ /${2:regexp}/
	${3}
snippet shnm 
	${1:target}.should_not =~ /${2:regexp}/
	${3}
snippet shm Should match
	${1:target}.should match( /${2:regexp}/ )${3}
snippet shb
	${1:target}.should be( ${2:result} )
	${3}
snippet shnb
	${1:target}.should_not be( ${2:result} )
	${3}
snippet shbko
	${1:target}.should be_a_kind_of( ${2:klass} )
	${3}
snippet shnbko
	${1:target}.should_not be_a_kind_of( ${2:klass} )
	${3}
snippet shbio
	${1:target}.should be_instance_of( ${2:klass} )
	${3}
snippet shnbio
	${1:target}.should_not be_instance_of( ${2:klass} )
	${3}
snippet shbc
	${1:target}.should be_close( ${2:result}, ${3:tolerance} )
	${4}
snippet shnbc
	${1:target}.should_not be_close( ${2:result}, ${3:tolerance} )
	${4}
snippet shh
	${1:target}.should have( ${2:num} ).${3:things}
	${4}
snippet shhal
	${1:target}.should have_at_least( ${2:num} ).${3:things}
	${4}
snippet shham
	${1:target}.should have_at_most( ${2:num} ).${3:things}
	${4}
snippet shbp
	${1:target}.should ${2:be_${3:predicate}} ${4}
snippet shnbp
	${1:target}.should_not ${2:be_${3:predicate}} ${4}
snippet exre
	expect { ${1} }.to raise_error( ${2:ErrorClass}, /${3:message match}/i )
	${4}
snippet exnre
	expect { ${1} }.to_not raise_error( ${2:ErrorClass} )
	${3}
snippet shre
	lambda { ${1} }.should raise_error( ${2:ErrorClass}, /${3:message match}/i )
	${4}
snippet shnre
	lambda { ${1} }.should_not raise_error( ${2:ErrorClass} )
	${3}
snippet shr
	${1:mock}.should_receive( :${2:message} )${3}
snippet shnr
	${1:mock}.should_not_receive( :${2:message} )${3}
snippet shrt
	${1:target}.should respond_to( :${2:sym} )
snippet shnrt
	${1:target}.should_not respond_to( :${2:sym} )
snippet shbl
	it_should_behave_like "${1:shared behavior}"
	${2}
snippet sim
	def ${1:matcher_method}( expected )
		simple_matcher do |given, matcher|
			matcher.description = "${2:verb} with #{expected.inspect}"
			matcher.failure_message = "expected #{given.inspect} to $2 with #{expected.inspect}"
			matcher.negative_failure_message = "expected #{given.inspect} not to $2 with #{expected.inspect}"
			given.${3:...checks something and returns a boolean}
		end
	end
