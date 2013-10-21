if has("gui_running")
  " Start without the toolbar (T) and without right hand scrollbar (r)
  set guioptions=egmt

  " Fullscreen takes up entire screen
  set fuoptions=maxhorz,maxvert

  " Command-Shift-F for Ack
  map <D-F> :Ack<space>

  " Command-][ to increase/decrease indentation
  vmap <D-]> >gv
  vmap <D-[> <gv

  " Map Command-# to switch tabs
  map  <D-0> 0gt
  imap <D-0> <Esc>0gt
  map  <D-1> 1gt
  imap <D-1> <Esc>1gt
  map  <D-2> 2gt
  imap <D-2> <Esc>2gt
  map  <D-3> 3gt
  imap <D-3> <Esc>3gt
  map  <D-4> 4gt
  imap <D-4> <Esc>4gt
  map  <D-5> 5gt
  imap <D-5> <Esc>5gt
  map  <D-6> 6gt
  imap <D-6> <Esc>6gt
  map  <D-7> 7gt
  imap <D-7> <Esc>7gt
  map  <D-8> 8gt
  imap <D-8> <Esc>8gt
  map  <D-9> 9gt
  imap <D-9> <Esc>9gt
endif

if has("gui_macvim")
  fun! b:has_display(name)
    let _ = system("system_profiler SPDisplaysDataType | grep -i '" . a:name . "'")
    return v:shell_error == 0
  endfun

  let cinema_display = b:has_display("Cinema HD")
  let thunderbolt_display = b:has_display("Thunderbolt Display")

  if cinema_display || thunderbolt_display
    set guifont=Source\ Code\ Pro\ for\ Powerline:h15
  else
    set guifont=Source\ Code\ Pro\ for\ Powerline:h13
  endif
else
  set guifont=Source\ Code\ Pro\ for\ Powerline:h13
endif
