" Name:    PickAColor
" Author:  Israel Chauca F.
" Version: 1.1
" License: Released under the BSD license, read :h PickAColorLicense for the
"          actual license.
"          http://www.opensource.org/licenses/bsd-license.php
" Credits: Some of the code that make this script is modified or just
"          shamelessly copied from the following sources:
"
"          - Maximilian Nickel
"            http://www.vim.org/scripts/script.php?script_id=3014
"
"          - Petr Mejzlik
"            http://www.vim.org/scripts/script.php?script_id=2756
"
"          - Web color names:
"            http://www.w3.org/TR/css3-color/#svg-color
"
"          - X11 color names:
"            http://cvsweb.xfree86.org/cvsweb/xc/programs/rgb/rgb.txt?rev=1.2
"
"          - Algorithm to convert from RGB to HSL:
"            http://www.easyrgb.com/index.php?X=MATH&H=18#text18

" Initialization: {{{

" Vim 7.2 or later.
if v:version < 702
	echoerr "PickAColor: this plugin requires vim >= 7.2!"
	finish
endif

" Don't load script when already loaded:
if exists("g:loaded_pickacolor")
  "finish
endif
let g:loaded_pickacolor = 1

let s:quit = ""
let s:activate = ""
let s:mac = 0
let s:no_gui = 0

" Check if running on a mac.
if has("gui_macvim") && has('gui_running')
  let s:app = "MacVim"
  let s:mac = 1
elseif $TERM_PROGRAM == "Apple_Terminal"
  let s:app = "Terminal"
  let s:mac = 1
elseif $TERM_PROGRAM == "iTerm.app"
  let s:app = "iTerm"
  let s:mac = 1
elseif has('mac')
  let s:app = "System Events"
  let s:quit = "quit"
  let s:activate = 'activate'
  let s:mac = 1
endif

" If not on a mac, must have python.
if s:mac == 0 && !has('python')
    let s:no_gui = 1
endif

" If has python, we need the gtk module.
if system('python -c "import gtk"') != ''
  let s:no_gui = 1
endif


" The color to be loaded if not editing an existing color.
let s:default_color = exists('g:pickacolor_default_color') ? g:pickacolor_default_color : '#888888'

" Palette to store colors in the window (0=disabled, 1=enabled).
let s:display_palette = exists('g:pickacolor_display_palette') ? g:pickacolor_display_palette : 1

" Which set of color names to be used (0=X11, 1=Web).
let s:use_web_colors = exists('g:pickacolor_use_web_colors') ? g:pickacolor_use_web_colors : 1

" If enabled, the default color will be overriden by the color in
" the clipboard, if the clipboard contains a color specification. (note: in X11 the x11-selection is used)
" (0=disabled, 1=enabled).
let s:get_clipboard = exists('g:pickacolor_get_clipboard') ? g:pickacolor_get_clipboard : 1

" If enabled, the chosen color is copied to clipboard. (note: in X11 the x11-selection is used)
" (0=disabled, 1=enabled).
let s:set_clipboard = exists('g:pickacolor_set_clipboard') ? g:pickacolor_set_clipboard : 1

" If enabled, a color specified by a hexcode can be edited. (0=disabled, 1=enabled).
let s:edit_hex = exists('g:pickacolor_edit_hex') ? g:pickacolor_edit_hex : 1

" If enabled, a color specified by a name can be edited. (0=disabled, 1=enabled).
let s:edit_names = exists('g:pickacolor_edit_names') ? g:pickacolor_edit_names : 1
" }}}

function! RetrieveAColor(where, what) " {{{
  let result = []
  let s:save_cursor = getpos('.') " save cursor position
  let save_reg = @" " backup the unnamed register
  let @" = ''
  if a:where == 'text'
    if a:what == 'name'
      normal! e
      normal! b
      normal! yw
      let color_name = substitute(@", '^\s\|\s$','','g') " word under the cursor (for named colors)
      if index(g:color_names, color_name) != -1
        let s:replacing_name = 1
        let s:name_to_replace = color_name
        let result = split(g:color_names_values[color_name].rgb, ',')
      endif
    elseif a:what == 'hex'
      if '#' =~ '\k'
        normal! e
        normal! b
        normal! yw
      else
        normal! e
        normal! 2b
        normal! y2w
      endif
      "echom 'reg: ' . @"
      let color_hex = substitute(@", '.*\(#[0-9a-fA-F]\+\).*', '\1','')  " all the hexcode including '#'
      if color_hex =~ '^#[0-9a-fA-F]\+$'
        let result = HEX2RGB(color_hex)
      endif
      if result != []
        let s:replacing_hex = 1
      endif
    endif
  elseif a:where == 'clipboard'
    " word from green #45a5ee the clipboard (for named colors)
    let color_name = substitute(@*, '^\s\|\s$','','g')
    " string from the clipboard (all the hexcode including '#')
    let color_hex = substitute(@*, '.*\(#[0-9a-fA-F]\+\).*', '\1','')
    if index(g:color_names, color_name) != -1
      let result = split(g:color_names_values[color_name].rgb, ',')
    endif
    if result == []
      let result = HEX2RGB(color_hex)
    endif
  endif
  let @" = save_reg " restore the unnamed register
  call setpos('.', s:save_cursor) " restore cursor position.
  return result
endfunction "}}}

function! ReplaceAColor(color) " {{{
  let s:save_cursor = getpos('.') " save cursor position
  let save_reg = @" " backup the unnamed register
  "echom a:color
  if s:replacing_name
    normal! e
    normal! b
    normal! yw
    call setpos('.', s:save_cursor)
    "exec 'normal! eb"_cw' . substitute(@", '[a-zA-Z][a-zA-Z0-9]\+', a:color,'') . "\<Esc>"
    normal! e
    normal! b
    exec 'normal! "_cw' . substitute(@", '[a-zA-Z][a-zA-Z0-9]\+', a:color,'') . "\<Esc>"
  elseif s:replacing_hex
    if '#' =~ '\k'
      normal! e
      normal! b
      normal! yw
      call setpos('.', s:save_cursor)
      exec 'normal! eb"_cw' . substitute(@", '#[0-9a-fA-F]\+', a:color,'') . "\<Esc>"
      normal! e
      normal! b
      "exec 'normal! "_cw' . substitute(@", '#[0-9a-fA-F]\+', a:color,'') . "\<Esc>"
      normal! e
      normal! b
      exec '"_cw' . substitute(@", '#[0-9a-fA-F]\+', a:color,'') . "\<Esc>"
    else
      normal! e
      normal! 2b
      normal! y2w
      call setpos('.', s:save_cursor)
      "exec 'normal! e2b"_c2w' . substitute(@", '#[0-9a-fA-F]\+', a:color,'') . "\<Esc>"
      normal! e
      normal! 2b
      exec 'normal! "_c2w' . substitute(@", '#[0-9a-fA-F]\+', a:color,'') . "\<Esc>"
    endif
  endif
  call setpos('.', s:save_cursor)
  let @" = save_reg " restore the unnamed register
endfunction "}}}

function! PickAColor(default_color) " {{{
  if s:mac == 1 " {{{
    " This is the AppleScript magic:
    let s:ascrpt = ['-e "tell application \"' . s:app . '\""',
          \ '-e "' . s:activate . '"',
          \ "-e \"set AppleScript's text item delimiters to {\\\",\\\"}\"",
          \ '-e "set theColor to (choose color default color {' . str2nr(a:default_color[0])*256 . ", " . str2nr(a:default_color[1])*256 . ", " . str2nr(a:default_color[2])*256 . '}) as text"',
          \ '-e "' . s:quit . '"',
          \ '-e "end tell"',
          \ '-e "return theColor"']

    return split(system("osascript " . join(s:ascrpt, ' ') . " 2>/dev/null"), ',')
  endif " }}}

  let default_color = printf('#%02x%02x%02x', a:default_color[0], a:default_color[1], a:default_color[2])
  let rgb = []
python << endpython

import vim
import gtk, sys

# message strings
wnd_title_insert = "Insert a color"

csd = gtk.ColorSelectionDialog(wnd_title_insert)
cs = csd.colorsel

cs.set_current_color(gtk.gdk.color_parse(vim.eval("default_color")))

cs.set_current_alpha(65536)
cs.set_has_opacity_control(False)
cs.set_has_palette(int(vim.eval("s:display_palette")))

if csd.run()==gtk.RESPONSE_OK:
    c = cs.get_current_color()
    s = [str(int(c.red)),',',str(int(c.green)),',',str(int(c.blue))]
    thecolor = ''.join(s)
    vim.command(":let rgb = split('%s',',')" % thecolor)

csd.destroy()

endpython

  return rgb
endfunction "}}}

function! GetAColor(name, fmt) "{{{1
  let default_color = []
  if s:edit_names && default_color == []
    let default_color = RetrieveAColor('text', 'name')
  endif

  if s:edit_hex && default_color == []
    let default_color = RetrieveAColor('text', 'hex')
  endif

  if s:get_clipboard && default_color == []
    let default_color = RetrieveAColor('clipboard', '')
  endif

  if default_color == []
    if s:default_color =~ '#[0-9a-fA-F]\+'
      let default_color = HEX2RGB(s:default_color)
    elseif index(g:color_names, s:default_color) != -1
      let default_color = s:default_color
    else
      let default_color == [0,0,0]
    endif
  endif

  if a:name == ""
    " Perform the magic:
    let rgb = PickAColor(default_color)
    if rgb == []
      " Most likely, user pressed cancel:
      return ""
    endif
    if a:fmt == "raw"
      " Raw RGB
      return printf('%d,%d,%d', str2nr(rgb[0]), str2nr(rgb[1]), str2nr(rgb[2]))
    elseif a:fmt == "rgb"
      " Web RGB
      return printf('%d,%d,%d', str2nr(rgb[0])/256, str2nr(rgb[1])/256, str2nr(rgb[2])/256)
    elseif a:fmt == "hex"
      " Hexadecimal
      return printf('#%02x%02x%02x', str2nr(rgb[0])/256, str2nr(rgb[1])/256, str2nr(rgb[2])/256)
    elseif a:fmt == "hsl"
      " HSL
      return RGB2HSL(rgb)
    else
      echoerr "If you see this, the end of the world is around the corner!"
      return ""
    endif
  else
    let name = substitute(tolower(a:name), '\s\|_\|-', '', 'g')
    if index(g:color_names, name) != -1
      if a:fmt == "hsl"
        exec "let rgb = split(g:color_names_values." . name . ".rgb, ',')"
        return RGB2HSL([str2nr(rgb[0])*256, str2nr(rgb[1])*256, str2nr(rgb[2])*256])
      elseif a:fmt == "raw"
        exec "let rgb = split(g:color_names_values." . name . ".rgb, ',')"
      return printf('%d,%d,%d', str2nr(rgb[0])*256, str2nr(rgb[1])*256, str2nr(rgb[2])*256)
      else
        exec "return g:color_names_values." . name . "." . a:fmt
      endif
    else
      echoerr name . " is not a color name!"
      return ""
    endif
  endif
endfunction "}}}

function! RGB2HSL(rgb) "{{{1
  let rgb = a:rgb
  let rgb[0] = str2float(rgb[0])/(256.0*256)
  let rgb[1] = str2float(rgb[1])/(256.0*256)
  let rgb[2] = str2float(rgb[2])/(256.0*256)
  let max = 0.0
  let max = rgb[0] > max ? rgb[0] : max
  let max = rgb[1] > max ? rgb[1] : max
  let max = rgb[2] > max ? rgb[2] : max

  let min = 1.0
  let min = rgb[0] < min ? rgb[0] : min
  let min = rgb[1] < min ? rgb[1] : min
  let min = rgb[2] < min ? rgb[2] : min

  let delta = max - min

  let l = (max + min)/2.0

  if delta == 0
    let h = 0.0
    let s = 0.0
  else
    if l < 0.5
      let s = max/(max+min)
    else
      let s = max/(2-max-min)
    endif
    let deltaR = (((max-rgb[0])/6)+(delta/2))/delta
    let deltaG = (((max-rgb[1])/6)+(delta/2))/delta
    let deltaB = (((max-rgb[2])/6)+(delta/2))/delta
    if rgb[0] == max
      let h = deltaB - deltaG
    elseif rgb[1] == max
      let h = (1.0/3) + deltaR - deltaB
    elseif rgb[2] == max
      let h = (2.0/3) + deltaG - deltaR
    else
      echoerr 'You should never read this!'
    endif
    if h < 0
      let h += 1
    elseif h > 1
      let h -= 1
    endif
  endif
  return printf('%.0f,%.0f%%,%.0f%%', h*360, s*100, l*100)
endfunction "}}}

function! HEX2RGB(hex) " {{{1
  let hex = substitute(a:hex, '#\|\s','','')
  if hex =~ '[^0-9a-fA-F]'
    "echoerr "HEX2RGB: '" . a:hex . "' is not a hexadecimal value."
    return []
  endif
  let l = len(hex)
  if (l - (3*(l/3))) != 0 " l mod 3
    "echoerr "HEX2RGB: '" . a:hex . "' is a badly formatted string."
    return []
  endif
  let l = l/3
  let rgb = [strpart(hex,0,l),strpart(hex,l,l),strpart(hex,(2*l),l)]
  let i = 0
  for h in rgb
    let rgb[i] = '0x'.rgb[i] + 0
    let i += 1
  endfor
  return rgb
endfunction " }}}1

function! InsertAColor(name, fmt) "{{{1
  let s:replacing_name = 0
  let s:replacing_hex = 0
  let output = GetAColor(a:name, a:fmt)
  if output == '0'
    echoerr 'PickAColor: Something is wrong!'
    return ''
  elseif output == ''
    return ''
  endif
  if s:set_clipboard
    let @* = output
  endif
  if s:replacing_name || s:replacing_hex
    "echom 'Replacing' . s:replacing_name . s:replacing_hex
    call ReplaceAColor(output)
  else
    "echom 'Appending'
    exe "normal! a" . output . "\<Esc>"
  endif
  return ''
endfunction "}}}

function! PickAName(str, ...) "{{{1
  let result = []
  for name in g:color_names
    if match(name, substitute(tolower(a:str), '\s\|_\|-', '', 'g')) == 0
      let result = add(result, name)
    endif
  endfor
  for name in g:color_names
    if match(name, substitute(tolower(a:str), '\s\|_\|-', '', 'g')) > 0
      let result = add(result, name)
    endif
  endfor
  return result
endfunction "}}}

if s:use_web_colors
" Web named colors values: {{{
  let color_names_values = {
      \ "aliceblue":            {"hex": '#f0f8ff', "rgb": '240,248,255'},
      \ "antiquewhite":         {"hex": '#faebd7', "rgb": '250,235,215'},
      \ "aqua":                 {"hex": '#00ffff', "rgb": '0,255,255'  },
      \ "aquamarine":           {"hex": '#7fffd4', "rgb": '127,255,212'},
      \ "azure":                {"hex": '#f0ffff', "rgb": '240,255,255'},
      \ "beige":                {"hex": '#f5f5dc', "rgb": '245,245,220'},
      \ "bisque":               {"hex": '#ffe4c4', "rgb": '255,228,196'},
      \ "black":                {"hex": '#000000', "rgb": '0,0,0'      },
      \ "blanchedalmond":       {"hex": '#ffebcd', "rgb": '255,235,205'},
      \ "blue":                 {"hex": '#0000ff', "rgb": '0,0,255'    },
      \ "blueviolet":           {"hex": '#8a2be2', "rgb": '138,43,226' },
      \ "brown":                {"hex": '#a52a2a', "rgb": '165,42,42'  },
      \ "burlywood":            {"hex": '#deb887', "rgb": '222,184,135'},
      \ "cadetblue":            {"hex": '#5f9ea0', "rgb": '95,158,160' },
      \ "chartreuse":           {"hex": '#7fff00', "rgb": '127,255,0'  },
      \ "chocolate":            {"hex": '#d2691e', "rgb": '210,105,30' },
      \ "coral":                {"hex": '#ff7f50', "rgb": '255,127,80' },
      \ "cornflowerblue":       {"hex": '#6495ed', "rgb": '100,149,237'},
      \ "cornsilk":             {"hex": '#fff8dc', "rgb": '255,248,220'},
      \ "crimson":              {"hex": '#dc143c', "rgb": '220,20,60'  },
      \ "cyan":                 {"hex": '#00ffff', "rgb": '0,255,255'  },
      \ "darkblue":             {"hex": '#00008b', "rgb": '0,0,139'    },
      \ "darkcyan":             {"hex": '#008b8b', "rgb": '0,139,139'  },
      \ "darkgoldenrod":        {"hex": '#b8860b', "rgb": '184,134,11' },
      \ "darkgray":             {"hex": '#a9a9a9', "rgb": '169,169,169'},
      \ "darkgreen":            {"hex": '#006400', "rgb": '0,100,0'    },
      \ "darkgrey":             {"hex": '#a9a9a9', "rgb": '169,169,169'},
      \ "darkkhaki":            {"hex": '#bdb76b', "rgb": '189,183,107'},
      \ "darkmagenta":          {"hex": '#8b008b', "rgb": '139,0,139'  },
      \ "darkolivegreen":       {"hex": '#556b2f', "rgb": '85,107,47'  },
      \ "darkorange":           {"hex": '#ff8c00', "rgb": '255,140,0'  },
      \ "darkorchid":           {"hex": '#9932cc', "rgb": '153,50,204' },
      \ "darkred":              {"hex": '#8b0000', "rgb": '139,0,0'    },
      \ "darksalmon":           {"hex": '#e9967a', "rgb": '233,150,122'},
      \ "darkseagreen":         {"hex": '#8fbc8f', "rgb": '143,188,143'},
      \ "darkslateblue":        {"hex": '#483d8b', "rgb": '72,61,139'  },
      \ "darkslategray":        {"hex": '#2f4f4f', "rgb": '47,79,79'   },
      \ "darkslategrey":        {"hex": '#2f4f4f', "rgb": '47,79,79'   },
      \ "darkturquoise":        {"hex": '#00ced1', "rgb": '0,206,209'  },
      \ "darkviolet":           {"hex": '#9400d3', "rgb": '148,0,211'  },
      \ "deeppink":             {"hex": '#ff1493', "rgb": '255,20,147' },
      \ "deepskyblue":          {"hex": '#00bfff', "rgb": '0,191,255'  },
      \ "dimgray":              {"hex": '#696969', "rgb": '105,105,105'},
      \ "dimgrey":              {"hex": '#696969', "rgb": '105,105,105'},
      \ "dodgerblue":           {"hex": '#1e90ff', "rgb": '30,144,255' },
      \ "firebrick":            {"hex": '#b22222', "rgb": '178,34,34'  },
      \ "floralwhite":          {"hex": '#fffaf0', "rgb": '255,250,240'},
      \ "forestgreen":          {"hex": '#228b22', "rgb": '34,139,34'  },
      \ "fuchsia":              {"hex": '#ff00ff', "rgb": '255,0,255'  },
      \ "gainsboro":            {"hex": '#dcdcdc', "rgb": '220,220,220'},
      \ "ghostwhite":           {"hex": '#f8f8ff', "rgb": '248,248,255'},
      \ "gold":                 {"hex": '#ffd700', "rgb": '255,215,0'  },
      \ "goldenrod":            {"hex": '#daa520', "rgb": '218,165,32' },
      \ "gray":                 {"hex": '#808080', "rgb": '128,128,128'},
      \ "green":                {"hex": '#008000', "rgb": '0,128,0'    },
      \ "greenyellow":          {"hex": '#adff2f', "rgb": '173,255,47' },
      \ "grey":                 {"hex": '#808080', "rgb": '128,128,128'},
      \ "honeydew":             {"hex": '#f0fff0', "rgb": '240,255,240'},
      \ "hotpink":              {"hex": '#ff69b4', "rgb": '255,105,180'},
      \ "indianred":            {"hex": '#cd5c5c', "rgb": '205,92,92'  },
      \ "indigo":               {"hex": '#4b0082', "rgb": '75,0,130'   },
      \ "ivory":                {"hex": '#fffff0', "rgb": '255,255,240'},
      \ "khaki":                {"hex": '#f0e68c', "rgb": '240,230,140'},
      \ "lavender":             {"hex": '#e6e6fa', "rgb": '230,230,250'},
      \ "lavenderblush":        {"hex": '#fff0f5', "rgb": '255,240,245'},
      \ "lawngreen":            {"hex": '#7cfc00', "rgb": '124,252,0'  },
      \ "lemonchiffon":         {"hex": '#fffacd', "rgb": '255,250,205'},
      \ "lightblue":            {"hex": '#add8e6', "rgb": '173,216,230'},
      \ "lightcoral":           {"hex": '#f08080', "rgb": '240,128,128'},
      \ "lightcyan":            {"hex": '#e0ffff', "rgb": '224,255,255'},
      \ "lightgoldenrodyellow": {"hex": '#fafad2', "rgb": '250,250,210'},
      \ "lightgray":            {"hex": '#d3d3d3', "rgb": '211,211,211'},
      \ "lightgreen":           {"hex": '#90ee90', "rgb": '144,238,144'},
      \ "lightgrey":            {"hex": '#d3d3d3', "rgb": '211,211,211'},
      \ "lightpink":            {"hex": '#ffb6c1', "rgb": '255,182,193'},
      \ "lightsalmon":          {"hex": '#ffa07a', "rgb": '255,160,122'},
      \ "lightseagreen":        {"hex": '#20b2aa', "rgb": '32,178,170' },
      \ "lightskyblue":         {"hex": '#87cefa', "rgb": '135,206,250'},
      \ "lightslategray":       {"hex": '#778899', "rgb": '119,136,153'},
      \ "lightslategrey":       {"hex": '#778899', "rgb": '119,136,153'},
      \ "lightsteelblue":       {"hex": '#b0c4de', "rgb": '176,196,222'},
      \ "lightyellow":          {"hex": '#ffffe0', "rgb": '255,255,224'},
      \ "lime":                 {"hex": '#00ff00', "rgb": '0,255,0'    },
      \ "limegreen":            {"hex": '#32cd32', "rgb": '50,205,50'  },
      \ "linen":                {"hex": '#faf0e6', "rgb": '250,240,230'},
      \ "magenta":              {"hex": '#ff00ff', "rgb": '255,0,255'  },
      \ "maroon":               {"hex": '#800000', "rgb": '128,0,0'    },
      \ "mediumaquamarine":     {"hex": '#66cdaa', "rgb": '102,205,170'},
      \ "mediumblue":           {"hex": '#0000cd', "rgb": '0,0,205'    },
      \ "mediumorchid":         {"hex": '#ba55d3', "rgb": '186,85,211' },
      \ "mediumpurple":         {"hex": '#9370db', "rgb": '147,112,219'},
      \ "mediumseagreen":       {"hex": '#3cb371', "rgb": '60,179,113' },
      \ "mediumslateblue":      {"hex": '#7b68ee', "rgb": '123,104,238'},
      \ "mediumspringgreen":    {"hex": '#00fa9a', "rgb": '0,250,154'  },
      \ "mediumturquoise":      {"hex": '#48d1cc', "rgb": '72,209,204' },
      \ "mediumvioletred":      {"hex": '#c71585', "rgb": '199,21,133' },
      \ "midnightblue":         {"hex": '#191970', "rgb": '25,25,112'  },
      \ "mintcream":            {"hex": '#f5fffa', "rgb": '245,255,250'},
      \ "mistyrose":            {"hex": '#ffe4e1', "rgb": '255,228,225'},
      \ "moccasin":             {"hex": '#ffe4b5', "rgb": '255,228,181'},
      \ "navajowhite":          {"hex": '#ffdead', "rgb": '255,222,173'},
      \ "navy":                 {"hex": '#000080', "rgb": '0,0,128'    },
      \ "oldlace":              {"hex": '#fdf5e6', "rgb": '253,245,230'},
      \ "olive":                {"hex": '#808000', "rgb": '128,128,0'  },
      \ "olivedrab":            {"hex": '#6b8e23', "rgb": '107,142,35' },
      \ "orange":               {"hex": '#ffa500', "rgb": '255,165,0'  },
      \ "orangered":            {"hex": '#ff4500', "rgb": '255,69,0'   },
      \ "orchid":               {"hex": '#da70d6', "rgb": '218,112,214'},
      \ "palegoldenrod":        {"hex": '#eee8aa', "rgb": '238,232,170'},
      \ "palegreen":            {"hex": '#98fb98', "rgb": '152,251,152'},
      \ "paleturquoise":        {"hex": '#afeeee', "rgb": '175,238,238'},
      \ "palevioletred":        {"hex": '#db7093', "rgb": '219,112,147'},
      \ "papayawhip":           {"hex": '#ffefd5', "rgb": '255,239,213'},
      \ "peachpuff":            {"hex": '#ffdab9', "rgb": '255,218,185'},
      \ "peru":                 {"hex": '#cd853f', "rgb": '205,133,63' },
      \ "pink":                 {"hex": '#ffc0cb', "rgb": '255,192,203'},
      \ "plum":                 {"hex": '#dda0dd', "rgb": '221,160,221'},
      \ "powderblue":           {"hex": '#b0e0e6', "rgb": '176,224,230'},
      \ "purple":               {"hex": '#800080', "rgb": '128,0,128'  },
      \ "red":                  {"hex": '#ff0000', "rgb": '255,0,0'    },
      \ "rosybrown":            {"hex": '#bc8f8f', "rgb": '188,143,143'},
      \ "royalblue":            {"hex": '#4169e1', "rgb": '65,105,225' },
      \ "saddlebrown":          {"hex": '#8b4513', "rgb": '139,69,19'  },
      \ "salmon":               {"hex": '#fa8072', "rgb": '250,128,114'},
      \ "sandybrown":           {"hex": '#f4a460', "rgb": '244,164,96' },
      \ "seagreen":             {"hex": '#2e8b57', "rgb": '46,139,87'  },
      \ "seashell":             {"hex": '#fff5ee', "rgb": '255,245,238'},
      \ "sienna":               {"hex": '#a0522d', "rgb": '160,82,45'  },
      \ "silver":               {"hex": '#c0c0c0', "rgb": '192,192,192'},
      \ "skyblue":              {"hex": '#87ceeb', "rgb": '135,206,235'},
      \ "slateblue":            {"hex": '#6a5acd', "rgb": '106,90,205' },
      \ "slategray":            {"hex": '#708090', "rgb": '112,128,144'},
      \ "slategrey":            {"hex": '#708090', "rgb": '112,128,144'},
      \ "snow":                 {"hex": '#fffafa', "rgb": '255,250,250'},
      \ "springgreen":          {"hex": '#00ff7f', "rgb": '0,255,127'  },
      \ "steelblue":            {"hex": '#4682b4', "rgb": '70,130,180' },
      \ "tan":                  {"hex": '#d2b48c', "rgb": '210,180,140'},
      \ "teal":                 {"hex": '#008080', "rgb": '0,128,128'  },
      \ "thistle":              {"hex": '#d8bfd8', "rgb": '216,191,216'},
      \ "tomato":               {"hex": '#ff6347', "rgb": '255,99,71'  },
      \ "turquoise":            {"hex": '#40e0d0', "rgb": '64,224,208' },
      \ "violet":               {"hex": '#ee82ee', "rgb": '238,130,238'},
      \ "wheat":                {"hex": '#f5deb3', "rgb": '245,222,179'},
      \ "white":                {"hex": '#ffffff', "rgb": '255,255,255'},
      \ "whitesmoke":           {"hex": '#f5f5f5', "rgb": '245,245,245'},
      \ "yellow":               {"hex": '#ffff00', "rgb": '255,255,0'  },
      \ "yellowgreen":          {"hex": '#9acd32', "rgb": '154,205,50' },
      \ } "}}}
else
  " X11 named colors values: {{{
  let color_names_values = {
      \ "snow": {"hex": '#FFFAFA', "rgb": '255,250,250'},
      \ "GhostWhite": {"hex": '#F8F8FF', "rgb": '248,248,255'},
      \ "WhiteSmoke": {"hex": '#F5F5F5', "rgb": '245,245,245'},
      \ "gainsboro": {"hex": '#DCDCDC', "rgb": '220,220,220'},
      \ "FloralWhite": {"hex": '#FFFAF0', "rgb": '255,250,240'},
      \ "OldLace": {"hex": '#FDF5E6', "rgb": '253,245,230'},
      \ "linen": {"hex": '#FAF0E6', "rgb": '250,240,230'},
      \ "AntiqueWhite": {"hex": '#FAEBD7', "rgb": '250,235,215'},
      \ "PapayaWhip": {"hex": '#FFEFD5', "rgb": '255,239,213'},
      \ "BlanchedAlmond": {"hex": '#FFEBCD', "rgb": '255,235,205'},
      \ "bisque": {"hex": '#FFE4C4', "rgb": '255,228,196'},
      \ "PeachPuff": {"hex": '#FFDAB9', "rgb": '255,218,185'},
      \ "NavajoWhite": {"hex": '#FFDEAD', "rgb": '255,222,173'},
      \ "moccasin": {"hex": '#FFE4B5', "rgb": '255,228,181'},
      \ "cornsilk": {"hex": '#FFF8DC', "rgb": '255,248,220'},
      \ "ivory": {"hex": '#FFFFF0', "rgb": '255,255,240'},
      \ "LemonChiffon": {"hex": '#FFFACD', "rgb": '255,250,205'},
      \ "seashell": {"hex": '#FFF5EE', "rgb": '255,245,238'},
      \ "honeydew": {"hex": '#F0FFF0', "rgb": '240,255,240'},
      \ "MintCream": {"hex": '#F5FFFA', "rgb": '245,255,250'},
      \ "azure": {"hex": '#F0FFFF', "rgb": '240,255,255'},
      \ "AliceBlue": {"hex": '#F0F8FF', "rgb": '240,248,255'},
      \ "lavender": {"hex": '#E6E6FA', "rgb": '230,230,250'},
      \ "LavenderBlush": {"hex": '#FFF0F5', "rgb": '255,240,245'},
      \ "MistyRose": {"hex": '#FFE4E1', "rgb": '255,228,225'},
      \ "white": {"hex": '#FFFFFF', "rgb": '255,255,255'},
      \ "black": {"hex": '#000', "rgb": '0,0,0'},
      \ "DarkSlateGray": {"hex": '#2F4F4F', "rgb": '47,79,79'},
      \ "DarkSlateGrey": {"hex": '#2F4F4F', "rgb": '47,79,79'},
      \ "DimGray": {"hex": '#696969', "rgb": '105,105,105'},
      \ "DimGrey": {"hex": '#696969', "rgb": '105,105,105'},
      \ "SlateGray": {"hex": '#708090', "rgb": '112,128,144'},
      \ "SlateGrey": {"hex": '#708090', "rgb": '112,128,144'},
      \ "LightSlateGray": {"hex": '#778899', "rgb": '119,136,153'},
      \ "LightSlateGrey": {"hex": '#778899', "rgb": '119,136,153'},
      \ "gray": {"hex": '#BEBEBE', "rgb": '190,190,190'},
      \ "grey": {"hex": '#BEBEBE', "rgb": '190,190,190'},
      \ "LightGrey": {"hex": '#D3D3D3', "rgb": '211,211,211'},
      \ "LightGray": {"hex": '#D3D3D3', "rgb": '211,211,211'},
      \ "MidnightBlue": {"hex": '#191970', "rgb": '25,25,112'},
      \ "navy": {"hex": '#0080', "rgb": '0,0,128'},
      \ "NavyBlue": {"hex": '#0080', "rgb": '0,0,128'},
      \ "CornflowerBlue": {"hex": '#6495ED', "rgb": '100,149,237'},
      \ "DarkSlateBlue": {"hex": '#483D8B', "rgb": '72,61,139'},
      \ "SlateBlue": {"hex": '#6A5ACD', "rgb": '106,90,205'},
      \ "MediumSlateBlue": {"hex": '#7B68EE', "rgb": '123,104,238'},
      \ "LightSlateBlue": {"hex": '#8470FF', "rgb": '132,112,255'},
      \ "MediumBlue": {"hex": '#00CD', "rgb": '0,0,205'},
      \ "RoyalBlue": {"hex": '#4169E1', "rgb": '65,105,225'},
      \ "blue": {"hex": '#00FF', "rgb": '0,0,255'},
      \ "DodgerBlue": {"hex": '#1E90FF', "rgb": '30,144,255'},
      \ "DeepSkyBlue": {"hex": '#0BFFF', "rgb": '0,191,255'},
      \ "SkyBlue": {"hex": '#87CEEB', "rgb": '135,206,235'},
      \ "LightSkyBlue": {"hex": '#87CEFA', "rgb": '135,206,250'},
      \ "SteelBlue": {"hex": '#4682B4', "rgb": '70,130,180'},
      \ "LightSteelBlue": {"hex": '#B0C4DE', "rgb": '176,196,222'},
      \ "LightBlue": {"hex": '#ADD8E6', "rgb": '173,216,230'},
      \ "PowderBlue": {"hex": '#B0E0E6', "rgb": '176,224,230'},
      \ "PaleTurquoise": {"hex": '#AFEEEE', "rgb": '175,238,238'},
      \ "DarkTurquoise": {"hex": '#0CED1', "rgb": '0,206,209'},
      \ "MediumTurquoise": {"hex": '#48D1CC', "rgb": '72,209,204'},
      \ "turquoise": {"hex": '#40E0D0', "rgb": '64,224,208'},
      \ "cyan": {"hex": '#0FFFF', "rgb": '0,255,255'},
      \ "LightCyan": {"hex": '#E0FFFF', "rgb": '224,255,255'},
      \ "CadetBlue": {"hex": '#5F9EA0', "rgb": '95,158,160'},
      \ "MediumAquamarine": {"hex": '#66CDAA', "rgb": '102,205,170'},
      \ "aquamarine": {"hex": '#7FFFD4', "rgb": '127,255,212'},
      \ "DarkGreen": {"hex": '#0640', "rgb": '0,100,0'},
      \ "DarkOliveGreen": {"hex": '#556B2F', "rgb": '85,107,47'},
      \ "DarkSeaGreen": {"hex": '#8FBC8F', "rgb": '143,188,143'},
      \ "SeaGreen": {"hex": '#2E8B57', "rgb": '46,139,87'},
      \ "MediumSeaGreen": {"hex": '#3CB371', "rgb": '60,179,113'},
      \ "LightSeaGreen": {"hex": '#20B2AA', "rgb": '32,178,170'},
      \ "PaleGreen": {"hex": '#98FB98', "rgb": '152,251,152'},
      \ "SpringGreen": {"hex": '#0FF7F', "rgb": '0,255,127'},
      \ "LawnGreen": {"hex": '#7CFC0', "rgb": '124,252,0'},
      \ "green": {"hex": '#0FF0', "rgb": '0,255,0'},
      \ "chartreuse": {"hex": '#7FFF0', "rgb": '127,255,0'},
      \ "MediumSpringGreen": {"hex": '#0FA9A', "rgb": '0,250,154'},
      \ "GreenYellow": {"hex": '#ADFF2F', "rgb": '173,255,47'},
      \ "LimeGreen": {"hex": '#32CD32', "rgb": '50,205,50'},
      \ "YellowGreen": {"hex": '#9ACD32', "rgb": '154,205,50'},
      \ "ForestGreen": {"hex": '#228B22', "rgb": '34,139,34'},
      \ "OliveDrab": {"hex": '#6B8E23', "rgb": '107,142,35'},
      \ "DarkKhaki": {"hex": '#BDB76B', "rgb": '189,183,107'},
      \ "khaki": {"hex": '#F0E68C', "rgb": '240,230,140'},
      \ "PaleGoldenrod": {"hex": '#EEE8AA', "rgb": '238,232,170'},
      \ "LightGoldenrodYellow": {"hex": '#FAFAD2', "rgb": '250,250,210'},
      \ "LightYellow": {"hex": '#FFFFE0', "rgb": '255,255,224'},
      \ "yellow": {"hex": '#FFFF0', "rgb": '255,255,0'},
      \ "gold": {"hex": '#FFD70', "rgb": '255,215,0'},
      \ "LightGoldenrod": {"hex": '#EEDD82', "rgb": '238,221,130'},
      \ "goldenrod": {"hex": '#DAA520', "rgb": '218,165,32'},
      \ "DarkGoldenrod": {"hex": '#B886B', "rgb": '184,134,11'},
      \ "RosyBrown": {"hex": '#BC8F8F', "rgb": '188,143,143'},
      \ "IndianRed": {"hex": '#CD5C5C', "rgb": '205,92,92'},
      \ "SaddleBrown": {"hex": '#8B4513', "rgb": '139,69,19'},
      \ "sienna": {"hex": '#A0522D', "rgb": '160,82,45'},
      \ "peru": {"hex": '#CD853F', "rgb": '205,133,63'},
      \ "burlywood": {"hex": '#DEB887', "rgb": '222,184,135'},
      \ "beige": {"hex": '#F5F5DC', "rgb": '245,245,220'},
      \ "wheat": {"hex": '#F5DEB3', "rgb": '245,222,179'},
      \ "SandyBrown": {"hex": '#F4A460', "rgb": '244,164,96'},
      \ "tan": {"hex": '#D2B48C', "rgb": '210,180,140'},
      \ "chocolate": {"hex": '#D2691E', "rgb": '210,105,30'},
      \ "firebrick": {"hex": '#B22222', "rgb": '178,34,34'},
      \ "brown": {"hex": '#A52A2A', "rgb": '165,42,42'},
      \ "DarkSalmon": {"hex": '#E9967A', "rgb": '233,150,122'},
      \ "salmon": {"hex": '#FA8072', "rgb": '250,128,114'},
      \ "LightSalmon": {"hex": '#FFA07A', "rgb": '255,160,122'},
      \ "orange": {"hex": '#FFA50', "rgb": '255,165,0'},
      \ "DarkOrange": {"hex": '#FF8C0', "rgb": '255,140,0'},
      \ "coral": {"hex": '#FF7F50', "rgb": '255,127,80'},
      \ "LightCoral": {"hex": '#F08080', "rgb": '240,128,128'},
      \ "tomato": {"hex": '#FF6347', "rgb": '255,99,71'},
      \ "OrangeRed": {"hex": '#FF450', "rgb": '255,69,0'},
      \ "red": {"hex": '#FF00', "rgb": '255,0,0'},
      \ "HotPink": {"hex": '#FF69B4', "rgb": '255,105,180'},
      \ "DeepPink": {"hex": '#FF1493', "rgb": '255,20,147'},
      \ "pink": {"hex": '#FFC0CB', "rgb": '255,192,203'},
      \ "LightPink": {"hex": '#FFB6C1', "rgb": '255,182,193'},
      \ "PaleVioletRed": {"hex": '#DB7093', "rgb": '219,112,147'},
      \ "maroon": {"hex": '#B03060', "rgb": '176,48,96'},
      \ "MediumVioletRed": {"hex": '#C71585', "rgb": '199,21,133'},
      \ "VioletRed": {"hex": '#D02090', "rgb": '208,32,144'},
      \ "magenta": {"hex": '#FF0FF', "rgb": '255,0,255'},
      \ "violet": {"hex": '#EE82EE', "rgb": '238,130,238'},
      \ "plum": {"hex": '#DDA0DD', "rgb": '221,160,221'},
      \ "orchid": {"hex": '#DA70D6', "rgb": '218,112,214'},
      \ "MediumOrchid": {"hex": '#BA55D3', "rgb": '186,85,211'},
      \ "DarkOrchid": {"hex": '#9932CC', "rgb": '153,50,204'},
      \ "DarkViolet": {"hex": '#940D3', "rgb": '148,0,211'},
      \ "BlueViolet": {"hex": '#8A2BE2', "rgb": '138,43,226'},
      \ "purple": {"hex": '#A020F0', "rgb": '160,32,240'},
      \ "MediumPurple": {"hex": '#9370DB', "rgb": '147,112,219'},
      \ "thistle": {"hex": '#D8BFD8', "rgb": '216,191,216'},
      \ "snow1": {"hex": '#FFFAFA', "rgb": '255,250,250'},
      \ "snow2": {"hex": '#EEE9E9', "rgb": '238,233,233'},
      \ "snow3": {"hex": '#CDC9C9', "rgb": '205,201,201'},
      \ "snow4": {"hex": '#8B8989', "rgb": '139,137,137'},
      \ "seashell1": {"hex": '#FFF5EE', "rgb": '255,245,238'},
      \ "seashell2": {"hex": '#EEE5DE', "rgb": '238,229,222'},
      \ "seashell3": {"hex": '#CDC5BF', "rgb": '205,197,191'},
      \ "seashell4": {"hex": '#8B8682', "rgb": '139,134,130'},
      \ "AntiqueWhite1": {"hex": '#FFEFDB', "rgb": '255,239,219'},
      \ "AntiqueWhite2": {"hex": '#EEDFCC', "rgb": '238,223,204'},
      \ "AntiqueWhite3": {"hex": '#CDC0B0', "rgb": '205,192,176'},
      \ "AntiqueWhite4": {"hex": '#8B8378', "rgb": '139,131,120'},
      \ "bisque1": {"hex": '#FFE4C4', "rgb": '255,228,196'},
      \ "bisque2": {"hex": '#EED5B7', "rgb": '238,213,183'},
      \ "bisque3": {"hex": '#CDB79E', "rgb": '205,183,158'},
      \ "bisque4": {"hex": '#8B7D6B', "rgb": '139,125,107'},
      \ "PeachPuff1": {"hex": '#FFDAB9', "rgb": '255,218,185'},
      \ "PeachPuff2": {"hex": '#EECBAD', "rgb": '238,203,173'},
      \ "PeachPuff3": {"hex": '#CDAF95', "rgb": '205,175,149'},
      \ "PeachPuff4": {"hex": '#8B7765', "rgb": '139,119,101'},
      \ "NavajoWhite1": {"hex": '#FFDEAD', "rgb": '255,222,173'},
      \ "NavajoWhite2": {"hex": '#EECFA1', "rgb": '238,207,161'},
      \ "NavajoWhite3": {"hex": '#CDB38B', "rgb": '205,179,139'},
      \ "NavajoWhite4": {"hex": '#8B795E', "rgb": '139,121,94'},
      \ "LemonChiffon1": {"hex": '#FFFACD', "rgb": '255,250,205'},
      \ "LemonChiffon2": {"hex": '#EEE9BF', "rgb": '238,233,191'},
      \ "LemonChiffon3": {"hex": '#CDC9A5', "rgb": '205,201,165'},
      \ "LemonChiffon4": {"hex": '#8B8970', "rgb": '139,137,112'},
      \ "cornsilk1": {"hex": '#FFF8DC', "rgb": '255,248,220'},
      \ "cornsilk2": {"hex": '#EEE8CD', "rgb": '238,232,205'},
      \ "cornsilk3": {"hex": '#CDC8B1', "rgb": '205,200,177'},
      \ "cornsilk4": {"hex": '#8B8878', "rgb": '139,136,120'},
      \ "ivory1": {"hex": '#FFFFF0', "rgb": '255,255,240'},
      \ "ivory2": {"hex": '#EEEEE0', "rgb": '238,238,224'},
      \ "ivory3": {"hex": '#CDCDC1', "rgb": '205,205,193'},
      \ "ivory4": {"hex": '#8B8B83', "rgb": '139,139,131'},
      \ "honeydew1": {"hex": '#F0FFF0', "rgb": '240,255,240'},
      \ "honeydew2": {"hex": '#E0EEE0', "rgb": '224,238,224'},
      \ "honeydew3": {"hex": '#C1CDC1', "rgb": '193,205,193'},
      \ "honeydew4": {"hex": '#838B83', "rgb": '131,139,131'},
      \ "LavenderBlush1": {"hex": '#FFF0F5', "rgb": '255,240,245'},
      \ "LavenderBlush2": {"hex": '#EEE0E5', "rgb": '238,224,229'},
      \ "LavenderBlush3": {"hex": '#CDC1C5', "rgb": '205,193,197'},
      \ "LavenderBlush4": {"hex": '#8B8386', "rgb": '139,131,134'},
      \ "MistyRose1": {"hex": '#FFE4E1', "rgb": '255,228,225'},
      \ "MistyRose2": {"hex": '#EED5D2', "rgb": '238,213,210'},
      \ "MistyRose3": {"hex": '#CDB7B5', "rgb": '205,183,181'},
      \ "MistyRose4": {"hex": '#8B7D7B', "rgb": '139,125,123'},
      \ "azure1": {"hex": '#F0FFFF', "rgb": '240,255,255'},
      \ "azure2": {"hex": '#E0EEEE', "rgb": '224,238,238'},
      \ "azure3": {"hex": '#C1CDCD', "rgb": '193,205,205'},
      \ "azure4": {"hex": '#838B8B', "rgb": '131,139,139'},
      \ "SlateBlue1": {"hex": '#836FFF', "rgb": '131,111,255'},
      \ "SlateBlue2": {"hex": '#7A67EE', "rgb": '122,103,238'},
      \ "SlateBlue3": {"hex": '#6959CD', "rgb": '105,89,205'},
      \ "SlateBlue4": {"hex": '#473C8B', "rgb": '71,60,139'},
      \ "RoyalBlue1": {"hex": '#4876FF', "rgb": '72,118,255'},
      \ "RoyalBlue2": {"hex": '#436EEE', "rgb": '67,110,238'},
      \ "RoyalBlue3": {"hex": '#3A5FCD', "rgb": '58,95,205'},
      \ "RoyalBlue4": {"hex": '#27408B', "rgb": '39,64,139'},
      \ "blue1": {"hex": '#00FF', "rgb": '0,0,255'},
      \ "blue2": {"hex": '#00EE', "rgb": '0,0,238'},
      \ "blue3": {"hex": '#00CD', "rgb": '0,0,205'},
      \ "blue4": {"hex": '#008B', "rgb": '0,0,139'},
      \ "DodgerBlue1": {"hex": '#1E90FF', "rgb": '30,144,255'},
      \ "DodgerBlue2": {"hex": '#1C86EE', "rgb": '28,134,238'},
      \ "DodgerBlue3": {"hex": '#1874CD', "rgb": '24,116,205'},
      \ "DodgerBlue4": {"hex": '#104E8B', "rgb": '16,78,139'},
      \ "SteelBlue1": {"hex": '#63B8FF', "rgb": '99,184,255'},
      \ "SteelBlue2": {"hex": '#5CACEE', "rgb": '92,172,238'},
      \ "SteelBlue3": {"hex": '#4F94CD', "rgb": '79,148,205'},
      \ "SteelBlue4": {"hex": '#36648B', "rgb": '54,100,139'},
      \ "DeepSkyBlue1": {"hex": '#0BFFF', "rgb": '0,191,255'},
      \ "DeepSkyBlue2": {"hex": '#0B2EE', "rgb": '0,178,238'},
      \ "DeepSkyBlue3": {"hex": '#09ACD', "rgb": '0,154,205'},
      \ "DeepSkyBlue4": {"hex": '#0688B', "rgb": '0,104,139'},
      \ "SkyBlue1": {"hex": '#87CEFF', "rgb": '135,206,255'},
      \ "SkyBlue2": {"hex": '#7EC0EE', "rgb": '126,192,238'},
      \ "SkyBlue3": {"hex": '#6CA6CD', "rgb": '108,166,205'},
      \ "SkyBlue4": {"hex": '#4A708B', "rgb": '74,112,139'},
      \ "LightSkyBlue1": {"hex": '#B0E2FF', "rgb": '176,226,255'},
      \ "LightSkyBlue2": {"hex": '#A4D3EE', "rgb": '164,211,238'},
      \ "LightSkyBlue3": {"hex": '#8DB6CD', "rgb": '141,182,205'},
      \ "LightSkyBlue4": {"hex": '#607B8B', "rgb": '96,123,139'},
      \ "SlateGray1": {"hex": '#C6E2FF', "rgb": '198,226,255'},
      \ "SlateGray2": {"hex": '#B9D3EE', "rgb": '185,211,238'},
      \ "SlateGray3": {"hex": '#9FB6CD', "rgb": '159,182,205'},
      \ "SlateGray4": {"hex": '#6C7B8B', "rgb": '108,123,139'},
      \ "LightSteelBlue1": {"hex": '#CAE1FF', "rgb": '202,225,255'},
      \ "LightSteelBlue2": {"hex": '#BCD2EE', "rgb": '188,210,238'},
      \ "LightSteelBlue3": {"hex": '#A2B5CD', "rgb": '162,181,205'},
      \ "LightSteelBlue4": {"hex": '#6E7B8B', "rgb": '110,123,139'},
      \ "LightBlue1": {"hex": '#BFEFFF', "rgb": '191,239,255'},
      \ "LightBlue2": {"hex": '#B2DFEE', "rgb": '178,223,238'},
      \ "LightBlue3": {"hex": '#9AC0CD', "rgb": '154,192,205'},
      \ "LightBlue4": {"hex": '#68838B', "rgb": '104,131,139'},
      \ "LightCyan1": {"hex": '#E0FFFF', "rgb": '224,255,255'},
      \ "LightCyan2": {"hex": '#D1EEEE', "rgb": '209,238,238'},
      \ "LightCyan3": {"hex": '#B4CDCD', "rgb": '180,205,205'},
      \ "LightCyan4": {"hex": '#7A8B8B', "rgb": '122,139,139'},
      \ "PaleTurquoise1": {"hex": '#BBFFFF', "rgb": '187,255,255'},
      \ "PaleTurquoise2": {"hex": '#AEEEEE', "rgb": '174,238,238'},
      \ "PaleTurquoise3": {"hex": '#96CDCD', "rgb": '150,205,205'},
      \ "PaleTurquoise4": {"hex": '#668B8B', "rgb": '102,139,139'},
      \ "CadetBlue1": {"hex": '#98F5FF', "rgb": '152,245,255'},
      \ "CadetBlue2": {"hex": '#8EE5EE', "rgb": '142,229,238'},
      \ "CadetBlue3": {"hex": '#7AC5CD', "rgb": '122,197,205'},
      \ "CadetBlue4": {"hex": '#53868B', "rgb": '83,134,139'},
      \ "turquoise1": {"hex": '#0F5FF', "rgb": '0,245,255'},
      \ "turquoise2": {"hex": '#0E5EE', "rgb": '0,229,238'},
      \ "turquoise3": {"hex": '#0C5CD', "rgb": '0,197,205'},
      \ "turquoise4": {"hex": '#0868B', "rgb": '0,134,139'},
      \ "cyan1": {"hex": '#0FFFF', "rgb": '0,255,255'},
      \ "cyan2": {"hex": '#0EEEE', "rgb": '0,238,238'},
      \ "cyan3": {"hex": '#0CDCD', "rgb": '0,205,205'},
      \ "cyan4": {"hex": '#08B8B', "rgb": '0,139,139'},
      \ "DarkSlateGray1": {"hex": '#97FFFF', "rgb": '151,255,255'},
      \ "DarkSlateGray2": {"hex": '#8DEEEE', "rgb": '141,238,238'},
      \ "DarkSlateGray3": {"hex": '#79CDCD', "rgb": '121,205,205'},
      \ "DarkSlateGray4": {"hex": '#528B8B', "rgb": '82,139,139'},
      \ "aquamarine1": {"hex": '#7FFFD4', "rgb": '127,255,212'},
      \ "aquamarine2": {"hex": '#76EEC6', "rgb": '118,238,198'},
      \ "aquamarine3": {"hex": '#66CDAA', "rgb": '102,205,170'},
      \ "aquamarine4": {"hex": '#458B74', "rgb": '69,139,116'},
      \ "DarkSeaGreen1": {"hex": '#C1FFC1', "rgb": '193,255,193'},
      \ "DarkSeaGreen2": {"hex": '#B4EEB4', "rgb": '180,238,180'},
      \ "DarkSeaGreen3": {"hex": '#9BCD9B', "rgb": '155,205,155'},
      \ "DarkSeaGreen4": {"hex": '#698B69', "rgb": '105,139,105'},
      \ "SeaGreen1": {"hex": '#54FF9F', "rgb": '84,255,159'},
      \ "SeaGreen2": {"hex": '#4EEE94', "rgb": '78,238,148'},
      \ "SeaGreen3": {"hex": '#43CD80', "rgb": '67,205,128'},
      \ "SeaGreen4": {"hex": '#2E8B57', "rgb": '46,139,87'},
      \ "PaleGreen1": {"hex": '#9AFF9A', "rgb": '154,255,154'},
      \ "PaleGreen2": {"hex": '#90EE90', "rgb": '144,238,144'},
      \ "PaleGreen3": {"hex": '#7CCD7C', "rgb": '124,205,124'},
      \ "PaleGreen4": {"hex": '#548B54', "rgb": '84,139,84'},
      \ "SpringGreen1": {"hex": '#0FF7F', "rgb": '0,255,127'},
      \ "SpringGreen2": {"hex": '#0EE76', "rgb": '0,238,118'},
      \ "SpringGreen3": {"hex": '#0CD66', "rgb": '0,205,102'},
      \ "SpringGreen4": {"hex": '#08B45', "rgb": '0,139,69'},
      \ "green1": {"hex": '#0FF0', "rgb": '0,255,0'},
      \ "green2": {"hex": '#0EE0', "rgb": '0,238,0'},
      \ "green3": {"hex": '#0CD0', "rgb": '0,205,0'},
      \ "green4": {"hex": '#08B0', "rgb": '0,139,0'},
      \ "chartreuse1": {"hex": '#7FFF0', "rgb": '127,255,0'},
      \ "chartreuse2": {"hex": '#76EE0', "rgb": '118,238,0'},
      \ "chartreuse3": {"hex": '#66CD0', "rgb": '102,205,0'},
      \ "chartreuse4": {"hex": '#458B0', "rgb": '69,139,0'},
      \ "OliveDrab1": {"hex": '#C0FF3E', "rgb": '192,255,62'},
      \ "OliveDrab2": {"hex": '#B3EE3A', "rgb": '179,238,58'},
      \ "OliveDrab3": {"hex": '#9ACD32', "rgb": '154,205,50'},
      \ "OliveDrab4": {"hex": '#698B22', "rgb": '105,139,34'},
      \ "DarkOliveGreen1": {"hex": '#CAFF70', "rgb": '202,255,112'},
      \ "DarkOliveGreen2": {"hex": '#BCEE68', "rgb": '188,238,104'},
      \ "DarkOliveGreen3": {"hex": '#A2CD5A', "rgb": '162,205,90'},
      \ "DarkOliveGreen4": {"hex": '#6E8B3D', "rgb": '110,139,61'},
      \ "khaki1": {"hex": '#FFF68F', "rgb": '255,246,143'},
      \ "khaki2": {"hex": '#EEE685', "rgb": '238,230,133'},
      \ "khaki3": {"hex": '#CDC673', "rgb": '205,198,115'},
      \ "khaki4": {"hex": '#8B864E', "rgb": '139,134,78'},
      \ "LightGoldenrod1": {"hex": '#FFEC8B', "rgb": '255,236,139'},
      \ "LightGoldenrod2": {"hex": '#EEDC82', "rgb": '238,220,130'},
      \ "LightGoldenrod3": {"hex": '#CDBE70', "rgb": '205,190,112'},
      \ "LightGoldenrod4": {"hex": '#8B814C', "rgb": '139,129,76'},
      \ "LightYellow1": {"hex": '#FFFFE0', "rgb": '255,255,224'},
      \ "LightYellow2": {"hex": '#EEEED1', "rgb": '238,238,209'},
      \ "LightYellow3": {"hex": '#CDCDB4', "rgb": '205,205,180'},
      \ "LightYellow4": {"hex": '#8B8B7A', "rgb": '139,139,122'},
      \ "yellow1": {"hex": '#FFFF0', "rgb": '255,255,0'},
      \ "yellow2": {"hex": '#EEEE0', "rgb": '238,238,0'},
      \ "yellow3": {"hex": '#CDCD0', "rgb": '205,205,0'},
      \ "yellow4": {"hex": '#8B8B0', "rgb": '139,139,0'},
      \ "gold1": {"hex": '#FFD70', "rgb": '255,215,0'},
      \ "gold2": {"hex": '#EEC90', "rgb": '238,201,0'},
      \ "gold3": {"hex": '#CDAD0', "rgb": '205,173,0'},
      \ "gold4": {"hex": '#8B750', "rgb": '139,117,0'},
      \ "goldenrod1": {"hex": '#FFC125', "rgb": '255,193,37'},
      \ "goldenrod2": {"hex": '#EEB422', "rgb": '238,180,34'},
      \ "goldenrod3": {"hex": '#CD9B1D', "rgb": '205,155,29'},
      \ "goldenrod4": {"hex": '#8B6914', "rgb": '139,105,20'},
      \ "DarkGoldenrod1": {"hex": '#FFB9F', "rgb": '255,185,15'},
      \ "DarkGoldenrod2": {"hex": '#EEADE', "rgb": '238,173,14'},
      \ "DarkGoldenrod3": {"hex": '#CD95C', "rgb": '205,149,12'},
      \ "DarkGoldenrod4": {"hex": '#8B658', "rgb": '139,101,8'},
      \ "RosyBrown1": {"hex": '#FFC1C1', "rgb": '255,193,193'},
      \ "RosyBrown2": {"hex": '#EEB4B4', "rgb": '238,180,180'},
      \ "RosyBrown3": {"hex": '#CD9B9B', "rgb": '205,155,155'},
      \ "RosyBrown4": {"hex": '#8B6969', "rgb": '139,105,105'},
      \ "IndianRed1": {"hex": '#FF6A6A', "rgb": '255,106,106'},
      \ "IndianRed2": {"hex": '#EE6363', "rgb": '238,99,99'},
      \ "IndianRed3": {"hex": '#CD5555', "rgb": '205,85,85'},
      \ "IndianRed4": {"hex": '#8B3A3A', "rgb": '139,58,58'},
      \ "sienna1": {"hex": '#FF8247', "rgb": '255,130,71'},
      \ "sienna2": {"hex": '#EE7942', "rgb": '238,121,66'},
      \ "sienna3": {"hex": '#CD6839', "rgb": '205,104,57'},
      \ "sienna4": {"hex": '#8B4726', "rgb": '139,71,38'},
      \ "burlywood1": {"hex": '#FFD39B', "rgb": '255,211,155'},
      \ "burlywood2": {"hex": '#EEC591', "rgb": '238,197,145'},
      \ "burlywood3": {"hex": '#CDAA7D', "rgb": '205,170,125'},
      \ "burlywood4": {"hex": '#8B7355', "rgb": '139,115,85'},
      \ "wheat1": {"hex": '#FFE7BA', "rgb": '255,231,186'},
      \ "wheat2": {"hex": '#EED8AE', "rgb": '238,216,174'},
      \ "wheat3": {"hex": '#CDBA96', "rgb": '205,186,150'},
      \ "wheat4": {"hex": '#8B7E66', "rgb": '139,126,102'},
      \ "tan1": {"hex": '#FFA54F', "rgb": '255,165,79'},
      \ "tan2": {"hex": '#EE9A49', "rgb": '238,154,73'},
      \ "tan3": {"hex": '#CD853F', "rgb": '205,133,63'},
      \ "tan4": {"hex": '#8B5A2B', "rgb": '139,90,43'},
      \ "chocolate1": {"hex": '#FF7F24', "rgb": '255,127,36'},
      \ "chocolate2": {"hex": '#EE7621', "rgb": '238,118,33'},
      \ "chocolate3": {"hex": '#CD661D', "rgb": '205,102,29'},
      \ "chocolate4": {"hex": '#8B4513', "rgb": '139,69,19'},
      \ "firebrick1": {"hex": '#FF3030', "rgb": '255,48,48'},
      \ "firebrick2": {"hex": '#EE2C2C', "rgb": '238,44,44'},
      \ "firebrick3": {"hex": '#CD2626', "rgb": '205,38,38'},
      \ "firebrick4": {"hex": '#8B1A1A', "rgb": '139,26,26'},
      \ "brown1": {"hex": '#FF4040', "rgb": '255,64,64'},
      \ "brown2": {"hex": '#EE3B3B', "rgb": '238,59,59'},
      \ "brown3": {"hex": '#CD3333', "rgb": '205,51,51'},
      \ "brown4": {"hex": '#8B2323', "rgb": '139,35,35'},
      \ "salmon1": {"hex": '#FF8C69', "rgb": '255,140,105'},
      \ "salmon2": {"hex": '#EE8262', "rgb": '238,130,98'},
      \ "salmon3": {"hex": '#CD7054', "rgb": '205,112,84'},
      \ "salmon4": {"hex": '#8B4C39', "rgb": '139,76,57'},
      \ "LightSalmon1": {"hex": '#FFA07A', "rgb": '255,160,122'},
      \ "LightSalmon2": {"hex": '#EE9572', "rgb": '238,149,114'},
      \ "LightSalmon3": {"hex": '#CD8162', "rgb": '205,129,98'},
      \ "LightSalmon4": {"hex": '#8B5742', "rgb": '139,87,66'},
      \ "orange1": {"hex": '#FFA50', "rgb": '255,165,0'},
      \ "orange2": {"hex": '#EE9A0', "rgb": '238,154,0'},
      \ "orange3": {"hex": '#CD850', "rgb": '205,133,0'},
      \ "orange4": {"hex": '#8B5A0', "rgb": '139,90,0'},
      \ "DarkOrange1": {"hex": '#FF7F0', "rgb": '255,127,0'},
      \ "DarkOrange2": {"hex": '#EE760', "rgb": '238,118,0'},
      \ "DarkOrange3": {"hex": '#CD660', "rgb": '205,102,0'},
      \ "DarkOrange4": {"hex": '#8B450', "rgb": '139,69,0'},
      \ "coral1": {"hex": '#FF7256', "rgb": '255,114,86'},
      \ "coral2": {"hex": '#EE6A50', "rgb": '238,106,80'},
      \ "coral3": {"hex": '#CD5B45', "rgb": '205,91,69'},
      \ "coral4": {"hex": '#8B3E2F', "rgb": '139,62,47'},
      \ "tomato1": {"hex": '#FF6347', "rgb": '255,99,71'},
      \ "tomato2": {"hex": '#EE5C42', "rgb": '238,92,66'},
      \ "tomato3": {"hex": '#CD4F39', "rgb": '205,79,57'},
      \ "tomato4": {"hex": '#8B3626', "rgb": '139,54,38'},
      \ "OrangeRed1": {"hex": '#FF450', "rgb": '255,69,0'},
      \ "OrangeRed2": {"hex": '#EE400', "rgb": '238,64,0'},
      \ "OrangeRed3": {"hex": '#CD370', "rgb": '205,55,0'},
      \ "OrangeRed4": {"hex": '#8B250', "rgb": '139,37,0'},
      \ "red1": {"hex": '#FF00', "rgb": '255,0,0'},
      \ "red2": {"hex": '#EE00', "rgb": '238,0,0'},
      \ "red3": {"hex": '#CD00', "rgb": '205,0,0'},
      \ "red4": {"hex": '#8B00', "rgb": '139,0,0'},
      \ "DeepPink1": {"hex": '#FF1493', "rgb": '255,20,147'},
      \ "DeepPink2": {"hex": '#EE1289', "rgb": '238,18,137'},
      \ "DeepPink3": {"hex": '#CD1076', "rgb": '205,16,118'},
      \ "DeepPink4": {"hex": '#8BA50', "rgb": '139,10,80'},
      \ "HotPink1": {"hex": '#FF6EB4', "rgb": '255,110,180'},
      \ "HotPink2": {"hex": '#EE6AA7', "rgb": '238,106,167'},
      \ "HotPink3": {"hex": '#CD6090', "rgb": '205,96,144'},
      \ "HotPink4": {"hex": '#8B3A62', "rgb": '139,58,98'},
      \ "pink1": {"hex": '#FFB5C5', "rgb": '255,181,197'},
      \ "pink2": {"hex": '#EEA9B8', "rgb": '238,169,184'},
      \ "pink3": {"hex": '#CD919E', "rgb": '205,145,158'},
      \ "pink4": {"hex": '#8B636C', "rgb": '139,99,108'},
      \ "LightPink1": {"hex": '#FFAEB9', "rgb": '255,174,185'},
      \ "LightPink2": {"hex": '#EEA2AD', "rgb": '238,162,173'},
      \ "LightPink3": {"hex": '#CD8C95', "rgb": '205,140,149'},
      \ "LightPink4": {"hex": '#8B5F65', "rgb": '139,95,101'},
      \ "PaleVioletRed1": {"hex": '#FF82AB', "rgb": '255,130,171'},
      \ "PaleVioletRed2": {"hex": '#EE799F', "rgb": '238,121,159'},
      \ "PaleVioletRed3": {"hex": '#CD6889', "rgb": '205,104,137'},
      \ "PaleVioletRed4": {"hex": '#8B475D', "rgb": '139,71,93'},
      \ "maroon1": {"hex": '#FF34B3', "rgb": '255,52,179'},
      \ "maroon2": {"hex": '#EE30A7', "rgb": '238,48,167'},
      \ "maroon3": {"hex": '#CD2990', "rgb": '205,41,144'},
      \ "maroon4": {"hex": '#8B1C62', "rgb": '139,28,98'},
      \ "VioletRed1": {"hex": '#FF3E96', "rgb": '255,62,150'},
      \ "VioletRed2": {"hex": '#EE3A8C', "rgb": '238,58,140'},
      \ "VioletRed3": {"hex": '#CD3278', "rgb": '205,50,120'},
      \ "VioletRed4": {"hex": '#8B2252', "rgb": '139,34,82'},
      \ "magenta1": {"hex": '#FF0FF', "rgb": '255,0,255'},
      \ "magenta2": {"hex": '#EE0EE', "rgb": '238,0,238'},
      \ "magenta3": {"hex": '#CD0CD', "rgb": '205,0,205'},
      \ "magenta4": {"hex": '#8B08B', "rgb": '139,0,139'},
      \ "orchid1": {"hex": '#FF83FA', "rgb": '255,131,250'},
      \ "orchid2": {"hex": '#EE7AE9', "rgb": '238,122,233'},
      \ "orchid3": {"hex": '#CD69C9', "rgb": '205,105,201'},
      \ "orchid4": {"hex": '#8B4789', "rgb": '139,71,137'},
      \ "plum1": {"hex": '#FFBBFF', "rgb": '255,187,255'},
      \ "plum2": {"hex": '#EEAEEE', "rgb": '238,174,238'},
      \ "plum3": {"hex": '#CD96CD', "rgb": '205,150,205'},
      \ "plum4": {"hex": '#8B668B', "rgb": '139,102,139'},
      \ "MediumOrchid1": {"hex": '#E066FF', "rgb": '224,102,255'},
      \ "MediumOrchid2": {"hex": '#D15FEE', "rgb": '209,95,238'},
      \ "MediumOrchid3": {"hex": '#B452CD', "rgb": '180,82,205'},
      \ "MediumOrchid4": {"hex": '#7A378B', "rgb": '122,55,139'},
      \ "DarkOrchid1": {"hex": '#BF3EFF', "rgb": '191,62,255'},
      \ "DarkOrchid2": {"hex": '#B23AEE', "rgb": '178,58,238'},
      \ "DarkOrchid3": {"hex": '#9A32CD', "rgb": '154,50,205'},
      \ "DarkOrchid4": {"hex": '#68228B', "rgb": '104,34,139'},
      \ "purple1": {"hex": '#9B30FF', "rgb": '155,48,255'},
      \ "purple2": {"hex": '#912CEE', "rgb": '145,44,238'},
      \ "purple3": {"hex": '#7D26CD', "rgb": '125,38,205'},
      \ "purple4": {"hex": '#551A8B', "rgb": '85,26,139'},
      \ "MediumPurple1": {"hex": '#AB82FF', "rgb": '171,130,255'},
      \ "MediumPurple2": {"hex": '#9F79EE', "rgb": '159,121,238'},
      \ "MediumPurple3": {"hex": '#8968CD', "rgb": '137,104,205'},
      \ "MediumPurple4": {"hex": '#5D478B', "rgb": '93,71,139'},
      \ "thistle1": {"hex": '#FFE1FF', "rgb": '255,225,255'},
      \ "thistle2": {"hex": '#EED2EE', "rgb": '238,210,238'},
      \ "thistle3": {"hex": '#CDB5CD', "rgb": '205,181,205'},
      \ "thistle4": {"hex": '#8B7B8B', "rgb": '139,123,139'},
      \ "gray0": {"hex": '#000', "rgb": '0,0,0'},
      \ "grey0": {"hex": '#000', "rgb": '0,0,0'},
      \ "gray1": {"hex": '#333', "rgb": '3,3,3'},
      \ "grey1": {"hex": '#333', "rgb": '3,3,3'},
      \ "gray2": {"hex": '#555', "rgb": '5,5,5'},
      \ "grey2": {"hex": '#555', "rgb": '5,5,5'},
      \ "gray3": {"hex": '#888', "rgb": '8,8,8'},
      \ "grey3": {"hex": '#888', "rgb": '8,8,8'},
      \ "gray4": {"hex": '#AAA', "rgb": '10,10,10'},
      \ "grey4": {"hex": '#AAA', "rgb": '10,10,10'},
      \ "gray5": {"hex": '#DDD', "rgb": '13,13,13'},
      \ "grey5": {"hex": '#DDD', "rgb": '13,13,13'},
      \ "gray6": {"hex": '#FFF', "rgb": '15,15,15'},
      \ "grey6": {"hex": '#FFF', "rgb": '15,15,15'},
      \ "gray7": {"hex": '#121212', "rgb": '18,18,18'},
      \ "grey7": {"hex": '#121212', "rgb": '18,18,18'},
      \ "gray8": {"hex": '#141414', "rgb": '20,20,20'},
      \ "grey8": {"hex": '#141414', "rgb": '20,20,20'},
      \ "gray9": {"hex": '#171717', "rgb": '23,23,23'},
      \ "grey9": {"hex": '#171717', "rgb": '23,23,23'},
      \ "gray10": {"hex": '#1A1A1A', "rgb": '26,26,26'},
      \ "grey10": {"hex": '#1A1A1A', "rgb": '26,26,26'},
      \ "gray11": {"hex": '#1C1C1C', "rgb": '28,28,28'},
      \ "grey11": {"hex": '#1C1C1C', "rgb": '28,28,28'},
      \ "gray12": {"hex": '#1F1F1F', "rgb": '31,31,31'},
      \ "grey12": {"hex": '#1F1F1F', "rgb": '31,31,31'},
      \ "gray13": {"hex": '#212121', "rgb": '33,33,33'},
      \ "grey13": {"hex": '#212121', "rgb": '33,33,33'},
      \ "gray14": {"hex": '#242424', "rgb": '36,36,36'},
      \ "grey14": {"hex": '#242424', "rgb": '36,36,36'},
      \ "gray15": {"hex": '#262626', "rgb": '38,38,38'},
      \ "grey15": {"hex": '#262626', "rgb": '38,38,38'},
      \ "gray16": {"hex": '#292929', "rgb": '41,41,41'},
      \ "grey16": {"hex": '#292929', "rgb": '41,41,41'},
      \ "gray17": {"hex": '#2B2B2B', "rgb": '43,43,43'},
      \ "grey17": {"hex": '#2B2B2B', "rgb": '43,43,43'},
      \ "gray18": {"hex": '#2E2E2E', "rgb": '46,46,46'},
      \ "grey18": {"hex": '#2E2E2E', "rgb": '46,46,46'},
      \ "gray19": {"hex": '#303030', "rgb": '48,48,48'},
      \ "grey19": {"hex": '#303030', "rgb": '48,48,48'},
      \ "gray20": {"hex": '#333333', "rgb": '51,51,51'},
      \ "grey20": {"hex": '#333333', "rgb": '51,51,51'},
      \ "gray21": {"hex": '#363636', "rgb": '54,54,54'},
      \ "grey21": {"hex": '#363636', "rgb": '54,54,54'},
      \ "gray22": {"hex": '#383838', "rgb": '56,56,56'},
      \ "grey22": {"hex": '#383838', "rgb": '56,56,56'},
      \ "gray23": {"hex": '#3B3B3B', "rgb": '59,59,59'},
      \ "grey23": {"hex": '#3B3B3B', "rgb": '59,59,59'},
      \ "gray24": {"hex": '#3D3D3D', "rgb": '61,61,61'},
      \ "grey24": {"hex": '#3D3D3D', "rgb": '61,61,61'},
      \ "gray25": {"hex": '#404040', "rgb": '64,64,64'},
      \ "grey25": {"hex": '#404040', "rgb": '64,64,64'},
      \ "gray26": {"hex": '#424242', "rgb": '66,66,66'},
      \ "grey26": {"hex": '#424242', "rgb": '66,66,66'},
      \ "gray27": {"hex": '#454545', "rgb": '69,69,69'},
      \ "grey27": {"hex": '#454545', "rgb": '69,69,69'},
      \ "gray28": {"hex": '#474747', "rgb": '71,71,71'},
      \ "grey28": {"hex": '#474747', "rgb": '71,71,71'},
      \ "gray29": {"hex": '#4A4A4A', "rgb": '74,74,74'},
      \ "grey29": {"hex": '#4A4A4A', "rgb": '74,74,74'},
      \ "gray30": {"hex": '#4D4D4D', "rgb": '77,77,77'},
      \ "grey30": {"hex": '#4D4D4D', "rgb": '77,77,77'},
      \ "gray31": {"hex": '#4F4F4F', "rgb": '79,79,79'},
      \ "grey31": {"hex": '#4F4F4F', "rgb": '79,79,79'},
      \ "gray32": {"hex": '#525252', "rgb": '82,82,82'},
      \ "grey32": {"hex": '#525252', "rgb": '82,82,82'},
      \ "gray33": {"hex": '#545454', "rgb": '84,84,84'},
      \ "grey33": {"hex": '#545454', "rgb": '84,84,84'},
      \ "gray34": {"hex": '#575757', "rgb": '87,87,87'},
      \ "grey34": {"hex": '#575757', "rgb": '87,87,87'},
      \ "gray35": {"hex": '#595959', "rgb": '89,89,89'},
      \ "grey35": {"hex": '#595959', "rgb": '89,89,89'},
      \ "gray36": {"hex": '#5C5C5C', "rgb": '92,92,92'},
      \ "grey36": {"hex": '#5C5C5C', "rgb": '92,92,92'},
      \ "gray37": {"hex": '#5E5E5E', "rgb": '94,94,94'},
      \ "grey37": {"hex": '#5E5E5E', "rgb": '94,94,94'},
      \ "gray38": {"hex": '#616161', "rgb": '97,97,97'},
      \ "grey38": {"hex": '#616161', "rgb": '97,97,97'},
      \ "gray39": {"hex": '#636363', "rgb": '99,99,99'},
      \ "grey39": {"hex": '#636363', "rgb": '99,99,99'},
      \ "gray40": {"hex": '#666666', "rgb": '102,102,102'},
      \ "grey40": {"hex": '#666666', "rgb": '102,102,102'},
      \ "gray41": {"hex": '#696969', "rgb": '105,105,105'},
      \ "grey41": {"hex": '#696969', "rgb": '105,105,105'},
      \ "gray42": {"hex": '#6B6B6B', "rgb": '107,107,107'},
      \ "grey42": {"hex": '#6B6B6B', "rgb": '107,107,107'},
      \ "gray43": {"hex": '#6E6E6E', "rgb": '110,110,110'},
      \ "grey43": {"hex": '#6E6E6E', "rgb": '110,110,110'},
      \ "gray44": {"hex": '#707070', "rgb": '112,112,112'},
      \ "grey44": {"hex": '#707070', "rgb": '112,112,112'},
      \ "gray45": {"hex": '#737373', "rgb": '115,115,115'},
      \ "grey45": {"hex": '#737373', "rgb": '115,115,115'},
      \ "gray46": {"hex": '#757575', "rgb": '117,117,117'},
      \ "grey46": {"hex": '#757575', "rgb": '117,117,117'},
      \ "gray47": {"hex": '#787878', "rgb": '120,120,120'},
      \ "grey47": {"hex": '#787878', "rgb": '120,120,120'},
      \ "gray48": {"hex": '#7A7A7A', "rgb": '122,122,122'},
      \ "grey48": {"hex": '#7A7A7A', "rgb": '122,122,122'},
      \ "gray49": {"hex": '#7D7D7D', "rgb": '125,125,125'},
      \ "grey49": {"hex": '#7D7D7D', "rgb": '125,125,125'},
      \ "gray50": {"hex": '#7F7F7F', "rgb": '127,127,127'},
      \ "grey50": {"hex": '#7F7F7F', "rgb": '127,127,127'},
      \ "gray51": {"hex": '#828282', "rgb": '130,130,130'},
      \ "grey51": {"hex": '#828282', "rgb": '130,130,130'},
      \ "gray52": {"hex": '#858585', "rgb": '133,133,133'},
      \ "grey52": {"hex": '#858585', "rgb": '133,133,133'},
      \ "gray53": {"hex": '#878787', "rgb": '135,135,135'},
      \ "grey53": {"hex": '#878787', "rgb": '135,135,135'},
      \ "gray54": {"hex": '#8A8A8A', "rgb": '138,138,138'},
      \ "grey54": {"hex": '#8A8A8A', "rgb": '138,138,138'},
      \ "gray55": {"hex": '#8C8C8C', "rgb": '140,140,140'},
      \ "grey55": {"hex": '#8C8C8C', "rgb": '140,140,140'},
      \ "gray56": {"hex": '#8F8F8F', "rgb": '143,143,143'},
      \ "grey56": {"hex": '#8F8F8F', "rgb": '143,143,143'},
      \ "gray57": {"hex": '#919191', "rgb": '145,145,145'},
      \ "grey57": {"hex": '#919191', "rgb": '145,145,145'},
      \ "gray58": {"hex": '#949494', "rgb": '148,148,148'},
      \ "grey58": {"hex": '#949494', "rgb": '148,148,148'},
      \ "gray59": {"hex": '#969696', "rgb": '150,150,150'},
      \ "grey59": {"hex": '#969696', "rgb": '150,150,150'},
      \ "gray60": {"hex": '#999999', "rgb": '153,153,153'},
      \ "grey60": {"hex": '#999999', "rgb": '153,153,153'},
      \ "gray61": {"hex": '#9C9C9C', "rgb": '156,156,156'},
      \ "grey61": {"hex": '#9C9C9C', "rgb": '156,156,156'},
      \ "gray62": {"hex": '#9E9E9E', "rgb": '158,158,158'},
      \ "grey62": {"hex": '#9E9E9E', "rgb": '158,158,158'},
      \ "gray63": {"hex": '#A1A1A1', "rgb": '161,161,161'},
      \ "grey63": {"hex": '#A1A1A1', "rgb": '161,161,161'},
      \ "gray64": {"hex": '#A3A3A3', "rgb": '163,163,163'},
      \ "grey64": {"hex": '#A3A3A3', "rgb": '163,163,163'},
      \ "gray65": {"hex": '#A6A6A6', "rgb": '166,166,166'},
      \ "grey65": {"hex": '#A6A6A6', "rgb": '166,166,166'},
      \ "gray66": {"hex": '#A8A8A8', "rgb": '168,168,168'},
      \ "grey66": {"hex": '#A8A8A8', "rgb": '168,168,168'},
      \ "gray67": {"hex": '#ABABAB', "rgb": '171,171,171'},
      \ "grey67": {"hex": '#ABABAB', "rgb": '171,171,171'},
      \ "gray68": {"hex": '#ADADAD', "rgb": '173,173,173'},
      \ "grey68": {"hex": '#ADADAD', "rgb": '173,173,173'},
      \ "gray69": {"hex": '#B0B0B0', "rgb": '176,176,176'},
      \ "grey69": {"hex": '#B0B0B0', "rgb": '176,176,176'},
      \ "gray70": {"hex": '#B3B3B3', "rgb": '179,179,179'},
      \ "grey70": {"hex": '#B3B3B3', "rgb": '179,179,179'},
      \ "gray71": {"hex": '#B5B5B5', "rgb": '181,181,181'},
      \ "grey71": {"hex": '#B5B5B5', "rgb": '181,181,181'},
      \ "gray72": {"hex": '#B8B8B8', "rgb": '184,184,184'},
      \ "grey72": {"hex": '#B8B8B8', "rgb": '184,184,184'},
      \ "gray73": {"hex": '#BABABA', "rgb": '186,186,186'},
      \ "grey73": {"hex": '#BABABA', "rgb": '186,186,186'},
      \ "gray74": {"hex": '#BDBDBD', "rgb": '189,189,189'},
      \ "grey74": {"hex": '#BDBDBD', "rgb": '189,189,189'},
      \ "gray75": {"hex": '#BFBFBF', "rgb": '191,191,191'},
      \ "grey75": {"hex": '#BFBFBF', "rgb": '191,191,191'},
      \ "gray76": {"hex": '#C2C2C2', "rgb": '194,194,194'},
      \ "grey76": {"hex": '#C2C2C2', "rgb": '194,194,194'},
      \ "gray77": {"hex": '#C4C4C4', "rgb": '196,196,196'},
      \ "grey77": {"hex": '#C4C4C4', "rgb": '196,196,196'},
      \ "gray78": {"hex": '#C7C7C7', "rgb": '199,199,199'},
      \ "grey78": {"hex": '#C7C7C7', "rgb": '199,199,199'},
      \ "gray79": {"hex": '#C9C9C9', "rgb": '201,201,201'},
      \ "grey79": {"hex": '#C9C9C9', "rgb": '201,201,201'},
      \ "gray80": {"hex": '#CCCCCC', "rgb": '204,204,204'},
      \ "grey80": {"hex": '#CCCCCC', "rgb": '204,204,204'},
      \ "gray81": {"hex": '#CFCFCF', "rgb": '207,207,207'},
      \ "grey81": {"hex": '#CFCFCF', "rgb": '207,207,207'},
      \ "gray82": {"hex": '#D1D1D1', "rgb": '209,209,209'},
      \ "grey82": {"hex": '#D1D1D1', "rgb": '209,209,209'},
      \ "gray83": {"hex": '#D4D4D4', "rgb": '212,212,212'},
      \ "grey83": {"hex": '#D4D4D4', "rgb": '212,212,212'},
      \ "gray84": {"hex": '#D6D6D6', "rgb": '214,214,214'},
      \ "grey84": {"hex": '#D6D6D6', "rgb": '214,214,214'},
      \ "gray85": {"hex": '#D9D9D9', "rgb": '217,217,217'},
      \ "grey85": {"hex": '#D9D9D9', "rgb": '217,217,217'},
      \ "gray86": {"hex": '#DBDBDB', "rgb": '219,219,219'},
      \ "grey86": {"hex": '#DBDBDB', "rgb": '219,219,219'},
      \ "gray87": {"hex": '#DEDEDE', "rgb": '222,222,222'},
      \ "grey87": {"hex": '#DEDEDE', "rgb": '222,222,222'},
      \ "gray88": {"hex": '#E0E0E0', "rgb": '224,224,224'},
      \ "grey88": {"hex": '#E0E0E0', "rgb": '224,224,224'},
      \ "gray89": {"hex": '#E3E3E3', "rgb": '227,227,227'},
      \ "grey89": {"hex": '#E3E3E3', "rgb": '227,227,227'},
      \ "gray90": {"hex": '#E5E5E5', "rgb": '229,229,229'},
      \ "grey90": {"hex": '#E5E5E5', "rgb": '229,229,229'},
      \ "gray91": {"hex": '#E8E8E8', "rgb": '232,232,232'},
      \ "grey91": {"hex": '#E8E8E8', "rgb": '232,232,232'},
      \ "gray92": {"hex": '#EBEBEB', "rgb": '235,235,235'},
      \ "grey92": {"hex": '#EBEBEB', "rgb": '235,235,235'},
      \ "gray93": {"hex": '#EDEDED', "rgb": '237,237,237'},
      \ "grey93": {"hex": '#EDEDED', "rgb": '237,237,237'},
      \ "gray94": {"hex": '#F0F0F0', "rgb": '240,240,240'},
      \ "grey94": {"hex": '#F0F0F0', "rgb": '240,240,240'},
      \ "gray95": {"hex": '#F2F2F2', "rgb": '242,242,242'},
      \ "grey95": {"hex": '#F2F2F2', "rgb": '242,242,242'},
      \ "gray96": {"hex": '#F5F5F5', "rgb": '245,245,245'},
      \ "grey96": {"hex": '#F5F5F5', "rgb": '245,245,245'},
      \ "gray97": {"hex": '#F7F7F7', "rgb": '247,247,247'},
      \ "grey97": {"hex": '#F7F7F7', "rgb": '247,247,247'},
      \ "gray98": {"hex": '#FAFAFA', "rgb": '250,250,250'},
      \ "grey98": {"hex": '#FAFAFA', "rgb": '250,250,250'},
      \ "gray99": {"hex": '#FCFCFC', "rgb": '252,252,252'},
      \ "grey99": {"hex": '#FCFCFC', "rgb": '252,252,252'},
      \ "gray100": {"hex": '#FFFFFF', "rgb": '255,255,255'},
      \ "grey100": {"hex": '#FFFFFF', "rgb": '255,255,255'},
      \ "DarkGrey": {"hex": '#A9A9A9', "rgb": '169,169,169'},
      \ "DarkGray": {"hex": '#A9A9A9', "rgb": '169,169,169'},
      \ "DarkBlue": {"hex": '#008B', "rgb": '0,0,139'},
      \ "DarkCyan": {"hex": '#08B8B', "rgb": '0,139,139'},
      \ "DarkMagenta": {"hex": '#8B08B', "rgb": '139,0,139'},
      \ "DarkRed": {"hex": '#8B00', "rgb": '139,0,0'},
      \ "LightGreen": {"hex": '#90EE90', "rgb": '144,238,144'},
      \ } " }}}
endif

" List of color names:
let color_names = sort(keys(color_names_values))

if s:no_gui
  " Do not try to open color chooser dialog.
  command! -complete=customlist,PickAName -nargs=+ PickRGB :call InsertAColor('<args>', "rgb")
  command! -complete=customlist,PickAName -nargs=+ PickHEX :call InsertAColor('<args>', "hex")
  command! -complete=customlist,PickAName -nargs=+ PickHSL :call InsertAColor('<args>', "hsl")
  command! -complete=customlist,PickAName -nargs=+ PickRAW :call InsertAColor('<args>', "raw")
else
  command! -complete=customlist,PickAName -nargs=* PickRGB :call InsertAColor('<args>', "rgb")
  command! -complete=customlist,PickAName -nargs=* PickHEX :call InsertAColor('<args>', "hex")
  command! -complete=customlist,PickAName -nargs=* PickHSL :call InsertAColor('<args>', "hsl")
  command! -complete=customlist,PickAName -nargs=* PickRAW :call InsertAColor('<args>', "raw")
endif

" GetLatestVimScripts: 3026 1 :AutoInstall: PickAColor.vim
" vim:foldmethod=marker
