command! -nargs=0 Rroutes :Rfind routes.rb
command! -nargs=0 RTroutes :RTfind routes.rb
command! -nargs=0 RSroutes :RSfind routes.rb

command! -nargs=0 Rschema :Rfind db/schema.rb
command! -nargs=0 RTschema :RTfind db/schema.rb
command! -nargs=0 RSschema :RSfind db/schema.rb

Rnavcommand feature     features                  -glob=*    -suffix=.feature
Rnavcommand steps       features/step_definitions -glob=*    -suffix=_steps.rb
Rnavcommand support     features/support          -glob=*

Rnavcommand sass app/stylesheets -suffix=.sass
