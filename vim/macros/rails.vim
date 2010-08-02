command! -nargs=0 Rroutes :Rfind routes.rb
command! -nargs=0 RTroutes :RTfind routes.rb
command! -nargs=0 RSroutes :RSfind routes.rb

command! -nargs=0 Rschema :Rfind db/schema.rb
command! -nargs=0 RTschema :RTfind db/schema.rb
command! -nargs=0 RSschema :RSfind db/schema.rb

command! -nargs=0 Rconfig :Rfind application.yml
command! -nargs=0 RSconfig :RSfind application.yml

Rnavcommand sass app/stylesheets -suffix=.sass
Rnavcommand ajs app/javascripts -suffix=.js
