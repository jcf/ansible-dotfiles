" Cucumber navigation commands {{{
  Rnavcommand feature features                  -suffix=.feature
" }}}

" Ember.js navigation commands {{{
  Rnavcommand econtroller app/assets/javascripts/controllers -suffix=_controller.js.coffee -default=controller()
  Rnavcommand ehelper     app/assets/javascripts/helpers     -suffix=.js.coffee
  Rnavcommand emodel      app/assets/javascripts/models      -suffix=.js.coffee            -default=model()
  Rnavcommand eroutes     app/assets/javascripts/routes      -suffix=.js.coffee            -default=app_router
  Rnavcommand etemplate   app/assets/javascripts/templates   -suffix=.handlebars           -default=application
  Rnavcommand eview       app/assets/javascripts/views       -suffix=_view.js.coffee       -default=application_view
" }}}

" Uploaders
Rnavcommand uploader app/uploaders  -suffix=_uploader.rb

" Jobs/Workers
Rnavcommand worker app/workers app/jobs -suffix=.rb

" Decorators
Rnavcommand decorator app/decorators -suffix=_decorator.rb

" Serializers
Rnavcommand serializer app/serializers -suffix=_serializer.rb

" Blueprints
Rnavcommand blueprint spec/blueprints -default=model()

" Jasmine
Rnavcommand jasmine spec/javascripts -default=support/jasmine.yml
