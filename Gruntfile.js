module.exports = function(grunt) {

  // Configuration goes here
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),

    jade: {
        compile: {
          pretty : true,
          files: [{
            expand: true, 
            src: "**/*.jade", 
            dest: "priv/public_prod/html/", 
            cwd: "priv/public_dev/html/", 
            ext: '.html'

          }]
        }         
      },

    coffee: {
      compile: {
        files : {
          'priv/public_prod/js/trackr.js': 'priv/public_dev/js/trackr.coffee', // 1:1 compile
          'priv/public_prod/js/bullet.js': 'priv/public_dev/js/bullet.coffee', // 1:1 compile
          'priv/public_prod/js/app.js': ['priv/public_dev/js/app/*.coffee']
        }
      }
    },

    uglify: {
      my_target: {
        files: {
          'priv/public_prod/js/min/trackr.js': ['priv/public_prod/js/trackr.js'],
          'priv/public_prod/js/min/bullet.js': ['priv/public_prod/js/bullet.js'],
          'priv/public_prod/js/min/app.js': ['priv/public_prod/js/app.js']
        }
      }
    },

    lint: {
      files: [
        'priv/public_prod/js/**.js'
      ]
    },

    exec: {
      compile_elixir: {
        command: 'mix compile'
      },
      run_extests : {
        command : 'for i in ./test/*.exs ; do printf "\n\n========\nFile: %s" "$i";  elixir $i; done'
      }
    },

    // Some typical JSHint options and globals
    jshint: {
      options: {
        curly: true,
        eqeqeq: true,
        immed: true,
        latedef: true,
        newcap: true,
        noarg: true,
        sub: true,
        undef: true,
        boss: true,
        eqnull: true,
        browser: true
      },
      globals: {
        jQuery: true
      }
    },

    stylus: {
      compile: {
        //options: {
        //  paths: ['path/to/import', 'another/to/import'],
        //  urlfunc: 'embedurl', // use embedurl('test.png') in our code to trigger Data URI embedding
        //  use: [
        //    require('fluidity') // use stylus plugin at compile time
        //  ],
        //  import: [      //  @import 'foo', 'bar/moo', etc. into every .styl file
        //    'foo',       //  that is compiled. These might be findable based on values you gave
        //    'bar/moo'    //  to `paths`, or a plugin you added under `use`
        //  ]
        //},
        files: {
          'priv/public_prod/css/app.css': ['priv/public_dev/css/*.styl'] // compile and concat into single file
        }
      }
    },

    watch: {
      scripts: {
        files: ['**/*.coffee'],
        tasks: ['coffee', 'uglify'],
        options: {
          spawn: false
        },
      },
      elixir: {
        files: ['**/*.ex'], 
        tasks: ['exec'],
        options: {
          spawn: false
        }
      },
      stylus: {
        files: ['**/*.styl'], 
        tasks: ['stylus'],
        options: {
          spawn: false
        }
      },
      jade: {
        files: ['**/*.jade'], 
        tasks: ['jade'],
        options: {
          spawn: false
        }
      }
      
    }

  });

  // Load plugins here
  grunt.loadNpmTasks('grunt-contrib');
  grunt.loadNpmTasks('grunt-exec');
  grunt.loadNpmTasks('grunt-contrib-jshint');
  grunt.loadNpmTasks('grunt-contrib-stylus');
  grunt.loadNpmTasks('grunt-contrib-jade');

  // Define your tasks here

};