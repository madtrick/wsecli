task :clean do
  sh "rebar clean"
end

task :build => :clean do
  sh "rebar compile"
end

task :test_features do
  sh "ERL_FLAGS='-config app.config' rebar compile run-features path=test/acceptance skip_deps=true"
end

task :spec do
  sh "rebar compile && ERL_LIBS='deps/' ./espec test/spec/"
end

task :default => :build
