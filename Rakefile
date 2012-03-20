task :clean do
  sh "rebar clean"
end

task :build => :clean do
  sh "rebar compile"
end

task :spec do
  sh "rebar compile && ERL_LIBS='deps/' ./espec test/spec/"
end

task :default => :build
