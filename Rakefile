task :clean do
  deprecate
  sh "rebar clean"
end

task :build => :clean do
  deprecate
  sh "rebar compile"
end

task :shell do
  deprecate
  sh "erl -pa ebin deps/*/ebin -s reloader"
end

task :default => :build

def deprecate
  puts "WARNING this task is deprecated and will be removed in future releases. Use its counterpart in the makefile"
end
