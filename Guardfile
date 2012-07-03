# A sample Guardfile
# More info at https://github.com/guard/guard#readme

require 'guard'
require 'guard/guard'

module ::Guard
  class Abstract < ::Guard::Guard
    def initialize(watchers = [], options = {})
      options[:src] ||= "src"
      options[:test] ||= "test"
      options[:templates] ||= "templates"
      options[:ebin] ||= "ebin"
      super
    end

    def run_all
      perform
    end

    def run_changed
      perform
    end

    def execute(string)
      pipe = IO.popen(string)
      pid = pipe.pid
      pid, status = Process.wait2(pid)
      output = pipe.read
      pipe.close
      return [status, output]
    end

    def perform
      puts "#{self.class} triggered"
      cmd = get_command
      puts "Running: #{cmd}"
      status, output = execute(cmd)
      unless status == 0
        notify(output, :title => self.class.name, :image => :failed)
      end
    end
  end

  class Compile < Abstract
    def get_command
      "./rebar skip_deps=true compile"
    end
  end

  class CT < Abstract
    def get_command
      "./rebar skip_deps=true ct"
    end
  end

  class Eunit < Abstract
    def get_command
      "./rebar skip_deps=true eunit"
    end
  end
end

guard 'compile' do
  watch(%r{src/.*\.erl$})
end

guard 'ct' do
  watch(%r{test/.*_SUITE\.erl$})
  watch(%r{ebin/.*beam$})
end

guard "eunit" do
  watch(%r{test/.*_tests\.erl$})
  watch(%r{ebin/*beam$})
end

guard "compass" do

end
