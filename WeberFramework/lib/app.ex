defmodule WeberFramework do

  require Weber.Templates.ViewsLoader
  
  def start(_type, _args) do
    # Set resources
    Weber.Templates.ViewsLoader.set_up_resources(File.cwd!)
    # compile all views
    Weber.Templates.ViewsLoader.compile_views(File.cwd!)
    # start weber application
    Weber.run_weber
  end

  def stop(_state) do
    :ok
  end
  
end