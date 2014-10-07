defmodule Route do

  import Weber.Route
  require Weber.Route
  
  route on("GET", "/json", :WeberFramework.Main, :action_json)
     |> on("GET", "/plaintext", :WeberFramework.Main, :action_text)

end
