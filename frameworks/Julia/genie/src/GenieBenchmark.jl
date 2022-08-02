module GenieBenchmark

using Genie

const up = Genie.up
export up

function main()
  Genie.genie(; context = @__MODULE__)
end

end
