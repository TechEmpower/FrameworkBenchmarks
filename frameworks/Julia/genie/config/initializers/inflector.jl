import Inflector, Genie

if ! isempty(Genie.config.inflector_irregulars)
  push!(Inflector.IRREGULAR_NOUNS, Genie.config.inflector_irregulars...)
end