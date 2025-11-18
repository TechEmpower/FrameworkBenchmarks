class World < ActiveRecord::Base
  self.table_name = name

  alias_attribute(:randomNumber, :randomnumber) \
    if connection.adapter_name.downcase.start_with?('postgres')

  if connection.adapter_name.downcase.start_with?('mysql')
    def self.upsert_all(attributes, on_duplicate: :update, update_only: nil, returning: nil, unique_by: nil, record_timestamps: nil)
      # On MySQL Batch updates verification isn't supported yet by TechEmpower.
      # https://github.com/TechEmpower/FrameworkBenchmarks/issues/5983
      attributes.each do |attrs|
        where(id: attrs[:id]).update_all(randomNumber: attrs[:randomNumber])
      end
    end
  end
end
