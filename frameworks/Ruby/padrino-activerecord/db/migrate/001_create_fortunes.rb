class CreateFortunes < ActiveRecord::Migration[5.2]
  def self.up
    create_table :fortunes do |t|
      
      t.timestamps null: false
    end
  end

  def self.down
    drop_table :fortunes
  end
end
