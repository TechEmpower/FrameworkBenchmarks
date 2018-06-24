class CreateWorlds < ActiveRecord::Migration[5.2]
  def self.up
    create_table :worlds do |t|
      
      t.timestamps null: false
    end
  end

  def self.down
    drop_table :worlds
  end
end
