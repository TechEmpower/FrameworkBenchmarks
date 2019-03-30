class Models::World
  include Onyx::SQL::Model

  schema world do
    pkey id : Int32
    type random_number : Int32, key: "randomnumber"
  end
end
