require "../../spec_helper"

describe PG::Decoders do
  test_decode "array", "'{}'::integer[]", [] of Int32
  test_decode "array", "ARRAY[9]", [9]
  test_decode "array", "ARRAY[8,9]", [8, 9]
  test_decode "array", "'{{9,8},{7,6},{5,4}}'::integer[]",
    [[9, 8], [7, 6], [5, 4]]
  test_decode "array", "'{ {9,8,7}, {6,5,4} }'::integer[] ",
    [[9, 8, 7], [6, 5, 4]]
  test_decode "array", "'{{{1,2},{3,4}},{{9,8},{7,6}}}'::integer[]",
    [[[1, 2], [3, 4]], [[9, 8], [7, 6]]]
  test_decode "array", "ARRAY[1, null, 2] ", [1, nil, 2]
  test_decode "array", "('[3:5]={1,2,3}'::integer[])", [nil, nil, 1, 2, 3]

  it "allows special-case casting on simple arrays" do
    value = PG_DB.query_one("select '{}'::integer[]", &.read(Array(Int32)))
    typeof(value).should eq(Array(Int32))
    value.empty?.should be_true

    value = PG_DB.query_one("select '{1,2,3}'::integer[]", &.read(Array(Int32)))
    typeof(value).should eq(Array(Int32))
    value.should eq([1, 2, 3])

    value = PG_DB.query_one("select '{1,2,3,null}'::integer[]", &.read(Array(Int32?)))
    typeof(value).should eq(Array(Int32?))
    value.should eq([1, 2, 3, nil])

    value = PG_DB.query_one("select '{{1,2,3},{4,5,6}}'::integer[]", &.read(Array(Array(Int32))))
    typeof(value).should eq(Array(Array(Int32)))
    value.should eq([[1, 2, 3], [4, 5, 6]])
  end

  it "reads array as nilable" do
    value = PG_DB.query_one("select '{1,2,3}'::integer[]", &.read(Array(Int32)?))
    typeof(value).should eq(Array(Int32)?)
    value.should eq([1, 2, 3])

    value = PG_DB.query_one("select null", &.read(Array(Int32)?))
    typeof(value).should eq(Array(Int32)?)
    value.should be_nil
  end

  it "raises when reading null in non-null array" do
    expect_raises(PG::RuntimeError) do
      PG_DB.query_one("select '{1,2,3,null}'::integer[]", &.read(Array(Int32)))
    end
  end

  it "errors on negative lower bounds" do
    expect_raises(PG::RuntimeError) do
      PG_DB.query_one("select '[-2:-0]={1,2,3}'::integer[]", &.read)
    end
  end

  test_decode "bool array", "$${t,f,t}$$::bool[]", [true, false, true]
  test_decode "char array", "$${a, b}$$::\"char\"[]", ['a', 'b']
  test_decode "int2 array", "$${1,2}$$::int2[]", [1, 2]
  test_decode "text array", "$${hello, world}$$::text[]", ["hello", "world"]
  test_decode "int8 array", "$${1,2}$$::int8[]", [1, 2]
  test_decode "float4 array", "$${1.1,2.2}$$::float4[]", [1.1_f32, 2.2_f32]
  test_decode "float8 array", "$${1.1,2.2}$$::float8[]", [1.1_f64, 2.2_f64]
end
