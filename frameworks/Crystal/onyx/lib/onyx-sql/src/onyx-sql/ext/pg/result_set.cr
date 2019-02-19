require "pg"

class PG::ResultSet < DB::ResultSet
  def read_raw : Bytes | Nil
    col_bytesize = conn.read_i32

    if col_bytesize == -1
      @column_index += 1
      return nil
    end

    sized_io = IO::Sized.new(conn.soc, col_bytesize)

    begin
      slice = Bytes.new(col_bytesize)
      sized_io.read_fully(slice)
    ensure
      conn.soc.skip(sized_io.read_remaining) if sized_io.read_remaining > 0
      @column_index += 1
    end

    slice
  rescue IO::Error
    raise DB::ConnectionLost.new(statement.connection)
  end
end
