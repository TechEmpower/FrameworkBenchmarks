cnxpool = Pools.Pool{LibPQ.Connection}(25)

function cnxisok(cnx::LibPQ.Connection)
    return LibPQ.status(cnx) == LibPQ.libpq_c.CONNECTION_OK
end

function newconnection()
    return LibPQ.Connection(
        "postgresql://benchmarkdbuser:benchmarkdbpass@tfb-database:5432/hello_world",
        # "postgresql://benchmarkdbuser:benchmarkdbpass@localhost:5555/hello_world"
    )
end

function withdb(f)
    connection = Base.acquire(newconnection, cnxpool; isvalid = cnxisok)
    result = f(connection)

    !cnxisok(connection) && LibPQ.reset!(connection)
    Base.release(cnxpool, connection)

    return result
end
