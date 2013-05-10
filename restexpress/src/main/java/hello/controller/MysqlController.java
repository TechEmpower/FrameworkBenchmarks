package hello.controller;

import hello.domain.World;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

import com.strategicgains.restexpress.Request;
import com.strategicgains.restexpress.Response;

public class MysqlController
{
	// Database details.
	private static final String DB_QUERY = "SELECT * FROM World WHERE id = ?";
	private static final int DB_ROWS = 10000;

	private DataSource mysqlDataSource;

	public MysqlController(DataSource dataSource)
	{
		super();
		this.mysqlDataSource = dataSource;
	}

	public World[] read(Request request, Response response)
	throws SQLException
	{
		final DataSource source = mysqlDataSource;

		// Get the count of queries to run.
		int count = 1;
		try
		{
			count = Integer.parseInt(request.getHeader("queries"));
		
			// Bounds check.
			if (count > 500)
			{
				count = 500;
			}
			if (count < 1)
			{
				count = 1;
			}
		}
		catch(NumberFormatException nfexc)
		{
			// do nothing
		}

		// Fetch some rows from the database.
		final World[] worlds = new World[count];
		final Random random = ThreadLocalRandom.current();

		Connection conn = source.getConnection();
		PreparedStatement statement = conn.prepareStatement(DB_QUERY, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);

		// Run the query the number of times requested.
		for (int i = 0; i < count; i++)
		{
			final Long id = (long) (random.nextInt(DB_ROWS) + 1);
			statement.setLong(1, id);

			try (ResultSet results = statement.executeQuery())
			{
				if (results.next())
				{
					worlds[i] = new World(id, results.getInt("randomNumber"));
				}
			}
		}

		return worlds;
	}
}
