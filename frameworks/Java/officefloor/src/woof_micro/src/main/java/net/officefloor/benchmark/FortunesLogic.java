package net.officefloor.benchmark;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * Fortunes logic.
 */
public class FortunesLogic {

	public Fortune[] getFortunes(Connection connection) throws SQLException {
		try (PreparedStatement statement = connection.prepareStatement("SELECT ID, MESSAGE FROM FORTUNE",
				ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
			List<Fortune> fortunes = new ArrayList<>(12);
			fortunes.add(new Fortune(0, "Additional fortune added at request time."));
			ResultSet resultSet = statement.executeQuery();
			while (resultSet.next()) {
				fortunes.add(new Fortune(resultSet.getInt(1), resultSet.getString(2)));
				fortunes.sort((a, b) -> a.getMessage().compareTo(b.getMessage()));
			}
			return fortunes.toArray(new Fortune[fortunes.size()]);
		}
	}

}
