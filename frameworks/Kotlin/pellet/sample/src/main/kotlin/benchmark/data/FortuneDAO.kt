package benchmark.data

interface FortuneDAO {

    suspend fun fetchFortunes(): List<Fortune>
}