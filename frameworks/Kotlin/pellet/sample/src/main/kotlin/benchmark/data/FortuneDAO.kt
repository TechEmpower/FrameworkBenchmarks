package benchmark.data

interface FortuneDAO {

    fun fetchFortunes(): List<Fortune>
}