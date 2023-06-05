package benchmark.data

interface WorldDAO {

    suspend fun fetchWorld(): WorldDTO
    suspend fun updateWorlds(worlds: List<WorldDTO>)
}