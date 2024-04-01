package benchmark.data

interface WorldDAO {

    fun fetchWorld(): WorldDTO
    fun updateWorlds(worlds: List<WorldDTO>)
}