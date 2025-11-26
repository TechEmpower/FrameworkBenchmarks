enum class SystemType {
    LINUX_X86_64,
    LINUX_AMD64,
    LINUX_AARCH64,
    OSX_X86_64,
    OSX_AARCH64,
    WINDOWS_AMD64,
    WINDOWS_AARCH64,
    UNKNOWN,
}

data class SystemInfo(
    val type: SystemType,
    val os: OperatingSystem,
) {
    data class OperatingSystem(
        val name: String,
        val arch: String,
    )
}

val CURRENT_SYSTEM_INFO by lazy {
    val os = System.getProperty("os.name").lowercase()
    val arch = System.getProperty("os.arch").lowercase()
    val type = when {
        os.startsWith("linux") && arch == "x86_64" -> SystemType.LINUX_X86_64
        os.startsWith("linux") && arch == "amd64" -> SystemType.LINUX_AMD64
        os.startsWith("linux") && arch == "aarch64" -> SystemType.LINUX_AARCH64
        os.startsWith("mac") && arch == "x86_64" -> SystemType.OSX_X86_64
        os.startsWith("mac") && arch == "aarch64" -> SystemType.OSX_AARCH64
        os.startsWith("windows") && arch == "amd64" -> SystemType.WINDOWS_AMD64
        os.startsWith("windows") && arch == "aarch64" -> SystemType.WINDOWS_AARCH64
        else -> SystemType.UNKNOWN
    }

    SystemInfo(
        type = type,
        os = SystemInfo.OperatingSystem(
            name = os,
            arch = arch,
        )
    )
}

fun resolvePlatformSpecificNettyDependencies(version: String) = when (CURRENT_SYSTEM_INFO.type) {
    SystemType.LINUX_X86_64,
    SystemType.LINUX_AMD64 -> arrayOf(
        "io.netty:netty-transport-native-io_uring:$version:linux-x86_64",
    )
    SystemType.LINUX_AARCH64 -> arrayOf(
        "io.netty:netty-transport-native-io_uring:$version:linux-aarch_64",
    )
    SystemType.OSX_AARCH64 -> arrayOf(
        "io.netty:netty-transport-native-kqueue:$version:osx-aarch_64",
        "io.netty:netty-resolver-dns-native-macos:$version:osx-aarch_64",
    )
    else -> throw IllegalStateException("Unsupported system: $CURRENT_SYSTEM_INFO")
}