plugins {
    //id("nl.littlerobots.version-catalog-update") version "1.0.1"
    id("com.github.ben-manes.versions") version "0.53.0"
}

tasks.wrapper {
    distributionType = Wrapper.DistributionType.ALL
}
