package com.hexagonkt

import org.testcontainers.containers.GenericContainer
import org.testcontainers.utility.DockerImageName

class DockerContainer(imageName: String) : GenericContainer<DockerContainer>(DockerImageName.parse(imageName))
