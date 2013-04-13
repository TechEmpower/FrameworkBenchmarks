
package com.example.helloworld;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

import org.hibernate.validator.constraints.NotEmpty;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.yammer.dropwizard.config.Configuration;
import com.yammer.dropwizard.db.DatabaseConfiguration;

public class HelloWorldConfiguration
    extends Configuration
{
  @Valid
  @NotNull
  @JsonProperty
  private DatabaseConfiguration database    = new DatabaseConfiguration();

  public DatabaseConfiguration getDatabaseConfiguration()
  {
    return database;
  }
}
