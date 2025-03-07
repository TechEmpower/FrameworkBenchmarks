package com.litongjava.tio.http.server.services;

public class CacheName {
  // `cacheName`（缓存名称）
  private String name;
  // `timeToLiveSeconds`（生存时间）和`timeToIdleSeconds`（闲置时间）。
  private Long timeToLiveSeconds;
  private Long timeToIdleSeconds;

  public CacheName() {
  }

  public CacheName(String name, Long timeToLiveSeconds, Long timeToIdleSeconds) {
    super();
    this.name = name;
    this.timeToLiveSeconds = timeToLiveSeconds;
    this.timeToIdleSeconds = timeToIdleSeconds;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Long getTimeToLiveSeconds() {
    return timeToLiveSeconds;
  }

  public void setTimeToLiveSeconds(Long timeToLiveSeconds) {
    this.timeToLiveSeconds = timeToLiveSeconds;
  }

  public Long getTimeToIdleSeconds() {
    return timeToIdleSeconds;
  }

  public void setTimeToIdleSeconds(Long timeToIdleSeconds) {
    this.timeToIdleSeconds = timeToIdleSeconds;
  }

  @Override
  public String toString() {
    return "CacheName [name=" + name + ", timeToLiveSeconds=" + timeToLiveSeconds + ", timeToIdleSeconds="
        + timeToIdleSeconds + "]";
  }

}