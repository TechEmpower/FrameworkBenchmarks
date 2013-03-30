# --- Created by Ebean DDL
# To stop Ebean DDL generation, remove this comment and start using Evolutions

# --- !Ups

create table world (
  id                        bigint not null,
  randomNumber              bigint,
  constraint pk_world primary key (id))
;

create sequence world_seq;




# --- !Downs

SET REFERENTIAL_INTEGRITY FALSE;

drop table if exists world;

SET REFERENTIAL_INTEGRITY TRUE;

drop sequence if exists world_seq;

