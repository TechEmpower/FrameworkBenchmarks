require "granite_orm/adapter/pg"

Granite::ORM.settings.database_url = Amber.settings.database_url
Granite::ORM.settings.logger = Amber.settings.logger
