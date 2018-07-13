LuckyFlow.configure do
  settings.stop_retrying_after = 200.milliseconds
  settings.base_uri = Lucky::RouteHelper.settings.base_uri
end
Spec.before_each { LuckyFlow::Server::INSTANCE.reset }
