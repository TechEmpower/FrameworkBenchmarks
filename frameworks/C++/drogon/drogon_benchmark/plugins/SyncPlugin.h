/**
 *
 *  SyncPlugin.h
 *
 */

#pragma once

#include <drogon/plugins/Plugin.h>

class SyncPlugin : public drogon::Plugin<SyncPlugin>
{
  public:
    SyncPlugin()
    {
    }
    /// This method must be called by drogon to initialize and start the plugin.
    /// It must be implemented by the user.
    virtual void initAndStart(const Json::Value &config) override;

    /// This method must be called by drogon to shutdown the plugin.
    /// It must be implemented by the user.
    virtual void shutdown() override;
};
