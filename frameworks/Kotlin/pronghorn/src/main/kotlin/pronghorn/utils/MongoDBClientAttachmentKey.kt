package pronghorn.utils

import com.mongodb.async.client.MongoClient
import tech.pronghorn.coroutines.core.WorkerAttachmentKey

object MongoDBClientAttachmentKey : WorkerAttachmentKey<MongoClient>
