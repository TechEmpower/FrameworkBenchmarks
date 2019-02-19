module DB
  module BeginTransaction
    # Creates a transaction from the current context.
    # If is expected that either `Transaction#commit` or `Transaction#rollback`
    # are called explictly to release the context.
    abstract def begin_transaction : Transaction

    # yields a transaction from the current context.
    # Query the database through `Transaction#connection` object.
    # If an exception is thrown within the block a rollback is performed.
    # The exception thrown is blubbled unless it is a `DB::Rollback`.
    # From the yielded object `Transaction#commit` or `Transaction#rollback`
    # can be called explicitly.
    def transaction
      tx = begin_transaction
      begin
        yield tx
      rescue e
        tx.rollback unless tx.closed?
        raise e unless e.is_a?(DB::Rollback)
      else
        tx.commit unless tx.closed?
      end
    end
  end
end
