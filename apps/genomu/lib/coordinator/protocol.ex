defprotocol Genomu.Coordinator.Protocol do
  @type state
  @type dispatch_to

  alias Genomu.Coordinator, as: GC

  @spec init(dispatch_to) :: {:ok, state}
  def init(arg)

  @spec quorums(dispatch_to, state) :: {:ok, [GC.Quorum.t], state}
  def quorums(dispatch_to, state)

  @spec message(dispatch_to, GC.Quorum.t, state) :: {:ok, message :: term, state}
  def message(dispatch_to, quorum, state)

  @spec handle_response(dispatch_to, response :: term, partition :: term, 
                        GC.Quorum.t, state) :: 
        {:ok, GC.Quorum.t, state} | {:ignore, GC.Quorum.t, state}
  def handle_response(dispatch_to, response, partition, quorum, state)

  @spec finalize(dispatch_to, state) :: {:noreply, state} | 
                                        {:reply, response :: term, state}
  def finalize(dispatch_to, state)
end