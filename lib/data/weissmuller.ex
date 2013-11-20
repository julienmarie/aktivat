defmodule Data.Weissmuller do
  use GenServer.Behaviour

  defrecord State, indexes: nil, bits: nil, counters: nil, servernames: nil

  def add_servers(ring, servers) do
    Enum.each(servers, fn(server) ->  
       :gen_server.call(:weissmuller, {:prepare_server, ring, server})
    end)
  end

  def add_servers(ring, node, servers) do
    Enum.each(servers, fn(server) ->  
      Architecture.Server.propagate_db(ring, node, server)
    end)
  end

  def add_server(ring, server) do
     :gen_server.call(:weissmuller, {:prepare_server, ring, server})
  end

  def add_server(ring, node, server) do
     Architecture.Server.propagate_db(ring, node, server)
  end

  def receive_db(ring, node, server) do
    :gen_server.call(:weissmuller, {:add_server, ring, node, server})
  end

  def status() do
    :gen_server.call(:weissmuller, :status)
  end

  def start_link() do
    :gen_server.start_link({ :local, :weissmuller }, __MODULE__, [], [])
  end

  def init(_) do
    :global.register_name(binary_to_atom("weissmuller-#{atom_to_binary(node())}"), self)     
    servernames = Enum.map(File.stream!("#{:code.priv_dir(:trackr)}/servers.txt"), &String.rstrip(&1))
    :gen_server.cast(:architecture_server, {:create_ring, "indexes"})
    :gen_server.cast(:architecture_server, {:create_ring, "bits"})
    :gen_server.cast(:architecture_server, {:create_ring, "counters"})
    {:ok, State.new(indexes: HashDict.new(), bits: HashDict.new(), counters: HashDict.new(), servernames: servernames)}
  end

  

  def handle_call({:add_server, ring, thenode, server}, _from, state) do
    t = Enum.filter(state.servernames, fn(x) -> if x == thenode do false else true end end)
    :gen_server.cast(:architecture_server, {:add_node, ring, thenode})
    :hottub.start_link(binary_to_atom(thenode), 5, Data.Redis.Server, :start_link, [[{:ring, ring}, {:servername, thenode}]])
    Enum.each(:gen_server.call(binary_to_atom(thenode), {:list_workers}), fn(x) -> 
      :gen_server.cast(x, {:setup, server})
    end)
    server = case HashDict.has_key?(apply(state, binary_to_atom(ring), []), thenode) do
      true  -> HashDict.put(apply(state, binary_to_atom(ring), []), thenode, HashSet.put(HashDict.fetch!(apply(state, binary_to_atom(ring), []), thenode), server))
      _     -> HashDict.put(apply(state, binary_to_atom(ring), []), thenode, HashSet.new([server]))
    end
    {:reply, {:ok, {thenode,server}}, state.update([{binary_to_atom(ring), server}, {:servernames, t}])}
  end

  def handle_call({:prepare_server, ring, server}, _from, state) do
    [thenode|servernames] = state.servernames
    Architecture.Server.propagate_db(ring, thenode, server)
    {:reply, {ring, thenode, server}, state.update(servernames: servernames)}
  end

  def handle_cast(:sync_dbs, state) do
    case state.bits do
      nil -> :ok
      bits   -> Enum.each(bits, fn({thenode, servers}) -> 
        Enum.each(servers, fn(server) ->
          Architecture.Server.propagate_db("bits", thenode, server) 
        end) 
      end )
    end
    case state.indexes do
      nil -> :ok
      indexes   -> Enum.each(indexes, fn({thenode, servers}) -> 
        Enum.each(servers, fn(server) ->
          Architecture.Server.propagate_db("indexes", thenode, server) 
        end)
      end )
    end
    case state.counters do
      nil -> :ok
      counters   -> Enum.each(counters, fn({thenode, servers}) -> 
        Enum.each(servers, fn(server) ->
          Architecture.Server.propagate_db("counters", thenode, server) 
        end)
      end )
    end
    {:noreply, state}
  end

  def handle_call(:status, _from, state) do
    {:reply, state, state}
  end



end
